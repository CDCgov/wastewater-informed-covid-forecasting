import argparse
import itertools
from pathlib import Path

import azure.batch.models as batchmodels
import yaml
from azuretools.auth import EnvCredentialHandler
from azuretools.client import get_batch_service_client
from azuretools.task import get_container_settings, get_task_config
from azuretools.util import ensure_listlike


def main(
    eval_config_file: str,
    job_id: str,
    pool_id: str,
    job_type: str,
    exclude_ww_model: bool,
    container_image_name: str = "renewalww",
    container_image_version: str = "latest",
) -> None:
    """
    Create an Azure batch evaluation job according to the given
    YAML-formatted evaluation configuration file

    Parameters
    ----------
    eval_config_file
        Path to the YAML-formatted evaluation configuration file

    job_id
        ID for the batch job to create.

    pool_id
        ID of the batch pool on which to run the job.
        Must already exist.

    job_type
        ``fit`` to run model fitting, ``postprocess`` to
        run post-processing, or ``both`` to run both,
        with postprocess tasks as dependencies of their
        associated fit jobs.

    exclude_ww_model
        If ``True``, fit only the hospital admissions-only model,
        otherwise fit both models.

    container_image_name
        Name of the container to use for the job.
        This container should exist within the Azure
        Container Registry account associated to
        the job. Default 'renewalww'.
        The container registry account name and endpoint
        will be obtained from local environment variables
        via a :class``azuretools.auth.EnvCredentialHandler`.

    container_image_version
        Version of the container to use. Default 'latest'.

    Returns
    -------
    None
        Creating the job and its tasks as a side effect.
    """
    valid_job_types = ["fit", "postprocess", "both"]
    if job_type not in valid_job_types:
        raise ValueError(
            f"Invalid job_type. Must be one of {valid_job_types}, but got {job_type}."
        )

    creds = EnvCredentialHandler()

    batch_service_client = get_batch_service_client(creds)
    uses_deps = job_type == "both"
    job = batchmodels.JobAddParameter(
        id=job_id,
        pool_info=batchmodels.PoolInformation(pool_id=pool_id),
        uses_task_dependencies=uses_deps,
    )

    try:
        batch_service_client.job.add(job)
    except batchmodels.BatchErrorException as err:
        if err.error.code != "JobExists":
            raise
        else:
            print(f"Job {job_id} already exists.")

    container_image = (
        f"{creds.azure_container_registry_account}."
        f"{creds.azure_container_registry_domain}/"
        f"{container_image_name}:{container_image_version}"
    )
    container_settings = get_container_settings(
        container_image,
        working_directory="containerImageDefault",
        mount_pairs=[
            {"source": "input", "target": "/input"},
            {
                "source": "output",
                "target": "/output",
            },
        ],
    )

    with open(eval_config_file, "r") as stream:
        eval_spec = yaml.safe_load(stream)

    for key in [
        "location_ww",
        "forecast_date_ww",
        "scenario",
        "location_hosp",
        "forecast_date_hosp",
    ]:
        eval_spec[key] = ensure_listlike(eval_spec[key])
    log_dir = Path("output", "logs")
    config_name = eval_spec["name_of_config"]

    def add_task(
        R_config_index: int,
        location: str,
        forecast_date: str,
        scenario: str,
        model: str,
        task_type: str,
        uses_task_dependencies: bool,
    ) -> None:
        """
        Helper function to add tasks as we loop through.
        """
        task_name = f"{scenario}-{forecast_date}-{location}"
        task_id = f"{job_id}-{task_type}-{task_name}"
        task_deps = None
        if task_type == "postprocess" and uses_task_dependencies:
            task_deps = batchmodels.TaskDependencies(
                task_ids=[f"{job_id}-fit-{task_name}"]
            )

        base_call = (
            "/bin/sh -c '"
            f"mkdir -p {log_dir}; "
            f"Rscript run_eval.R "
            f"{R_config_index} "
            f"input/config/eval/{config_name}.yaml "
            "input/params.toml "
            f"{model} "
            f"{task_type}"
            f" > {log_dir}/{task_id}-stdout.txt "
            f" 2> {log_dir}/{task_id}-stderr.txt"
            "'"
        )
        task = get_task_config(
            task_id,
            base_call=base_call,
            container_settings=container_settings,
            depends_on=task_deps,
        )
        batch_service_client.task.add(job_id, task)
        return None

    to_run = ["hosp"] if exclude_ww_model else ["ww", "hosp"]
    task_types = (
        ["fit", "postprocess"]
        if job_type == "both"
        else ensure_listlike(job_type)
    )
    for model, task_type in itertools.product(to_run, task_types):
        for i_row, (loc, f_date, scen) in enumerate(
            zip(
                eval_spec[f"location_{model}"],
                eval_spec[f"forecast_date_{model}"],
                eval_spec["scenario"],
            )
        ):
            add_task(
                R_config_index=i_row + 1,  # R is 1-indexed, Python 0-indexed
                location=loc,
                forecast_date=f_date,
                scenario=scen if model == "ww" else "no_wastewater",
                model=model,
                task_type=task_type,
                uses_task_dependencies=uses_deps,
            )
            pass
        pass
    return None


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description=(
            "Set up an Azure batch job from an evaluation configuration file."
        ),
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    )
    parser.add_argument(
        "eval_config_file",
        type=str,
        help="Path to a YAML-formatted configuration file",
    )

    parser.add_argument(
        "job_id", type=str, help="Name for the Azure batch job"
    )

    parser.add_argument(
        "pool_id",
        type=str,
        help="Name of the Azure batch pool on which to run the job",
    )

    parser.add_argument(
        "--job-type",
        type=str,
        default="both",
        help="Type(s) of job to run (`fit`, `postprocess`, or `both`)",
    )

    parser.add_argument(
        "--container-image-name",
        type=str,
        help="Name of the container to use for the job.",
        default="renewalww",
    )

    parser.add_argument(
        "--container-image-version",
        type=str,
        help="Version of the container to use for the job.",
        default="latest",
    )

    parser.add_argument(
        "--exclude-ww-model",
        action=argparse.BooleanOptionalAction,
        help="Exclude the wastewater model from fitting?",
    )
    parsed = vars(parser.parse_args())

    main(**parsed)
