import argparse
import itertools
from pathlib import Path

import azure.batch.models as batchmodels
import yaml
from azuretools.auth import EnvCredentialHandler
from azuretools.client import get_batch_service_client
from azuretools.task import get_container_settings, get_task_config
from azuretools.util import ensure_listlike
from azuretools.job import create_job

def main(
    eval_config_file: str,
    job_id: str,
    pool_id: str,
    job_type: str,
    exclude_ww_model: bool,
    container_image_name: str = "renewalww",
    container_image_version: str = "latest",
    locations_only: list[str] = None,
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

    locations_only
        Locations to include in the job, as a list of strings
        containing USPS two-letter codes to include.
        Useful for troubleshooting or for rerunning.
        If not provided, use all locations specified
        in the config.

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

    create_job(batch_service_client, job)

    container_image = (
        f"ghcr.io/cdcgov/{container_image_name}:{container_image_version}"
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

    def add_task(
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
            f"--forecast-date {forecast_date} "
            f"--eval-date {eval_spec['eval_date']} "
            f"--location {location} "
            f"--model {model} "
            f"--scenario {scenario} "
            f"--hosp-data-dir {eval_spec['hosp_data_dir']} "
            f"--ww-data-dir {eval_spec['ww_data_dir']} "
            f'--ww-data-mapping "{eval_spec["ww_data_mapping"]}" '
            f"--scenario-dir {eval_spec['scenario_dir']} "
            f"--calibration-time {eval_spec['calibration_time']} "
            f"--forecast-horizon {eval_spec['forecast_time']} "
            f"--params-path input/params.toml "
            f"--output-dir {eval_spec['output_dir']} "
            f"--raw-output-dir {eval_spec['raw_output_dir']} "
            f"--seed {eval_spec['seed']} "
            f"--iter-sampling {eval_spec['iter_sampling']} "
            f"--n-chains {eval_spec['n_chains']} "
            f"--adapt-delta {eval_spec['adapt_delta']} "
            f"--max-treedepth {eval_spec['max_treedepth']} "
            f"--task-type {task_type} "
            f"> {log_dir}/{task_id}-stdout.txt "
            f"2> {log_dir}/{task_id}-stderr.txt"
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
        for loc, f_date, scen in zip(
            eval_spec[f"location_{model}"],
            eval_spec[f"forecast_date_{model}"],
            eval_spec["scenario"],
        ):
            if locations_only is None or loc in locations_only:
                add_task(
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
        "--locations-only",
        type=str,
        help=(
            "Two-letter USPS location abbreviations to "
            "include in the job, as a whitespace-separated "
            "string. Useful for troubleshooting or for rerunning. "
            "If not provided, use all locations specified "
            "in the config."
        ),
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
    parsed = parser.parse_args()
    if parsed.locations_only is not None:
        parsed.locations_only = parsed.locations_only.split()
    main(**vars(parsed))
