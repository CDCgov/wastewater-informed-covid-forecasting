import argparse
from pathlib import Path

import azure.batch.models as batchmodels
import yaml
from azuretools.auth import EnvCredentialHandler
from azuretools.client import get_batch_service_client
from azuretools.task import get_container_settings, get_task_config
from azuretools.util import ensure_listlike


def main(
    eval_config_file: str,
    job_type: str,
    job_id: str,
    pool_id: str,
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

    job_type
        Either ``fit`` to run model fitting or ``postprocess`` to
        run post-processing.

    job_id
        ID for the batch job to create.

    pool_id
        ID of the batch pool on which to run the job.
        Must already exist.

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
    valid_job_types = ["fit", "postprocess"]
    if job_type not in valid_job_types:
        raise ValueError(
            f"Invalid job_type. Must be one of {valid_job_types}, "
            f"but got {job_type}."
        )

    creds = EnvCredentialHandler()

    batch_service_client = get_batch_service_client(creds)

    job = batchmodels.JobAddParameter(
        id=job_id,
        pool_info=batchmodels.PoolInformation(pool_id=pool_id),
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
    raw_output_dir = eval_spec["raw_output_dir"]
    log_dir = Path(raw_output_dir, "logs")
    config_name = eval_spec["name_of_config"]

    if not exclude_ww_model:
        # Set up each of the wastewater model run iterations
        for config_row in range(
            0, len(eval_spec["location_ww"])
        ):  # Edit this to test
            R_config_index = config_row + 1  # R is 1-indexed
            this_location = eval_spec["location_ww"][config_row]
            this_forecast_date = eval_spec["forecast_date_ww"][config_row]
            this_scenario = eval_spec["scenario"][config_row]
            task_name = (
                f"{job_type}-{this_scenario}-"
                f"{this_forecast_date}-{this_location}"
            )
            base_call = (
                "/bin/sh -c '"
                f"Rscript run_eval.R "
                f"{R_config_index} "
                f"input/config/eval/{config_name}.yaml "
                "input/params.toml "
                "ww "
                f"{job_type}"
                f" > {log_dir}/stdout-{task_name}.txt "
                f" 2> {log_dir}/stderr-{task_name}.txt"
                "'"
            )
            task = get_task_config(
                f"{job_id}-{task_name}",
                base_call=base_call,
                container_settings=container_settings,
            )
            batch_service_client.task.add(job_id, task)

    # Set up each of the hospital admissions model run iterations
    for config_row in range(
        0, len(eval_spec["location_hosp"])
    ):  # Edit this to test
        R_config_index = config_row + 1  # R is 1-indexed
        this_location = eval_spec["location_hosp"][config_row]
        this_forecast_date = eval_spec["forecast_date_hosp"][config_row]
        this_scenario = "no_wastewater"
        task_name = (
            f"{job_type}-{this_scenario}-"
            f"{this_forecast_date}-{this_location}"
        )
        base_call = (
            "/bin/sh -c '"
            f"Rscript run_eval.R "
            f"{R_config_index}  "
            f"input/config/eval/{config_name}.yaml "
            "input/params.toml "
            "hosp "
            f"{job_type}"
            f" > {log_dir}/{task_name}-stdout.txt "
            f" 2> {log_dir}/{task_name}-stderr.txt"
            "'"
        )
        task = get_task_config(
            f"{job_id}-{task_name}",
            base_call=base_call,
            container_settings=container_settings,
        )
        batch_service_client.task.add(job_id, task)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description=(
            "Set up an Azure batch job from an"
            "evaluation configuration file."
        )
    )
    parser.add_argument(
        "eval_config_file",
        type=str,
        help="Path to a YAML-formatted configuration file",
    )
    parser.add_argument(
        "job_type",
        type=str,
        help="Type of job to run (either `fit` or `postprocess`)",
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
