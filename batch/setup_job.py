import argparse
import itertools

import azure.batch.models as batchmodels
import yaml
from azuretools.auth import EnvCredentialHandler
from azuretools.client import get_batch_service_client
from azuretools.job import create_job
from azuretools.task import get_container_settings, get_task_config
from azuretools.util import ensure_listlike


def base_call(
    location: str,
    forecast_date: str,
    scenario: str,
    model: str,
    task_type: str,
    eval_spec: dict[str, str],
) -> str:
    """
    Construct a base call for a fitting, postprocessing,
    trend fitting, or diff computation job.
    """
    if task_type in ["fit", "postprocess"]:
        return (
            "/bin/sh -c '"
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
            f"--calibration-time {int(eval_spec['calibration_time'])} "
            f"--forecast-horizon {int(eval_spec['forecast_time'])} "
            f"--params-path input/params.toml "
            f"--output-dir {eval_spec['output_dir']} "
            f"--raw-output-dir {eval_spec['raw_output_dir']} "
            f"--seed {int(eval_spec['seed'])} "
            f"--iter-sampling {int(eval_spec['iter_sampling'])} "
            f"--n-chains {int(eval_spec['n_chains'])} "
            f"--adapt-delta {float(eval_spec['adapt_delta'])} "
            f"--max-treedepth {int(eval_spec['max_treedepth'])} "
            f"--scoring-offset {float(eval_spec['scoring_offset'])} "
            f"--task-type {task_type}"
            "'"
        )
    elif task_type == "trendfit":
        return (
            "/bin/sh -c '"
            f"Rscript fit_trend.R "
            f"{forecast_date} "
            f"{location} "
            f"{scenario} "
            f"{int(eval_spec['trend_hosp_lookback_days'])} "
            f"{int(eval_spec['trend_ww_lookback_days'])} "
            f"{eval_spec['raw_output_dir']} "
            f"{eval_spec['output_dir']} "
            f"{int(eval_spec['seed'])} "
            f"--iter-sampling {int(eval_spec['iter_sampling'])} "
            f"--n-chains {int(eval_spec['n_chains'])} "
            f"--adapt-delta {float(eval_spec['adapt_delta'])} "
            f"--max-treedepth {int(eval_spec['max_treedepth'])}"
            "'"
        )
    elif task_type == "diff":
        return (
            "/bin/sh -c '"
            f"Rscript compute_diff.R "
            f"{forecast_date} "
            f"{location} "
            f"{scenario} "
            f"{eval_spec['raw_output_dir']} "
            f"{eval_spec['output_dir']} "
            f"{float(eval_spec['forecast_log_diff_offset'])}"
            "'"
        )
    else:
        raise ValueError(
            f"Unknown task type {task_type}. "
            "Expected one of 'fit', 'trendfit', "
            "'postprocess', or 'diff'"
        )


def main(
    eval_config_file: str,
    job_id: str,
    pool_id: str,
    job_type: str,
    container_image_name: str = "renewalww",
    container_image_version: str = "latest",
    locations_only: list[str] = None,
    models_only: list[str] = None,
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
        ``fit`` to run model fitting, ``trendfit`` to perform
        log-linear fits to data trends, ``postprocess`` to
        run single-fit post-processing, ``diff`` to compute
        differences between paired models (ww and hosp-only),
        or ``all`` to run all, with dependency handling.

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

    models_only
        Models to include in the job, as a list of strings
        containing "ww", "hosp", or both.
        Useful for troubleshooting or for rerunning.
        If not provided, use both models.


    Returns
    -------
    None
        Creating the job and its tasks as a side effect.
    """
    valid_job_types = ["fit", "trendfit", "postprocess", "diff", "all"]
    if job_type not in valid_job_types:
        raise ValueError(
            f"Invalid job_type. Must be one of {valid_job_types}, but got {job_type}."
        )
    if job_type == "all":
        task_types = ["fit", "trendfit", "postprocess", "diff"]
    else:
        task_types = ensure_listlike(job_type)

    creds = EnvCredentialHandler()

    batch_service_client = get_batch_service_client(creds)
    uses_deps = job_type == "all"
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
    log_blob_container = "wastewater-azure-logs"

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
        associated_no_ww_task = f"no_wastewater-{forecast_date}-{location}"
        task_id = f"{job_id}-{task_type}-{task_name}"
        task_deps = None
        if uses_task_dependencies:
            if task_type == "diff":
                task_deps = batchmodels.TaskDependencies(
                    task_ids=list(
                        {
                            f"{job_id}-postprocess-{task_name}",
                            f"{job_id}-postprocess-{associated_no_ww_task}",
                        }
                    )
                )
            elif task_type == "postprocess" and model == "ww":
                task_deps = batchmodels.TaskDependencies(
                    task_ids=[
                        f"{job_id}-fit-{task_name}",
                        f"{job_id}-trendfit-{task_name}",
                    ]
                )
            elif task_type != "fit":
                task_deps = batchmodels.TaskDependencies(
                    task_ids=[
                        f"{job_id}-fit-{task_name}",
                    ]
                )
        call = base_call(
            location, forecast_date, scenario, model, task_type, eval_spec
        )
        task = get_task_config(
            task_id,
            base_call=call,
            container_settings=container_settings,
            depends_on=task_deps,
            log_blob_container=log_blob_container,
            log_blob_account=creds.azure_blob_storage_account,
            log_subdir=job_id,
            log_compute_node_identity_reference=creds.compute_node_identity_reference,
        )
        batch_service_client.task.add(job_id, task)
        return None

    possible_tasks = itertools.product(["ww", "hosp"], task_types)

    def task_filter(task):
        model, task_type = task
        model_valid = models_only is None or model in models_only
        task_type_invalid = (
            task_type in ["trendfit", "diff"] and model == "hosp"
        )
        return model_valid and not task_type_invalid

    tasks_to_create = filter(task_filter, possible_tasks)

    for model, task_type in tasks_to_create:
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
        default="all",
        help="Type(s) of job to run (`fit`, `trendfit`, `postprocess`, `diff`, or `all`.)",
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
        "--models-only",
        type=str,
        help=(
            "Models to include in the job, as a whitespace-"
            "separated string containing 'ww', 'hosp', "
            "or both. Useful for troubleshooting or rerunning. "
            "If not provided, use both models."
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

    parsed = parser.parse_args()
    if parsed.locations_only is not None:
        parsed.locations_only = parsed.locations_only.split()
    if parsed.models_only is not None:
        parsed.models_only = parsed.models_only.split()
    main(**vars(parsed))
