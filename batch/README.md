# Running the wastewater forecast evaluation pipeline on Azure Batch
This readme is divided into the following subsections:
- [General setup for interacting with CFA's Azure resources](#general-setup-for-interacting-with-cfas-azure-resources)
- [Walkthrough: running an evaluation job on Azure Batch](#walkthrough-running-an-evaluation-job-on-azure-batch)
- [Customizing and configuring the evaluation pipeline](#customizing-and-configuring-the-evaluation-pipeline)


## General setup for interacting with CFA's Azure resources

This section guides you through setting up your system (typically the CFA VAP) to run our pipeline. It also recommends some optional tools we have found helpful.

### System requirements
This guide assumes you are working on a Debian/Ubuntu family Linux machine or in an equivalent virtual machine (e.g. a WSL2 Ubuntu from Windows). It assumes you are comfortable working at the Unix command line. Unless otherwise specified, it expected you to run commands from the top-level project directory of this project / repository (i.e. one level up from the `batch` subdirectory in which this README is located).

### Installing needed command line utilities.

#### Update `apt`
Before installing things with `apt`, it's good to update package lists:

```bash
sudo apt update
```

#### The Azure CLI and logging in to Azure
You will need the Azure command line tool `az`. Install it by following Microsoft's [Option 2: Step-by-step installation instructions](https://learn.microsoft.com/en-us/cli/azure/install-azure-cli-linux?pivots=apt#option-2-step-by-step-installation-instructions) tutorial. Once you have followed it, confirm that `az` has been successfully installed by running

```bash
which az
```

Now confirm you can log in to Azure at the command line. Run

```bash
az login
```
This will prompt you to go to a website to log in. A browser window will open automatically if you have one set up. CFA VAP WSL2 setups don't have a browser set up by default, so you may need to click on or copy paste the link you see in the terminal into a browser you open manually. In the browser, log in with your `@ext.cdc.gov` account when prompted. Once you have logged in, you can return to the command prompt.

#### Credential handling
The batch `setup_job.py` and `setup_pool.py` scripts require you to:
- Be within the `ext.cdc.gov` subdomain (typically via working on the CFA VAP).
- Be logged into an Azure account with access to the `cfa-predict` Azure keyvault.
- Be logged into an Azure account with privileges to create pools, jobs, and tasks on within the default batch account specified in that keyvault.

Provided all of that is true, the pool creation and job creation scripts should run succesfully without need for further authentication or configuration on your part. If you encounter authentication or privileges errors and have checked the above conditions, speak to a CFA Predict technical administrator.

#### Python
You will need a working installation of Python 3. Install it with:

```bash
sudo apt install -y python3-pip
```

Confirm you have a working installation by running `python3` at the command line.

#### Python virtual environments
If you would like to isolate this project's required dependencies from the rest of your system Python 3 installation, you can use a Python virtual environment. This project is set up to use [`uv`](https://docs.astral.sh/uv/). Follow official instructions there to download and install it, and confirm you have it with `which uv`. The rest of this tutorial will assume you are using `uv`. If you are not, you will have to install dependencies against your system Python (or in your preferred virtual environment of choice), and replace all `uv run python` commands the appropriate `python`/`python3` etc command for your environment.


### Useful Azure GUI applications
We recommend installing two official Azure desktop applications. Both are graphical user inferaces ("GUIs") for Azure resources. They will help you monitor your work on Azure.

#### Azure Storage Explorer ([link to download](https://azure.microsoft.com/en-us/products/storage/storage-explorer/))
This application allows you to look at the navigate through the contents Azure blob storage containers as though they were a local files. You can use it to download files or directories, as well as to delete or rename files within Blob storage.

#### Azure Batch Explorer ([link to download](https://azure.github.io/BatchExplorer/))
This application allows you to monitor Azure batch "pools" (groups of virtual machines), "jobs" (sets of programs to run on those pools), and "tasks" (individaul components of a job).

### Azure web portal
You may also want to familiarize yourself with the [Azure web portal](https://portal.azure.com), which you can use in place of the two GUI applications above, as well as for other Azure tasks such as checking the status of OCI containers in an Azure container registry account. That said, we suggest defaulting to using the GUIs, as we find them more user-friendly.


## Walkthrough: running an evaluation job on Azure Batch

This section walks you through running an example evaluation job on Azure Batch.

### Prerequisites and checks
Once you have followed the [general set-up instructions](#general-setup-for-interacting-with-cfas-azure-resources) above, you should:
- Be logged in to Azure. You can check this via:
```bash
az account show
```
- Be inside a Python virtual environment in which the dependencies specified in `pyproject.toml` have been installed.


### Configuration files
We specify evaluation jobs using [YAML-formatted](https://yaml.org/spec/) configuration files. These tell the pipeline to make and evaluate forecasts for one or more individual "forecasting problems". A forecasting problem is a forecast for a particular location as of a particular date using a particular model and particular set of available data.

In our manuscript analysis, the as-of dates are Mondays in 2023-2024, the locations are individual U.S. states, dictricts, or territories, the data is all data that would have been available on the Monday as-of date, and the models are a hospital admissions-only model and a wastewater-informed model.

We provide an `example_eval_config.yaml` within this repo at `input/config/eval/example_eval_config.yaml`, and a copy is pre-uploaded to the `wastewater-input` Blob storage container within the `cfaazurebatchprd` Blob storage account. We'll use that configuration file in this walkthrough. When you're ready to run your own custom jobs, see the section on [creating your own configuration file](#creating-a-configuration-file) below.


### Create a pool
`setup_pool.py` sets up a "pool" of virtual machines on Azure that will actually run your code when asked (by a "task" that forms part of a "job").

`setup_pool.py` needs to know some things from the config file, like how to authenticate, what container to associate with the pool (each pool can be associated with a default container), how to handle autoscaling, how to handle networking, what kind of virtual machines are desired, etc. You can view these specifications under Batch Accounts `cfaprdba > pools > wastewater_runner > jsonview` or under `Pools` in the [Azure Batch Explorer](#azure-batch-explorer) application.

We'll create a pool named `wastewater-demo-pool`:
```bash
uv run python batch/setup_pool.py wastewater-demo-pool
```

We can now run compute jobs on our `wastewater-demo-pool`.

### Set up a specific job
A job is a set of tasks. Each task (by default) gets handed to 1 "node" (virtual machine) which (by default) runs it within a specified [OCI container](https://en.wikipedia.org/wiki/Open_Container_Initiative). "Containers" in this sense are a way of packaging code so it can run easily on a variety of operating systems / computers. For more on containers and how to customize the one we're use here, see ["Building the container image"](#building-the-container-image) below.

> [!NOTE]
> Containers have a default working directory. Azure Batch tasks in general _don't_ default to starting in the container's own default working directory. In this tutorial, we _would_ like to start our tasks in the container's working directory. But we are interacting with Batch via `cfa-cloudops`, which explicitly asks Azure to use the container's default working directory.

In our example, `setup_job.py` creates a bunch of tasks. All of them consist of running the `run_eval.R` script for a given model fitting or postprocessing problem. To see a detailed help message that lists all the arguments, run
```bash
Rscript run_eval.R --help
```

Here, we give a few examples. This command runs model fitting with the wastewater model for Ohio on forecast data 2023-10-23:
```
Rscript run_eval.R --forecast-date 2023-10-23 --eval-date 2025-03-10 --location OH --model ww --scenario status_quo --hosp-data-dir input/hosp_data/vintage_datasets --ww-data-dir input/ww_data/monday_datasets --ww-data-mapping "Monday: Monday, Wednesday: Monday" --scenario-dir input/config/eval/scenarios --calibration-time 90.0 --forecast-horizon 28.0 --params-path input/params.toml --output-dir output/eval_latest --raw-output-dir output/eval_latest/raw_output --seed 123.0 --iter-sampling 500.0 --n-chains 4.0 --adapt-delta 0.95 --max-treedepth 12.0 --task-type fit
```
This command runs model post-processing with the hospital admissions-only model for Arizona with forecast date 2023-10-16:
```
Rscript run_eval.R --forecast-date 2023-10-16 --eval-date 2025-03-10 --location AZ --model hosp --scenario no_wastewater --hosp-data-dir input/hosp_data/vintage_datasets --ww-data-dir input/ww_data/monday_datasets --ww-data-mapping "Monday: Monday, Wednesday: Monday" --scenario-dir input/config/eval/scenarios --calibration-time 90.0 --forecast-horizon 28.0 --params-path input/params.toml --output-dir output/eval_latest --raw-output-dir output/eval_latest/raw_output --task-type postprocess
```

The file [`input/params.toml`](../input/params.toml) specifies hyperparameters for priors and other model configuration that is shared across individual forecasting problems. It is tracked in this repo, so you should already have a copy.

To save you writing this all out by hand for each forecasting problem, `setup_job.py` loops over all the forecast problems in `input/config/eval/example_eval_config.yaml`, creating tasks for each one.

By default, it creates a set of model fitting tasks and their associated postprocessing tasks for all entries in the specified config file. It can be configured to set up only fitting jobs, only postprocessing jobs, or only jobs for certain locations. Running `uv run python batch/setup_job.py --help` displays a full help message.

#### Model fitting and postprocessing
Let's run `setup_job.py` to create a model fitting and postprocessing job. We'll name our job `my-demo-job` and have it run on the `wastewater-demo-pool` we just created. We'll use our local copy of the example configuration file (`example_eval_config.yaml`) and the corresponding copy of it Blob storage container `wastewater-input`.

```bash
uv run python batch/setup_job.py input/config/eval/example_eval_config.yaml my-demo-job wastewater-demo-pool
```

This should create a job named `my-demo-job` consisting of tasks that are named by forecast dates, locations, scenarios and task type. (either `fit` or `postprocess`). Confirm that this has happened by looking for the job and its tasks in the Azure Batch Explorer or in the Batch section of the Azure web portal.

> [!CAUTION]
> If you or someone else previously have previously created a job and tasks with these names the script will error, telling you that the tasks already exist. To fix this, delete the tasks, delete and re-create the job, or create a new job with a distinct name, e.g. `my-demo-fit-job-2`.

Once your job is finished, examine the `wastewater-example-output` Blob storage container and confirm that output files have been generated.

If you watch the tasks in action (e.g. via the Batch Explorer), you'll see that the `postprocess` tasks do not kick off until after their associated `fit` tasks have finished. This is because `setup_job.py` is configured to make [the postprocess tasks "depend" on their associated fit tasks](https://learn.microsoft.com/en-us/azure/batch/batch-task-dependencies).

## Customizing and configuring the evaluation pipeline

This section explains how to customize and configure the pipeline.

### Command line arguments to `setup_job.py`
`setup_job.py` takes a number of command line arguments, as follows:

```bash
usage: setup_job.py [-h] [--job-type JOB_TYPE]
                    [--locations-only LOCATIONS_ONLY]
                    [--container-image-name CONTAINER_IMAGE_NAME]
                    [--container-image-version CONTAINER_IMAGE_VERSION]
                    [--exclude-ww-model | --no-exclude-ww-model]
                    eval_config_file job_id pool_id

Set up an Azure batch job from an evaluation configuration file.

positional arguments:
  eval_config_file      Path to a YAML-formatted configuration file
  job_id                Name for the Azure batch job
  pool_id               Name of the Azure batch pool on which to run the job

options:
  -h, --help            show this help message and exit
  --job-type JOB_TYPE   Type(s) of job to run (`fit`, `postprocess`, or
                        `both`) (default: both)
  --locations-only LOCATIONS_ONLY
                        Two-letter USPS location abbreviations to include in
                        the job, as a whitespace-separated string. Useful for
                        troubleshooting or for rerunning. If not provided, use
                        all locations specified in the config. (default: None)
  --container-image-name CONTAINER_IMAGE_NAME
                        Name of the container to use for the job. (default:
                        renewalww)
  --container-image-version CONTAINER_IMAGE_VERSION
                        Version of the container to use for the job. (default:
                        latest)
  --exclude-ww-model, --no-exclude-ww-model
                        Exclude the wastewater model from fitting? (default:
                        None)
```

As the message suggests, you can view this help message by running

```bash
uv run python setup_job.py -h
```

#### Custom container images and versions.
The default values of `--container-image-name` and `--container-image-version` are `renewalww` and `latest`, respectively. This corresponds to the container image built and pushed via Github actions that reflecting the current state of `prod`.

#### Custom job types
The default `--job-type`, `both`, means running fitting followed by postprocessing tasks. We could override it to set up a fitting-only job:

```bash
uv run python batch/setup_job.py input/config/eval/example_eval_config.yaml my-demo-fit-job wastewater-demo-pool --job-type fit
```

or a manual postprocessing-only job:

```bash
uv run python batch/setup_job.py input/config/eval/example_eval_config.yaml my-demo-postprocess-job wastewater-demo-pool --job-type postprocess
```

Note that if you run a manual `fit`-only job followed by a manual `postprocess`-only job, you will need to confirm manually that fitting tasks have finished before kicking off their associated postprocessing tasks. In general, only kick off a manual postprocessing job once the entire associated manual fitting job has completed.

### Creating a configuration file
The [`src/setup_eval.R`](../src/setup_eval.R) script can help you write properly formatted evaluation configuration YAML files. Remember to mirror config versions between your local `input/config/eval` directory and the one in your input Azure Blob storage container.

### Uploading data
The [walkthrough](#walkthrough-running-an-evaluation-job-on-azure-batch) uses data and configuration that are already in Azure Blob Storage. You can upload data to blob storage via the [Azure Storage Explorer](#azure-storage-explorer) GUI, but if you would like to work programmatically, we provide an `upload_data.py` script.

For example

```bash
uv run python batch/upload_data.py -g *.csv input/hosp_data wastewater-input
```

will give you the option to upload anything with the `.csv` extension in your local folder `input/hosp_data` to a blob storage container (bucket) named `wastewater-input`. It will use the blob storage account specified `azureconfig.sh`.

Upload any needed input data in the file structure specified by your evaluation configuration file, including the file itself (e.g. `input/config/eval/eval_config.yaml`).

### Building a job container
Now that data is in blob storage, we next turn to creating a very different thing, confusingly also called a "container": a Docker-compatible [container](https://www.docker.com/resources/what-container/) in which to run our project code.

We want to make it easy for an arbitrary virtual machine ("node") within Azure to run our code, with minimal set-up. Why? That will in turn makes it easy for us to add and subtract these "nodes" from our job(s) as needed—even automatically!—while trusting that each new one will be able to do its just for us.

There are a number of ways to make it easy for a standard virtual machine to run your code in the way you want. Using Docker-style "containers" is one such solution; we use it here because Azure Batch's infrastructure supports it well. In particular, Azure has its own internal [container registries](https://www.redhat.com/en/topics/cloud-native-apps/what-is-a-container-registry) that nodes can access. We'll put our container in one of those, and then tell our group of Batch nodes (called a "pool") how to retrieve it and run it.

For the default setup we used in this tutorial, there was already be a "container image" for this project in the Azure Container Registry (ACR). Container images reflecting the `prod` branch and all open pull requests are [built via Github actions](../.github/workflows/container-build-push.yaml) and pushed to the [Github Container Registry](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-container-registry) as `renewalww:latest` (for `prod`) and `renewalww:{name of the PR branch}` for pull requests. [Here's a link to the currently available builds](../../../pkgs/container/renewalww).

You might, however, want to build a container image locally, either for local testing or for your own understanding. This section walks you through doing so.

#### Podman (recommended) or Docker

To build and push a custom container image, you will need an Open Container Initiative (OCI)-compatible container engine, such as `docker` or `podman`.

This tutorial uses `podman` as a drop-in replacement for docker. Install it with:

```bash
sudo apt install -y podman
```

#### Make
GNU Make is not required to run this tutorial, but the provided `Makefile` can help you with some repetitive tasks, particularly building the container. If you would like to use it, install make with:

```bash
sudo apt install -y make
```

You will need at least `make` version `4.0`. Confirm this with

```bash
make --version
```

### Building the container image
The first step is building the [container image](https://docs.docker.com/guides/docker-concepts/the-basics/what-is-an-image/). The recipe for this is specified in the repository `Containerfile`.

The default `Containerfile` adds all the code in the repository that's not in the `.containerignore`, which works like a git ignore, with all the interesting stuff (dependencies and package installation) happening in `setup_container.R`.

We want to _build_ this image (locally) and then _tag_ it with a reference to the place we'd like to upload it (namely, within a particular private Azure container registry we own, `cfaprdbatchcr`.

The `Makefile` automates this.

```
make container_build
```

The commands it actually runs (which you can also run manually) are:

```
podman build -t renewalww .
```
and then

```
podman tag renewalww ghcr.io/cdcgov/renewalww:latest
```
The first command builds the container and gives it the local name `renewalww`. The second adds a reference to where and what we'll put it in the cloud: `ghcr.io/cdcgov/renewalww:latest`.

Throughout, the Makefile uses `podman` as its container engine. If you prefer a different engine (such as `docker`), run the variable `make` commands with the variable `DOCKER_COMMAND` set your preferred engine, e.g.:

```
make container_build DOCKER_COMMAND=docker
```

### Get the container onto the container registry
Now we can get our container into the registry by "`push`-ing" it. First we need to authenticate to the Github container registry. You will need a [Github Personal Access Token (classic)](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens). Set one up and then follow the instructions [here to log in to `ghcr.io`](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-container-registry#authenticating-with-a-personal-access-token-classic). Github recommends storing it in an environment variable. Here and in the `Makefile`, we assume you've stored your token in the environment variable `GH_PAT` and your github username in the environment variable `GH_USERNAME`.

```bash
docker login ghcr.io -u $GH_USERNAME -p $GH_PAT
```

The Makefile provides a shortcut:

```bash
make ghcr_login
```

`make ghcr_login` sets `DOCKER_COMMAND` to `podman` for you unless you explicitly override this, e.g.:

```
make ghcr_login DOCKER_COMMAND=docker
```

Once you've authenticated, push the container with:

```bash
podman push ghcr.io/cdcgov/wastewater-informed-covid-forecasting/renewalww:latest
```

or just use the Makefile:

```bash
make container_push
```

Confirm that the container is now present in the registry by navigating to the [`renewalww` container page in this repo](../../..//pkgs/container/renewalww).
