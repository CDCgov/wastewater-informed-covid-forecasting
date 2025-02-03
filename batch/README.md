# Running the wastewater forecast evaluation pipeline on Azure Batch

## General setup to interact with Batch.

### System requirements
This guide assumes you are working on a Debian/Ubuntu family Linux machine or in an equivalent virtual machine (e.g. a WSL2 Ubuntu from Windows). It assumes you are comfortable working at the Unix command line, and are working at the top-level project directory (one level up from the `batch` subdirectory in which this README is located).

### Installing needed command line utilities.

#### Update apt
Before start installing things with `apt`, it's always good practice to update:

```bash
sudo apt update
```

#### The Azure CLI
You will need the Azure command line tool `az`. Install it by following Microsoft's [Option 2: Step-by-step installation instructions](https://learn.microsoft.com/en-us/cli/azure/install-azure-cli-linux?pivots=apt#option-2-step-by-step-installation-instructions) tutorial. It finishes with:

```bash
sudo apt-get install azure-cli
```

Confirm that it is installed by checking the output of
```bash
which az
```

#### Podman (recommended) or Docker
For the default settings given in `batch_config.toml`, there should be a already container image for this project in the Azure Container Registry (ACR). To build and push one for yourself, however, you will need an Open Container Initiative (OCI)-compatible container engine, such as `docker` or `podman`. Frustratingly, Azure assumes you are using `docker`, so it requires some commands to start with `docker <command>`. Fortunately, `podman` works as a drop-in replacement if you install `podman-docker`. `podman-docker` simply creates an wrapper application at `/usr/bin/docker` that points to your `podman` installation.

This tutorial uses `podman` as a drop-in replacement for docker. Install it with:

```bash
sudo apt install -y podman
```
Azure expects the container engine to be _named_ `docker`, but `podman` works fine. To use `podman` whenever `docker` is called, set up a symlink:


```bash
sudo ln -s /usr/bin/podman /usr/bin/docker
```
Confirm this works with
```bash
docker --version
```
You should see a message that ends with:

```bash
podman version <A VERSION NUMBER>
```

__Warning__: the above is a bit of a hack. The lack of easy interfaces between `podman` and the Azure container registry has been [an open issue for some time](https://github.com/Azure/azure-cli/issues/14768#issue-678300971).

If you ever decide to replace `podman` with actual `docker`, you may wish to run `sudo rm /usr/bin/docker` to remove the symlink before installing real `docker`.

#### Make
GNU Make is not required to run this tutorial, but the provided `Makefile` can help you with some repetitive tasks, particularly buiding the container. If you would like to use it, install make with:

```bash
sudo apt install -y make
```

You will need at least `make` version `4.0`. Confirm this with

```bash
make --version
```

#### Python
You will need a working installation of Python 3. Install and the default package manager `pip`. Install them with

```bash
sudo apt install -y python3-pip
```

Confirm you have working installations with `which python3` and `which pip`

#### Python virtual environments
If you would like to isolate this project's required dependencies from the rest of your system Python 3 installation, you can use a Python [virtual environment](https://docs.python.org/3/library/venv.html).

With an up-to-date Python installation, you can create one by running the following command in the top-level project directory.
```bash
python3 -m venv .
```

Then activate it by running the following command, also from the top-level project directory.
```bash
source bin/activate
```

Note that if you close and reopen your Terminal window, you may need to reactivate that virtual environment by again running `source bin/activate`.

### Installing Python dependencies.

Once you have your Python set up, you install needed python dependencies by running the following from the top-level project directory.

```bash
pip install -r batch/requirements.txt
```

### Useful GUI applications
For checking your work, it may also be worth installing the [Azure Storage Explorer](https://azure.microsoft.com/en-us/products/storage/storage-explorer/) graphical application. You will also want to familiarize yourself with the [Azure web portal](https://portal.azure.com).

### Log in to Azure
Confirm you can log in to Azure with `az login`

## Step-by-step directions to run a batch job.

Upon successful completion of the set-up instructions above, you should be logged in to azure (`az login` and click on link) and be inside a python virtual environment `python3 -m venv .` `source bin/activate`. You will also want to make sure you have installed python dependencies `pip install -r batch/requirements.txt`.

### Set up your `batch_config.toml`
This configuration file defines many parameters of your Batch setup. While it does not contain secrets, we don't recommend committing it to the repository. It is fine to share among analysts.

Using details in the `[Authentication]` section of the config, the code should be able to retrieve a set of valid credentials on your behalf from an Azure Key Vault on behalf of the user (you), provided the user has run `az login`.

### Getting data into blob storage
`blob_storage.py` defines helper functions that get used in `upload_data.py`. Currently, blob storage container creation is on demand as needed when `upload_data.py` is run. i.e. if the requested upload location doesn't already exist, it tries to create it.

`upload_data.py` is a script used to upload the data according to the structure specified in the `batch_config.toml`. For example

```bash
pyhton3 batch/upload_data.py -g *.csv input/hosp_data batch_config.toml
```
will give you the option to upload anything with the `.csv` extension in your local folder `input/hosp_data` to blob storage. Specifically, it will use the blob storage account specified in the `[Storage]` section of `batch_config.toml` and the specific bucket that you've designed under `input_blob_storage_container_name` in the `[Container]` section.

This is the step where you will want to upload all the needed input data in the file structure specified by your `eval_config_file` (e.g. `input/config/eval/eval_config.yaml`)

### Containerize and push the container to the container registry
Once your data is in blob storage, we next turn to creating a very different thing, confusingly also called a "container": a Docker-compatible [container](https://www.docker.com/resources/what-container/) in which to run our project code.

We want to make it easy for an arbitrary virtual machine ("node") within Azure to run our code, with minimal set-up. Why? That will in turn makes it easy for us to add and subtract these "nodes" from our job(s) as needed—even automatically!—while trusting that each new one will be able to do its just for us.

There are a number of ways to make it easy for a standard virtual machine to run your code in the way you want. Using Docker-style "containers" is one such solution; we use it here because Azure Batch's infrastructure supports it well. In particular, Azure has its own internal [container registries](https://www.redhat.com/en/topics/cloud-native-apps/what-is-a-container-registry) that nodes can access. We'll put our container in one of those, and then tell our group of Batch nodes (called a "pool") how to retrieve it and run it.
### Building the container image
The first step is building the [container image](https://docs.docker.com/guides/docker-concepts/the-basics/what-is-an-image/). The recipe for this is specified in the repository `Containerfile`.

The default `Containerfile` adds all the code in the repository that's not in the `.containerignore`, which works like a git ignore, with all the interesting stuff (dependencies and package installation) happening in `setup_container.R`.

We want to _build_ this image (locally) and then _tag_ it with a reference to the place we'd like to upload it (namely, within a particular private Azure container registry we own, `cfaprdbatchcr`
The `Makefile` automates this, and only rebuilds the container if the source files have changed:
```
make container_build
```
The commands it actually runs (which you can also do manually) are:
```
docker build -t renewalww .
```
and then
```
docker tag renewalww cfaprdbatchcr.azurecr.io/renewalww:latest
```
The first step builds the container and gives it the local name `renewalww`. The second adds a reference to where and what we'll put it in the cloud: `cfaprdbatchcr.azurecr.io/renewalww:latest`.
### Get the container onto the container registry
Now we can get our container into the registry by "`push`-ing" it. First we need to authenticate to our private Azure container registry (here `cfaprdbatchcr`). Note that this is not the same thing as just logging into Azure itself.
```bash
az acr login --name cfaprdbatchcr
```
Again, the Makefile provides a shortcut:
```bash
make acr_login
```
Once you've authenticated, push the container with:
```bash
docker push cfaprdbatchcr.azurecr.io/renewalww:latest
```
or use the Makefile:

```bash
make container_push
```

Look to see if it's there in portal.azure.com cfaprdbatchcr > services > repositories> {name_of_project_container}

### Create the pool
`setup_pool.py` sets up a "pool" of virtual machines on Azure that will actually run your code when asked (by a "task" that forms part of a "job").

`setup_pool.py` needs to know some things from the config file, like how to authenticate, what container to associate with the pool (each pool can be associated with a default container), how to handle autoscaling, how to handle networking, what kind of virtual machines are desired, etc. You can view these specifications under Batch Accounts `cfaprdba > pools > wastewater_runner > jsonview`

To run:
```bash
python3 batch/setup_pool.py batch_config.toml
```
Now we have a pool and we can run jobs on it.

### Set up a specific job
A job is a set of tasks, each task (by default) gets handed to 1 virtual machine which (by default) runs it within the specified container.

A gotcha: unlike most code run in a container, Batch tasks _don't_ default to starting in the container's own default working directory. In this tutorial, we _would_ like to start our tasks in the container's working directory. For that reason, `setup_job.py` contains [this line](https://github.com/cdcent/cfa-forecast-renewal-ww/blob/91080eaf42ad63f3b1de9e89c6221f58fa55a941/batch/setup_job.py#L70), which explicitly instructs Azure to use the container's default working directory.

In our example `setup_job.py` creates a bunch of tasks, all of them consist of running the following command for different values of {config_index}:
```
Rscript pipeline/command_line_eval_{script_type}_ww.R {config_index}  input/config/eval/eval_config.yaml input/params.toml
```
Each invocation of that command will perform one of the model fits specified in `eval_config.yaml`; which one depends on the value of `{config_index}`. `setup_job.py` loops over possible values of `{config_index}`, creating tasks for each one.
We run `setup_job.py` to create the job and its constituent tasks:
```bash
python3 batch/setup_job.py batch_config.toml {local_path_to_eval_config_file} {script_type} {OPTIONAL --exclude_ww_model}
```

This should create a job with the name specified in `batch_config.toml` consistent of tasks that are named by forecast dates, locations, scenarios and the script type, with current options being either `fit` or `post_process`. Currently, we have not set up infrastructure for these to run sequentially automatically, so you must run `fit` and then `post_process`. Note, if you previously used this job name and the tasks have not been deleted, you will be told that the jobs already exist. Either delete the tasks or create a new job (go back to `setup_pool.py` with a modified `batch_config.toml` which has a different `job_id`.

To view the jobs, navigate in Home to Batch accounts > `cfaprdba`> `job_id`
