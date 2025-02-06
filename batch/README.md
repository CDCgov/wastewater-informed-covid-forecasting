# Running the wastewater forecast evaluation pipeline on Azure Batch
This readme is divided into subsections. Click their headers to expand or collapse them.

<details>
<summary><h2>General setup to interact with Batch</h2>
  
This section guides you through setting up your system (typically the CFA VAP) to run our pipeline. It also recommends some optional tools we have found helpful.
</summary>

### System requirements
This guide assumes you are working on a Debian/Ubuntu family Linux machine or in an equivalent virtual machine (e.g. a WSL2 Ubuntu from Windows). It assumes you are comfortable working at the Unix command line, and are working at the top-level project directory (one level up from the `batch` subdirectory in which this README is located).

### Installing needed command line utilities.

#### Update apt
Before start installing things with `apt`, it's always good practice to update:

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

#### Podman (recommended) or Docker
For the default setup we'll use in this tutorial, there should already be a "container image" for this project in the Azure Container Registry (ACR). To build and push one for yourself, however, you will need an Open Container Initiative (OCI)-compatible container engine, such as `docker` or `podman`. Frustratingly, Azure assumes you are using `docker`, so it requires some commands to start with `docker <command>`. Fortunately, `podman` works as a drop-in replacement if you install `podman-docker`. `podman-docker` simply creates an wrapper application at `/usr/bin/docker` that points to your `podman` installation.

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

> [!WARNING]
> the above is a bit of a hack. The lack of easy interfaces between `podman` and the Azure container registry has been [an open issue for some time](https://github.com/Azure/azure-cli/issues/14768#issue-678300971).

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

Once you have your Python virtual environment set up, install needed Python dependencies by running the following from the top-level project directory.

```bash
pip install -r batch/requirements.txt
```

### Set up environment variables
We'll use the `EnvCredentialHandler` from the [`azuretools`](https://github.com/CDCgov/cfa-azuretools) Python library to handle credentials for CFA Azure resources. It looks for key configuration in your environment variables. CFA's STF Team provide a secret-free (but private) `azureconfig.sh` script to configure environment variables appropriately in their [SharePoint](https://cdc.sharepoint.com/:u:/r/teams/CenterforForecastingandOutbreakAnalytics/Shared%20Documents/General/02%20-%20Predict/Real%20Time%20Monitoring%20(RTM)%20Branch/Short%20Term%20Forecasts/azure/azureconfig.sh?csf=1&web=1&e=e7YBqr). Contact @dylanhmorris if you believe you should have access and do not. We recommend setting these environment variables as part of your Terminal setup.

#### Recommended approach
1. Save `azureconfig.sh` to your Linux user home directory. Confirm this worked by running `ls ~/` and checking that `azureconfig.sh` is among the files listed.
1. Create or open the [`.bash_profile`](https://linuxopsys.com/dotfiles-in-linux-explained) file in your user home directory, i.e. the file located at 
```
~/.bash_profile
```

3. Add the following line to your `.bash_profile`:
```bash
. azureconfig.sh
```
Note the `.`!


Provided you have logged in to Azure at the command line (via `az login`) `EnvCredentialHandler` will be able to retrieve a set of valid credentials on your behalf from an Azure Key Vault. It will do this using the values of the environment variables defined in `azureconfig.sh`.


### Useful Azure GUI applications
We recommend installing two official Azure desktop applications. Both are graphical user inferaces ("GUIs") for Azure resources. They will help you monitor your work on Azure.

#### [Azure Storage Explorer](https://azure.microsoft.com/en-us/products/storage/storage-explorer/)
This application allows you to look at the navigate through the contents Azure blob storage containers as though they were a local files. You can use it to download files or directories, as well as to delete or rename files within Blob storage.

#### [Azure Batch Explorer](https://azure.github.io/BatchExplorer/)
This application allows you to monitor Azure batch "pools" (groups of virtual machines), "jobs" (sets of programs to run on those pools), and "tasks" (individaul components of a job). 

### Azure web portal
You may also want to familiarize yourself with the [Azure web portal](https://portal.azure.com), which you can use in place of the two GUI applications above, as well as for other Azure tasks such as checking the status of OCI containers in an Azure container registry account. That said, we suggest defaulting to using the GUIs, as we find them more user-friendly.

</details>


<details><summary><h2>Step-by-step directions to run an evaluation job on Batch</h2>

This section walks you through running an example evaluation job on Azure Batch.
</summary>

### Prerequisites and checks
Once you have followed the [general set-up instructions](#general-setup-to-interact-with-batch) above, you should:
- Be logged in to Azure. You can check this with `az account show`. 
- Be inside a Python virtual environment in which the dependencies specified in `batch/requirements.txt` have been installed.
- Have appropriately environment variables. You can check this by printing one to the terminal, e.g. via `echo $AZURE_BATCH_ACCOUNT`.

### Getting data into blob storage
You can upload data to blob storage via the Azure Storage Explorer GUI, but if you would like to work programmatically, we provide an `upload_data.py` script. For example

```bash
python3 batch/upload_data.py -g *.csv input/hosp_data wastewater-input
```
will give you the option to upload anything with the `.csv` extension in your local folder `input/hosp_data` to a blob storage container (bucket) named `wastewater-input`. It will use the blob storage account specified `azureconfig.sh`.

Upload any needed input data in the file structure specified by your evaluation configuration file, including the file itself (e.g. `input/config/eval/eval_config.yaml`).

### Containerize and push the container to the container registry
Once your data is in blob storage, we next turn to creating a very different thing, confusingly also called a "container": a Docker-compatible [container](https://www.docker.com/resources/what-container/) in which to run our project code.

We want to make it easy for an arbitrary virtual machine ("node") within Azure to run our code, with minimal set-up. Why? That will in turn makes it easy for us to add and subtract these "nodes" from our job(s) as needed—even automatically!—while trusting that each new one will be able to do its just for us.

There are a number of ways to make it easy for a standard virtual machine to run your code in the way you want. Using Docker-style "containers" is one such solution; we use it here because Azure Batch's infrastructure supports it well. In particular, Azure has its own internal [container registries](https://www.redhat.com/en/topics/cloud-native-apps/what-is-a-container-registry) that nodes can access. We'll put our container in one of those, and then tell our group of Batch nodes (called a "pool") how to retrieve it and run it.


### Building the container image
The first step is building the [container image](https://docs.docker.com/guides/docker-concepts/the-basics/what-is-an-image/). The recipe for this is specified in the repository `Containerfile`.

The default `Containerfile` adds all the code in the repository that's not in the `.containerignore`, which works like a git ignore, with all the interesting stuff (dependencies and package installation) happening in `setup_container.R`.

We want to _build_ this image (locally) and then _tag_ it with a reference to the place we'd like to upload it (namely, within a particular private Azure container registry we own, `cfaprdbatchcr`
The `Makefile` automates this:

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
or just use the Makefile:

```bash
make container_push
```

Look to see if the container is now there there in `portal.azure.com` under `cfaprdbatchcr > services > repositories> renewalww`

### Create the pool
`setup_pool.py` sets up a "pool" of virtual machines on Azure that will actually run your code when asked (by a "task" that forms part of a "job").

`setup_pool.py` needs to know some things from the config file, like how to authenticate, what container to associate with the pool (each pool can be associated with a default container), how to handle autoscaling, how to handle networking, what kind of virtual machines are desired, etc. You can view these specifications under Batch Accounts `cfaprdba > pools > wastewater_runner > jsonview`

Let's create a pool named `wastewater-demo-pool`:
```bash
python3 batch/setup_pool.py wastewater-demo-pool
```

We can now run compute jobs on our `wastewater-demo-pool`.

### Set up a specific job
A job is a set of tasks, each task (by default) gets handed to 1 virtual machine which (by default) runs it within the specified container.

A gotcha: unlike most code run in a container, Batch tasks _don't_ default to starting in the container's own default working directory. In this tutorial, we _would_ like to start our tasks in the container's working directory. For that reason, `setup_job.py` contains [this line](https://github.com/cdcent/cfa-forecast-renewal-ww/blob/91080eaf42ad63f3b1de9e89c6221f58fa55a941/batch/setup_job.py#L70), which explicitly instructs Azure to use the container's default working directory.

In our example, `setup_job.py` creates a bunch of tasks, all of them consist of running the following command for different values of {config_index}:
```
Rscript pipeline/command_line_eval_{script_type}_ww.R {config_index}  input/config/eval/eval_config.yaml input/params.toml
```
Each invocation of that command will perform one of the model fits specified in `eval_config.yaml`; which one depends on the value of `{config_index}`. `setup_job.py` loops over possible values of `{config_index}`, creating tasks for each one.

Let's run `setup_job.py` to create a model fitting job and its constituent tasks. We'll name it `my-demo-fit-job` and have it run on the `wastewater-demo-pool` we just created. Let's image we have a properly formatted configuration file named `eval_config.yaml` stored in `input/config/eval/eval_config.yaml`. You can create one using the [`src/setup_eval.R`](../src/setup_eval.R) R script. Note this must be the same config file you uploaded to blob storage in azure storage container for the wastewater input.

```bash
python3 batch/setup_job.py input/eval_config.yaml fit my-demo-fit-job wastewater-demo-pool
```

This should create a job named `my-demo-fit-job` consisting of tasks that are named by forecast dates, locations, scenarios and the the job type. (here `fit`). Once your fitting job is finished, set up a second job to postprocess it by running:

```bash
python3 batch/setup_job.py input/config/eval/eval_config.yaml post_process my-demo-postprocess-job wastewater-demo-pool
````

Note that you should wait for all tasks in `fit` to finish before kicking off the `post_process` job. Eventually, we may unify these into a single job, in which the postprocess tasks wait for the corresponding fitting tasks to finish, but we have not yet implemented this.

> [!NOTE]
> If you previously have previously used a job and tasks with these names and not deleted them, the script will error, telling you that the tasks already exist. Delete the tasks, delete and re-create the job, or create a new job with a distinct name, e.g. `my-demo-fit-job-2`.

To view all your jobs, navigate in Home to `Batch` > `accounts` > `cfaprdba`> `job_id`, or use the Batch Explorer.
<\details>
