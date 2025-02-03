# make preamble via https://tech.davis-hansson.com/p/make/
SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

ifeq ($(origin .RECIPEPREFIX), undefined)
  $(error This Make does not support .RECIPEPREFIX. Please use GNU Make 4.0 or later)
endif
.RECIPEPREFIX = >
.DEFAULT_GOAL := all

######################
# shell configuration
#####################
MKDIR = @mkdir -p
RM = @rm
RMDIR = @rmdir
RSCRIPT = Rscript --vanilla
PYTHON = python3
AZURE_CLI = az
CONTAINER_ENGINE = docker
CONTAINER_IMAGE_BUILD_CMD = $(CONTAINER_ENGINE) build

##############################
# directory and file structure
##############################
CONTAINER_REGISTRY_NAME = cfaprdbatchcr
CTR_REGISTRY_PATH = $(CONTAINER_REGISTRY_NAME).azurecr.io
ACR_LOGIN_COMMAND = $(AZURE_CLI) acr login \
    --name $(CONTAINER_REGISTRY_NAME)

CONTAINER_IMAGE_NAME = renewalww
CONTAINER_REMOTE = $(CTR_REGISTRY_PATH)/$(CONTAINER_IMAGE_NAME):latest

DATA_DIR = input
CONFIG = batch_config.toml
BATCH_DIR = batch

#######################
# Recipes
#######################

CONTAINER_DEPS = Containerfile .containerignore

DATA_DEPS := $(wildcard $(DATA_DIR)/**/*.toml) $(wildcard $(DATA_DIR)/**/*.csv)  $(wildcard $(DATA_DIR)/*.toml) $(wildcard $(DATA_DIR)/*.csv)

.PHONY: echo_data
echo_data:
> @echo $(DATA_DEPS)

.data_remote_up_to_date: $(BATCH_DIR)/upload_data.py $(DATA_DEPS)
> $(PYTHON) $< -rf $(DATA_DIR) $(CONFIG)
> touch $@

.PHONY: .container_local_up_to_date
.container_local_up_to_date: $(CONTAINER_DEPS)
> $(CONTAINER_IMAGE_BUILD_CMD) -t $(CONTAINER_IMAGE_NAME) .
> $(CONTAINER_ENGINE) tag $(CONTAINER_IMAGE_NAME) $(CONTAINER_REMOTE)
> touch $@

.container_remote_up_to_date: .container_local_up_to_date
> $(ACR_LOGIN_COMMAND)
> $(CONTAINER_ENGINE) push $(CONTAINER_REMOTE)
> touch $@

.pool_up_to_date: $(BATCH_DIR)/setup_pool.py $(CONFIG) .container_remote_up_to_date
> $(PYTHON) $< $(CONFIG)
> touch $@

.job_up_to_date: $(BATCH_DIR)/setup_job.py $(CONFIG) .pool_up_to_date .data_remote_up_to_date
> $(PYTHON) $< $(CONFIG)
> touch $@


#######################
# shortcuts
#######################

.PHONY: container_build container_push data_upload pool_update job_update acrlogin

container_build: .container_local_up_to_date

container_push: .container_remote_up_to_date

data_upload: .data_remote_up_to_date

pool_update: .pool_up_to_date

job_update: .job_up_to_date

acr_login:
> $(ACR_LOGIN_COMMAND)


.PHONY: create_pool create_job

.PHONY: clean

clean:
> $(RM) -f *~ #*
> $(RM) -rf __pycache__
> $(RM) -f $(DATA_DIR)/*
> $(MKDIR) $(DATA_DIR)
> $(RMDIR) $(DATA_DIR)
> $(RM) -f .*_up_to_date

all: .job_up_to_date
