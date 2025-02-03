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
CTR_REGISTRY_PATH = $(AZURE_CONTAINER_REGISTRY_ACCOUNT).$(AZURE_CONTAINER_REGISTRY_DOMAIN)
ACR_LOGIN_COMMAND = $(AZURE_CLI) acr login \
    --name $(AZURE_CONTAINER_REGISTRY_ACCOUNT)

CONTAINER_IMAGE_NAME = renewalww
CONTAINER_IMAGE_VERSION = latest
CONTAINER_REMOTE = $(CTR_REGISTRY_PATH)/$(CONTAINER_IMAGE_NAME):$(CONTAINER_IMAGE_VERSION)

DATA_DIR = input
BATCH_DIR = batch

#######################
# Recipes
#######################

CONTAINER_DEPS = Containerfile .containerignore

DATA_DEPS := $(wildcard $(DATA_DIR)/**/*.toml) $(wildcard $(DATA_DIR)/**/*.csv)  $(wildcard $(DATA_DIR)/*.toml) $(wildcard $(DATA_DIR)/*.csv)

.PHONY: echo_data
echo_data:
> @echo $(DATA_DEPS)

.PHONY: container_build container_push data_upload pool_update job_update acrlogin

data_upload: $(BATCH_DIR)/upload_data.py $(DATA_DEPS)
> $(PYTHON) $< -rf $(DATA_DIR) $(CONFIG)

container_build: $(CONTAINER_DEPS)
> $(CONTAINER_IMAGE_BUILD_CMD) -t $(CONTAINER_IMAGE_NAME) .
> $(CONTAINER_ENGINE) tag $(CONTAINER_IMAGE_NAME) $(CONTAINER_REMOTE)

container_push: container_build
> $(ACR_LOGIN_COMMAND)
> $(CONTAINER_ENGINE) push $(CONTAINER_REMOTE)
> touch $@

acr_login:
> $(ACR_LOGIN_COMMAND)


#######################
# shortcuts
#######################

.PHONY: clean

clean:
> $(RM) -f *~ #*
> $(RM) -rf __pycache__
> $(RM) -f $(DATA_DIR)/*
> $(MKDIR) $(DATA_DIR)
> $(RMDIR) $(DATA_DIR)

all: container_push
