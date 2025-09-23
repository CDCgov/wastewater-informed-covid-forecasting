#!/usr/bin/env python3

import argparse

import azuretools.defaults as d
from azure.mgmt.batch import models
from azuretools import blob
from azuretools.auth import EnvCredentialHandler
from azuretools.client import get_batch_management_client


def main(pool_name: str) -> None:
    """
    Set up a pool with a given name
    and default configuration.

    Parameters
    ----------
    pool_name
       name for the pool

    Returns
    -------
    None
    """

    creds = EnvCredentialHandler()
    client = get_batch_management_client(creds)
    node_id_ref = creds.compute_node_identity_reference
    mount_config = blob.get_node_mount_config(
        storage_containers=[
            "wastewater-input",
            "wastewater-ms-output",
        ],
        mount_names=["input", "output"],
        account_names=creds.azure_blob_storage_account,
        identity_references=node_id_ref,
    )
    pool_config = d.get_default_pool_config(
        pool_name=pool_name,
        subnet_id=creds.azure_subnet_id,
        user_assigned_identity=creds.azure_user_assigned_identity,
        mount_configuration=mount_config,
        vm_size=d.default_vm_size,
    )

    d.assign_container_config(
        pool_config,
        models.ContainerConfiguration(
            type="dockerCompatible",
            container_image_names=["ghcr.io/cdcgov/renewalww:latest"],
        ),
    )

    client.pool.create(
        resource_group_name=creds.azure_resource_group_name,
        account_name=creds.azure_batch_account,
        pool_name=pool_name,
        parameters=pool_config,
    )


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description=(
            "Set up an Azure batch pool using the azuretools defaults"
        )
    )
    parser.add_argument(
        "pool_name",
        type=str,
        help="A name for the pool",
    )

    parsed = vars(parser.parse_args())

    main(parsed["pool_name"])
