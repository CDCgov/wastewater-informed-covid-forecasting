#!/usr/bin/env python3

import argparse
import cfa.cloudops


def main(pool_name: str, vm_size: str) -> None:
    """
    Set up a pool with a given name
    and default configuration.

    Parameters
    ----------
    pool_name
       name for the pool

    vm_size
       cfa-cloudops VM 't-shirt size' or Azure Batch
       VM name for the pool nodes.

    Returns
    -------
    None
    """

    client = cfa.cloudops.CloudClient(keyvault="cfa-predict")
    client.create_pool(
        pool_name=pool_name,
        vm_size=vm_size,
        mounts=[
            dict(source="wastewater-input", target="input"),
            dict(source="wastewater-ms-output", target="output"),
        ],
        container_image_name="ghcr.io/cdcgov/renewalww:latest",
        max_autoscale_nodes=400,
        low_priority_nodes=0,
        cache_blobfuse=True,
        replace_existing_pool=True,
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

    parser.add_argument(
        "--vm-size",
        type=str,
        help=(
            "cfa-cloudops VM 't-shirt size' or Azure VM "
            "name for the pool nodes"
        ),
        default="small",
    )

    parsed = vars(parser.parse_args())

    main(parsed["pool_name"], parsed["vm_size"])
