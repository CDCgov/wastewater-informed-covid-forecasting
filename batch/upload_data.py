#!/usr/bin/env python3

import argparse
import os
from glob import glob

from azuretools.auth import EnvCredentialHandler
from azuretools.blob import (
    create_storage_container_if_not_exists,
    get_blob_service_client,
    upload_to_storage_container,
)


def main(
    source_directory: str,
    storage_container_name: str,
    glob_pattern: str = "**",
    recursive: bool = False,
    force: bool = False,
    dir_root_remote: str = ".",
) -> None:
    """
    Upload data files from a local directory to a
    blob storage container, creating it if it doesn't
    exist, and (virtually) preserving the path structure
    relative to that local directory.

    Parameters
    ----------
    source_directory
        Directory containing files to upload. By default, all
        files within the directory are uploaded, and any
        directory substructure is (virtually) preserved
        via (virtual) blob storage sub-directories.
        Passed as the local_root_dir keyword argument
        to upload_to_storage_container().

    storage_container_name
        Name of the blob storage container to which to
        upload the files.

    glob_pattern
        Pattern to match for files to upload.
        Default '**' (upload all files).

    recursive
        Search for files to upload recursively
        (i.e. in subdirectories of the input
        directory)? Default False.

    force
        Bypass checks and upload without waiting
        for user input. Default False.

    dir_root_remote
        Root directory for the relative file paths
        within the blob storage container (i.e. the
        remote directory whose contents and internal
        structure should mirror that of source_directory).
        Passed as the remote_root_dir keyword argument
        to upload_to_storage_container().
        Default "." (start at the blob storage
        container root)

    Returns
    -------
    None

    Raises
    ------
    Error if insufficient command line arguments provided.
    """
    creds = EnvCredentialHandler()
    client = get_blob_service_client(creds)

    max_display = 10  # max number of files to print

    candidates_to_upload = glob(
        glob_pattern, root_dir=source_directory, recursive=recursive
    )
    files_to_upload = [
        x
        for x in candidates_to_upload
        if not os.path.isdir(os.path.join(source_directory, x))
    ]

    if not force:
        n_total_files = len(files_to_upload)

        if n_total_files > max_display:
            conditional_suffix = ". First {}: ".format(max_display)
        else:
            conditional_suffix = ": "
        print(
            "\n{} files to upload "
            "from local directory '{}' to "
            "blob storage container virtual "
            "directory '{}'"
            "{}\n\n{}".format(
                n_total_files,
                source_directory,
                dir_root_remote,
                conditional_suffix,
                files_to_upload[:max_display],
            )
        )

        print(
            "\nContinue uploading? (Y to continue, anything else"
            " to exit. Not case-sensitive. Run with force=True"
            " to bypass this interactive check.)"
        )
        do_continue = input("").lower() == "y"
    else:
        do_continue = True
    if do_continue:
        create_storage_container_if_not_exists(storage_container_name, client)
        upload_to_storage_container(
            files_to_upload,
            storage_container_name,
            client,
            local_root_dir=source_directory,
            remote_root_dir=dir_root_remote,
        )
    else:
        print("Upload canceled")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description=("Upload files from a directory to Azure blob storage"),
        allow_abbrev=True,
    )
    parser.add_argument(
        "-f",
        "--force",
        action="store_true",
        help="Bypass interactive check before uploading?",
    )
    parser.add_argument(
        "-g",
        "--glob-pattern",
        type=str,
        help=(
            "Glob pattern to use when finding files to upload. "
            "Default '**' (upload everything)"
        ),
        default="**",
    )
    parser.add_argument(
        "-r",
        "--recursive",
        action="store_true",
        help=("Recursively search subdirectories for files to upload?"),
    )
    parser.add_argument(
        "-d",
        "--dir-root-remote",
        type=str,
        default=".",
        help=(
            "Directory within the blob storage"
            "container whose structure should mirror"
            "that of source_directory"
        ),
    )
    parser.add_argument(
        "source_directory",
        type=str,
        help=(
            "Path to the directory from which to upload files "
            "(by default all files)"
        ),
    )
    parser.add_argument(
        "storage_container_name",
        type=str,
        help=("Name of the storage container to which to upload files."),
    )

    parsed = vars(parser.parse_args())
    main(**parsed)
