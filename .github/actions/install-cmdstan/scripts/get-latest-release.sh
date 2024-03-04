#!/bin/bash

# Detect the operating system and install jq
if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    sudo apt-get update
    sudo apt-get install -y jq
elif [[ "$OSTYPE" == "darwin"* ]]; then
    brew install jq
else
    echo "Unsupported OS for this script"
    exit 1
fi

# Function to get the latest CmdStan version using GitHub API
get_latest_version() {
    local retries=3
    local wait_time=5
    local status=0
    local version=""

    for ((i=0; i<retries; i++)); do
        version=$(curl -s https://api.github.com/repos/stan-dev/cmdstan/releases/latest | jq -r '.tag_name' | tr -d 'v')
        status=$?
        if [ $status -eq 0 ] && [ -n "$version" ]; then
            echo $version
            return 0
        fi
        sleep $wait_time
        wait_time=$((wait_time*2))
    done

    return 1
}

# Fetch the latest release version of CmdStan
version=$(get_latest_version)

if [ $? -ne 0 ] || [ -z "$version" ]; then
    echo "Failed to fetch the latest CmdStan version"
    exit 1
fi

# Pass the version to the GitHub environment
echo "CMDSTAN_VERSION=$version" >> $GITHUB_ENV

echo "CmdStan latest version: $version"
