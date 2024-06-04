# PowerShell script to fetch the latest CmdStan release version with enhanced retry logic

# Initialize retry parameters
$max_attempts = 5
$wait_time = 5 # seconds
$version = $null

for ($attempt = 1; $attempt -le $max_attempts; $attempt++) {
    try {
        # Using Invoke-RestMethod to fetch the latest release data
        $response = Invoke-RestMethod -Uri "https://api.github.com/repos/stan-dev/cmdstan/releases/latest" -Method Get -ErrorAction Stop
        $version = $response.tag_name -replace '^v', '' # Remove 'v' from version if present

        # Check if the version is successfully retrieved
        if (-not [string]::IsNullOrWhiteSpace($version)) {
            "CMDSTAN_VERSION=$version" | Out-File -Append -FilePath $env:GITHUB_ENV
            Write-Host "CmdStan latest version: $version"
            break
        }
    } catch {
        # Handle different types of errors
        if ($_.Exception.Response) {
            $statusCode = $_.Exception.Response.StatusCode.value__
            Write-Host "HTTP status code: $statusCode"
        }

        Write-Host "Attempt $attempt of $max_attempts failed. Retrying in $wait_time seconds..."
        Start-Sleep -Seconds $wait_time
        $wait_time = $wait_time * 2 # Exponential backoff
    }
}

# Check if the version was never set and handle the failure
if ([string]::IsNullOrWhiteSpace($version)) {
    Write-Host "Failed to fetch CmdStan version after $max_attempts attempts."
    exit 1
}
