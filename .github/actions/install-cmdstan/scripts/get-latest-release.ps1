# PowerShell script to fetch the latest CmdStan release version with retry logic

# Initialize retry parameters
$max_attempts = 5
$wait_time = 5 # seconds

for ($attempt = 1; $attempt -le $max_attempts; $attempt++) {
    try {
        $response = Invoke-RestMethod -Uri "https://api.github.com/repos/stan-dev/cmdstan/releases/latest" -ErrorAction Stop
        $version = $response.tag_name -replace '^v', ''

        if (-not [string]::IsNullOrWhiteSpace($version)) {
            "CMDSTAN_VERSION=$version" | Out-File -Append -FilePath $env:GITHUB_ENV
            Write-Host "CmdStan latest version: $version"
            break
        }
    } catch {
        Write-Host "Attempt $attempt of $max_attempts failed. Retrying in $wait_time seconds..."
        Start-Sleep -Seconds $wait_time
        $wait_time = $wait_time * 2
    }
}

if ([string]::IsNullOrWhiteSpace($version)) {
    Write-Host "Failed to fetch CmdStan version after $max_attempts attempts."
    exit 1
}
