# Spellcheck Installation Script for Windows
# This script tries multiple methods to install shellcheck

Write-Host "========================================" -ForegroundColor Green
Write-Host "Shellcheck Installation Script" -ForegroundColor Green
Write-Host "========================================" -ForegroundColor Green
Write-Host ""

# Function to test if shellcheck is installed
function Test-Shellcheck {
    try {
        $version = & shellcheck --version 2>$null
        if ($LASTEXITCODE -eq 0) {
            Write-Host "SUCCESS: Shellcheck is already installed!" -ForegroundColor Green
            Write-Host $version
            return $true
        }
    }
    catch {
        return $false
    }
    return $false
}

# Check if already installed
Write-Host "Checking if shellcheck is already installed..." -ForegroundColor Yellow
if (Test-Shellcheck) {
    Write-Host "Shellcheck is working properly!" -ForegroundColor Green
    exit 0
}

Write-Host "Shellcheck not found. Attempting installation..." -ForegroundColor Yellow
Write-Host ""

# Method 1: Try winget (Windows Package Manager)
Write-Host "Method 1: Trying Windows Package Manager (winget)..." -ForegroundColor Cyan
try {
    & winget install --id koalaman.shellcheck --accept-source-agreements --accept-package-agreements 2>&1 | Out-Null
    Write-Host "Winget installation attempted. Checking result..." -ForegroundColor Yellow
    
    # Wait a moment for installation to complete
    Start-Sleep -Seconds 3
    
    if (Test-Shellcheck) {
        Write-Host "SUCCESS: Shellcheck installed via winget!" -ForegroundColor Green
        exit 0
    }
}
catch {
    Write-Host "Winget installation failed or winget not available." -ForegroundColor Red
}

# Method 2: Try Chocolatey
Write-Host ""
Write-Host "Method 2: Checking for Chocolatey..." -ForegroundColor Cyan
try {
    $null = & choco --version 2>$null
    if ($LASTEXITCODE -eq 0) {
        Write-Host "Chocolatey found. Installing shellcheck..." -ForegroundColor Yellow
        & choco install shellcheck -y
        Start-Sleep -Seconds 3
        if (Test-Shellcheck) {
            Write-Host "SUCCESS: Shellcheck installed via Chocolatey!" -ForegroundColor Green
            exit 0
        }
    }
}
catch {
    Write-Host "Chocolatey not available." -ForegroundColor Red
}

# Method 3: Try Scoop
Write-Host ""
Write-Host "Method 3: Checking for Scoop..." -ForegroundColor Cyan
try {
    $scoopVersion = & scoop --version 2>$null
    if ($LASTEXITCODE -eq 0) {
        Write-Host "Scoop found. Installing shellcheck..." -ForegroundColor Yellow
        & scoop install shellcheck
        Start-Sleep -Seconds 3
        if (Test-Shellcheck) {
            Write-Host "SUCCESS: Shellcheck installed via Scoop!" -ForegroundColor Green
            exit 0
        }
    }
}
catch {
    Write-Host "Scoop not available." -ForegroundColor Red
}

# Method 4: Manual download instructions
Write-Host ""
Write-Host "Method 4: Manual Installation Instructions" -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Yellow
Write-Host "If automatic installation failed, please follow these steps:" -ForegroundColor White
Write-Host ""
Write-Host "1. Download shellcheck from: https://github.com/koalaman/shellcheck/releases" -ForegroundColor White
Write-Host "2. Look for the Windows binary (shellcheck-vX.X.X.windows.x86_64.zip)" -ForegroundColor White
Write-Host "3. Extract the zip file" -ForegroundColor White
Write-Host "4. Copy shellcheck.exe to a directory in your PATH" -ForegroundColor White
Write-Host "   (e.g., C:\Windows\System32 or create C:\tools\shellcheck)" -ForegroundColor White
Write-Host "5. Add the directory to your PATH environment variable if needed" -ForegroundColor White
Write-Host ""
Write-Host "Alternative: Install a package manager first:" -ForegroundColor Yellow
Write-Host "- Winget: Should be pre-installed on Windows 10/11" -ForegroundColor White
Write-Host "- Chocolatey: https://chocolatey.org/install" -ForegroundColor White
Write-Host "- Scoop: https://scoop.sh/" -ForegroundColor White

Write-Host ""
Write-Host "========================================" -ForegroundColor Green
Write-Host "Installation Script Complete" -ForegroundColor Green
Write-Host "========================================" -ForegroundColor Green
