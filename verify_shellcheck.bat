@echo off
echo ========================================
echo Shellcheck Installation Verification
echo ========================================
echo.

echo Checking if shellcheck is installed...
shellcheck --version
if %errorlevel% equ 0 (
    echo SUCCESS: Shellcheck is installed and working!
    echo.
    echo Testing shellcheck functionality...
    echo Creating a test script...
    echo echo "Hello World" > test_script.sh
    echo Running shellcheck on test script...
    shellcheck test_script.sh
    echo.
    echo Cleaning up test file...
    del test_script.sh
) else (
    echo FAILED: Shellcheck is not installed or not in PATH
    echo.
    echo Trying alternative installation methods...
    echo.
    echo Method 1: Trying winget installation...
    winget install --id koalaman.shellcheck --accept-source-agreements --accept-package-agreements
    echo.
    echo Method 2: If winget failed, you can try Chocolatey:
    echo choco install shellcheck
    echo.
    echo Method 3: Or download directly from GitHub:
    echo https://github.com/koalaman/shellcheck/releases
    echo.
    echo Method 4: Or use Scoop:
    echo scoop install shellcheck
)

echo.
echo ========================================
echo Verification Complete
echo ========================================
pause
