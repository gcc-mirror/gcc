@echo off
echo ========================================
echo Shellcheck Functionality Test
echo ========================================
echo.

echo Testing if shellcheck is installed...
shellcheck --version >nul 2>&1
if %errorlevel% equ 0 (
    echo SUCCESS: Shellcheck is installed!
    echo.
    
    echo Getting shellcheck version:
    shellcheck --version
    echo.
    
    echo Testing shellcheck on problematic script...
    echo ----------------------------------------
    shellcheck test_script_with_issues.sh
    echo.
    
    echo Testing shellcheck on improved script...
    echo ----------------------------------------
    shellcheck test_script_improved.sh
    echo.
    
    echo Testing different output formats...
    echo ----------------------------------------
    echo JSON format:
    shellcheck -f json test_script_with_issues.sh
    echo.
    
    echo GCC format:
    shellcheck -f gcc test_script_with_issues.sh
    echo.
    
    echo SUCCESS: Shellcheck is working properly!
    
) else (
    echo FAILED: Shellcheck is not installed or not working
    echo.
    echo Please run one of these installation scripts:
    echo - verify_shellcheck.bat
    echo - install_shellcheck.ps1
    echo.
    echo Or install manually from: https://github.com/koalaman/shellcheck/releases
)

echo.
echo ========================================
echo Test Complete
echo ========================================
pause
