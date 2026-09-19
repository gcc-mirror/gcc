# Shellcheck Usage Guide

## What is Shellcheck?
Shellcheck is a static analysis tool for shell scripts that finds bugs in your bash/sh shell scripts. It helps identify common issues like:
- Syntax errors
- Semantic problems
- Suggestions for improvement
- Portability issues

## Basic Usage

### Command Syntax
```bash
shellcheck [options] script.sh
```

### Common Options
- `-f format` : Output format (checkstyle, diff, gcc, json, json1, quiet, tty)
- `-e CODE` : Exclude specific error codes
- `-S severity` : Minimum severity (error, warning, info, style)
- `-s shell` : Specify shell dialect (bash, sh, dash, ksh)
- `--color=auto` : Enable colored output

## Example Shell Scripts to Test

### 1. Simple Script with Issues
```bash
#!/bin/bash
# test_script_with_issues.sh

echo "Hello World"
if [ $1 = "test" ]; then
    echo "Test mode"
fi

for i in $(ls *.txt); do
    echo $i
done
```

### 2. Improved Script
```bash
#!/bin/bash
# test_script_improved.sh

echo "Hello World"
if [ "$1" = "test" ]; then
    echo "Test mode"
fi

for i in *.txt; do
    echo "$i"
done
```

## Common Issues Shellcheck Finds

1. **Unquoted Variables**: `$var` should be `"$var"`
2. **Dangerous Comparisons**: `[ $x = y ]` should be `[ "$x" = "y" ]`
3. **Useless Use of Commands**: `$(ls)` should use globs instead
4. **Missing Shebangs**: Scripts should start with `#!/bin/bash`

## Integration with Editors

### VS Code
Install the "shellcheck" extension for real-time linting.

### Vim
Add shellcheck integration with ALE or syntastic plugins.

## Continuous Integration
Add shellcheck to your CI pipeline:
```yaml
# GitHub Actions example
- name: Run ShellCheck
  uses: ludeeus/action-shellcheck@master
