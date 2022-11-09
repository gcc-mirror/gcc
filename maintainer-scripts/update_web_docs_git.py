#!/usr/bin/env python3

# Generate documentation from Sphinx files.

import argparse
import os
import shutil
import subprocess
import tempfile
from pathlib import Path

GITROOT = '/git/gcc.git'

parser = argparse.ArgumentParser(description='Update web documentation.')
parser.add_argument('output_folder', help='Output folder')
parser.add_argument('--gitrepo', help=f'Git repository (default: {GITROOT})',
                    default=GITROOT)
parser.add_argument('--sphinx-build', help='Path to sphinx-build binary')
parser.add_argument('-v', '--verbose', action='store_true',
                    help='Verbose output')
args = parser.parse_args()


def find_configs():
    for root, _, files in os.walk('.'):
        for f in files:
            full = os.path.join(root, f)
            if f == 'conf.py':
                # find name of the documentation
                lines = open(full).read().splitlines()
                docname = None
                for line in lines:
                    if line.startswith('name = '):
                        docname = line.split()[-1].strip("'")
                        break
                assert docname
                yield (Path(root).resolve(), docname)


def create_source_tarball(output, configs):
    pwd = Path('.').resolve()
    subfolders = {'doc'}
    explicit_files = {'gcc/BASE-VER', 'gcc/DEV-PHASE', 'gcc/DATESTAMP'}

    for location, _ in configs:
        location = location.relative_to(pwd)
        while not location.name == 'doc':
            location = location.parent
        subfolders.add(location)

    sources = Path('sources')
    sources.mkdir()

    # Copy all subfolders and files
    for subfolder in subfolders:
        shutil.copytree(subfolder, sources / subfolder)

    for filename in explicit_files:
        shutil.copy(filename, sources / filename)

    shutil.make_archive(Path(output, 'docs-sources'), 'gztar',
                        sources)
    print('sources tarball has been created')


with tempfile.TemporaryDirectory() as folder:
    print(f'Using {folder} as temporary directory')
    os.chdir(folder)
    subprocess.check_output(f'git clone {args.gitrepo} --depth=1', shell=True)
    os.chdir('gcc')
    configs = list(find_configs())

    # Prepare folders
    output = Path(args.output_folder)
    if not output.exists():
        output.mkdir()

    # Create source tarball
    create_source_tarball(output, configs)

    temp = Path('tmp').resolve()
    temp.mkdir()

    # Build and copy the documentation
    for i, (config_folder, docname) in enumerate(sorted(configs)):
        print(f'=== building {i + 1}/{len(configs)}: {docname} ===')

        # Build HTML
        cmd = f'make -C doc html SOURCEDIR={config_folder} BUILDDIR={temp}/{docname}'
        if args.sphinx_build:
            cmd += f' SPHINXBUILD={args.sphinx_build}'
        subprocess.run(cmd, shell=True, check=True,
                       capture_output=not args.verbose)
        os.unlink(f'{temp}/{docname}/html/.buildinfo')
        shutil.copytree(f'{temp}/{docname}/html', f'{output}/{docname}',
                        dirs_exist_ok=True)

        # Build PDF
        cmd = f'make -C doc latexpdf SOURCEDIR={config_folder} BUILDDIR={temp}/pdf/{docname}'
        if args.sphinx_build:
            cmd += f' SPHINXBUILD={args.sphinx_build}'
        subprocess.run(cmd, shell=True, check=True,
                       capture_output=not args.verbose)
        shutil.copyfile(f'{temp}/pdf/{docname}/latex/{docname}.pdf',
                        f'{output}/{docname}.pdf')
