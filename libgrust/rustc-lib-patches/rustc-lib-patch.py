#!/usr/bin/env python3

# Copyright (C) 2026 Free Software Foundation, Inc.
#
# This file is part of GCC.
#
# GCC is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free
# Software Foundation; either version 3, or (at your option) any later
# version.
#
# GCC is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License
# along with GCC; see the file COPYING3.  If not see
# <http://www.gnu.org/licenses/>.


"""Apply the adjacent patch series to the vendored Rust libraries."""

import argparse
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile

SCRIPT_DIR = Path(__file__).resolve().parent
RUSTC_LIB_DIR = SCRIPT_DIR.parent / "rustc-lib"
PATCHES_DIR = SCRIPT_DIR / "patches"
PATCH_FILENAME_SUFFIX = ".for-rfl-patch"


def git(directory, *arguments, data=None, check=False):
    result = subprocess.run(
        ["git", "-C", str(directory), *arguments],
        input=data,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    if check and result.returncode:
        raise RuntimeError(result.stderr.decode(errors="replace").strip())
    return result


def apply_patch(directory, data, reverse=False, check=False):
    # strip a/libgrust/rustc-lib/ from repository-relative patch paths
    arguments = ["apply", "-p3"]
    prefix = git(directory, "rev-parse", "--show-prefix")

    if prefix.returncode == 0 and prefix.stdout.strip():
        arguments.append(f"--directory={prefix.stdout.decode().strip()}")
    if reverse:
        arguments.append("--reverse")
    if check:
        arguments.append("--check")
    return git(directory, *arguments, data=data)


def patch_in_place(directory=RUSTC_LIB_DIR):
    for patch in sorted(PATCHES_DIR.glob(f"*{PATCH_FILENAME_SUFFIX}")):
        data = patch.read_bytes()
        if apply_patch(directory, data, check=True).returncode:
            # skip patches that can't be applied because they might have already been applied
            continue
        result = apply_patch(directory, data)
        if result.returncode:
            raise RuntimeError(
                f"failed to apply {patch.name}: "
                f"{result.stderr.decode(errors='replace').strip()}"
            )


def patch_library(output=None):
    if output is not None:
        shutil.copytree(RUSTC_LIB_DIR, output)
        patch_in_place(output)
    else:
        patch_in_place()


def generate_patch(name):
    if not name.endswith(PATCH_FILENAME_SUFFIX):
        raise RuntimeError(f"patch filename should end with {PATCH_FILENAME_SUFFIX}")
    output_patch = PATCHES_DIR / name
    if output_patch.exists():
        raise RuntimeError(f"patch filename already exists: {output_patch}")
    prefix = (
        git(
            RUSTC_LIB_DIR,
            "rev-parse",
            "--show-prefix",
            check=True,
        )
        .stdout.decode()
        .strip()
    )
    tree = git(
        RUSTC_LIB_DIR,
        "rev-parse",
        f"HEAD:{prefix.rstrip('/')}",
        check=True,
    ).stdout
    objects = git(
        RUSTC_LIB_DIR,
        "rev-parse",
        "--git-path",
        "objects",
        check=True,
    ).stdout
    objects = (RUSTC_LIB_DIR / objects.decode().strip()).resolve()
    patches = sorted(PATCHES_DIR.glob(f"*{PATCH_FILENAME_SUFFIX}"))

    # using a temporary repository so generation cannot change the user's index
    with tempfile.TemporaryDirectory(prefix="rustc-lib-patch-") as temporary:
        directory = Path(temporary)
        git(directory, "init", "--quiet", check=True)
        (directory / ".git/objects/info/alternates").write_text(str(objects) + "\n")
        git(directory, "read-tree", tree.decode().strip(), check=True)
        for patch in patches:
            result = git(directory, "apply", "--cached", "-p3", data=patch.read_bytes())
            if result.returncode:
                raise RuntimeError(
                    f"failed to apply {patch.name} to HEAD: "
                    f"{result.stderr.decode(errors='replace').strip()}"
                )
        baseline = git(directory, "write-tree", check=True).stdout.decode().strip()
        git(
            directory,
            f"--work-tree={RUSTC_LIB_DIR}",
            "add",
            "-A",
            "--",
            ".",
            check=True,
        )
        data = git(
            directory,
            "diff",
            "--cached",
            "--binary",
            "--no-ext-diff",
            "--no-textconv",
            "--no-renames",
            "--no-color",
            "--src-prefix=a/libgrust/rustc-lib/",
            "--dst-prefix=b/libgrust/rustc-lib/",
            baseline,
            "--",
            ".",
            check=True,
        ).stdout

    if not data:
        print("No changes beyond the existing patch series.")
        return

    with output_patch.open("xb") as output:
        output.write(data)
    print(f"Generated {output_patch}")


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    mode = parser.add_mutually_exclusive_group()
    mode.add_argument(
        "-o",
        "--output",
        type=Path,
        help="write a patched copy to a new directory instead of patching in place",
    )
    mode.add_argument(
        "--generate",
        metavar=f"NAME{PATCH_FILENAME_SUFFIX}",
        help=f"write local changes beyond the existing patches to patches/NAME{PATCH_FILENAME_SUFFIX}",
    )
    args = parser.parse_args(argv)
    try:
        if args.generate is not None:
            generate_patch(args.generate)
        else:
            patch_library(args.output)
    except (RuntimeError, OSError) as error:
        print(f"{parser.prog}: error: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
