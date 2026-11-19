#!/usr/bin/env python3
# Copyright (C) 2026 Free Software Foundation, Inc.
# This file is part of GCC, distributed under the GNU General Public License,
# version 3 or (at your option) any later version.  See COPYING3.
"""Native Linux RUNPATH tests; no third-party Python packages required.

Without --gcobol, test the spec generator and ELF dependency semantics with
the system C compiler.  With --gcobol, also test an installed patched GCobol.
Run installed tests in a clean container without a GCobol ld.so.conf entry.
"""

import argparse
import os
from pathlib import Path
import re
import shutil
import subprocess
import tempfile


def run(args, *, input=None, ok=True, env=None):
    result = subprocess.run([str(a) for a in args], input=input, text=True,
                            stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                            env=env or clean_env)
    if ok and result.returncode:
        raise AssertionError(f"{args}\n{result.stdout}{result.stderr}")
    return result


def dynamic(path):
    return run([args.readelf, "-d", path]).stdout


def check_runpath(path, expected):
    tags = dynamic(path)
    assert "(RUNPATH)" in tags, tags
    assert "(RPATH)" not in tags, tags
    actual = re.search(r"\(RUNPATH\).*\[(.*)\]", tags).group(1).split(":")
    assert os.path.normpath(expected) in [os.path.normpath(p) for p in actual], (expected, actual)


def mechanism(root):
    """Exercise real GCC spec expansion, GNU ELF tags and the dynamic loader."""
    # Include characters which have special meanings to specs and sed.
    lib = root / "runtime space%&"
    lib.mkdir()
    source = root / "test.c"
    source.write_text("int main(void) { return 0; }\n")
    template = (src / "libgcobol.spec.in").read_text()
    # Use the production configure expression, rather than a second copy.
    config = (src / "configure.ac").read_text()
    expression = re.search(r"LIBGCOBOL_RPATH_SPEC='([^']*)'", config).group(1)
    template = template.replace("@LIBGCOBOL_RPATH_SPEC@", expression)
    template = template.replace("@LIBQUADSPEC@", "").replace("@LIBM@", "")
    spec = root / "test.spec"
    spec.write_text(run(["sh", src / "mk-rpath-spec.sh", lib],
                        input=template).stdout)
    exe = root / "hello"
    run([args.cc, f"-specs={spec}", source, "-o", exe])
    check_runpath(exe, lib)
    run([exe])
    print("PASS: generated specs preserve the runtime path and emit DT_RUNPATH")

    for option in ("-static", "-static-pie", "-nostdlib", "-nodefaultlibs",
                   "-c", "-S", "-E", "-r"):
        command = run([args.cc, f"-specs={spec}", option, "-###", source])
        assert "--enable-new-dtags" not in command.stderr, command.stderr
    print("PASS: specs omit RUNPATH for suppressed/default-library-free/static links")

    # A unique library name prevents the system cache from masking a missing
    # transitive RUNPATH.  Only the parent is on the executable's NEEDED list.
    leaf = root / "leaf.c"
    leaf.write_text("int gcobol_runpath_leaf(void) { return 42; }\n")
    parent = root / "parent.c"
    parent.write_text("extern int gcobol_runpath_leaf(void);\n"
                      "int gcobol_runpath_parent(void) { return gcobol_runpath_leaf(); }\n")
    source.write_text("extern int gcobol_runpath_parent(void);\n"
                      "int main(void) { return gcobol_runpath_parent() != 42; }\n")
    run([args.cc, "-shared", "-fPIC", leaf, "-o", lib / "libgcobol_rpath_leaf.so"])
    parent_so = lib / "libgcobol_rpath_parent.so"
    link_parent = [args.cc, "-shared", "-fPIC", parent, f"-L{lib}",
                   "-lgcobol_rpath_leaf", "-o", parent_so]
    run(link_parent)
    run([args.cc, f"-specs={spec}", source, f"-L{lib}",
         "-lgcobol_rpath_parent", "-Xlinker", "-rpath-link", "-Xlinker", lib,
         "-o", exe])
    assert "[libgcobol_rpath_leaf.so]" not in dynamic(exe)
    result = run([exe], ok=False)
    assert result.returncode != 0 and "libgcobol_rpath_leaf.so" in result.stderr
    run(link_parent + ["-Wl,--enable-new-dtags", "-Xlinker", "-rpath",
                       "-Xlinker", lib])
    check_runpath(parent_so, lib)
    run([exe])
    print("PASS: a transitive private dependency needs and uses its parent's RUNPATH")

    # Environment overrides remain possible with RUNPATH (unlike old RPATH).
    override = root / "override"
    override.mkdir()
    leaf.write_text("int gcobol_runpath_leaf(void) { return 13; }\n")
    run([args.cc, "-shared", "-fPIC", leaf, "-o", override / "libgcobol_rpath_leaf.so"])
    env = dict(clean_env, LD_LIBRARY_PATH=str(override))
    assert run([exe], ok=False, env=env).returncode == 1
    print("PASS: LD_LIBRARY_PATH can deliberately override DT_RUNPATH")


def installed(root):
    compiler = shutil.which(args.gcobol) or str(Path(args.gcobol).resolve())
    runtime = Path(args.runtime_dir).resolve()
    source = root / "hello.cob"
    source.write_text('       identification division.\n'
                      '       program-id. hello.\n'
                      '       procedure division.\n'
                      '           display "RUNPATH-OK"\n'
                      '           stop run.\n')
    exe = root / "hello-cobol"
    for options in ([], ["-lgcobol"], ["-dialect", "mf"], ["-static-libgcobol"]):
        run([compiler, source, *options, "-o", exe])
        check_runpath(exe, runtime)
        assert run([exe]).stdout.strip() == "RUNPATH-OK"
        print(f"PASS: installed compiler links and runs with {options}")
    obj = root / "hello.o"
    run([compiler, "-c", "-main", source, "-o", obj])
    run([compiler, obj, "-o", exe])
    check_runpath(exe, runtime)
    assert run([exe]).stdout.strip() == "RUNPATH-OK"
    print("PASS: separate compilation and object-only linking")
    run([compiler, "-shared", "-fPIC", source, "-o", root / "hello.so"])
    check_runpath(root / "hello.so", runtime)
    print("PASS: generated shared libraries have their own RUNPATH")
    for option in ("-c", "-S", "-E", "-r", "-nostdlib", "-nodefaultlibs",
                   "-static", "-static-pie"):
        command = run([compiler, option, "-###", source])
        assert "--enable-new-dtags" not in command.stderr, command.stderr
    print("PASS: installed driver suppression and static-link cases")
    for name in ("gcobol", "gcobol_posix", "gcobol_compat_gnu"):
        check_runpath(runtime / f"lib{name}.so", runtime)
    print("PASS: all three installed runtime DSOs have DT_RUNPATH")
    # The ELF interpreter reports actual paths without executing the program.
    headers = run([args.readelf, "-l", exe]).stdout
    loader = re.search(r"Requesting program interpreter: (.*?)\]", headers).group(1)
    dependencies = run([loader, "--inhibit-cache", "--list", exe]).stdout
    assert "not found" not in dependencies, dependencies
    run([loader, "--inhibit-cache", exe])
    print("PASS: execution also succeeds with the system library cache inhibited")
    print(dependencies.rstrip())


parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("--cc", default="gcc")
parser.add_argument("--readelf", default="readelf")
parser.add_argument("--gcobol", help="installed patched native Linux driver")
parser.add_argument("--runtime-dir", help="configured toolexeclibdir")
args = parser.parse_args()
if bool(args.gcobol) != bool(args.runtime_dir):
    parser.error("--gcobol and --runtime-dir must be used together")
src = Path(__file__).resolve().parents[1]
clean_env = {k: v for k, v in os.environ.items()
             if k not in ("LD_LIBRARY_PATH", "LD_PRELOAD", "LD_AUDIT",
                          "LD_RUN_PATH", "LIBRARY_PATH", "GCC_EXEC_PREFIX",
                          "COMPILER_PATH")}
with tempfile.TemporaryDirectory(prefix="gcobol-runpath-") as temporary:
    root = Path(temporary)
    mechanism(root)
    if args.gcobol:
        installed(root)
