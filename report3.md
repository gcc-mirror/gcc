# Automatic Parallel Compilation Viability: Final Report

## Complete Tasks

For the third evaluation, we expected to deliver the product as a
series of patches for trunk.  The patch series were in fact delivered
[1], but several items must be fixed before merge.

This report mainly has a lot in common with the second report, as this
phase was mostly bug fixing and rebasing.

Overall, the project works and speedups ranges from 0.95x to 3.3x.
Bootstrap is working, and therefore this can be used in an experimental
state.

## How to use

1. Clone the autopar_devel branch:
```
git clone --single-branch --branch devel/autopar_devel \
  git://gcc.gnu.org/git/gcc.git gcc_autopar_devel
```
2. Follow the standard compilation options provided in the Compiling
GCC page, and install it on some directory. For instance:

```
cd gcc_autopar_devel
mkdir build && cd build
../configure --disable-bootstrap --enable-languages=c,c++
make -j 8
make DESTDIR=/tmp/gcc11_autopar install
```

3. If you want to test whether your version is working, just launch
Gcc with `-fparallel-jobs=2` when compiling a file with -c.

5. If you want to compile a project with this version it uses GNU
Makefiles, you must modify the compilation rule command and prepend a
`+` token to it. For example, in Git's Makefile, Change:
```
$(C_OBJ): %.o: %.c GIT-CFLAGS $(missing_dep_dirs)
	$(QUIET_CC)$(CC) -o $*.o -c $(dep_args) $(ALL_CFLAGS) $(EXTRA_CPPFLAGS) $<
```
to:
```
$(C_OBJ): %.o: %.c GIT-CFLAGS $(missing_dep_dirs)
	+$(QUIET_CC)$(CC) -o $*.o -c $(dep_args) $(ALL_CFLAGS) $(EXTRA_CPPFLAGS) $<
```
as well as point the CC variable to the installed gcc, and
append a `-fparallel-jobs=jobserver` on your CFLAGS variable.

# How the parallelism works in this project

In LTO, the Whole Program Analysis decides how to partition the
callgraph for running the LTRANS stage in parallel.  This project
works very similar to this, however with some changes.

The first was to modify the LTO structure so that it accepts
the compilation without IR streaming to files.  This avoid an IO
overhead when compiling in parallel.

The second was to use a custom partitioner to find which nodes
should be in the same partition.  This was mainly done to bring COMDAT
together, as well as symbols that are part of other symbols, and even
private symbols so that we do not output hidden global symbols.

However, experiment showed that bringing private symbols together did
not yield a interesting speedup on some large files, and therefore
we implemented two modes of partitioning:

1. Partition without static promotion. This is the safer method to use,
as we do not modify symbols in the Compilation Unit. This may lead to
speedups in files that have multiple entries points with low
connectivity between then (such as insn-emit.c), however this will not
provide speedups when this hypothesis is not true (gimple-match.c is an
example of this). This is the default mode.

2. Partition with static promotion to global. This is a more aggressive
method, as we can decide to promote some functions to global to increase
parallelism opportunity. This also will change the final assembler name
of the promoted function to avoid collision with functions of others
Compilation Units. To use this mode, the user has to manually specify
--param=promote-statics=1, as they must be aware of this.

Currently, partitioner 2. do not account the number of nodes to be
promoted.  Implementing this certainly will reduce impact on produced
code.

## Jobserver Integration

We implemented a interface to communicate with the GNU Make's Jobserver
that is able to detect when the GNU Make Jobserver is active, thanks to
Nathan Sidwell. This works as follows:

When -fparallel-jobs=jobserver is provided, GCC will try to detect if
there is a running Jobserver in which we can communicate to. If true,
we return the token that Make originally gave to us, then we wait for
make for a new token that, when provided, will launch a forked child
process with the partition information; or fall back to default
compilation if Jobserver is not detected with a warning. Now if
-fparallel-jobs=auto, this warning will not be provided and the default
number of jobs will be set to 2, unless a single core CPU is detected.
For more information about the implementation, check gcc/jobserver.cc
file in the autopar_devel branch.

## Speedups

Speedups ranged from 0.95x to 1.9x on a Quad-Core Intel Core-i7 8565U,
and from 0.99x to 3.3x when testing on a 32-Cores AMD Opteron 6376.
As a comparison, the theoretical maximum speedup by parallelizing this
part of compilation is 4x, so there are things that can be improved
here.

Results are shown in the following table. There are three columns:

* Sequential, which means running the compilation with just "-c -O2".
* Without Static Promotion, which means appending -fparallel-jobs=<NUM_THREADS>
* With Static Promotion, which means appending -fparallel-jobs=<NUM_THREADS>
and --param=promote-statics=1
* Speedup, which is Sequential / MIN (W/O Static Promotion, Static Promotion)
* LTO Emulated Parallelism, which consist of running a two step:
   g++ -c <FILE> -O2 -o t.il.o -flto -fno-fat-lto-objects && \
   g++ -o t.o t.il.o -O2 -r -flinker-output=nolto-rel -flto=<NUM_THREADS>

The test was the result of a single execution with a previous warm up
execution. The compiled GCC had checking enabled, and therefore release
version might have better timings in both sequential and parallel, but the
speedup may remain the same.

CPU: Intel Core-i7 8565U (4 Cores, 8 Threads)
|                |            | Without Static | With Static |         | LTO Emulated |
| File           | Sequential |    Promotion   |  Promotion  | Speedup |  Parallelism |
|----------------|------------|----------------|-----------------------|--------------|
| gimple-match.c |     60s    |       63s      |     34s     |   1.7x  |     32s      |
| insn-emit.c    |     37s    |       19s      |     20s     |   1.9x  |     20s      |

CPU: 4x AMD Opteron 6376 (32 Cores, 64 Threads)
|                |            | Without Static | With Static |         | LTO Emulated |
| File           | Sequential |    Promotion   |  Promotion  | Speedup |  Parallelism |
|----------------|------------|----------------|-----------------------|--------------|
| gimple-match.c |    2m42s   |      2m43s     |     49s     |   3.3x  |     46s      |
| insn-emit.c    |    1m35s   |       30s      |     30s     |   3.1x  |     32s      |

There was also a reduction from 35m32s to 29m30s when bootstrapping
GCC when -fparallel-jobs=8 on the Opteron machine.

## Conclusions

This project can still be improved on a lot of places. For example,
just by looking at the table above, we can conclude that:

* Promoting statics to globals can increase parallelism opportunity on
compilation.
* Even with the IO Streaming overhead, the LTO Emulated Parallelism
was faster than with our method. This means that either our
partitioner is slow or we are losing time into something which can
be improved.
* This can be used as a way to reduce overall compilation time in
many-core machines.

## Fixed bugs from phase 2:

* Make -fparallel-jobs=jobserver automatically detect if jobserver is
present.
* Fix a bug which caused the resulting object file to not be the same
in every compilation, therefore making bootstrap comparison work as
expected.
* Fix a bug which caused LTO to crash due to a uninitialized output
FILE pointer.

## TODOs:

* Maybe increase minimal partition size (--param=lto-min-partitions).
* Improve checking for unreasonable bias (unbalanced partitons).
* Use cost of partitioning symbols into account.
* Avoid forking for the first (or last) partitions.
* Modify the driver to use the SPEC language instead of injecting
commands in the execute () function.
* Use the main process to write the assembler files instead of the
worker process.  This removes the requirement for sorting later in
the driver.
* Remove hidden "-fPIC" and "fPIE" from the driver.
* Check for race condition in additional_asm_file, as it is created
later in the compilation process.
* Replace all array allocations of size greater or equal the number
of nodes in the graph from the stack to the heap (i.e. avoid alloca).
* Use 64-bit integers when computing sizes.
* Check for ways to reduce the time spent inside the kernel.
* Add support to every GCC frontend.
* Rely on ipa_summary instead of setting a custom cost to nodes
without summary.
* Merge the current partitioning logic with the LTO logic.
* Merge compute_boundary function with what is already done in LTO.

Cosmetic:

* Change n to num_nodes when that is related to the number of nodes of
the graph.
* Explain better what the compression[] array does in lto-partition.c

[1] - https://gcc.gnu.org/pipermail/gcc-patches/2020-August/552346.html
