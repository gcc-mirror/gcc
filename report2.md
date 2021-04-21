# Automatic Parallel Compilation Viability: Second Report

## Complete Tasks

For the second evaluation, we expected to have a working version of the
project that result in speedups when compile large files, and have a
working integration with the GNU Jobserver. Both of these tasks are
complete, and I will discuss some decisions made to achive this.

## Compilation Unit Partitioning

In the first implementation, we had a bug where partitioning were done
before LTRANS stage were executed, implying in information loss and
undesired collateral effects such as bad inlining. This was fixed later
on.

After this, we decided to implement two modes of partitioning:

1. Partition without static promotion. This is the safer method to use,
as we do not modify symbols in the Compilation Unit. This may lead to
speedups in files that have multiple entries points with low
connectivity between then (such as insn-emit.c), however this will not
provide speedups when this hypothesis is not true (gimple-match.c is an
example of this).

2. Partition with static promotion to global. This is a more aggressive
method, as we can decide to promote some functions to global to increase
parallelism opportunity. This also will change the final assembler name
of the promoted function to avoid collision with functions of others
Compilation Units. To use this mode, the user has to manually specify
--param=promote-statics=1, as they must be aware of this.

We also changed the conditions to decide whether a symbol will be in
some partition or not, as implemented in `lto_merge_comdats_map` in
lto-cgraph.c:

1. Find what symbols may have to bring COMDATs to its partition. We do
that by defining a COMDAT frontier and marking all these symbols to be
partitioned together.

2. Find symbols that may be part of other symbols, and mark them to be
partitioned together.

3. If static promotion is disabled, mark symbols that references and
calls static functions, and mark them to be partitioned together.

Although condition 1. required some more complicated analysis, this
reduced code complexity of the previous partitioner.


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

## Current Status

Currently, we have the following scenario:

1. Bootstrap works with promote statics disabled, and when enabled
object comparison fails on stage3 because promoted symbols name are
randomized for now. This impacts reproducible builds and safer methods
must be employed to fix this, such as maybe using the file SHA hash
as a way to avoid name colision.

2. Jobserver integration works but modifications to the Makefile are
necessary. So far, the current branch is not able to bootstrap with
-fparallel-jobs=jobserver because such modifications are required to
GCC's Makefiles. However, we managed to compile Git with this option
enabled, therefore supporting the claim that it is working to a certain
level.

3. Speedups ranged from 0.95x to 1.9x on a Quad-Core Intel Core-i7 8565U
when testing with two files in GCC, as stated in the following table.
The test was the result of a single execution with a previous warm up
execution. The compiled GCC had checking enabled, and therefore release
version might have better timings in both sequential and parallel, but the
speedup may remain the same.

|                |            | Without Static | With Static |         |
| File           | Sequential |    Promotion   |  Promotion  | Speedup |
|----------------|------------|----------------|-----------------------|
| gimple-match.c |     60s    |       63s      |     34s     |   1.7x  |
| insn-emit.c    |     37s    |       19s      |     20s     |   1.9x  |

4. Supported Frontends: C, C++, Fortran. Others frontends can also be
easily supported if they do not require the GCC driver to spawn multiple
processes to compile a single file to assembler.

## How to use this

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


## Conclusions

The current project is going well, although conservative approaches
such as disable static promotion may impact negatively the parallelism
potential. One way to fix this in the future is to communicate the final
assembler file back to the main GCC process and then concatenate back
into one assembler file, rather than do partial linking of multiple
assembler files. This certainly will increase Interprocess Communication
costs, but it may be worth.
