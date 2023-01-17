/* { dg-do run } */
/* This test case is meant to 'abort' in device execution, which works as
   expected with '--param openacc-kernels=parloops':

   GCN:

       GCN Kernel Aborted

   nvptx:

       libgomp: cuStreamSynchronize error: unspecified launch failure (perhaps abort was called)

   ..., or:

       libgomp: cuStreamSynchronize error: an illegal instruction was encountered

   However, with '--param openacc-kernels=decompose', for '-O0', '-O1', this
   does *not* 'abort' in device execution, but instead we run into whatever the
   compiler generates for (implicit) host-side '__builtin_unreachable ()':

       Segmentation fault (core dumped)

   ..., or:

       Illegal instruction (core dumped)

   (This, unfortunately, still means "correct" execution of this test case...)

   And, with '--param openacc-kernels=decompose', with '-O2' and higher, we
   get things like the following:

   GCN:

       libgomp: Called kernel must be initialized

   ..., potentially followed by:

       libgomp: Duplicate node
       WARNING: program timed out.

   nvptx:

       libgomp: cuModuleLoadData error: unspecified launch failure

   ..., or:

       libgomp: cuModuleLoadData error: an illegal instruction was encountered

   That is, for nvptx, the code doesn't even load?

   Worse, on one system, this process then shows 100 % CPU utilization, GPU
   locks up; process un-SIGKILLable, system needs to be (forcefully) rebooted.

   Until we understand what's happening (how the decomposed OpenACC 'kernels'
   code is different from 'abort-1.c', for example), play it safe:

   { dg-additional-options {--param openacc-kernels=parloops} }
*/

#include <stdio.h>
#include <stdlib.h>

int
main (void)
{
  fprintf (stderr, "CheCKpOInT\n");
#pragma acc kernels
  {
    abort ();
  }

  return 0;
}

/* { dg-output "CheCKpOInT" } */
/* { dg-shouldfail ""  } */
