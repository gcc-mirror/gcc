/* 'throw' in OpenACC compute region.  */

/* { dg-require-effective-target exceptions }
   { dg-additional-options -fexceptions } */
/* { dg-additional-options -fdump-tree-optimized-raw }
   { dg-additional-options -foffload-options=-fdump-tree-optimized-raw } */
/* { dg-bogus {Size expression must be absolute\.} PR119737 { target { openacc_radeon_accel_selected && __OPTIMIZE__ } xfail *-*-* } 0 }
   { dg-ice PR119737 { openacc_radeon_accel_selected && __OPTIMIZE__ } }
   { dg-excess-errors {'mkoffload' failure etc.} { xfail { openacc_radeon_accel_selected && __OPTIMIZE__ } } } */

/* See also '../libgomp.c++/target-exceptions-throw-1.C'.  */

/* See also '../../../gcc/testsuite/g++.target/gcn/exceptions-throw-1.C',
   '../../../gcc/testsuite/g++.target/nvptx/exceptions-throw-1.C'.  */

#include <iostream>

class MyException
{
};

int main()
{
  std::cerr << "CheCKpOInT\n";
#pragma omp target
#pragma acc serial
  /* { dg-bogus {using 'vector_length \(32\)', ignoring 1} {} { target openacc_nvidia_accel_selected xfail *-*-* } .-1 } */
  {
    MyException e1;
    throw e1;
  }
}

/* { dg-output {CheCKpOInT[\r\n]+} }

   { dg-final { scan-tree-dump-times {gimple_call <__cxa_allocate_exception, } 1 optimized } }
   { dg-final { scan-tree-dump-times {gimple_call <__cxa_throw, } 1 optimized } }
   { dg-final { scan-offload-tree-dump-times {gimple_call <__cxa_allocate_exception, } 1 optimized } }
   { dg-final { scan-offload-tree-dump-times {gimple_call <__cxa_throw, } 1 optimized } }
   For host execution, we print something like:
       terminate called after throwing an instance of 'MyException'
       Aborted (core dumped)
   { dg-output {.*MyException} { target openacc_host_selected } }
   For GCN, nvptx offload execution, we don't print anything, but just 'abort'.

   { dg-shouldfail {'MyException' exception} } */
