/* 'throw' in OpenACC compute region, caught.  */

/* { dg-require-effective-target exceptions }
   { dg-additional-options -fexceptions } */
/* { dg-additional-options -fdump-tree-optimized-raw }
   { dg-additional-options -foffload-options=-fdump-tree-optimized-raw } */
/* { dg-bogus {undefined symbol: typeinfo name for MyException} PR119806 { target { openacc_radeon_accel_selected && { ! __OPTIMIZE__ } } xfail *-*-* } 0 }
   { dg-excess-errors {'mkoffload' failure etc.} { xfail { openacc_radeon_accel_selected && { ! __OPTIMIZE__ } } } } */
/* { dg-bogus {Initial value type mismatch} PR119806 { target { openacc_nvidia_accel_selected && { ! __OPTIMIZE__ } } xfail *-*-* } 0 }
   { dg-excess-errors {'mkoffload' failure etc.} { xfail { openacc_nvidia_accel_selected && { ! __OPTIMIZE__ } } } } */

/* See also '../libgomp.c++/target-exceptions-throw-2.C'.  */

/* See also '../../../gcc/testsuite/g++.target/gcn/exceptions-throw-2.C',
   '../../../gcc/testsuite/g++.target/nvptx/exceptions-throw-2.C'.  */

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
    try
      {
	MyException e1;
	throw e1;
      }
    catch (const MyException &e)
      {
	__builtin_printf("caught '%s'\n", "MyException");
      }
  }
}

/* { dg-output {CheCKpOInT[\r\n]+} }

   { dg-final { scan-tree-dump-times {gimple_call <__cxa_allocate_exception, } 1 optimized } }
   { dg-final { scan-tree-dump-times {gimple_call <__cxa_throw, } 1 optimized } }
   { dg-final { scan-offload-tree-dump-times {gimple_call <__cxa_allocate_exception, } 1 optimized } }
   { dg-final { scan-offload-tree-dump-times {gimple_call <__cxa_throw, } 1 optimized } }
   { dg-output {.*caught 'MyException'[\r\n]+} { target openacc_host_selected } }
   For GCN, nvptx offload execution, we don't print anything, but just 'abort'.

   For GCN, nvptx offload execution, there is no 'catch'ing; any exception is fatal.
   { dg-shouldfail {'MyException' exception} { ! openacc_host_selected } } */
