/* 'throw' in OpenACC compute region, dead code.  */

/* { dg-require-effective-target exceptions }
   { dg-additional-options -fexceptions } */
/* Wrong code for offloading execution.
   { dg-skip-if PR119692 { ! openacc_host_selected } }
   { dg-additional-options -fdump-tree-gimple } */
/* { dg-additional-options -fdump-tree-optimized-raw } */

/* See also '../libgomp.c++/target-exceptions-throw-3.C'.  */

/* See also '../../../gcc/testsuite/g++.target/gcn/exceptions-throw-3.C',
   '../../../gcc/testsuite/g++.target/nvptx/exceptions-throw-3.C'.  */

/* For PR119692 workarounds.  */
#ifndef DEFAULT
# define DEFAULT
#endif

class MyException
{
};

int main()
{
#pragma omp target DEFAULT
#pragma acc serial DEFAULT
  /* { dg-bogus {using 'vector_length \(32\)', ignoring 1} {} { target openacc_nvidia_accel_selected xfail *-*-* } .-1 } */
  {
    bool a = false;
    asm volatile ("" : : "r" (&a) : "memory");
    if (a)
      {
	MyException e1;
	throw e1;
      }
  }
}

/* { dg-final { scan-tree-dump-not {(?n)#pragma omp target oacc_serial map\(tofrom:_ZTI11MyException \[len: [0-9]+\]\)$} gimple { xfail *-*-* } } } */

/* { dg-final { scan-tree-dump-times {gimple_call <__cxa_allocate_exception, } 1 optimized } }
   { dg-final { scan-tree-dump-times {gimple_call <__cxa_throw, } 1 optimized } } */
