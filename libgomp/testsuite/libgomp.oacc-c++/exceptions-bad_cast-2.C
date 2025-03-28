/* 'std::bad_cast' exception in OpenACC compute region, caught.  */

/* { dg-require-effective-target exceptions }
   { dg-additional-options -fexceptions } */
/* { dg-additional-options -fdump-tree-optimized-raw }
   { dg-additional-options -foffload-options=-fdump-tree-optimized-raw } */
/* { dg-bogus {_ZTISt8bad_cast} PR119734 { target openacc_nvidia_accel_selected xfail *-*-* } 0 }
   { dg-excess-errors {'mkoffload' failure etc.} { xfail openacc_nvidia_accel_selected } } */

/* See also '../libgomp.c++/target-exceptions-bad_cast-2.C'.  */

/* See also '../../../gcc/testsuite/g++.target/gcn/exceptions-bad_cast-2.C',
   '../../../gcc/testsuite/g++.target/nvptx/exceptions-bad_cast-2.C'.  */

#include <iostream>
#include <typeinfo>

struct C1
{
  virtual void f()
  {}
};

struct C2 : C1
{
};

int main()
{
  std::cerr << "CheCKpOInT\n";
#pragma omp target
#pragma acc serial
  {
    C1 c1;
    try
      {
	[[maybe_unused]]
	C2 &c2 = dynamic_cast<C2 &>(c1);
	/* 'std::bad_cast' is thrown.  */
      }
    catch (const std::bad_cast &e)
      {
	__builtin_printf("caught '%s'\n", e.what());
      }
  }
}

/* { dg-output {CheCKpOInT[\r\n]+} }

   { dg-final { scan-tree-dump-times {gimple_call <__cxa_bad_cast, } 1 optimized } }
   { dg-final { scan-offload-tree-dump-times {gimple_call <__cxa_bad_cast, } 1 optimized } }
   { dg-output {.*caught 'std::bad_cast'[\r\n]+} { target openacc_host_selected } }
   For GCN, nvptx offload execution, we don't print anything, but just 'abort'.

   TODO For GCN, nvptx offload execution, this currently doesn't 'abort' due to
   the 'std::bad_cast' exception, but rather due to SIGSEGV in 'dynamic_cast';
   PR119692.

   For GCN, nvptx offload execution, there is no 'catch'ing; any exception is fatal.
   { dg-shouldfail {'std::bad_cast' exception} { ! openacc_host_selected } } */
