/* 'std::bad_cast' exception in OpenACC compute region, caught.  */

/* { dg-require-effective-target exceptions }
   { dg-additional-options -fexceptions } */
/* { dg-skip-if {} { ! openacc_host_selected } }
   Offloading compilation not yet supported; see
   'exceptions-bad_cast-2-offload-sorry-GCN.C',
   'exceptions-bad_cast-2-offload-sorry-nvptx.C'.  */
/* { dg-additional-options -fdump-tree-optimized-raw } */

/* See also '../libgomp.c++/target-exceptions-bad_cast-2.C'.  */

/* See also '../../../gcc/testsuite/g++.target/gcn/exceptions-bad_cast-2.C',
   '../../../gcc/testsuite/g++.target/nvptx/exceptions-bad_cast-2.C'.  */

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

/* { dg-final { scan-tree-dump-times {gimple_call <__cxa_bad_cast, } 1 optimized } }
   { dg-output {caught 'std::bad_cast'[\r\n]+} } */
