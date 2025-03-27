/* 'throw' in OpenACC compute region, caught.  */

/* { dg-require-effective-target exceptions }
   { dg-additional-options -fexceptions } */
/* { dg-skip-if {} { ! openacc_host_selected } }
   Offloading compilation not yet supported; see
   'exceptions-throw-2-offload-sorry-GCN.C',
   'exceptions-throw-2-offload-sorry-nvptx.C'.  */
/* { dg-additional-options -fdump-tree-optimized-raw } */

/* See also '../libgomp.c++/target-exceptions-throw-2.C'.  */

/* See also '../../../gcc/testsuite/g++.target/gcn/exceptions-throw-2.C',
   '../../../gcc/testsuite/g++.target/nvptx/exceptions-throw-2.C'.  */

class MyException
{
};

int main()
{
#pragma omp target
#pragma acc serial
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

/* { dg-final { scan-tree-dump-times {gimple_call <__cxa_allocate_exception, } 1 optimized } }
   { dg-final { scan-tree-dump-times {gimple_call <__cxa_throw, } 1 optimized } }
   { dg-output {caught 'MyException'[\r\n]+} } */
