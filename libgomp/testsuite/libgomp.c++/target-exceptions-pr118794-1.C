/* Exception handling constructs in dead code.  */

/* { dg-require-effective-target exceptions }
   { dg-additional-options -fexceptions } */
/* { dg-additional-options -O0 } */
/* { dg-additional-options -fdump-tree-optimized-raw }
   { dg-additional-options -foffload-options=-fdump-tree-optimized-raw } */

/* See also '../../../gcc/testsuite/g++.target/gcn/exceptions-pr118794-1.C',
   '../../../gcc/testsuite/g++.target/nvptx/exceptions-pr118794-1.C'.  */

#pragma omp begin declare target

bool ok = false;

template <typename T>
struct C
{
  C()
  {
    ok = true;
  }
  C(int) {};
  ~C() {};

  __attribute__((noipa))
  void m()
  {
    C c;
  }
};

inline void f()
{
  C<double> c(1);
  c.m();
}

#pragma omp end declare target

int main()
{
#pragma omp target
  {
    f();
  }
#pragma omp target update from(ok)
  if (!ok)
    __builtin_abort();
}

/* In this specific C++ arrangement, distilled from PR118794, GCC synthesizes
   '__builtin_eh_pointer', '__builtin_unwind_resume' calls as dead code in 'f':
   { dg-final { scan-tree-dump-times {gimple_call <__builtin_eh_pointer, } 1 optimized { target { ! { arm_eabi || tic6x-*-* } } } } }
   { dg-final { scan-tree-dump-times {gimple_call <__builtin_unwind_resume, } 1 optimized { target { ! { arm_eabi || tic6x-*-* } } } } }
   ..., just 'targetm.arm_eabi_unwinder' is different:
   { dg-final { scan-tree-dump-times {gimple_call <__builtin_cxa_end_cleanup, } 1 optimized { target { arm_eabi || tic6x-*-* } } } }
   { dg-final { scan-offload-tree-dump-times {gimple_call <__builtin_eh_pointer, } 1 optimized } }
   { dg-final { scan-offload-tree-dump-times {gimple_call <__builtin_unwind_resume, } 1 optimized } } */
