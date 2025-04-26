/* { dg-additional-options "-fdump-tree-gimple" }  */

/* PR c++/118859  */

void f_var(int *y) {
 #pragma omp target is_device_ptr(y)
 {
   if (*y != 5)
     __builtin_abort ();
   *y += 10;
 }
}
#pragma omp declare variant(f_var) match(construct={dispatch}) adjust_args(need_device_ptr : 1)
void f(int *);

static void test()
{
 int x = 5;
 #pragma omp target enter data map(x)

 #pragma omp dispatch
   f(&x);

 #pragma omp target exit data map(x)
 if (x != 15)
   __builtin_abort ();
}

int main()
{
 test();
}

// { dg-final { scan-tree-dump "D\\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(&x, D\\.\[0-9\]+\\);" "gimple" } }
// { dg-final { scan-tree-dump "f_var \\(D\\.\[0-9\]+\\);" "gimple" } }
