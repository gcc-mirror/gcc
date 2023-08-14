/* { dg-do run } */

#include <cassert>

void test_ref ()
{
  int c_orig = 5;
  int &c = c_orig;

#pragma omp target map(tofrom: c)
  {
    c++;
  }

  assert (c == 6);
}

void test_ref_to_ptr ()
{
  int val = 5;
  int *ptr_orig = &val;
  int *&ptr_ref = ptr_orig;

#pragma omp target map(tofrom: ptr_ref[0])
  {
    (*ptr_ref)++;
  }

  assert (val == 6);
}

void test_ref_to_array ()
{
  int arr[1];
  int (&arr_ref)[1] = arr;

  arr_ref[0] = 5;

#pragma omp target map(tofrom: arr_ref[0:1])
  {
    arr_ref[0]++;
  }

  assert (arr_ref[0] == 6);

#pragma omp target map(tofrom: arr_ref[0])
  {
    arr_ref[0]++;
  }

  assert (arr_ref[0] == 7);
}

void test_ref_to_ptr_array ()
{
  int *arr[1];
  int *(&arr_ref)[1] = arr;
  int val = 5;

  arr_ref[0] = &val;

#pragma omp target data map(alloc: arr_ref, arr_ref[0])
  {
#pragma omp target map(tofrom: arr_ref[0][0:1])
    {
      arr_ref[0][0]++;
    }
  }

  assert (arr_ref[0][0] == 6);
}

int main ()
{
  test_ref ();
  test_ref_to_ptr ();
  test_ref_to_array ();
  test_ref_to_ptr_array ();
  return 0;
}
