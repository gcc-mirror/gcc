#include <cstring>
#include <cassert>

static int lo()
{
  return 30;
}

static int len()
{
  return 10;
}

template<typename T>
void foo ()
{
  T arr[100];
  T *ptr;

  memset (arr, '\0', sizeof arr);

#pragma omp target enter data map(to: arr[0:100])

  for (int i = 0; i < 100; i++)
    arr[i] = i;

  ptr = &arr[10];

#pragma omp target update to(*ptr)

  for (int i = lo (); i < lo () + len (); i++)
    arr[i] = i * 2;

#pragma omp target update to(arr[lo():len()])

#pragma omp target exit data map(from: arr[0:100])

  assert (arr[10] == 10);
  for (int i = lo (); i < lo () + len (); i++)
    assert (arr[i] == i * 2);
}

int
main ()
{
  char arr[100];
  char *ptr;

  memset (arr, '\0', sizeof arr);

#pragma omp target enter data map(to: arr[0:100])

  for (int i = 0; i < 100; i++)
    arr[i] = i;

  ptr = &arr[10];

#pragma omp target update to(*ptr)

  for (int i = lo (); i < lo () + len (); i++)
    arr[i] = i * 2;

#pragma omp target update to(arr[lo():len()])

#pragma omp target exit data map(from: arr[0:100])

  assert (arr[10] == 10);
  for (int i = lo (); i < lo () + len (); i++)
    assert (arr[i] == i * 2);

  foo<short> ();

  return 0;
}

