#include <assert.h>

struct Cube
{
  int x;
  int y;
  int z;
};

#pragma omp declare target
int
foo (short a)
{
  switch (a)
    {
    case 1:
      return 11;
      break;
    case 33:
      return 333;
      break;
    case 55:
      return 55;
      break;
    default:
      return -1;
    }
}

int
bar (int a)
{
  int *ptr = &a;

  *ptr = 100;
  return a + *ptr;
}

struct Cube
baz (struct Cube c)
{
  c.x = 11;
  return c;
}

#pragma omp end declare target

#define s 100

int
main (int argc)
{
  /* Test 1: argument types: char to short.  */

  int array[s];
#pragma omp target map(tofrom : array[ : s])
  {
    for (char i = 0; i < s; i++)
      array[i] = foo (i);
  }

  for (int i = 0; i < s; i++)
    assert (array[i] == foo (i));

  /* Test 2: argument address is taken.  */
  int v = 2;

#pragma omp target map(tofrom : v)
  v = bar (v);

  assert (v == 200);

  /* Test 3: passing a structure as a function argument.  */
  struct Cube r;
  struct Cube c = {.x = 1, .y = 2, .z = 3};

#pragma omp target map(to : r) map(from : c)
  r = baz (c);

  assert (r.x == 11);
  assert (r.y == c.y);
  assert (r.z == c.z);
}
