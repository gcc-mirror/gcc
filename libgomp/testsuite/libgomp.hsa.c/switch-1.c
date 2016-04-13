#include <assert.h>

#define s 100

#pragma omp declare target
int
switch1 (int a)
{
  switch (a)
    {
    case 1:
      return 11;
    case 33:
      return 333;
    case 55:
      return 55;
    default:
      return -1;
    }
}

int
switch2 (int a)
{
  switch (a)
    {
    case 1 ... 11:
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
switch3 (int a)
{
  switch (a)
    {
    case 1 ... 11:
      return 11;
    case 12 ... 22:
      return 22;
    case 23 ... 33:
      return 33;
    case 34 ... 44:
      return 44;
    default:
      return 44;
    }
}

int
switch4 (int a, int b)
{
  switch (a)
    {
    case 1 ... 11:
      return a;
    case 12 ... 22:
      return b;
    case 23 ... 33:
      return a;
    case 34 ... 44:
      return b;
    default:
      return 12345;
    }
}

int
switch5 (int a, int b)
{
  switch (a)
    {
    case 1 ... 2:
      return 1;
    case 3 ... 4:
      return 2;
    case 5 ... 6:
      return 3;
    case 7 ... 11:
      return 4;
    }

  return -1;
}
#pragma omp end declare target

int
main (int argc)
{
  int array[s];

#pragma omp target map(tofrom : array[:s])
  {
    for (int i = 0; i < s; i++)
      array[i] = switch1 (i);
  }

  for (int i = 0; i < s; i++)
    assert (array[i] == switch1 (i));

#pragma omp target map(tofrom : array[:s])
  {
    for (int i = 0; i < s; i++)
      array[i] = switch2 (i);
  }

  for (int i = 0; i < s; i++)
    assert (array[i] == switch2 (i));

#pragma omp target map(tofrom : array[:s])
  {
    for (int i = 0; i < s; i++)
      array[i] = switch3 (i);
  }

  for (int i = 0; i < s; i++)
    assert (array[i] == switch3 (i));

#pragma omp target map(tofrom : array[:s])
  {
    for (int i = 0; i < s; i++)
      array[i] = switch4 (i, i + 1);
  }

  for (int i = 0; i < s; i++)
    assert (array[i] == switch4 (i, i + 1));

#pragma omp target map(tofrom : array[:s])
  {
    for (int i = 0; i < s; i++)
      array[i] = switch5 (i, i + 1);
  }

  for (int i = 0; i < s; i++)
    assert (array[i] == switch5 (i, i + 1));
}
