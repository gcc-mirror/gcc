#include <assert.h>

#define s 100

#pragma omp declare target
int
switch1 (unsigned a)
{
  switch (a)
    {
    case 1 ... 11:
      return 11;
    case 12 ... 13:
      return 22;
    default:
      return 44;
    }
}

int
switch2 (unsigned a)
{
  switch (a)
    {
    case 1 ... 5:
      return 1;
    case 9 ... 11:
      return a + 3;
    case 12 ... 13:
      return a + 3;
    default:
      return 44;
    }
}

#define OFFSET 12

int
switch3 (unsigned a)
{
  switch (a)
    {
    case (OFFSET + 0):
      return 1;
    case (OFFSET + 1)...(OFFSET + 11):
      return 11;
    case (OFFSET + 12)...(OFFSET + 13):
      return (OFFSET + 22);
    default:
      return (OFFSET + 44);
    }
}

int
switch4 (unsigned a)
{
  switch (a)
    {
    case -2:
      return 1;
    case -1:
      return a + 3;
    case 3:
      return a + 3;
    default:
      return 44;
    }
}
#pragma omp end declare target

#define low -33
#define high 55

int
main (int argc)
{
  int array[s];

#pragma omp target map(tofrom : array[:s])
  {
    for (int i = low; i < high; i++)
      array[i - low] = switch1 (i);
  }

  for (int i = low; i < high; i++)
    assert (array[i - low] == switch1 (i));

#pragma omp target map(tofrom : array[:s])
  {
    for (int i = low; i < high; i++)
      array[i - low] = switch2 (i);
  }

  for (int i = low; i < high; i++)
    assert (array[i - low] == switch2 (i));

#pragma omp target map(tofrom : array[:s])
  {
    for (int i = low; i < high; i++)
      array[i - low] = switch3 (i);
  }

  for (int i = low; i < high; i++)
    assert (array[i - low] == switch3 (i));

#pragma omp target map(tofrom : array[:s])
  {
    for (int i = low; i < high; i++)
      array[i - low] = switch4 (i);
  }

  for (int i = low; i < high; i++)
    assert (array[i - low] == switch4 (i));

  return 0;
}
