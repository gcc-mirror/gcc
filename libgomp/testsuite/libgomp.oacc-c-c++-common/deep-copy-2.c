#include <assert.h>
#include <stdlib.h>

int
main(int argc, char* argv[])
{
  struct foo {
    int *a, *b, c, d, *e;
  } s;

  s.a = (int *) malloc (16 * sizeof (int));
  s.b = (int *) malloc (16 * sizeof (int));
  s.e = (int *) malloc (16 * sizeof (int));

  #pragma acc data copy(s)
  {
    #pragma acc data copy(s.a[0:10])
    {
      #pragma acc parallel loop attach(s.a)
      for (int i = 0; i < 10; i++)
	s.a[i] = i;
    }
  }

  for (int i = 0; i < 10; i++)
    assert (s.a[i] == i);

  return 0;
}
