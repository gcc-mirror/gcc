#include <assert.h>
#include <stdlib.h>

struct str1 {
  int a;
  int b;
  int *c;
};

#define N 1024

int
main (int argc, char *argv[])
{
  struct str1 s;

  s.a = 1;
  s.b = 2;
  s.c = (int *) malloc (sizeof (int) * N);

  for (int i = 0; i < N; i++)
    s.c[i] = i + 10;

  #pragma acc enter data copyin(s.a, s.b, s.c[0:N])

  #pragma acc serial present(s.a, s.b, s.c[0:N]) /* { dg-warning "using .vector_length \\(32\\)., ignoring 1" "" { target openacc_nvidia_accel_selected } } */
  {
    s.a = 3;
    s.b = 4;
    for (int i = 0; i < N; i++)
      s.c[i] = i + 20;
  }

  #pragma acc exit data copyout(s.a, s.b, s.c[0:N])

  assert (s.a == 3);
  assert (s.b == 4);
  for (int i = 0; i < N; i++)
    assert (s.c[i] == i + 20);

  free (s.c);

  return 0;
}
