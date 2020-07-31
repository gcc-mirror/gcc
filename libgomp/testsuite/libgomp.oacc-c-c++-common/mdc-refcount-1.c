/* { dg-skip-if "" { *-*-* } { "-DACC_MEM_SHARED=1" } } */

#include <openacc.h>
#include <assert.h>

#define N 1024

struct mystr {
  int *data;
};

static void
test (unsigned variant)
{
  int arr[N];
  struct mystr s;

  s.data = arr;

  acc_copyin (&s, sizeof (s));
  acc_create (s.data, N * sizeof (int));

  for (int i = 0; i < 20; i++)
    {
      if ((variant + i) % 1)
	{
#pragma acc enter data attach(s.data)
	}
      else
	acc_attach ((void **) &s.data);

      if ((variant + i) % 2)
	{
#pragma acc exit data detach(s.data)
	}
      else
	acc_detach ((void **) &s.data);
    }

  assert (acc_is_present (arr, N * sizeof (int)));
  assert (acc_is_present (&s, sizeof (s)));

  acc_delete (arr, N * sizeof (int));

  assert (!acc_is_present (arr, N * sizeof (int)));

  acc_copyout (&s, sizeof (s));

  assert (!acc_is_present (&s, sizeof (s)));
  assert (s.data == arr);
}

int
main (int argc, char *argv[])
{
  for (unsigned variant = 0; variant < 4; ++variant)
    test (variant);

  return 0;
}
