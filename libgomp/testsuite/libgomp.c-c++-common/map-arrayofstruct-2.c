#include <stdlib.h>
#include <assert.h>

struct st {
  int *p;
};

int main (void)
{
  struct st s[10];

  for (int i = 0; i < 10; i++)
    s[i].p = (int *) calloc (5, sizeof (int));

  for (int i = 0; i < 10; i++)
    for (int j = 0; j < 10; j++)
      for (int k = 0; k < 10; k++)
	{
	  if (i == j || j == k || i == k)
	    continue;

#pragma omp target map(s[i].p, s[j].p, s[k].p, s[i].p[0:2], s[j].p[1:3], \
		       s[k].p[2])
	  {
	    s[i].p[0]++;
	    s[j].p[1]++;
	    s[k].p[2]++;
	  }

#pragma omp target map(s, s[i].p[0:2], s[j].p[1:3], s[k].p[2])
	  {
	    s[i].p[0]++;
	    s[j].p[1]++;
	    s[k].p[2]++;
	  }

#pragma omp target map(s[0:10], s[i].p[0:2], s[j].p[1:3], s[k].p[2])
	  {
	    s[i].p[0]++;
	    s[j].p[1]++;
	    s[k].p[2]++;
	  }
	}

  for (int i = 0; i < 10; i++)
    {
      assert (s[i].p[0] == 216);
      assert (s[i].p[1] == 216);
      assert (s[i].p[2] == 216);
      free (s[i].p);
    }

  return 0;
}

/* { dg-output "(\n|\r|\r\n)" { target offload_device_nonshared_as } } */
/* { dg-output "libgomp: Mapped array elements must be the same .*(\n|\r|\r\n)+" { target offload_device_nonshared_as } } */
/* { dg-shouldfail "" { offload_device_nonshared_as } } */
