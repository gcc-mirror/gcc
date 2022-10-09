#include <stdlib.h>
#include <assert.h>

struct st {
  int *p;
};

struct tt {
  struct st a[10];
};

struct ut {
  struct tt *t;
};

int main (void)
{
  struct tt *t = (struct tt *) malloc (sizeof *t);
  struct ut *u = (struct ut *) malloc (sizeof *u);

  for (int i = 0; i < 10; i++)
    t->a[i].p = (int *) calloc (5, sizeof (int));

  u->t = t;

  for (int i = 0; i < 10; i++)
    for (int j = 0; j < 10; j++)
      for (int k = 0; k < 10; k++)
	{
	  if (i == j || j == k || i == k)
	    continue;

	  /* This one can use "firstprivate" for T...  */
#pragma omp target map(t->a[i].p, t->a[j].p, t->a[k].p, \
		       t->a[i].p[0:2], t->a[j].p[1:3], t->a[k].p[2])
	  {
	    t->a[i].p[0]++;
	    t->a[j].p[1]++;
	    t->a[k].p[2]++;
	  }

	  /* ...but this one must use attach/detach for T.  */
#pragma omp target map(u->t, u->t->a[i].p, u->t->a[j].p, u->t->a[k].p, \
		       u->t->a[i].p[0:2], u->t->a[j].p[1:3], u->t->a[k].p[2])
	  {
	    u->t->a[i].p[0]++;
	    u->t->a[j].p[1]++;
	    u->t->a[k].p[2]++;
	  }
	}

  for (int i = 0; i < 10; i++)
    {
      assert (t->a[i].p[0] == 144);
      assert (t->a[i].p[1] == 144);
      assert (t->a[i].p[2] == 144);
      free (t->a[i].p);
    }

  free (u);
  free (t);

  return 0;
}

/* { dg-output "(\n|\r|\r\n)" { target offload_device_nonshared_as } } */
/* { dg-output "libgomp: Mapped array elements must be the same .*(\n|\r|\r\n)+" { target offload_device_nonshared_as } } */
/* { dg-shouldfail "" { offload_device_nonshared_as } } */
