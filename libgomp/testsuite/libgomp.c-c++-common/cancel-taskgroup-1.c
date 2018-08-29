/* { dg-do run } */
/* { dg-set-target-env-var OMP_CANCELLATION "true" } */

#include <stdlib.h>
#include <omp.h>

struct T { struct T *children[2]; int val; };

struct T *
search (struct T *tree, int val, int lvl)
{
  if (tree == NULL || tree->val == val)
    return tree;
  struct T *ret = NULL;
  int i;
  for (i = 0; i < 2; i++)
    #pragma omp task shared(ret) if(lvl < 10)
    {
      struct T *r = search (tree->children[i], val, lvl + 1);
      if (r)
	{
	  #pragma omp atomic write
	  ret = r;
	  #pragma omp cancel taskgroup
	}
    }
  #pragma omp taskwait
  return ret;
}

struct T *
searchp (struct T *tree, int val)
{
  struct T *ret;
  #pragma omp parallel shared(ret) firstprivate (tree, val)
  #pragma omp single
  #pragma omp taskgroup
  ret = search (tree, val, 0);
  return ret;
}

int
main ()
{
  /* Must be power of two minus 1.  */
  int size = 0x7ffff;
  struct T *trees = (struct T *) malloc (size * sizeof (struct T));
  if (trees == NULL)
    return 0;
  int i, l = 1, b = 0;
  for (i = 0; i < size; i++)
    {
      if (i == l)
	{
	  b = l;
	  l = l * 2 + 1;
	}
      trees[i].val = i;
      trees[i].children[0] = l == size ? NULL : &trees[l + (i - b) * 2];
      trees[i].children[1] = l == size ? NULL : &trees[l + (i - b) * 2 + 1];
    }
  for (i = 0; i < 50; i++)
    {
      int v = random () & size;
      if (searchp (&trees[0], v) != &trees[v])
	abort ();
    }
  free (trees);
  return 0;
}
