void abort (void);

int x[10000000];

void parloop (int N)
{
  int i;

  for (i = 0; i < N; i++)
    x[i] = i + 1;

  for (i = 0; i < N; i++)
    x[i] = i + 3;

  for (i = 0; i < N; i++)
    {
      if (x[i] != i + 3)
	abort ();
    }
}

int main(void)
{
  parloop(10000000);

  return 0;
}

/* Check that parallel code generation part make the right answer.  */
/* { dg-final { scan-tree-dump-times "2 loops carried no dependency" 1 "graphite" } } */
/* { dg-final { scan-tree-dump-times "loopfn" 8 "optimized" } } */
