#include <stdlib.h>

struct blk { int x, y; };
struct L
{
  #define N 10
  struct {
    int num_blocks[N];
    struct blk * blocks[N];
  } m;
};

void foo (struct L *l)
{
  for (int i = 0; i < N; i++)
    {
      l->m.blocks[i] = (struct blk *) malloc (sizeof (struct blk) * N);
      l->m.num_blocks[i] = N;
    }

  #pragma omp target enter data map(to:l[:1])
  for (int i = 0; i < N; i++)
    {
      #pragma omp target enter data map(to:l->m.blocks[i][:l->m.num_blocks[i]])
    }

  #pragma omp target
  {
    for (int i = 0; i < N; i++)
      for (int j = 0; j < N; j++)
	{
	  l->m.blocks[i][j].x = i + j;
	  l->m.blocks[i][j].y = i * j;
	}
  }

  for (int i = 0; i < N; i++)
    {
      #pragma omp target exit data map(from:l->m.blocks[i][:l->m.num_blocks[i]])
    }
  #pragma omp target exit data map(from:l[:1])


  for (int i = 0; i < N; i++)
    for (int j = 0; j < N; j++)
      {
	if (l->m.blocks[i][j].x != i + j)
	  abort ();
	if (l->m.blocks[i][j].y != i * j)
	  abort ();
      }

}

int main (void)
{
  struct L l;
  foo (&l);
  return 0;
}
