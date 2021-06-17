#include <cstdlib>

#define N 10
int main (void)
{
  int X, Y;
  #pragma omp target map(from: X, Y)
  {
    int x = 0, y = 0;

    for (int i = 0; i < N; i++)
      [&] (int v) { x += v; } (i);

    auto yinc = [&y] { y++; };
    for (int i = 0; i < N; i++)
      yinc ();

    X = x;
    Y = y;
  }

  int Xs = 0;
  for (int i = 0; i < N; i++)
    Xs += i;
  if (X != Xs)
    abort ();

  if (Y != N)
    abort ();
}
