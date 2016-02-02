#include <omp.h>
#include <stdlib.h>

int a = 1, b = 2, c = 3, d = 4;
int e[2] = { 5, 6 }, f[2] = { 7, 8 }, g[2] = { 9, 10 }, h[2] = { 11, 12 };

__attribute__((noinline, noclone)) void
use (int *k, int *l, int *m, int *n, int *o, int *p, int *q, int *r)
{
  asm volatile ("" : : "r" (k) : "memory");
  asm volatile ("" : : "r" (l) : "memory");
  asm volatile ("" : : "r" (m) : "memory");
  asm volatile ("" : : "r" (n) : "memory");
  asm volatile ("" : : "r" (o) : "memory");
  asm volatile ("" : : "r" (p) : "memory");
  asm volatile ("" : : "r" (q) : "memory");
  asm volatile ("" : : "r" (r) : "memory");
}

#pragma omp declare target to (use)

int
main ()
{
  int err = 0, r = -1, t[4];
  long s[4] = { -1, -2, -3, -4 };
  int j = 13, k = 14, l[2] = { 15, 16 }, m[2] = { 17, 18 };
  #pragma omp target private (a, b, e, f) firstprivate (c, d, g, h) map(from: r, s, t) \
		     map(tofrom: err, j, l) map(to: k, m)
  #pragma omp teams num_teams (4) thread_limit (8) private (b, f) firstprivate (d, h, k, m)
  {
    int u1 = k, u2[2] = { m[0], m[1] };
    int u3[64];
    int i;
    for (i = 0; i < 64; i++)
      u3[i] = k + i;
    #pragma omp parallel num_threads (1)
    {
      int v1, v2, v3;
      #pragma omp atomic read
	v1 = c;
      #pragma omp atomic read
	v2 = g[0];
      #pragma omp atomic read
	v3 = g[1];
      if ((v1 < 3 || v1 > 6)
	  || d != 4
	  || (v2 < 9 || v2 > 15 || (v2 & 1) == 0)
	  || (v3 < 10 || v3 > 19 || ((v3 - 10) % 3) != 0)
	  || h[0] != 11 || h[1] != 12 || k != 14 || m[0] != 17 || m[1] != 18)
	#pragma omp atomic write
	  err = 1;
      b = omp_get_team_num ();
      if (b >= 4)
	#pragma omp atomic write
	  err = 1;
      if (b == 0)
	{
	  a = omp_get_num_teams ();
	  e[0] = 2 * a;
	  e[1] = 3 * a;
	}
      f[0] = 2 * b;
      f[1] = 3 * b;
      #pragma omp atomic update
	c++;
      #pragma omp atomic update
	g[0] += 2;
      #pragma omp atomic update
	g[1] += 3;
      d++;
      h[0] += 2;
      h[1] += 3;
      k += b;
      m[0] += 2 * b;
      m[1] += 3 * b;
    }
    use (&a, &b, &c, &d, e, f, g, h);
    #pragma omp parallel firstprivate (u1, u2)
    {
      int w = omp_get_thread_num ();
      int x = 19;
      int y[2] = { 20, 21 };
      int v = 24;
      int ll[64];
      if (u1 != 14 || u2[0] != 17 || u2[1] != 18)
	#pragma omp atomic write
	  err = 1;
      u1 += w;
      u2[0] += 2 * w;
      u2[1] += 3 * w;
      use (&u1, u2, &t[b], l, &k, m, &j, h);
      #pragma omp master
	t[b] = omp_get_num_threads ();
      #pragma omp atomic update
	j++;
      #pragma omp atomic update
	l[0] += 2;
      #pragma omp atomic update
	l[1] += 3;
      #pragma omp atomic update
	k += 4;
      #pragma omp atomic update
	m[0] += 5;
      #pragma omp atomic update
	m[1] += 6;
      x += w;
      y[0] += 2 * w;
      y[1] += 3 * w;
      #pragma omp simd safelen(32) private (v)
      for (i = 0; i < 64; i++)
	{
	  v = 3 * i;
	  ll[i] = u1 + v * u2[0] + u2[1] + x + y[0] + y[1] + v + h[0] + u3[i];
	}
      #pragma omp barrier
      use (&u1, u2, &t[b], l, &k, m, &x, y);
      if (w < 0 || w > 8 || w != omp_get_thread_num () || u1 != 14 + w
	  || u2[0] != 17 + 2 * w || u2[1] != 18 + 3 * w
	  || x != 19 + w || y[0] != 20 + 2 * w || y[1] != 21 + 3 * w
	  || v != 24)
	#pragma omp atomic write
	  err = 1;
      for (i = 0; i < 64; i++)
	if (ll[i] != u1 + 3 * i * u2[0] + u2[1] + x + y[0] + y[1] + 3 * i + 13 + 14 + i)
	  #pragma omp atomic write
	    err = 1;
    }
    #pragma omp parallel num_threads (1)
    {
      if (b == 0)
	{
	  r = a;
	  if (a != omp_get_num_teams ()
	      || e[0] != 2 * a
	      || e[1] != 3 * a)
	    #pragma omp atomic write
	      err = 1;
	}
      int v1, v2, v3;
      #pragma omp atomic read
	v1 = c;
      #pragma omp atomic read
	v2 = g[0];
      #pragma omp atomic read
	v3 = g[1];
      s[b] = v1 * 65536L + v2 * 256L + v3;
      if (d != 5 || h[0] != 13 || h[1] != 15
	  || k != 14 + b + 4 * t[b]
	  || m[0] != 17 + 2 * b + 5 * t[b]
	  || m[1] != 18 + 3 * b + 6 * t[b]
	  || b != omp_get_team_num ()
	  || f[0] != 2 * b || f[1] != 3 * b)
	#pragma omp atomic write
	  err = 1;
    }
  }
  if (err != 0) abort ();
  if (r < 1 || r > 4) abort ();
  if (a != 1 || b != 2 || c != 3 || d != 4) abort ();
  if (e[0] != 5 || e[1] != 6 || f[0] != 7 || f[1] != 8) abort ();
  if (g[0] != 9 || g[1] != 10 || h[0] != 11 || h[1] != 12) abort ();
  int i, cnt = 0;
  for (i = 0; i < r; i++)
    if ((s[i] >> 16) < 3 + 1 || (s[i] >> 16) > 3 + 4
	|| ((s[i] >> 8) & 0xff) < 9 + 2 * 1 || ((s[i] >> 8) & 0xff) > 9 + 2 * 4
	|| (s[i] & 0xff) < 10 + 3 * 1 || (s[i] & 0xff) > 10 + 3 * 4
	|| t[i] < 1 || t[i] > 8)
      abort ();
    else
      cnt += t[i];
  if (j != 13 + cnt || l[0] != 15 + 2 * cnt || l[1] != 16 + 3 * cnt) abort ();
  return 0;
}
