// Defaults, if not "#include"d from ../libgomp.oacc-c++/routine-1-*.C.
#ifndef TEMPLATE
# define TEMPLATE
# define TYPE int
# define RETURN_1 TYPE
# define RETURN_2 
#endif

#include <stdlib.h>

#pragma acc routine
TEMPLATE
RETURN_1 fact(TYPE n) RETURN_2
{
  if (n == 0 || n == 1)
    return 1;
  else
    return n * fact (n - 1);
}

int main()
{
  int *s, *g, *w, *v, *gw, *gv, *wv, *gwv, i, n = 10;

  s = (int *) malloc (sizeof (int) * n);
  g = (int *) malloc (sizeof (int) * n);
  w = (int *) malloc (sizeof (int) * n);
  v = (int *) malloc (sizeof (int) * n);
  gw = (int *) malloc (sizeof (int) * n);
  gv = (int *) malloc (sizeof (int) * n);
  wv = (int *) malloc (sizeof (int) * n);
  gwv = (int *) malloc (sizeof (int) * n);

#pragma acc parallel loop async copyout(s[0:n]) seq
  for (i = 0; i < n; i++)
    s[i] = fact (i);

#pragma acc parallel loop async copyout(g[0:n]) gang
  for (i = 0; i < n; i++)
    g[i] = fact (i);

#pragma acc parallel loop async copyout(w[0:n]) worker
  for (i = 0; i < n; i++)
    w[i] = fact (i);

#pragma acc parallel loop async copyout(v[0:n]) vector
  for (i = 0; i < n; i++)
    v[i] = fact (i);

#pragma acc parallel loop async copyout(gw[0:n]) gang worker
  for (i = 0; i < n; i++)
    gw[i] = fact (i);

#pragma acc parallel loop async copyout(gv[0:n]) gang vector
  for (i = 0; i < n; i++)
    gv[i] = fact (i);

#pragma acc parallel loop async copyout(wv[0:n]) worker vector
  for (i = 0; i < n; i++)
    wv[i] = fact (i);

#pragma acc parallel loop async copyout(gwv[0:n]) gang worker vector
  for (i = 0; i < n; i++)
    gwv[i] = fact (i);

#pragma acc wait

  for (i = 0; i < n; i++)
    if (s[i] != fact (i))
      abort ();
  for (i = 0; i < n; i++)
    if (g[i] != s[i])
      abort ();
  for (i = 0; i < n; i++)
    if (w[i] != s[i])
      abort ();
  for (i = 0; i < n; i++)
    if (v[i] != s[i])
      abort ();
  for (i = 0; i < n; i++)
    if (gw[i] != s[i])
      abort ();
  for (i = 0; i < n; i++)
    if (gv[i] != s[i])
      abort ();
  for (i = 0; i < n; i++)
    if (wv[i] != s[i])
      abort ();
  for (i = 0; i < n; i++)
    if (gwv[i] != s[i])
      abort ();

  return 0;
}
