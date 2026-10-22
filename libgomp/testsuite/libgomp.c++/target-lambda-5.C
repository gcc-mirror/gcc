// PR c++/127559
// { dg-do run }
// { dg-additional-options -fdump-tree-gimple }

void
foo (int *a)
{
  #pragma omp target map (to: a[0])
    {
      auto b = [=] { return *a; };
      auto c = [=] { return b (); };
      if (c () != 42)
	__builtin_abort ();
    }
}

int
main ()
{
  int a = 42;
  foo (&a);
}

/* Locally declared lambdas 'b', 'c' must not be mapped.
   { dg-final { scan-tree-dump {(?n)#pragma omp target num_teams.* map\(to:\*a \[len: [0-9]+\]\) map\(firstprivate:a \[pointer assign, bias: 0\]\)$} gimple } } */
