extern
#ifdef __cplusplus
"C"
#endif
void abort (void);
#pragma omp declare reduction (foo: int: omp_out += omp_in - 1) initializer (omp_priv = 1)

int
main ()
{
  int i, v = 0;
  unsigned long long j;
  volatile unsigned long long sixtyfour = 64;
  int a = 0, b = 0, c = 0, d = 0, e = 0, f = 0, g = 0;
  #pragma omp parallel
  {
#define P(str) _Pragma (#str)
#define ONE_ORDERED_LOOP(var, i, max, n, clauses...) \
    P (omp for ordered reduction (task, foo: var) clauses)		  \
    for (i = 0; i < max; i++)						  \
      {									  \
        var++;								  \
        P (omp task in_reduction (foo: var))				  \
	var++;								  \
	_Pragma ("omp ordered")						  \
	if (v++ != i + n)						  \
	  abort ();							  \
      }									  \
    if (var != 128 || v != 64 + n)					  \
      abort ();								  \
    _Pragma ("omp barrier")
    ONE_ORDERED_LOOP (a, i, 64, 0, )
    ONE_ORDERED_LOOP (b, i, 64, 64, schedule (monotonic: static))
    ONE_ORDERED_LOOP (c, i, 64, 128, schedule (static, 1))
    ONE_ORDERED_LOOP (d, i, 64, 192, schedule (monotonic: runtime))
    ONE_ORDERED_LOOP (e, i, 64, 256, schedule (dynamic, 2))
    ONE_ORDERED_LOOP (f, i, 64, 320, schedule (monotonic: guided, 3))
    ONE_ORDERED_LOOP (g, i, 64, 384, schedule (auto))
    #pragma omp single
    { v = 0; a = 0; b = 0; c = 0; d = 0; e = 0; f = 0; g = 0; }
    ONE_ORDERED_LOOP (a, j, sixtyfour, 0, )
    ONE_ORDERED_LOOP (b, j, sixtyfour, 64, schedule (static))
    ONE_ORDERED_LOOP (c, j, sixtyfour, 128, schedule (monotonic: static, 1))
    ONE_ORDERED_LOOP (d, j, sixtyfour, 192, schedule (runtime))
    ONE_ORDERED_LOOP (e, j, sixtyfour, 256, schedule (monotonic: dynamic, 2))
    ONE_ORDERED_LOOP (f, j, sixtyfour, 320, schedule (guided, 3))
    ONE_ORDERED_LOOP (g, j, sixtyfour, 384, schedule (monotonic: auto))
  }
  return 0;
}
