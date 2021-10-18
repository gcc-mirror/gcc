/* { dg-do run } */

double d;
long double ld;

int
main ()
{
  double e = __builtin_copysign (0.0, -1.0), v;
  long double le = __builtin_copysignl (0.0L, -1.0L), lv;
  if (__builtin_memcmp (&d, &e, sizeof (d)) != 0)
    {
      /* Verify == comparison for atomics is done as with memcmp.  */
      #pragma omp atomic compare
      d = d == e ? 5.0 : d;
      #pragma omp atomic read
      v = d;
      if (v != 0.0)
	__builtin_abort ();
      #pragma omp atomic compare
      d = d == 0.0 ? 5.0 : d;
      #pragma omp atomic read
      v = d;
      if (v != 5.0)
	__builtin_abort ();
    }
  if (__builtin_memcmp (&ld, &le, sizeof (ld)) != 0)
    {
      __builtin_memset (&ld, 0xff, sizeof (ld));
      #pragma omp atomic write
      ld = 0.0L;
      __asm volatile ("" : : "g" (&ld) : "memory");
      /* Verify == comparison for atomics is done as with memcmp
	 with __builtin_clear_padding if needed.  */
      #pragma omp atomic compare
      ld = ld == le ? 5.0L : ld;
      #pragma omp atomic read
      lv = ld;
      if (lv != 0.0L)
	__builtin_abort ();
      #pragma omp atomic compare
      ld = ld == 0.0L ? 5.0L : ld;
      #pragma omp atomic read
      lv = ld;
      if (lv != 5.0L)
	__builtin_abort ();
    }
  return 0;
}
