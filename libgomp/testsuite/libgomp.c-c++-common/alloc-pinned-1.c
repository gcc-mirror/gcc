/* { dg-do run } */
/* { dg-additional-options "-foffload-memory=pinned" } */
/* { dg-xfail-run-if "Pinning not implemented on this host" { ! *-*-linux-gnu } } */

#if __cplusplus
#define EXTERNC extern "C"
#else
#define EXTERNC
#endif

/* Intercept the libgomp initialization call to check it happens.  */

int good = 0;

EXTERNC void
GOMP_enable_pinned_mode ()
{
  good = 1;
}

int
main ()
{
  if (!good)
    __builtin_exit (1);

  return 0;
}
