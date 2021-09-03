/* { dg-shouldfail "error directive" } */

#ifdef __cplusplus
extern "C"
#endif
void abort ();

int
foo (int i, int x)
{
  if (x)
    #pragma omp error severity(warning)	/* { dg-warning "'pragma omp error' encountered" } */
    i++;
  if (!x)
    ;
  else
    #pragma omp error severity(warning)	/* { dg-warning "'pragma omp error' encountered" } */
    i += 2;
  switch (0)
    #pragma omp error severity(warning)	/* { dg-warning "'pragma omp error' encountered" } */
    {
    default:
      break;
    }
  while (0)
    #pragma omp error message("42 - 1")	severity (warning) /* { dg-warning "'pragma omp error' encountered: 42 - 1" } */
    i += 4;
  lab:
  #pragma omp error severity(warning) message("bar") at(compilation)	/* { dg-warning "'pragma omp error' encountered: bar" } */
    i += 8;
  return i;
}

int
main ()
{
  /* Initialize offloading early, so that any output this may produce doesn't
     disturb the 'dg-output' scanning below.  */
  #pragma omp target
  ;

  if (foo (5, 0) != 13 || foo (6, 1) != 17)
    abort ();
  #pragma omp error at (execution) severity (warning)
  const char *msg = "my message" + 2;
  #pragma omp error at (execution) severity (warning) message (msg + 1)
  #pragma omp target
  {
    #pragma omp error at (execution) severity (warning) message ("hello from a distance")
  }
  #pragma omp error at (execution) severity (fatal) message (msg - 2)
  #pragma omp error at (execution) severity (warning) message ("foobar")
  return 0;
}

/* { dg-output "libgomp: error directive encountered(\n|\r|\r\n)(\n|\r|\r\n)" } */
/* { dg-output "libgomp: error directive encountered: message(\n|\r|\r\n)(\n|\r|\r\n)" } */
/* { dg-output "libgomp: error directive encountered: hello from a distance(\n|\r|\r\n)(\n|\r|\r\n)" } */
/* { dg-output "libgomp: fatal error: error directive encountered: my message" } */
