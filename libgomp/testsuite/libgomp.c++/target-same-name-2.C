/* { dg-additional-sources "target-same-name-2-a.C target-same-name-2-b.C" } */
/* PR middle-end/104285 */

/* Both files create the same symbol, which caused issues
   in non-host lto1. */

int test_a ();
int test_a2 ();
int test_b ();
int test_b2 ();

int
main ()
{
  if (test_a () != 42)
    __builtin_abort ();
  if (test_a2 () != 441)
    __builtin_abort ();
  if (test_b () != 42)
    __builtin_abort ();
  if (test_b2 () != 442)
    __builtin_abort ();
  return 0;
}
