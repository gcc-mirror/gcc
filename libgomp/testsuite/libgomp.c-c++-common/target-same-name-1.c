/* { dg-additional-sources "target-same-name-1-a.c target-same-name-1-b.c" } */
/* PR middle-end/104285 */

/* Both files create the same static symbol, which caused issues
   in non-host lto1. */

int one ();
int two ();
int one_get_inc2_local_link ();
int two_get_inc4_local_link ();
int one_get_inc3_link_a ();
int two_get_inc5_link_a ();

int
main ()
{
  if (one () != 5)
    __builtin_abort ();
  if (two () != 7)
    __builtin_abort ();

  if (one_get_inc2_local_link () != 42)
    __builtin_abort ();
  if (two_get_inc4_local_link () != 55)
    __builtin_abort ();
  if (one_get_inc2_local_link () != 42+2)
    __builtin_abort ();
  if (two_get_inc4_local_link () != 55+4)
    __builtin_abort ();

  if (one_get_inc3_link_a () != 123)
    __builtin_abort ();
  if (two_get_inc5_link_a () != 123+3)
    __builtin_abort ();

/* FIXME: The last call did not increment the global var. */
/* PR middle-end/105015  */
#if 0
  if (one_get_inc3_link_a () != 123+3+5)
    __builtin_abort ();
  if (two_get_inc5_link_a () != 123+3+5+3)
    __builtin_abort ();
#endif

  return 0;
}
