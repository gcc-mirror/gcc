/* { dg-do link } */

extern int three (void);

#pragma acc routine (three) nohost
__attribute__((noinline))
int three(void)
{
  return 3;
}

int main(void)
{
  return (three() == 3) ? 0 : 1;
}

/* Expecting link to fail; "undefined reference to `three'" (or similar).
   { dg-excess-errors "" } */
