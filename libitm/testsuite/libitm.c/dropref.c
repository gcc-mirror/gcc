/* { dg-xfail-run-if "unsupported" { *-*-* } } */
#include <libitm.h>

char *pp;

int main()
{
  __transaction_atomic {
    _ITM_dropReferences (pp, 555);
  }
  return 0;
}
