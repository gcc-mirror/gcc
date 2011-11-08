/* { dg-xfail-run-if "unsupported" { *-*-* } } */
#include <stdlib.h>
#include <libitm.h>

/* Test that _ITM_dropReferences() forces a commit of given chunk.  */

unsigned char pp[100];

int main()
{
  int i;

  for(i=0; i < 100; ++i)
    pp[i]=0x22;

  __transaction_atomic {
    for(i=0; i < 100; ++i)
      pp[i]=0x33;

    /* This should write-through pp[0..49]...  */
    _ITM_dropReferences (pp, 50);

    /* ...while this should revert everything but pp[0..49].  */
    __transaction_cancel;
  }

  for(i=0; i < 50; ++i)
    if (pp[i] != 0x33)
      abort();

  for(i=50; i < 100; ++i)
    if (pp[i] != 0x22)
      abort();

  return 0;
}
