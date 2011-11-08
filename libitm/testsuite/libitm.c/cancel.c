#include <stdlib.h>
#include <libitm.h>

unsigned char pp[100];

void __attribute((transaction_may_cancel_outer,noinline)) cancel1()
{
  __transaction_cancel [[outer]];
}

int a, b;

int main()
{
  a = b = 0;

  __transaction_atomic {
    a = 1;
    __transaction_atomic {
      b = 1;
      __transaction_cancel;
    }
  }
  if (a != 1 || b != 0)
    abort();
  if (_ITM_inTransaction() != outsideTransaction)
    abort();

  __transaction_atomic [[outer]] {
    a = 2;
    __transaction_atomic {
      b = 2;
      __transaction_cancel [[outer]];
    }
  }
  if (a != 1 || b != 0)
    abort();
  if (_ITM_inTransaction() != outsideTransaction)
    abort();

  __transaction_atomic [[outer]] {
    a = 2;
    __transaction_atomic {
      b = 2;
      __transaction_cancel [[outer]];
      cancel1();
    }
  }
  if (a != 1 || b != 0)
    abort();
  if (_ITM_inTransaction() != outsideTransaction)
    abort();

  return 0;
}
