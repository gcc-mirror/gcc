#include "f2c.h"
#include "signal1.h"

void *
G77_signal_0 (integer * sigp, sig_pf proc)
{
  int sig;
  sig = (int) *sigp;

  return (void *) signal (sig, proc);
}
