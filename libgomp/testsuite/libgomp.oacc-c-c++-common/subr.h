
#if ACC_DEVICE_TYPE_nvidia

#pragma acc routine nohost
static int clock (void)
{
  int thetime;

  asm __volatile__ ("mov.u32 %0, %%clock;" : "=r"(thetime));

  return thetime;
}

#endif

void
delay (unsigned long *d_o, unsigned long delay)
{
  int start, ticks;

  start = clock ();

  ticks = 0;

  while (ticks < delay)
    ticks = clock () - start;

  return;
}

void
delay2 (unsigned long *d_o, unsigned long delay, unsigned long tid)
{
  int start, ticks;

  start = clock ();

  ticks = 0;

  while (ticks < delay)
    ticks = clock () - start;

  d_o[0] = tid;

  return;
}
