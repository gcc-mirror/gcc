/* { dg-skip-if "" { *-*-* } } */

#pragma acc routine nohost
int f1(int x)
{
  /* Make sure this fails host compilation.  */
#if defined ACC_DEVICE_TYPE_host
  asm ("IT'S A TRAP");
#elif defined ACC_DEVICE_TYPE_nvidia
  asm ("{\n\t  .reg .u32 %tid_x;\n\t  mov.u32 %tid_x, %tid.x;\n\t}");
#elif defined ACC_DEVICE_TYPE_radeon
  asm ("s_nop 0");
#else
# error Not ported to this ACC_DEVICE_TYPE
#endif

  return 2 * x;
}
