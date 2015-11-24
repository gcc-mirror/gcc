#include_next <asm/ptrace.h>
/* ARM_VFPREGS_SIZE has been added in 3.0 */
#if defined(__arm__) && !defined(ARM_VFPREGS_SIZE)
/* The size of the user-visible VFP state as seen by PTRACE_GET/SETVFPREGS
   and core dumps.  */
#define ARM_VFPREGS_SIZE ( 32 * 8 /*fpregs*/ + 4 /*fpscr*/ )
#endif
