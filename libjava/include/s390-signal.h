// s390-signal.h - Catch runtime signals and turn them into exceptions
// on an s390 based Linux system.

/* Copyright (C) 2002, 2010  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */


#ifndef JAVA_SIGNAL_H
#define JAVA_SIGNAL_H 1

#include <signal.h>
#include <sys/syscall.h>
#include <ucontext.h>
#include <limits.h>

#define HANDLE_SEGV 1
#define HANDLE_FPE 1

#define SIGNAL_HANDLER(_name)	\
static void _name (int, siginfo_t *_si __attribute__((unused)), \
		   ucontext_t *_uc __attribute__((unused)))

/* We no longer need to fiddle with the PSW address in the signal handler;
   this is now all handled correctly in MD_FALLBACK_FRAME_STATE_FOR.  */
#define MAKE_THROW_FRAME(_exception)


/* According to the JVM spec, "if the dividend is the negative integer 
   of the smallest magnitude and the divisor is -1, then overflow occurs 
   and the result is equal to the dividend.  Despite the overflow, no 
   exception occurs".

   We handle this by inspecting the instruction which generated the signal,
   and if dividend and divisor are as above, we simply return from the signal
   handler.  This causes execution to continue after the instruction.  
   Before returning, we the set result registers as expected.  */
#define UC_EXTENDED	0x00000001

#define HANDLE_DIVIDE_OVERFLOW						\
do									\
{									\
  unsigned char *_eip = (unsigned char *)				\
    __builtin_extract_return_addr (_si->si_addr);			\
  unsigned long *_regs = _uc->uc_mcontext.gregs;			\
  int _r1, _r2, _d2, _x2, _b2;						\
  struct                                                                \
  {                                                                     \
    unsigned long int uc_flags;                                         \
    struct ucontext *uc_link;                                           \
    stack_t uc_stack;                                                   \
    mcontext_t uc_mcontext;                                             \
    unsigned long sigmask[2];                                           \
    unsigned long ext_regs[16];						\
  } *_uc_ext = (typeof(_uc_ext))_uc;					\
									\
  /* First, a couple of helper routines to decode instructions.  */	\
  struct _decode 							\
    {									\
      /* Decode RR instruction format.  */				\
      static inline int _is_rr (unsigned char *_eip, 			\
				unsigned char _op,			\
				int *_r1, int *_r2) 			\
      {									\
	if (_eip[0] == _op)						\
	  {								\
	    *_r1 = _eip[1] >> 4;					\
	    *_r2 = _eip[1] & 0xf;					\
	    return 1;							\
	  }								\
	return 0;							\
      }									\
									\
      /* Decode RX instruction format.  */				\
      static inline int _is_rx (unsigned char *_eip,			\
				unsigned char _op,			\
				int *_r1, int *_d2, int *_x2, int *_b2) \
      {									\
	if (_eip[0] == _op)						\
	  {								\
	    *_r1 = _eip[1] >> 4;					\
	    *_x2 = _eip[1] & 0xf;					\
	    *_b2 = _eip[2] >> 4;					\
	    *_d2 = ((_eip[2] & 0xf) << 8) + _eip[3];			\
	    return 1;							\
	  }								\
	return 0;							\
      }									\
									\
      /* Decode RRE instruction format.  */				\
      static inline int _is_rre (unsigned char *_eip,			\
				 unsigned char _op1, unsigned char _op2,\
				 int *_r1, int *_r2)			\
      {									\
	if (_eip[0] == _op1 && _eip[1] == _op2)				\
	  {								\
	    *_r1 = _eip[3] >> 4;					\
	    *_r2 = _eip[3] & 0xf;					\
	    return 1;							\
	  }								\
	return 0;							\
      }									\
									\
      /* Decode RXY instruction format.  */				\
      static inline int _is_rxy (unsigned char *_eip,			\
				 unsigned char _op1, unsigned char _op2,\
				 int *_r1, int *_d2, int *_x2, int *_b2)\
      {									\
	if (_eip[0] == _op1 && _eip[5] == _op2)				\
	  {								\
	    *_r1 = _eip[1] >> 4;					\
	    *_x2 = _eip[1] & 0xf;					\
	    *_b2 = _eip[2] >> 4;					\
	    *_d2 = ((_eip[2] & 0xf) << 8) + _eip[3] + (_eip[4] << 12);	\
	    /* We have a 20-bit signed displacement.  */		\
	    *_d2 = (*_d2 ^ 0x80000) - 0x80000;				\
	    return 1;							\
	  }								\
	return 0;							\
      }									\
									\
      /* Compute effective address.  */					\
      static inline unsigned long _eff (unsigned long *_regs,		\
					long _d, int _x, int _b)	\
      {									\
	return _d + (_x? _regs[_x] : 0) + (_b? _regs[_b] : 0);		\
      }									\
									\
      static inline int is_long_long_min_p (unsigned long *_regs,       \
					    unsigned long *_ext_regs,   \
					    int _r)			\
      {									\
	return ((long long)_regs[_r]					\
		| (long long)_ext_regs[_r] << 32) ==			\
	  LONG_LONG_MIN;						\
      }									\
  };									\
									\
  /* DR r1,r2 */							\
  if (_decode::_is_rr (_eip, 0x1d, &_r1, &_r2)				\
      && (int) _regs[_r1] == -1 && (int) _regs[_r1+1] == INT_MIN	\
      && (int) _regs[_r2] == -1)					\
    {									\
      _regs[_r1] &= ~0xffffffff;					\
      return;								\
    }									\
 									\
  /* D r1,d2(x2,b2) */							\
  if (_decode::_is_rx (_eip, 0x5d, &_r1, &_d2, &_x2, &_b2)		\
      && (int) _regs[_r1] == -1 && (int) _regs[_r1+1] == INT_MIN	\
      && *(int *) _decode::_eff (_regs, _d2, _x2, _b2) == -1)		\
    {									\
      _regs[_r1] &= ~0xffffffff;					\
      return;								\
    }									\
									\
  /* DSGR r1,r2 */							\
  if (_decode::_is_rre (_eip, 0xb9, 0x0d, &_r1, &_r2)			\
      && (long) _regs[_r1+1] == LONG_MIN				\
      && (long) _regs[_r2] == -1L)					\
    {									\
      _regs[_r1] = 0;							\
      return;								\
    }									\
									\
  /* DSGFR r1,r2 */							\
  if (_decode::_is_rre (_eip, 0xb9, 0x1d, &_r1, &_r2)			\
      && (long) _regs[_r1+1] == LONG_MIN				\
      && (int) _regs[_r2] == -1)					\
    {									\
      _regs[_r1] = 0;							\
      return;								\
    }									\
									\
  /* DSG r1,d2(x2,b2) */						\
  if (_decode::_is_rxy (_eip, 0xe3, 0x0d, &_r1, &_d2, &_x2, &_b2)	\
      && (long) _regs[_r1+1] == LONG_MIN				\
      && *(long *) _decode::_eff (_regs, _d2, _x2, _b2) == -1L)		\
    {									\
      _regs[_r1] = 0;							\
      return;								\
    }									\
									\
  /* DSGF r1,d2(x2,b2) */						\
  if (_decode::_is_rxy (_eip, 0xe3, 0x1d, &_r1, &_d2, &_x2, &_b2)	\
      && (long) _regs[_r1+1] == LONG_MIN				\
      && *(int *) _decode::_eff (_regs, _d2, _x2, _b2) == -1)		\
    {									\
      _regs[_r1] = 0;							\
      return;								\
    }									\
                                                                        \
  /* The extended ucontext contains the upper halfs of the 64bit	\
     registers in 31bit applications.  */				\
  if (_uc->uc_flags & 1 == 1)						\
    {             							\
      /* DSGR r1,r2 */							\
      if (_decode::_is_rre (_eip, 0xb9, 0x0d, &_r1, &_r2)		\
	  && (int) _regs[_r2] == -1					\
	  && (int) _uc_ext->ext_regs[_r2] == -1				\
	  && _decode::is_long_long_min_p (_regs, _uc_ext->ext_regs,	\
					  _r1 + 1))			\
	{								\
	  _regs[_r1] = 0;						\
	  _uc_ext->ext_regs[_r1] = 0;					\
	  return;							\
	}								\
      									\
      /* DSGFR r1,r2 */							\
      if (_decode::_is_rre (_eip, 0xb9, 0x1d, &_r1, &_r2)		\
	  && (int) _regs[_r2] == -1					\
	  && _decode::is_long_long_min_p (_regs, _uc_ext->ext_regs,	\
					  _r1 + 1))			\
	{								\
	  _regs[_r1] = 0;						\
	  _uc_ext->ext_regs[_r1] = 0;					\
	  return;							\
	}								\
      									\
      /* DSG r1,d2(x2,b2) */						\
      if (_decode::_is_rxy (_eip, 0xe3, 0x0d, &_r1, &_d2, &_x2, &_b2)	\
	  && *(int *) _decode::_eff (_regs, _d2, _x2, _b2) == -1	\
	  && *(int *) _decode::_eff (_regs, _d2 + 4, _x2, _b2) == -1	\
	  && _decode::is_long_long_min_p (_regs, _uc_ext->ext_regs,	\
					  _r1 + 1))			\
	{								\
	  _regs[_r1] = 0;						\
	  _uc_ext->ext_regs[_r1] = 0;					\
	  return;							\
	}								\
	      								\
      /* DSGF r1,d2(x2,b2) */						\
      if (_decode::_is_rxy (_eip, 0xe3, 0x1d, &_r1, &_d2, &_x2, &_b2)	\
	  && *(int *) _decode::_eff (_regs, _d2, _x2, _b2) == -1	\
	  && _decode::is_long_long_min_p (_regs, _uc_ext->ext_regs,	\
					  _r1 + 1))			\
	{								\
	  _regs[_r1] = 0;						\
	  _uc_ext->ext_regs[_r1] = 0;					\
	  return;							\
	}								\
    }									\
 }                                                                      \
while (0)

/* For an explanation why we cannot simply use sigaction to
   install the handlers, see i386-signal.h.  */

/* We use old_kernel_sigaction here because we're calling the kernel
   directly rather than via glibc.  The sigaction structure that the
   syscall uses is a different shape from the one in userland and not
   visible to us in a header file so we define it here.  */

struct old_s390_kernel_sigaction {
	void (*k_sa_handler) (int, siginfo_t *, ucontext_t *);
	unsigned long k_sa_mask;
	unsigned long k_sa_flags;
	void (*sa_restorer) (void);
};

#define INIT_SEGV					\
do							\
  {							\
    struct old_s390_kernel_sigaction kact;		\
    kact.k_sa_handler = catch_segv;			\
    kact.k_sa_mask = 0;					\
    kact.k_sa_flags = SA_SIGINFO;			\
    syscall (SYS_sigaction, SIGSEGV, &kact, NULL);	\
  }							\
while (0)  

#define INIT_FPE						\
do								\
  {								\
    struct old_s390_kernel_sigaction kact;			\
    kact.k_sa_handler = catch_fpe;				\
    kact.k_sa_mask = 0;						\
    kact.k_sa_flags = SA_SIGINFO;				\
    syscall (SYS_sigaction, SIGFPE, &kact, NULL);		\
  }								\
while (0)  

#endif /* JAVA_SIGNAL_H */

