/* mpxrt.c                  -*-C++-*-
 *
 *************************************************************************
 *
 *  @copyright
 *  Copyright (C) 2014, Intel Corporation
 *  All rights reserved.
 *
 *  @copyright
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions
 *  are met:
 *
 *    * Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *    * Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in
 *      the documentation and/or other materials provided with the
 *      distribution.
 *    * Neither the name of Intel Corporation nor the names of its
 *      contributors may be used to endorse or promote products derived
 *      from this software without specific prior written permission.
 *
 *  @copyright
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 *  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 *  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
 *  OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
 *  AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 *  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
 *  WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 *  POSSIBILITY OF SUCH DAMAGE.
 *
 **************************************************************************/

#define __STDC_FORMAT_MACROS
#include "config.h"
#include <inttypes.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <signal.h>
#include <assert.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <sys/prctl.h>
#include <cpuid.h>
#include "mpxrt-utils.h"
#include "mpxrt.h"

#define MPX_ENABLE_BIT_NO 0
#define BNDPRESERVE_BIT_NO 1

struct xsave_hdr_struct
{
  uint64_t xstate_bv;
  uint64_t reserved1[2];
  uint64_t reserved2[5];
} __attribute__ ((packed));

struct bndregs_struct
{
  uint64_t bndregs[8];
} __attribute__ ((packed));

struct bndcsr_struct {
	uint64_t cfg_reg_u;
	uint64_t status_reg;
} __attribute__((packed));

struct xsave_struct
{
  uint8_t fpu_sse[512];
  struct xsave_hdr_struct xsave_hdr;
  uint8_t ymm[256];
  uint8_t lwp[128];
  struct bndregs_struct bndregs;
  struct bndcsr_struct bndcsr;
} __attribute__ ((packed));

/* Following vars are initialized at process startup only
   and thus are considered to be thread safe.  */
static void *l1base = NULL;
static int bndpreserve;
static int enable = 1;

/* Var holding number of occured BRs.  It is modified from
   signal handler only and thus it should be thread safe.  */
static uint64_t num_bnd_chk = 0;

static inline void
xrstor_state (struct xsave_struct *fx, uint64_t mask)
{
  uint32_t lmask = mask;
  uint32_t hmask = mask >> 32;

  asm volatile (".byte " REX_PREFIX "0x0f,0xae,0x2f\n\t"
		: : "D" (fx), "m" (*fx), "a" (lmask), "d" (hmask)
		:   "memory");
}

static inline void
xsave_state (struct xsave_struct *fx, uint64_t mask)
{
  uint32_t lmask = mask;
  uint32_t hmask = mask >> 32;

  asm volatile (".byte " REX_PREFIX "0x0f,0xae,0x27\n\t"
		: : "D" (fx), "m" (*fx), "a" (lmask), "d" (hmask)
		:   "memory");
}

static inline uint64_t
xgetbv (uint32_t index)
{
  uint32_t eax, edx;

  asm volatile (".byte 0x0f,0x01,0xd0" /* xgetbv */
		: "=a" (eax), "=d" (edx)
		: "c" (index));
  return eax + ((uint64_t)edx << 32);
}

static uint64_t
read_mpx_status_sig (ucontext_t *uctxt)
{
  uint8_t *regs = (uint8_t *)uctxt->uc_mcontext.fpregs + XSAVE_OFFSET_IN_FPMEM;
  struct xsave_struct *xsave_buf = (struct xsave_struct *)regs;
  return xsave_buf->bndcsr.status_reg;
}

static uint8_t *
get_next_inst_ip (uint8_t *addr)
{
  uint8_t *ip = addr;
  uint8_t  sib;

  /* Determine the prefix.  */
  switch (*ip)
    {
    case 0xf2:
    case 0xf3:
    case 0x66:
      ip++;
      break;
    }

  /* Look for rex prefix.  */
  if ((*ip & 0x40) == 0x40)
    ip++;

  /* Make sure we have a MPX instruction.  */
  if (*ip++ != 0x0f)
    return addr;

  /* Skip the op code byte.  */
  ip++;

  /* Get the moderm byte.  */
  uint8_t modrm = *ip++;

  /* Break it down into parts.  */
  uint8_t rm = modrm & 7;
  uint8_t mod = (modrm >> 6);

  /* Init the parts of the address mode.  */
  uint8_t base = 8;

  /* Is it a mem mode?  */
  if (mod != 3)
    {
      /* Look for scaled indexed addressing.  */
      if (rm == 4)
	{
	  /* SIB addressing.  */
	  sib = *ip++;
	  base = sib & 7;
	  switch (mod)
	    {
	    case 0:
	      if (base == 5)
		ip += 4;
	      break;

	    case 1:
	      ip++;
	      break;

	    case 2:
	      ip += 4;
	      break;
	    }
	}
      else
	{
	  /* MODRM addressing.  */
	  switch (mod)
	    {
	    case 0:
	      if (rm == 5)
		/* DISP32 addressing, no base.  */
		ip += 4;
	      break;

	    case 1:
	      ip++;
	      break;

	    case 2:
	      ip += 4;
	      break;
	    }
	}
    }
  return ip;
}

static void
handler (int sig __attribute__ ((unused)),
	 siginfo_t *info __attribute__ ((unused)),
	 void *vucontext,
	 struct xsave_struct *buf  __attribute__ ((unused)))
{
  ucontext_t* uctxt;
  greg_t trapno;
  greg_t ip;

  uctxt = vucontext;
  trapno = uctxt->uc_mcontext.gregs[REG_TRAPNO];
  ip = uctxt->uc_mcontext.gregs[REG_IP_IDX];

  if (trapno == 5)
    {
      uint64_t status = read_mpx_status_sig (uctxt);
      uint64_t br_reason =  status & 0x3;

      __mpxrt_write (VERB_BR, "Saw a #BR! status ");
      __mpxrt_write_uint (VERB_BR, status, 10);
      __mpxrt_write (VERB_BR, " at 0x");
      __mpxrt_write_uint (VERB_BR, ip, 16);
      __mpxrt_write (VERB_BR, "\n");

      switch (br_reason)
	{
	case 1: /* traditional BR */
	  num_bnd_chk++;
	  uctxt->uc_mcontext.gregs[REG_IP_IDX] =
	    (greg_t)get_next_inst_ip ((uint8_t *)ip);
	  if (__mpxrt_mode () == MPX_RT_STOP)
	    __mpxrt_stop ();
	  return;

	default:
	  __mpxrt_write (VERB_BR, "Unexpected status with bound exception: ");
	  __mpxrt_write_uint (VERB_BR, status, 10);
	  __mpxrt_write (VERB_BR, "\n");
	  break;
	}
    }
  else if (trapno == 14)
    {
      __mpxrt_write (VERB_ERROR, "In signal handler, trapno = ");
      __mpxrt_write_uint (VERB_ERROR, trapno, 10);
      __mpxrt_write (VERB_ERROR, ", ip = 0x");
      __mpxrt_write_uint (VERB_ERROR, ip, 16);
      __mpxrt_write (VERB_ERROR, "\n");
      __mpxrt_stop ();
    }
  else
    {
      __mpxrt_write (VERB_ERROR, "Unexpected trap ");
      __mpxrt_write_uint (VERB_ERROR, trapno, 10);
      __mpxrt_write (VERB_ERROR, "! at 0x");
      __mpxrt_write_uint (VERB_ERROR, ip, 16);
      __mpxrt_write (VERB_ERROR, "\n");
      __mpxrt_stop ();
    }
}

/* Using wrapper to the real handler in order to save the bnd regs
   using xsave before any unprefixed call. an unprefixed call to
   __i686.get_pc_thunk.bx is added by the linker in 32bit at the
   beginning of handler function since there are references to
   global variables.  */
static void
handler_wrap (int signum, siginfo_t* si, void* vucontext)
{
  /* Since the OS currently not handling chkptr regs.
     We need to store them for later use. They might be
     init due to unprefixed call,Jcc,ret. avoiding calling
     function since the function will be unprefixed as well.  */
  uint8_t __attribute__ ((__aligned__ (64))) buffer[4096];
  struct xsave_struct *xsave_buf = (struct xsave_struct *)buffer;
  uint64_t mask = 0x18;
  uint32_t lmask = mask;
  uint32_t hmask = mask >> 32;

  asm volatile (".byte " REX_PREFIX "0x0f,0xae,0x27\n\t"
		: : "D" (xsave_buf), "m" (*xsave_buf),
		  "a" (lmask), "d" (hmask)
		:   "memory");

  handler (signum, si, vucontext, xsave_buf);
}

static bool
check_mpx_support (void)
{
  unsigned int eax, ebx, ecx, edx;
  unsigned int max_level = __get_cpuid_max (0, NULL);

  if (max_level < 13)
    {
      __mpxrt_print (VERB_DEBUG, "No required CPUID level support.\n");
      return false;
    }

  __cpuid_count (0, 0, eax, ebx, ecx, edx);
  if (!(ecx & bit_XSAVE))
    {
      __mpxrt_print (VERB_DEBUG, "No XSAVE support.\n");
      return false;
    }

  if (!(ecx & bit_OSXSAVE))
    {
      __mpxrt_print (VERB_DEBUG, "No OSXSAVE support.\n");
      return false;
    }

  __cpuid_count (7, 0, eax, ebx, ecx, edx);
  if (!(ebx & bit_MPX))
    {
      __mpxrt_print (VERB_DEBUG, "No MPX support.\n");
      return false;
    }

  __cpuid_count (13, 0, eax, ebx, ecx, edx);
  if (!(eax & bit_BNDREGS))
    {
      __mpxrt_print (VERB_DEBUG, "No BNDREGS support.\n");
      return false;
    }

  if (!(eax & bit_BNDCSR))
    {
      __mpxrt_print (VERB_DEBUG, "No BNDCSR support.\n");
      return false;
    }

  return true;
}

static void
enable_mpx (void)
{
  uint8_t __attribute__ ((__aligned__ (64))) buffer[4096];
  struct xsave_struct *xsave_buf = (struct xsave_struct *)buffer;

  memset (buffer, 0, sizeof (buffer));
  xrstor_state (xsave_buf, 0x18);

  __mpxrt_print (VERB_DEBUG, "Initalizing MPX...\n");
  __mpxrt_print (VERB_DEBUG, "  Enable bit: %d\n", enable);
  __mpxrt_print (VERB_DEBUG, "  BNDPRESERVE bit: %d\n", bndpreserve);

  /* Enable MPX.  */
  xsave_buf->xsave_hdr.xstate_bv = 0x10;
  xsave_buf->bndcsr.cfg_reg_u = (unsigned long)l1base;
  xsave_buf->bndcsr.cfg_reg_u |= enable << MPX_ENABLE_BIT_NO;
  xsave_buf->bndcsr.cfg_reg_u |= bndpreserve << BNDPRESERVE_BIT_NO;
  xsave_buf->bndcsr.status_reg = 0;

  xrstor_state (xsave_buf, 0x10);
}

static void
disable_mpx (void)
{
  uint8_t __attribute__ ((__aligned__ (64))) buffer[4096];
  struct xsave_struct *xsave_buf = (struct xsave_struct *)buffer;

  memset(buffer, 0, sizeof(buffer));
  xrstor_state(xsave_buf, 0x18);

  /* Disable MPX.  */
  xsave_buf->xsave_hdr.xstate_bv = 0x10;
  xsave_buf->bndcsr.cfg_reg_u = 0;
  xsave_buf->bndcsr.status_reg = 0;

  xrstor_state(xsave_buf, 0x10);
}

static bool
process_specific_init (void)
{
  if (!check_mpx_support ())
    return false;

  l1base = mmap (NULL, MPX_L1_SIZE, PROT_READ | PROT_WRITE,
		 MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);
  if (l1base == MAP_FAILED)
    {
      perror ("mmap");
      exit (EXIT_FAILURE);
    }

  enable_mpx ();

  if (prctl (43, 0, 0, 0, 0))
    {
      __mpxrt_print (VERB_ERROR, "No MPX support\n");
      disable_mpx ();
      return false;
    }

  return true;
}

static bool
process_specific_finish (void)
{
  if (!check_mpx_support ())
    return false;

  if (prctl (44, 0, 0, 0, 0))
    {
      __mpxrt_print (VERB_ERROR, "No MPX support\n");
      return false;
    }

  munmap (l1base, MPX_L1_SIZE);

  return true;
}

static void
setup_handler (void)
{
  int r,rs;
  struct sigaction newact;

  /* #BR is mapped to sigsegv  */
  int signum  = SIGSEGV;

  newact.sa_handler = 0;
  newact.sa_sigaction = handler_wrap;

  /* sigset_t - signals to block while in the handler
     get the old signal mask.  */
  rs = sigprocmask (SIG_SETMASK, 0, &newact.sa_mask);
  assert (rs == 0);

  /* Call sa_sigaction, not sa_handler.  */
  newact.sa_flags = SA_SIGINFO;
  /* In case we call user's handler on SIGSEGV (not bound
     violation exception) we want to allow bound checking
     inside the user handler -> nested exception.  */
  newact.sa_flags |= SA_NODEFER;

  newact.sa_restorer = 0;
  r = sigaction (signum, &newact, 0);
  assert (r == 0);
}

/* Set constructor priority to two to make it run after the
   constructor in sigaction.c.  */
static void __attribute__ ((constructor (1005)))
mpxrt_prepare (void)
{
  __mpxrt_init_env_vars (&bndpreserve);
  setup_handler ();
  process_specific_init ();
}

static void __attribute__ ((destructor))
mpxrt_cleanup (void)
{
  __mpxrt_print_summary (num_bnd_chk, MPX_L1_SIZE);
  __mpxrt_utils_free ();
  process_specific_finish ();
}

/* Get address of bounds directory.  */
void *
get_bd ()
{
  return l1base;
}
