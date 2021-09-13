/* Copyright (C) 2009-2021 Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@redhat.com>.

   This file is part of the GNU Transactional Memory Library (libitm).

   Libitm is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   Libitm is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#include "config.h"

#include "libitm_i.h"
#include "dispatch.h"

extern "C" {

#ifndef HAVE_AS_AVX
// If we don't have an AVX capable assembler, we didn't set -mavx on the
// command-line either, which means that libitm.h defined neither this type
// nor the functions in this file.  Define the type and unconditionally
// wrap the file in extern "C" to make up for the lack of pre-declaration.
typedef float _ITM_TYPE_M256 __attribute__((vector_size(32), may_alias));
#endif

// Re-define the memcpy implementations so that we can frob the
// interface to deal with possibly missing AVX instruction set support.

#ifdef HAVE_AS_AVX
#define RETURN(X)	return X
#define STORE(X,Y)	X = Y
#define OUTPUT(T)	_ITM_TYPE_##T
#define INPUT(T,X)	, _ITM_TYPE_##T X
#else
/* Emit vmovaps (%rax),%ymm0.  */
#define RETURN(X) \
  asm volatile(".byte 0xc5,0xfc,0x28,0x00" : "=m"(X) : "a"(&X))
/* Emit vmovaps %ymm0,(%rax); vzeroupper.  */
#define STORE(X,Y) \
  asm volatile(".byte 0xc5,0xfc,0x29,0x00,0xc5,0xf8,0x77" : "=m"(X) : "a"(&X))
#define OUTPUT(T)	void
#define INPUT(T,X)
#endif

#undef ITM_READ_MEMCPY
#define ITM_READ_MEMCPY(T, LSMOD, TARGET, M2)				\
OUTPUT(T) ITM_REGPARM _ITM_##LSMOD##T (const _ITM_TYPE_##T *ptr)	\
{									\
  _ITM_TYPE_##T v;							\
  TARGET memtransfer##M2(&v, ptr, sizeof(_ITM_TYPE_##T), false,		\
			 GTM::abi_dispatch::NONTXNAL,			\
			 GTM::abi_dispatch::LSMOD);			\
  RETURN(v);								\
}

#undef ITM_WRITE_MEMCPY
#define ITM_WRITE_MEMCPY(T, LSMOD, TARGET, M2)				\
void ITM_REGPARM _ITM_##LSMOD##T (_ITM_TYPE_##T *ptr INPUT(T,in))	\
{									\
  _ITM_TYPE_##T v;							\
  STORE(v, in);								\
  TARGET memtransfer##M2(ptr, &v, sizeof(_ITM_TYPE_##T), false,		\
			 GTM::abi_dispatch::LSMOD,			\
			 GTM::abi_dispatch::NONTXNAL);			\
}

// ??? Use memcpy for now, until we have figured out how to best instantiate
// these loads/stores.
CREATE_DISPATCH_FUNCTIONS_T_MEMCPY(M256, GTM::abi_disp()->, )

void ITM_REGPARM
_ITM_LM256 (const _ITM_TYPE_M256 *ptr)
{
  GTM::GTM_LB (ptr, sizeof (*ptr));
}

} // extern "C"
