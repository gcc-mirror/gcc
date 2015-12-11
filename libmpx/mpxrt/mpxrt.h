/* mpxrt.h                  -*-C++-*-
 *
 *************************************************************************
 *
 *  @copyright
 *  Copyright (C) 2015, Intel Corporation
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
 *  A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
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
#ifdef __i386__

/* i386 directory size is 4MB.  */
#define NUM_L1_BITS 20
#define NUM_L2_BITS 10
#define NUM_IGN_BITS 2
#define MPX_L1_ADDR_MASK  0xfffff000UL
#define MPX_L2_ADDR_MASK  0xfffffffcUL
#define MPX_L2_VALID_MASK 0x00000001UL

#define REG_IP_IDX      REG_EIP
#define REX_PREFIX

#define XSAVE_OFFSET_IN_FPMEM    sizeof (struct _libc_fpstate)

#else /* __i386__ */

/* x86_64 directory size is 2GB.  */
#define NUM_L1_BITS 28
#define NUM_L2_BITS 17
#define NUM_IGN_BITS 3
#define MPX_L1_ADDR_MASK  0xfffffffffffff000ULL
#define MPX_L2_ADDR_MASK  0xfffffffffffffff8ULL
#define MPX_L2_VALID_MASK 0x0000000000000001ULL

#define REG_IP_IDX    REG_RIP
#define REX_PREFIX    "0x48, "

#define XSAVE_OFFSET_IN_FPMEM 0

#endif /* !__i386__ */

#define MPX_L1_SIZE ((1UL << NUM_L1_BITS) * sizeof (void *))

/* Get address of bounds directory.  */
void *
get_bd ();
