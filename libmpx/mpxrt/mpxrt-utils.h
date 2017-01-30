/* mpxrt-utils.h                  -*-C++-*-
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

#ifndef MPXRT_UTILS_H
#define MPXRT_UTILS_H

#include <stdint.h>

typedef enum {
  VERB_ERROR,
  VERB_INFO,
  VERB_BR,
  VERB_DEBUG
} verbose_type;

typedef enum {
  MPX_RT_COUNT,
  MPX_RT_STOP
} mpx_rt_mode_t;

typedef enum {
  MPX_RT_STOP_HANDLER_ABORT,
  MPX_RT_STOP_HANDLER_EXIT
} mpx_rt_stop_mode_handler_t;

void __mpxrt_init_env_vars (int* bndpreserve);
void __mpxrt_write_uint (verbose_type vt, uint64_t val, unsigned base);
void __mpxrt_write (verbose_type vt, const char* str);
void __mpxrt_print (verbose_type vt, const char* frmt, ...);
mpx_rt_mode_t __mpxrt_mode (void);
void __mpxrt_utils_free (void);
void __mpxrt_print_summary (uint64_t num_brs, uint64_t l1_size);
void __mpxrt_stop (void) __attribute__ ((noreturn));

#endif /* MPXRT_UTILS_H */
