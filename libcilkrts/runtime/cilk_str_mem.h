/* cilk_str_mem.h                  -*-C-*-
 *
 *************************************************************************
 *
 *  Copyright (C) 2014-2016, Intel Corporation
 *  All rights reserved.
 *  
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
 *  *********************************************************************
 *  
 *  PLEASE NOTE: This file is a downstream copy of a file mainitained in
 *  a repository at cilkplus.org. Changes made to this file that are not
 *  submitted through the contribution process detailed at
 *  http://www.cilkplus.org/submit-cilk-contribution will be lost the next
 *  time that a new version is released. Changes only submitted to the
 *  GNU compiler collection or posted to the git repository at
 *  https://bitbucket.org/intelcilkruntime/intel-cilk-runtime.git are
 *  not tracked.
 *  
 *  We welcome your contributions to this open source project. Thank you
 *  for your assistance in helping us improve Cilk Plus.
 **************************************************************************/

/**
 * @file cilk_str_mem.h
 *
 * @breif Interface to safe string/memmory C API to replace banned C API.
 */

#ifndef INCLUDED_CILK_STR_MEM_DOT_H
#define INCLUDED_CILK_STR_MEM_DOT_H

#ifdef __cplusplus
extern "C" {
#endif

#ifndef _WIN32

#   include "safe_lib.h"
#   include "snprintf_s.h"

#define cilk_strlen(str) strnlen_s(str, RSIZE_MAX_STR)
#define cilk_strcpy_s    strcpy_s
// Different sprintf entries with different argument lists.
#define cilk_snprintf_s  snprintf_s_s
#define cilk_snprintf_i  snprintf_s_i
#define cilk_snprintf_l  snprintf_s_l
#define cilk_snprintf_si snprintf_s_si
#define cilk_snprintf_sl snprintf_s_sl

#else // ! defined _WIN32

#define CILK_MAX_STR (4UL << 10) // 4KB
#define cilk_strlen(str) strnlen_s(str, CILK_MAX_STR)
#define cilk_strcpy_s    strcpy_s
#define cilk_snprintf_s  sprintf_s
#define cilk_snprintf_i  sprintf_s
#define cilk_snprintf_l  sprintf_s
#define cilk_snprintf_si sprintf_s
#define cilk_snprintf_sl sprintf_s

#endif // ! defined _WIN32

#ifdef __cplusplus
}
#endif

#endif // INCLUDED_CILK_STR_MEM_DOT_H
