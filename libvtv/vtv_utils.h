/* Copyright (C) 2012-2021 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#ifndef _VTV_UTILS_H
#define _VTV_UTILS_H 1

#include <stdlib.h>
#include "../include/vtv-change-permission.h"

static inline void
VTV_not_an_error (void)
{
}

/* Handler for verification runtime errors.  */
#define VTV_error abort

/* Assertion macros used in vtable verification runtime.  */
#define VTV_ASSERT(EXPR) \
  if (!(EXPR)) VTV_error();

#ifdef VTV_DEBUG
#define VTV_DEBUG_ASSERT(EXPR) \
                      ((bool) (!(EXPR)) ? VTV_error() : (void) 0)
#else
#define VTV_DEBUG_ASSERT(EXPR) ((void) 0)
#endif

/* Name of the section where we put general VTV variables for protection */
#define VTV_PROTECTED_VARS_SECTION ".vtable_map_vars"
#define VTV_PROTECTED_VAR \
                       __attribute__ ((section (VTV_PROTECTED_VARS_SECTION)))

/* The following logging routines try to use low level file access
   routines and avoid calling malloc. We need this so that we dont
   disturb the order of calls to dlopen.  Changing the order of dlopen
   calls may lead to deadlocks */
int __vtv_open_log (const char * name);
int __vtv_add_to_log (int log, const char * format, ...);
void __vtv_log_verification_failure (const char *, bool);

#endif /* VTV_UTILS_H */
