// Copyright (C) 2000 Free Software Foundation, Inc.
// 
// GNU CC is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.

// GNU CC is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with GNU CC; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA. 

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

#include "gansidecl.h" /* Needed to support macros used in eh-common.h. */
#include "eh-common.h"

/* The type of a function called to clean up an exception object.
   (These will be destructors.)  Under the old ABI, these take a
   second argument (the `in-charge' argument), that indicates whether
   or not do delete the object, and whether or not to destroy virtual
   bases.  Under the new ABI, there is no second argument.  */
#if !defined (__GXX_ABI_VERSION) || __GXX_ABI_VERSION < 100
typedef void (*cleanup_fn)(void *, int);
/* The `2' is the value for the in-charge parameter that indicates
   that virtual bases should be destroyed.  */
#define CALL_CLEANUP(FN, THIS) FN (THIS, 2)
#else
typedef void (*cleanup_fn)(void *);
#define CALL_CLEANUP(FN, THIS) FN (THIS)
#endif

/* C++-specific state about the current exception.  This must match
   init_exception_processing().

   Note that handlers and caught are not redundant; when rethrown, an
   exception can have multiple active handlers and still be considered
   uncaught.  */

struct cp_eh_info
{
  __eh_info eh_info;
  void *value;
  void *type;
  cleanup_fn cleanup;
  bool caught;
  cp_eh_info *next;
  long handlers;
  void *original_value;
};

extern "C" cp_eh_info *__uncatch_exception (void);
extern "C" void __recatch_exception (cp_eh_info *);
