/* Encoding of types for Objective C.
   Copyright (C) 1993, 1997, 2002, 2004, 2009 Free Software Foundation, Inc.

Author: Kresten Krab Thorup

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


#ifndef __encoding_INCLUDE_GNU
#define __encoding_INCLUDE_GNU

#include "objc-api.h"
#include <ctype.h>

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#define _C_CONST	'r'
#define _C_IN		'n'
#define _C_INOUT	'N'
#define _C_OUT      	'o'
#define _C_BYCOPY	'O'
#define _C_BYREF	'R'
#define _C_ONEWAY	'V'
#define _C_GCINVISIBLE	'!'

#define _F_CONST	0x01
#define _F_IN		0x01
#define _F_OUT		0x02
#define _F_INOUT	0x03
#define _F_BYCOPY	0x04
#define _F_BYREF	0x08
#define _F_ONEWAY	0x10
#define _F_GCINVISIBLE	0x20

int objc_aligned_size (const char *type);
int objc_sizeof_type (const char *type);
int objc_alignof_type (const char *type);
int objc_aligned_size (const char *type);
int objc_promoted_size (const char *type);

const char *objc_skip_type_qualifiers (const char *type);
const char *objc_skip_typespec (const char *type);
const char *objc_skip_offset (const char *type);
const char *objc_skip_argspec (const char *type);
int method_get_number_of_arguments (struct objc_method *);
int method_get_sizeof_arguments (struct objc_method *);

char *method_get_first_argument (struct objc_method *,
				 arglist_t argframe, 
				 const char **type);
char *method_get_next_argument (arglist_t argframe, 
				const char **type);
char *method_get_nth_argument (struct objc_method *m, 
			       arglist_t argframe,
			       int arg, 
			       const char **type);

unsigned objc_get_type_qualifiers (const char *type);


struct objc_struct_layout 
{
  const char *original_type;
  const char *type;
  const char *prev_type;
  unsigned int record_size;
  unsigned int record_align;
};

void objc_layout_structure (const char *type,
                            struct objc_struct_layout *layout);
BOOL  objc_layout_structure_next_member (struct objc_struct_layout *layout);
void objc_layout_finish_structure (struct objc_struct_layout *layout,
                                   unsigned int *size,
                                   unsigned int *align);
void objc_layout_structure_get_info (struct objc_struct_layout *layout,
                                     unsigned int *offset,
                                     unsigned int *align,
                                     const char **type);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __encoding_INCLUDE_GNU */
