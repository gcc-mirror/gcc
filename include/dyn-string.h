/* An abstract string datatype.
   Copyright (C) 1998, 1999, 2000, 2002 Free Software Foundation, Inc.
   Contributed by Mark Mitchell (mark@markmitchell.com).

This file is part of GCC.
   
GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


typedef struct dyn_string
{
  int allocated;	/* The amount of space allocated for the string.  */
  int length;		/* The actual length of the string.  */
  char *s;		/* The string itself, NUL-terminated.  */
}* dyn_string_t;

/* The length STR, in bytes, not including the terminating NUL.  */
#define dyn_string_length(STR)                                          \
  ((STR)->length)

/* The NTBS in which the contents of STR are stored.  */
#define dyn_string_buf(STR)                                             \
  ((STR)->s)

/* Compare DS1 to DS2 with strcmp.  */
#define dyn_string_compare(DS1, DS2)                                    \
  (strcmp ((DS1)->s, (DS2)->s))


/* dyn_string functions are used in the demangling implementation
   included in the G++ runtime library.  To prevent collisions with
   names in user programs, the functions that are used in the
   demangler are given implementation-reserved names.  */

#if defined(IN_LIBGCC2) || defined(IN_GLIBCPP_V3)

#define dyn_string_init                 __cxa_dyn_string_init
#define dyn_string_new                  __cxa_dyn_string_new
#define dyn_string_delete               __cxa_dyn_string_delete
#define dyn_string_release              __cxa_dyn_string_release
#define dyn_string_resize               __cxa_dyn_string_resize
#define dyn_string_clear                __cxa_dyn_string_clear
#define dyn_string_copy                 __cxa_dyn_string_copy
#define dyn_string_copy_cstr            __cxa_dyn_string_copy_cstr
#define dyn_string_prepend              __cxa_dyn_string_prepend
#define dyn_string_prepend_cstr         __cxa_dyn_string_prepend_cstr
#define dyn_string_insert               __cxa_dyn_string_insert
#define dyn_string_insert_cstr          __cxa_dyn_string_insert_cstr
#define dyn_string_insert_char          __cxa_dyn_string_insert_char
#define dyn_string_append               __cxa_dyn_string_append
#define dyn_string_append_cstr          __cxa_dyn_string_append_cstr
#define dyn_string_append_char          __cxa_dyn_string_append_char
#define dyn_string_substring            __cxa_dyn_string_substring
#define dyn_string_eq                   __cxa_dyn_string_eq

#endif /* IN_LIBGCC2 || IN_GLIBCPP_V3 */


extern int dyn_string_init              PARAMS ((struct dyn_string *, int));
extern dyn_string_t dyn_string_new      PARAMS ((int));
extern void dyn_string_delete           PARAMS ((dyn_string_t));
extern char *dyn_string_release         PARAMS ((dyn_string_t));
extern dyn_string_t dyn_string_resize   PARAMS ((dyn_string_t, int));
extern void dyn_string_clear            PARAMS ((dyn_string_t));
extern int dyn_string_copy              PARAMS ((dyn_string_t, dyn_string_t));
extern int dyn_string_copy_cstr         PARAMS ((dyn_string_t, const char *));
extern int dyn_string_prepend           PARAMS ((dyn_string_t, dyn_string_t));
extern int dyn_string_prepend_cstr      PARAMS ((dyn_string_t, const char *));
extern int dyn_string_insert            PARAMS ((dyn_string_t, int,
						 dyn_string_t));
extern int dyn_string_insert_cstr       PARAMS ((dyn_string_t, int,
						 const char *));
extern int dyn_string_insert_char       PARAMS ((dyn_string_t, int, int));
extern int dyn_string_append            PARAMS ((dyn_string_t, dyn_string_t));
extern int dyn_string_append_cstr       PARAMS ((dyn_string_t, const char *));
extern int dyn_string_append_char       PARAMS ((dyn_string_t, int));
extern int dyn_string_substring         PARAMS ((dyn_string_t, 
						 dyn_string_t, int, int));
extern int dyn_string_eq                PARAMS ((dyn_string_t, dyn_string_t));
