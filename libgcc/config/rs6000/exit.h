/* Copyright (C) 1991-2019 Free Software Foundation, Inc.

Derived from exit.h in GNU C Library.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#ifndef	_EXIT_H
#define _EXIT_H 1

#define attribute_hidden
#define INTDEF(name)

#include <stdbool.h>
#include <stdint.h>

enum
{
  ef_free,	/* `ef_free' MUST be zero!  */
  ef_us,
  ef_on,
  ef_at,
  ef_cxa
};

struct exit_function
  {
    /* `flavour' should be of type of the `enum' above but since we need
       this element in an atomic operation we have to use `long int'.  */
    long int flavor;
    union
      {
	void (*at) (void);
	struct
	  {
	    void (*fn) (int status, void *arg);
	    void *arg;
	  } on;
	struct
	  {
	    void (*fn) (void *arg, int status);
	    void *arg;
	    void *dso_handle;
	  } cxa;
      } func;
  };
struct exit_function_list
  {
    struct exit_function_list *next;
    size_t idx;
    struct exit_function fns[32];
  };
extern struct exit_function_list *__exit_funcs attribute_hidden;
extern struct exit_function_list *__quick_exit_funcs attribute_hidden;

extern struct exit_function *__new_exitfn (struct exit_function_list **listp);
extern uint64_t __new_exitfn_called attribute_hidden;

extern void __run_exit_handlers (int status, struct exit_function_list **listp,
				 bool run_list_atexit)
  attribute_hidden __attribute__ ((__noreturn__));

extern int __internal_atexit (void (*func) (void *), void *arg, void *d,
			      struct exit_function_list **listp)
  attribute_hidden;
extern int __cxa_at_quick_exit (void (*func) (void *), void *d);

extern int __cxa_atexit (void (*func) (void *), void *arg, void *d);
extern int __cxa_atexit_internal (void (*func) (void *), void *arg, void *d)
     attribute_hidden;

extern void __cxa_finalize (void *d);

#endif	/* exit.h  */
