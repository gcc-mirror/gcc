/* go-defer.h -- the defer stack.

   Copyright 2010 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

struct __go_panic_stack;

/* The defer stack is a list of these structures.  */

struct __go_defer_stack
{
  /* The next entry in the stack.  */
  struct __go_defer_stack *__next;

  /* The frame pointer for the function which called this defer
     statement.  */
  void *__frame;

  /* The value of the panic stack when this function is deferred.
     This function can not recover this value from the panic stack.
     This can happen if a deferred function uses its own defer
     statement.  */
  struct __go_panic_stack *__panic;

  /* The function to call.  */
  void (*__pfn) (void *);

  /* The argument to pass to the function.  */
  void *__arg;

  /* The return address that a recover thunk matches against.  This is
     set by __go_set_defer_retaddr which is called by the thunks
     created by defer statements.  */
  const void *__retaddr;
};
