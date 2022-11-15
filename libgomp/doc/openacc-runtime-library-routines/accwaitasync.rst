..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _acc_wait_async:

acc_wait_async -- Wait for completion of asynchronous operations.
*****************************************************************

Description
  This function enqueues a wait operation on queue :samp:`{async}` for any and all
  asynchronous operations enqueued on queue :samp:`{arg}`.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``acc_wait_async(int arg, int async);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``subroutine acc_wait_async(arg, async)``
     * -
       - ``integer(acc_handle_kind) arg, async``

Reference:
  :openacc:`2.6`, section
  3.2.12.
