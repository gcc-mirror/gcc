..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _acc_wait_all_async:

acc_wait_all_async -- Wait for completion of all asynchronous operations.
*************************************************************************

Description
  This function enqueues a wait operation on the queue :samp:`{async}` for any
  and all asynchronous operations that have been previously enqueued on
  any queue.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``acc_wait_all_async(int async);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``subroutine acc_wait_all_async(async)``
     * -
       - ``integer(acc_handle_kind) async``

Reference:
  :openacc:`2.6`, section
  3.2.14.
