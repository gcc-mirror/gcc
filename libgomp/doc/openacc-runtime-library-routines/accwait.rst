..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _acc_wait:

acc_wait -- Wait for completion of a specific asynchronous operation.
*********************************************************************

Description
  This function waits for completion of the asynchronous operation
  specified in :samp:`{arg}`.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``acc_wait(arg);``
     * - *Prototype (OpenACC 1.0 compatibility)*:
       - ``acc_async_wait(arg);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``subroutine acc_wait(arg)``
     * -
       - ``integer(acc_handle_kind) arg``
     * - *Interface (OpenACC 1.0 compatibility)*:
       - ``subroutine acc_async_wait(arg)``
     * -
       - ``integer(acc_handle_kind) arg``

Reference:
  :openacc:`2.6`, section
  3.2.11.
