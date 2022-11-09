..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _acc_async_test:

acc_async_test -- Test for completion of a specific asynchronous operation.
***************************************************************************

Description
  This function tests for completion of the asynchronous operation specified
  in :samp:`{arg}`. In C/C++, a non-zero value will be returned to indicate
  the specified asynchronous operation has completed. While Fortran will return
  a ``true``. If the asynchronous operation has not completed, C/C++ returns
  a zero and Fortran returns a ``false``.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``int acc_async_test(int arg);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``function acc_async_test(arg)``
     * -
       - ``integer(kind=acc_handle_kind) arg``
     * -
       - ``logical acc_async_test``

Reference:
  :openacc:`2.6`, section
  3.2.9.
