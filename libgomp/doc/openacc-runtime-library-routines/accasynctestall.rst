..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _acc_async_test_all:

acc_async_test_all -- Tests for completion of all asynchronous operations.
**************************************************************************

Description
  This function tests for completion of all asynchronous operations.
  In C/C++, a non-zero value will be returned to indicate all asynchronous
  operations have completed. While Fortran will return a ``true``. If
  any asynchronous operation has not completed, C/C++ returns a zero and
  Fortran returns a ``false``.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``int acc_async_test_all(void);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``function acc_async_test()``
     * -
       - ``logical acc_get_device_num``

Reference:
  :openacc:`2.6`, section
  3.2.10.
