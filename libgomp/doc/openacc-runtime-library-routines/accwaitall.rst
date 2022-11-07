..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _acc_wait_all:

acc_wait_all -- Waits for completion of all asynchronous operations.
********************************************************************

Description
  This function waits for the completion of all asynchronous operations.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``acc_wait_all(void);``
     * - *Prototype (OpenACC 1.0 compatibility)*:
       - ``acc_async_wait_all(void);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``subroutine acc_wait_all()``
     * - *Interface (OpenACC 1.0 compatibility)*:
       - ``subroutine acc_async_wait_all()``

Reference:
  :openacc:`2.6`, section
  3.2.13.