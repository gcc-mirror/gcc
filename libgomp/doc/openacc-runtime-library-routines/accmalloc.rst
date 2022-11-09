..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _acc_malloc:

acc_malloc -- Allocate device memory.
*************************************

Description
  This function allocates :samp:`{len}` bytes of device memory. It returns
  the device address of the allocated memory.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``d_void* acc_malloc(size_t len);``

Reference:
  :openacc:`2.6`, section
  3.2.18.
