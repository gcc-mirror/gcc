..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _acc_copyin:

acc_copyin -- Allocate device memory and copy host memory to it.
****************************************************************

Description
  In C/C++, this function allocates :samp:`{len}` bytes of device memory
  and maps it to the specified host address in :samp:`{a}`. The device
  address of the newly allocated device memory is returned.

  In Fortran, two (2) forms are supported. In the first form, :samp:`{a}` specifies
  a contiguous array section. The second form :samp:`{a}` specifies a
  variable or array element and :samp:`{len}` specifies the length in bytes.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``void *acc_copyin(h_void *a, size_t len);``
     * - *Prototype*:
       - ``void *acc_copyin_async(h_void *a, size_t len, int async);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``subroutine acc_copyin(a)``
     * -
       - ``type, dimension(:[,:]...) :: a``
     * - *Interface*:
       - ``subroutine acc_copyin(a, len)``
     * -
       - ``type, dimension(:[,:]...) :: a``
     * -
       - ``integer len``
     * - *Interface*:
       - ``subroutine acc_copyin_async(a, async)``
     * -
       - ``type, dimension(:[,:]...) :: a``
     * -
       - ``integer(acc_handle_kind) :: async``
     * - *Interface*:
       - ``subroutine acc_copyin_async(a, len, async)``
     * -
       - ``type, dimension(:[,:]...) :: a``
     * -
       - ``integer len``
     * -
       - ``integer(acc_handle_kind) :: async``

Reference:
  :openacc:`2.6`, section
  3.2.20.
