..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _acc_update_self:

acc_update_self -- Update host memory from mapped device memory.
****************************************************************

Description
  This function updates the host copy from the previously mapped device memory.
  The host memory is specified with the host address :samp:`{a}` and a length of
  :samp:`{len}` bytes.

  In Fortran, two (2) forms are supported. In the first form, :samp:`{a}` specifies
  a contiguous array section. The second form :samp:`{a}` specifies a variable or
  array element and :samp:`{len}` specifies the length in bytes.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``acc_update_self(h_void *a, size_t len);``
     * - *Prototype*:
       - ``acc_update_self_async(h_void *a, size_t len, int async);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``subroutine acc_update_self(a)``
     * -
       - ``type, dimension(:[,:]...) :: a``
     * - *Interface*:
       - ``subroutine acc_update_self(a, len)``
     * -
       - ``type, dimension(:[,:]...) :: a``
     * -
       - ``integer len``
     * - *Interface*:
       - ``subroutine acc_update_self_async(a, async)``
     * -
       - ``type, dimension(:[,:]...) :: a``
     * -
       - ``integer(acc_handle_kind) :: async``
     * - *Interface*:
       - ``subroutine acc_update_self_async(a, len, async)``
     * -
       - ``type, dimension(:[,:]...) :: a``
     * -
       - ``integer len``
     * -
       - ``integer(acc_handle_kind) :: async``

Reference:
  :openacc:`2.6`, section
  3.2.25.
