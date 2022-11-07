..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _acc_delete:

acc_delete -- Free device memory.
*********************************

Description
  This function frees previously allocated device memory specified by
  the device address :samp:`{a}` and the length of :samp:`{len}` bytes.

  In Fortran, two (2) forms are supported. In the first form, :samp:`{a}` specifies
  a contiguous array section. The second form :samp:`{a}` specifies a variable or
  array element and :samp:`{len}` specifies the length in bytes.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``acc_delete(h_void *a, size_t len);``
     * - *Prototype*:
       - ``acc_delete_async(h_void *a, size_t len, int async);``
     * - *Prototype*:
       - ``acc_delete_finalize(h_void *a, size_t len);``
     * - *Prototype*:
       - ``acc_delete_finalize_async(h_void *a, size_t len, int async);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``subroutine acc_delete(a)``
     * -
       - ``type, dimension(:[,:]...) :: a``
     * - *Interface*:
       - ``subroutine acc_delete(a, len)``
     * -
       - ``type, dimension(:[,:]...) :: a``
     * -
       - ``integer len``
     * - *Interface*:
       - ``subroutine acc_delete_async(a, async)``
     * -
       - ``type, dimension(:[,:]...) :: a``
     * -
       - ``integer(acc_handle_kind) :: async``
     * - *Interface*:
       - ``subroutine acc_delete_async(a, len, async)``
     * -
       - ``type, dimension(:[,:]...) :: a``
     * -
       - ``integer len``
     * -
       - ``integer(acc_handle_kind) :: async``
     * - *Interface*:
       - ``subroutine acc_delete_finalize(a)``
     * -
       - ``type, dimension(:[,:]...) :: a``
     * - *Interface*:
       - ``subroutine acc_delete_finalize(a, len)``
     * -
       - ``type, dimension(:[,:]...) :: a``
     * -
       - ``integer len``
     * - *Interface*:
       - ``subroutine acc_delete_async_finalize(a, async)``
     * -
       - ``type, dimension(:[,:]...) :: a``
     * -
       - ``integer(acc_handle_kind) :: async``
     * - *Interface*:
       - ``subroutine acc_delete_async_finalize(a, len, async)``
     * -
       - ``type, dimension(:[,:]...) :: a``
     * -
       - ``integer len``
     * -
       - ``integer(acc_handle_kind) :: async``

Reference:
  :openacc:`2.6`, section
  3.2.23.