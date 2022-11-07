..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _acc_memcpy_from_device:

acc_memcpy_from_device -- Copy device memory to host memory.
************************************************************

Description
  This function copies host memory specified by host address of :samp:`{src}` from
  device memory specified by the device address :samp:`{dest}` for a length of
  :samp:`{bytes}` bytes.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``acc_memcpy_from_device(d_void *dest, h_void *src, size_t bytes);``

Reference:
  :openacc:`2.6`, section
  3.2.32.