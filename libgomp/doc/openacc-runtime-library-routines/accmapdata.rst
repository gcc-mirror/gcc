..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _acc_map_data:

acc_map_data -- Map previously allocated device memory to host memory.
**********************************************************************

Description
  This function maps previously allocated device and host memory. The device
  memory is specified with the device address :samp:`{d}`. The host memory is
  specified with the host address :samp:`{h}` and a length of :samp:`{len}`.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``acc_map_data(h_void *h, d_void *d, size_t len);``

Reference:
  :openacc:`2.6`, section
  3.2.26.
