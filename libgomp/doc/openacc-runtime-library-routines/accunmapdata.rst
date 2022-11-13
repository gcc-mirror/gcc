..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _acc_unmap_data:

acc_unmap_data -- Unmap device memory from host memory.
*******************************************************

Description
  This function unmaps previously mapped device and host memory. The latter
  specified by :samp:`{h}`.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``acc_unmap_data(h_void *h);``

Reference:
  :openacc:`2.6`, section
  3.2.27.