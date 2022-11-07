..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _acc_detach:

acc_detach -- Let device pointer point to host-pointer target.
**************************************************************

Description
  This function updates a pointer on the device from pointing to a device-pointer
  address to pointing to the corresponding host data.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``acc_detach(h_void **ptr);``
     * - *Prototype*:
       - ``acc_detach_async(h_void **ptr, int async);``
     * - *Prototype*:
       - ``acc_detach_finalize(h_void **ptr);``
     * - *Prototype*:
       - ``acc_detach_finalize_async(h_void **ptr, int async);``

Reference:
  :openacc:`2.6`, section
  3.2.35.