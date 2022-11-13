..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _acc_get_device_num:

acc_get_device_num -- Get device number to be used.
***************************************************

Description
  This function returns which device number associated with the specified device
  type :samp:`{devicetype}`, will be used when executing a parallel or kernels
  region.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``int acc_get_device_num(acc_device_t devicetype);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``function acc_get_device_num(devicetype)``
     * -
       - ``integer(kind=acc_device_kind) devicetype``
     * -
       - ``integer acc_get_device_num``

Reference:
  :openacc:`2.6`, section
  3.2.5.