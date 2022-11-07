..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _acc_set_device_type:

acc_set_device_type -- Set type of device accelerator to use.
*************************************************************

Description
  This function indicates to the runtime library which device type, specified
  in :samp:`{devicetype}`, to use when executing a parallel or kernels region.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``acc_set_device_type(acc_device_t devicetype);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``subroutine acc_set_device_type(devicetype)``
     * -
       - ``integer(kind=acc_device_kind) devicetype``

Reference:
  :openacc:`2.6`, section
  3.2.2.