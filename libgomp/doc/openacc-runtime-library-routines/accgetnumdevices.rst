..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _acc_get_num_devices:

acc_get_num_devices -- Get number of devices for given device type
******************************************************************

Description
  This function returns a value indicating the number of devices available
  for the device type specified in :samp:`{devicetype}`.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``int acc_get_num_devices(acc_device_t devicetype);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``integer function acc_get_num_devices(devicetype)``
     * -
       - ``integer(kind=acc_device_kind) devicetype``

Reference:
  :openacc:`2.6`, section
  3.2.1.