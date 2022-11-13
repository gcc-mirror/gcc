..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _acc_on_device:

acc_on_device -- Whether executing on a particular device
*********************************************************

Description:
  This function returns whether the program is executing on a particular
  device specified in :samp:`{devicetype}`. In C/C++ a non-zero value is
  returned to indicate the device is executing on the specified device type.
  In Fortran, ``true`` will be returned. If the program is not executing
  on the specified device type C/C++ will return a zero, while Fortran will
  return ``false``.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``acc_on_device(acc_device_t devicetype);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``function acc_on_device(devicetype)``
     * -
       - ``integer(acc_device_kind) devicetype``
     * -
       - ``logical acc_on_device``

Reference:
  :openacc:`2.6`, section
  3.2.17.