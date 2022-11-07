..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _acc_set_device_num:

acc_set_device_num -- Set device number to use.
***********************************************

Description
  This function will indicate to the runtime which device number,
  specified by :samp:`{devicenum}`, associated with the specified device
  type :samp:`{devicetype}`.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``acc_set_device_num(int devicenum, acc_device_t devicetype);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``subroutine acc_set_device_num(devicenum, devicetype)``
     * -
       - ``integer devicenum``
     * -
       - ``integer(kind=acc_device_kind) devicetype``

Reference:
  :openacc:`2.6`, section
  3.2.4.