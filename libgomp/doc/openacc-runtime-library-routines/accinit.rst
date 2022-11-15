..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _acc_init:

acc_init -- Initialize runtime for a specific device type.
**********************************************************

Description
  This function initializes the runtime for the device type specified in
  :samp:`{devicetype}`.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``acc_init(acc_device_t devicetype);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``subroutine acc_init(devicetype)``
     * -
       - ``integer(acc_device_kind) devicetype``

Reference:
  :openacc:`2.6`, section
  3.2.7.
