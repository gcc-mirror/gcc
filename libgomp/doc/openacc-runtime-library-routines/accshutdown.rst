..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _acc_shutdown:

acc_shutdown -- Shuts down the runtime for a specific device type.
******************************************************************

Description
  This function shuts down the runtime for the device type specified in
  :samp:`{devicetype}`.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``acc_shutdown(acc_device_t devicetype);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``subroutine acc_shutdown(devicetype)``
     * -
       - ``integer(acc_device_kind) devicetype``

Reference:
  :openacc:`2.6`, section
  3.2.8.
