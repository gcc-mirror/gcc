..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _acc_deviceptr:

acc_deviceptr -- Get device pointer associated with specific host address.
**************************************************************************

Description
  This function returns the device address that has been mapped to the
  host address specified by :samp:`{h}`.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``void *acc_deviceptr(h_void *h);``

Reference:
  :openacc:`2.6`, section
  3.2.28.
