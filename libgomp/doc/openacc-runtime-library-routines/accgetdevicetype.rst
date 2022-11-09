..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _acc_get_device_type:

acc_get_device_type -- Get type of device accelerator to be used.
*****************************************************************

Description
  This function returns what device type will be used when executing a
  parallel or kernels region.

  This function returns ``acc_device_none`` if
  ``acc_get_device_type`` is called from
  ``acc_ev_device_init_start``, ``acc_ev_device_init_end``
  callbacks of the OpenACC Profiling Interface (:ref:`openacc-profiling-interface`), that is, if the device is currently being initialized.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``acc_device_t acc_get_device_type(void);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``function acc_get_device_type(void)``
     * -
       - ``integer(kind=acc_device_kind) acc_get_device_type``

Reference:
  :openacc:`2.6`, section
  3.2.3.
