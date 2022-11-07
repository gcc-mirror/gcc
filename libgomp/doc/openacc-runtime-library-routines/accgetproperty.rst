..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: acc_get_property, acc_get_property_string

.. _acc_get_property:

acc_get_property -- Get device property.
****************************************

Description
  These routines return the value of the specified :samp:`{property}` for the
  device being queried according to :samp:`{devicenum}` and :samp:`{devicetype}`.
  Integer-valued and string-valued properties are returned by
  ``acc_get_property`` and ``acc_get_property_string`` respectively.
  The Fortran ``acc_get_property_string`` subroutine returns the string
  retrieved in its fourth argument while the remaining entry points are
  functions, which pass the return value as their result.

  Note for Fortran, only: the OpenACC technical committee corrected and, hence,
  modified the interface introduced in OpenACC 2.6.  The kind-value parameter
  ``acc_device_property`` has been renamed to ``acc_device_property_kind``
  for consistency and the return type of the ``acc_get_property`` function is
  now a ``c_size_t`` integer instead of a ``acc_device_property`` integer.
  The parameter ``acc_device_property`` will continue to be provided,
  but might be removed in a future version of GCC.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``size_t acc_get_property(int devicenum, acc_device_t devicetype, acc_device_property_t property);``
     * - *Prototype*:
       - ``const char *acc_get_property_string(int devicenum, acc_device_t devicetype, acc_device_property_t property);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``function acc_get_property(devicenum, devicetype, property)``
     * - *Interface*:
       - ``subroutine acc_get_property_string(devicenum, devicetype, property, string)``
     * -
       - ``use ISO_C_Binding, only: c_size_t``
     * -
       - ``integer devicenum``
     * -
       - ``integer(kind=acc_device_kind) devicetype``
     * -
       - ``integer(kind=acc_device_property_kind) property``
     * -
       - ``integer(kind=c_size_t) acc_get_property``
     * -
       - ``character(*) string``

Reference:
  :openacc:`2.6`, section
  3.2.6.