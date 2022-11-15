..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: Environment Variable

.. _omp_default_device:

OMP_DEFAULT_DEVICE -- Set the device used in target regions
***********************************************************

Description:
  Set to choose the device which is used in a ``target`` region, unless the
  value is overridden by ``omp_set_default_device`` or by a ``device``
  clause.  The value shall be the nonnegative device number. If no device with
  the given device number exists, the code is executed on the host.  If unset,
  device number 0 will be used.

See also:
  :ref:`omp_get_default_device`, :ref:`omp_set_default_device`,

Reference:
  :openmp:`4.5`, Section 4.13
