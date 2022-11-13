..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: Environment Variable, Implementation specific setting

.. _omp_target_offload:

OMP_TARGET_OFFLOAD -- Controls offloading behaviour
***************************************************

Description:
  Specifies the behaviour with regard to offloading code to a device.  This
  variable can be set to one of three values - ``MANDATORY``, ``DISABLED``
  or ``DEFAULT``.

  If set to ``MANDATORY``, the program will terminate with an error if
  the offload device is not present or is not supported.  If set to
  ``DISABLED``, then offloading is disabled and all code will run on the
  host. If set to ``DEFAULT``, the program will try offloading to the
  device first, then fall back to running code on the host if it cannot.

  If undefined, then the program will behave as if ``DEFAULT`` was set.

Reference:
  :openmp:`5.0`, Section 6.17