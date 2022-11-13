..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: Environment Variable, Implementation specific setting

.. _gomp_stacksize:

GOMP_STACKSIZE -- Set default thread stack size
***********************************************

Description:
  Set the default thread stack size in kilobytes.  This is different from
  ``pthread_attr_setstacksize`` which gets the number of bytes as an
  argument.  If the stack size cannot be set due to system constraints, an
  error is reported and the initial stack size is left unchanged.  If undefined,
  the stack size is system dependent.

See also:
  :ref:`OMP_STACKSIZE`

Reference:
  `GCC Patches Mailinglist <https://gcc.gnu.org/ml/gcc-patches/2006-06/msg00493.html>`_,
  `GCC Patches Mailinglist (2) <https://gcc.gnu.org/ml/gcc-patches/2006-06/msg00496.html>`_