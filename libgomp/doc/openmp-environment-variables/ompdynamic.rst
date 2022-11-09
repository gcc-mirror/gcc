..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: Environment Variable

.. _omp_dynamic:

OMP_DYNAMIC -- Dynamic adjustment of threads
********************************************

Description:
  Enable or disable the dynamic adjustment of the number of threads
  within a team.  The value of this environment variable shall be
  ``TRUE`` or ``FALSE``.  If undefined, dynamic adjustment is
  disabled by default.

See also:
  :ref:`omp_set_dynamic`

Reference:
  :openmp:`4.5`, Section 4.3
