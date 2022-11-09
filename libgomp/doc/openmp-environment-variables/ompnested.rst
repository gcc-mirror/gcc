..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: Environment Variable, Implementation specific setting

.. _omp_nested:

OMP_NESTED -- Nested parallel regions
*************************************

Description:
  Enable or disable nested parallel regions, i.e., whether team members
  are allowed to create new teams.  The value of this environment variable
  shall be ``TRUE`` or ``FALSE``.  If set to ``TRUE``, the number
  of maximum active nested regions supported will by default be set to the
  maximum supported, otherwise it will be set to one.  If
  :envvar:`OMP_MAX_ACTIVE_LEVELS` is defined, its setting will override this
  setting.  If both are undefined, nested parallel regions are enabled if
  :envvar:`OMP_NUM_THREADS` or :envvar:`OMP_PROC_BINDS` are defined to a list with
  more than one item, otherwise they are disabled by default.

See also:
  :ref:`omp_set_max_active_levels`, :ref:`omp_set_nested`

Reference:
  :openmp:`4.5`, Section 4.6
