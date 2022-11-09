..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _omp_max_task_priority:

OMP_MAX_TASK_PRIORITY -- Set the maximum priority
*************************************************

number that can be set for a task.

.. index:: Environment Variable

Description:
  Specifies the initial value for the maximum priority value that can be
  set for a task.  The value of this variable shall be a non-negative
  integer, and zero is allowed.  If undefined, the default priority is
  0.

See also:
  :ref:`omp_get_max_task_priority`

Reference:
  :openmp:`4.5`, Section 4.14
