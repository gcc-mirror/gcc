..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: Environment Variable, Implementation specific setting

.. _omp_schedule:

OMP_SCHEDULE -- How threads are scheduled
*****************************************

Description:
  Allows to specify ``schedule type`` and ``chunk size``.
  The value of the variable shall have the form: ``type[,chunk]`` where
  ``type`` is one of ``static``, ``dynamic``, ``guided`` or ``auto``
  The optional ``chunk`` size shall be a positive integer.  If undefined,
  dynamic scheduling and a chunk size of 1 is used.

See also:
  :ref:`omp_set_schedule`

Reference:
  :openmp:`4.5`, Sections 2.7.1.1 and 4.1
