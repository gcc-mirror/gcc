..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: Environment Variable

.. _omp_display_env:

OMP_DISPLAY_ENV -- Show OpenMP version and environment variables
****************************************************************

Description:
  If set to ``TRUE``, the OpenMP version number and the values
  associated with the OpenMP environment variables are printed to ``stderr``.
  If set to ``VERBOSE``, it additionally shows the value of the environment
  variables which are GNU extensions.  If undefined or set to ``FALSE``,
  this information will not be shown.

Reference:
  :openmp:`4.5`, Section 4.12