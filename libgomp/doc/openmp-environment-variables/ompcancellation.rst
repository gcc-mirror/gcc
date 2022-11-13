..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: Environment Variable

.. _omp_cancellation:

OMP_CANCELLATION -- Set whether cancellation is activated
*********************************************************

Description:
  If set to ``TRUE``, the cancellation is activated.  If set to ``FALSE`` or
  if unset, cancellation is disabled and the ``cancel`` construct is ignored.

See also:
  :ref:`omp_get_cancellation`

Reference:
  :openmp:`4.5`, Section 4.11