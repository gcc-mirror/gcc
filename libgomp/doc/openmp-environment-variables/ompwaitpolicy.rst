..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: Environment Variable

.. _omp_wait_policy:

OMP_WAIT_POLICY -- How waiting threads are handled
**************************************************

Description:
  Specifies whether waiting threads should be active or passive.  If
  the value is ``PASSIVE``, waiting threads should not consume CPU
  power while waiting; while the value is ``ACTIVE`` specifies that
  they should.  If undefined, threads wait actively for a short time
  before waiting passively.

See also:
  :ref:`GOMP_SPINCOUNT`

Reference:
  :openmp:`4.5`, Section 4.8
