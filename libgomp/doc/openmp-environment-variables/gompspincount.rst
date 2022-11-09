..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: Environment Variable, Implementation specific setting

.. _gomp_spincount:

GOMP_SPINCOUNT -- Set the busy-wait spin count
**********************************************

Description:
  Determines how long a threads waits actively with consuming CPU power
  before waiting passively without consuming CPU power.  The value may be
  either ``INFINITE``, ``INFINITY`` to always wait actively or an
  integer which gives the number of spins of the busy-wait loop.  The
  integer may optionally be followed by the following suffixes acting
  as multiplication factors: ``k`` (kilo, thousand), ``M`` (mega,
  million), ``G`` (giga, billion), or ``T`` (tera, trillion).
  If undefined, 0 is used when :envvar:`OMP_WAIT_POLICY` is ``PASSIVE``,
  300,000 is used when :envvar:`OMP_WAIT_POLICY` is undefined and
  30 billion is used when :envvar:`OMP_WAIT_POLICY` is ``ACTIVE``.
  If there are more OpenMP threads than available CPUs, 1000 and 100
  spins are used for :envvar:`OMP_WAIT_POLICY` being ``ACTIVE`` or
  undefined, respectively; unless the :envvar:`GOMP_SPINCOUNT` is lower
  or :envvar:`OMP_WAIT_POLICY` is ``PASSIVE``.

See also:
  :ref:`OMP_WAIT_POLICY`
