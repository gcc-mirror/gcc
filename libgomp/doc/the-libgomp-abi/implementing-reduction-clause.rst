..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _implementing-reduction-clause:

Implementing REDUCTION clause
*****************************

The private struct mentioned in the previous section should have
a pointer to an array of the type of the variable, indexed by the
thread's :samp:`{team_id}`.  The thread stores its final value into the
array, and after the barrier, the primary thread iterates over the
array to collect the values.