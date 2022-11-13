..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _implementing-for-construct:

Implementing FOR construct
**************************

.. code-block:: c++

    #pragma omp parallel for
    for (i = lb; i <= ub; i++)
      body;

becomes

.. code-block:: c++

    void subfunction (void *data)
    {
      long _s0, _e0;
      while (GOMP_loop_static_next (&_s0, &_e0))
      {
        long _e1 = _e0, i;
        for (i = _s0; i < _e1; i++)
          body;
      }
      GOMP_loop_end_nowait ();
    }

    GOMP_parallel_loop_static (subfunction, NULL, 0, lb, ub+1, 1, 0);
    subfunction (NULL);
    GOMP_parallel_end ();

.. code-block:: c++

    #pragma omp for schedule(runtime)
    for (i = 0; i < n; i++)
      body;

becomes

.. code-block:: c++

    {
      long i, _s0, _e0;
      if (GOMP_loop_runtime_start (0, n, 1, &_s0, &_e0))
        do {
          long _e1 = _e0;
          for (i = _s0, i < _e0; i++)
            body;
        } while (GOMP_loop_runtime_next (&_s0, _&e0));
      GOMP_loop_end ();
    }

Note that while it looks like there is trickiness to propagating
a non-constant STEP, there isn't really.  We're explicitly allowed
to evaluate it as many times as we want, and any variables involved
should automatically be handled as PRIVATE or SHARED like any other
variables.  So the expression should remain evaluable in the
subfunction.  We can also pull it into a local variable if we like,
but since its supposed to remain unchanged, we can also not if we like.

If we have SCHEDULE(STATIC), and no ORDERED, then we ought to be
able to get away with no work-sharing context at all, since we can
simply perform the arithmetic directly in each thread to divide up
the iterations.  Which would mean that we wouldn't need to call any
of these routines.

There are separate routines for handling loops with an ORDERED
clause.  Bookkeeping for that is non-trivial...