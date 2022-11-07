..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _implementing-parallel-construct:

Implementing PARALLEL construct
*******************************

.. code-block:: c++

    #pragma omp parallel
    {
      body;
    }

becomes

.. code-block:: c++

    void subfunction (void *data)
    {
      use data;
      body;
    }

    setup data;
    GOMP_parallel_start (subfunction, &data, num_threads);
    subfunction (&data);
    GOMP_parallel_end ();

.. code-block:: c++

    void GOMP_parallel_start (void (*fn)(void *), void *data, unsigned num_threads)

The :samp:`{FN}` argument is the subfunction to be run in parallel.

The :samp:`{DATA}` argument is a pointer to a structure used to
communicate data in and out of the subfunction, as discussed
above with respect to FIRSTPRIVATE et al.

The :samp:`{NUM_THREADS}` argument is 1 if an IF clause is present
and false, or the value of the NUM_THREADS clause, if
present, or 0.

The function needs to create the appropriate number of
threads and/or launch them from the dock.  It needs to
create the team structure and assign team ids.

.. code-block:: c++

    void GOMP_parallel_end (void)

Tears down the team and returns us to the previous ``omp_in_parallel()`` state.