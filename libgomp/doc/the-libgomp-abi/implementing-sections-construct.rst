..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _implementing-sections-construct:

Implementing SECTIONS construct
*******************************

A block as

.. code-block:: c++

    #pragma omp sections
    {
      #pragma omp section
      stmt1;
      #pragma omp section
      stmt2;
      #pragma omp section
      stmt3;
    }

becomes

.. code-block:: c++

    for (i = GOMP_sections_start (3); i != 0; i = GOMP_sections_next ())
      switch (i)
        {
        case 1:
          stmt1;
          break;
        case 2:
          stmt2;
          break;
        case 3:
          stmt3;
          break;
        }
    GOMP_barrier ();