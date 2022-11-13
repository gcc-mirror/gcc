..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _implementing-master-construct:

Implementing MASTER construct
*****************************

.. code-block:: c++

  if (omp_get_thread_num () == 0)
    block

Alternately, we generate two copies of the parallel subfunction
and only include this in the version run by the primary thread.
Surely this is not worthwhile though...