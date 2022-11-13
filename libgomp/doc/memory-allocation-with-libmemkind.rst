..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _memory-allocation-with-libmemkind:

Memory allocation with libmemkind
*********************************

On Linux systems, where the `memkind
library <https://github.com/memkind/memkind>`_ (``libmemkind.so.0``) is available at runtime, it is used when
creating memory allocators requesting

* the memory space ``omp_high_bw_mem_space``

* the memory space ``omp_large_cap_mem_space``

* the partition trait ``omp_atv_interleaved``

.. -
   Offload-Target Specifics
   -