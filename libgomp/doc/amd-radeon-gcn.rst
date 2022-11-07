..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _amd-radeon:

AMD Radeon (GCN)
****************

On the hardware side, there is the hierarchy (fine to coarse):

* work item (thread)

* wavefront

* work group

* compute unite (CU)

All OpenMP and OpenACC levels are used, i.e.

* OpenMP's simd and OpenACC's vector map to work items (thread)

* OpenMP's threads ('parallel') and OpenACC's workers map
        to wavefronts

* OpenMP's teams and OpenACC's gang use a threadpool with the
        size of the number of teams or gangs, respectively.

The used sizes are

* Number of teams is the specified ``num_teams`` (OpenMP) or
        ``num_gangs`` (OpenACC) or otherwise the number of CU

* Number of wavefronts is 4 for gfx900 and 16 otherwise;
        ``num_threads`` (OpenMP) and ``num_workers`` (OpenACC)
        overrides this if smaller.

* The wavefront has 102 scalars and 64 vectors

* Number of workitems is always 64

* The hardware permits maximally 40 workgroups/CU and
        16 wavefronts/workgroup up to a limit of 40 wavefronts in total per CU.

* 80 scalars registers and 24 vector registers in non-kernel functions
        (the chosen procedure-calling API).

* For the kernel itself: as many as register pressure demands (number of
        teams and number of threads, scaled down if registers are exhausted)

The implementation remark:

* I/O within OpenMP target regions and OpenACC parallel/kernels is supported
        using the C library ``printf`` functions and the Fortran
        ``print`` / ``write`` statements.