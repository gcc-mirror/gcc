..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _nvptx:

nvptx
*****

On the hardware side, there is the hierarchy (fine to coarse):

* thread

* warp

* thread block

* streaming multiprocessor

All OpenMP and OpenACC levels are used, i.e.

* OpenMP's simd and OpenACC's vector map to threads

* OpenMP's threads ('parallel') and OpenACC's workers map to warps

* OpenMP's teams and OpenACC's gang use a threadpool with the
        size of the number of teams or gangs, respectively.

The used sizes are

* The ``warp_size`` is always 32

* CUDA kernel launched: ``dim={#teams,1,1}, blocks={#threads,warp_size,1}``.

Additional information can be obtained by setting the environment variable to
``GOMP_DEBUG=1`` (very verbose; grep for ``kernel.*launch`` for launch
parameters).

GCC generates generic PTX ISA code, which is just-in-time compiled by CUDA,
which caches the JIT in the user's directory (see CUDA documentation; can be
tuned by the environment variables ``CUDA_CACHE_{DISABLE,MAXSIZE,PATH}``.

Note: While PTX ISA is generic, the ``-mptx=`` and ``-march=`` commandline
options still affect the used PTX ISA code and, thus, the requirments on
CUDA version and hardware.

The implementation remark:

* I/O within OpenMP target regions and OpenACC parallel/kernels is supported
        using the C library ``printf`` functions. Note that the Fortran
        ``print`` / ``write`` statements are not supported, yet.

* Compilation OpenMP code that contains ``requires reverse_offload``
        requires at least ``-march=sm_35``, compiling for ``-march=sm_30``
        is not supported.

.. -
   The libgomp ABI
   -
