..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _acc_get_cuda_stream:

acc_get_cuda_stream -- Get CUDA stream handle.
**********************************************

Description
  This function returns the CUDA stream handle for the queue :samp:`{async}`.
  This handle is the same as used by the CUDA Runtime or Driver API's.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``void *acc_get_cuda_stream(int async);``

Reference:
  :openacc:`2.6`, section
  A.2.1.3.
