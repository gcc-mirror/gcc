..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _acc_set_cuda_stream:

acc_set_cuda_stream -- Set CUDA stream handle.
**********************************************

Description
  This function associates the stream handle specified by :samp:`{stream}` with
  the queue :samp:`{async}`.

  This cannot be used to change the stream handle associated with
  ``acc_async_sync``.

  The return value is not specified.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``int acc_set_cuda_stream(int async, void *stream);``

Reference:
  :openacc:`2.6`, section
  A.2.1.4.