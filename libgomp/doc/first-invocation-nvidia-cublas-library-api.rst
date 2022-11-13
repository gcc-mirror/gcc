..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

First invocation: NVIDIA CUBLAS library API
*******************************************

In this first use case (see below), a function in the CUBLAS library is called
prior to any of the functions in the OpenACC library. More specifically, the
function ``cublasCreate()``.

When invoked, the function initializes the library and allocates the
hardware resources on the host and the device on behalf of the caller. Once
the initialization and allocation has completed, a handle is returned to the
caller. The OpenACC library also requires initialization and allocation of
hardware resources. Since the CUBLAS library has already allocated the
hardware resources for the device, all that is left to do is to initialize
the OpenACC library and acquire the hardware resources on the host.

Prior to calling the OpenACC function that initializes the library and
allocate the host hardware resources, you need to acquire the device number
that was allocated during the call to ``cublasCreate()``. The invoking of the
runtime library function ``cudaGetDevice()`` accomplishes this. Once
acquired, the device number is passed along with the device type as
parameters to the OpenACC library function ``acc_set_device_num()``.

Once the call to ``acc_set_device_num()`` has completed, the OpenACC
library uses the  context that was created during the call to
``cublasCreate()``. In other words, both libraries will be sharing the
same context.

.. code-block:: c++

      /* Create the handle */
      s = cublasCreate(&h);
      if (s != CUBLAS_STATUS_SUCCESS)
      {
          fprintf(stderr, "cublasCreate failed %d\n", s);
          exit(EXIT_FAILURE);
      }

      /* Get the device number */
      e = cudaGetDevice(&dev);
      if (e != cudaSuccess)
      {
          fprintf(stderr, "cudaGetDevice failed %d\n", e);
          exit(EXIT_FAILURE);
      }

      /* Initialize OpenACC library and use device 'dev' */
      acc_set_device_num(dev, acc_device_nvidia);