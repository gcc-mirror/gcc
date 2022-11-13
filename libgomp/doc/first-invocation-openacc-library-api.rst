..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

First invocation: OpenACC library API
*************************************

In this second use case (see below), a function in the OpenACC library is
called prior to any of the functions in the CUBLAS library. More specificially,
the function ``acc_set_device_num()``.

In the use case presented here, the function ``acc_set_device_num()``
is used to both initialize the OpenACC library and allocate the hardware
resources on the host and the device. In the call to the function, the
call parameters specify which device to use and what device
type to use, i.e., ``acc_device_nvidia``. It should be noted that this
is but one method to initialize the OpenACC library and allocate the
appropriate hardware resources. Other methods are available through the
use of environment variables and these will be discussed in the next section.

Once the call to ``acc_set_device_num()`` has completed, other OpenACC
functions can be called as seen with multiple calls being made to
``acc_copyin()``. In addition, calls can be made to functions in the
CUBLAS library. In the use case a call to ``cublasCreate()`` is made
subsequent to the calls to ``acc_copyin()``.
As seen in the previous use case, a call to ``cublasCreate()``
initializes the CUBLAS library and allocates the hardware resources on the
host and the device.  However, since the device has already been allocated,
``cublasCreate()`` will only initialize the CUBLAS library and allocate
the appropriate hardware resources on the host. The context that was created
as part of the OpenACC initialization is shared with the CUBLAS library,
similarly to the first use case.

.. code-block:: c++

      dev = 0;

      acc_set_device_num(dev, acc_device_nvidia);

      /* Copy the first set to the device */
      d_X = acc_copyin(&h_X[0], N * sizeof (float));
      if (d_X == NULL)
      {
          fprintf(stderr, "copyin error h_X\n");
          exit(EXIT_FAILURE);
      }

      /* Copy the second set to the device */
      d_Y = acc_copyin(&h_Y1[0], N * sizeof (float));
      if (d_Y == NULL)
      {
          fprintf(stderr, "copyin error h_Y1\n");
          exit(EXIT_FAILURE);
      }

      /* Create the handle */
      s = cublasCreate(&h);
      if (s != CUBLAS_STATUS_SUCCESS)
      {
          fprintf(stderr, "cublasCreate failed %d\n", s);
          exit(EXIT_FAILURE);
      }

      /* Perform saxpy using CUBLAS library function */
      s = cublasSaxpy(h, N, &alpha, d_X, 1, d_Y, 1);
      if (s != CUBLAS_STATUS_SUCCESS)
      {
          fprintf(stderr, "cublasSaxpy failed %d\n", s);
          exit(EXIT_FAILURE);
      }

      /* Copy the results from the device */
      acc_memcpy_from_device(&h_Y1[0], d_Y, N * sizeof (float));