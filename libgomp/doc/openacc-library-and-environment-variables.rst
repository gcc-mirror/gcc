..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

OpenACC library and environment variables
*****************************************

There are two environment variables associated with the OpenACC library
that may be used to control the device type and device number:
:envvar:`ACC_DEVICE_TYPE` and :envvar:`ACC_DEVICE_NUM`, respectively. These two
environment variables can be used as an alternative to calling
``acc_set_device_num()``. As seen in the second use case, the device
type and device number were specified using ``acc_set_device_num()``.
If however, the aforementioned environment variables were set, then the
call to ``acc_set_device_num()`` would not be required.

The use of the environment variables is only relevant when an OpenACC function
is called prior to a call to ``cudaCreate()``. If ``cudaCreate()``
is called prior to a call to an OpenACC function, then you must call
``acc_set_device_num()`` [#f1]_.

.. -
   OpenACC Profiling Interface
   -

.. [#f1] More complete information
  about :envvar:`ACC_DEVICE_TYPE` and :envvar:`ACC_DEVICE_NUM` can be found in
  sections 4.1 and 4.2 of the `OpenACC <https://www.openacc.org>`_
  Application Programming Interface”, Version 2.6.
