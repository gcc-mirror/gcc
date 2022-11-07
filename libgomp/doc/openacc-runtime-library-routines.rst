..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _openacc-runtime-library-routines:

OpenACC Runtime Library Routines
--------------------------------

The runtime routines described here are defined by section 3 of the OpenACC
specifications in version 2.6.
They have C linkage, and do not throw exceptions.
Generally, they are available only for the host, with the exception of
``acc_on_device``, which is available for both the host and the
acceleration device.

.. toctree::
  :maxdepth: 2

  openacc-runtime-library-routines/accgetnumdevices
  openacc-runtime-library-routines/accsetdevicetype
  openacc-runtime-library-routines/accgetdevicetype
  openacc-runtime-library-routines/accsetdevicenum
  openacc-runtime-library-routines/accgetdevicenum
  openacc-runtime-library-routines/accgetproperty
  openacc-runtime-library-routines/accasynctest
  openacc-runtime-library-routines/accasynctestall
  openacc-runtime-library-routines/accwait
  openacc-runtime-library-routines/accwaitall
  openacc-runtime-library-routines/accwaitallasync
  openacc-runtime-library-routines/accwaitasync
  openacc-runtime-library-routines/accinit
  openacc-runtime-library-routines/accshutdown
  openacc-runtime-library-routines/accondevice
  openacc-runtime-library-routines/accmalloc
  openacc-runtime-library-routines/accfree
  openacc-runtime-library-routines/acccopyin
  openacc-runtime-library-routines/accpresentorcopyin
  openacc-runtime-library-routines/acccreate
  openacc-runtime-library-routines/accpresentorcreate
  openacc-runtime-library-routines/acccopyout
  openacc-runtime-library-routines/accdelete
  openacc-runtime-library-routines/accupdatedevice
  openacc-runtime-library-routines/accupdateself
  openacc-runtime-library-routines/accmapdata
  openacc-runtime-library-routines/accunmapdata
  openacc-runtime-library-routines/accdeviceptr
  openacc-runtime-library-routines/acchostptr
  openacc-runtime-library-routines/accispresent
  openacc-runtime-library-routines/accmemcpytodevice
  openacc-runtime-library-routines/accmemcpyfromdevice
  openacc-runtime-library-routines/accattach
  openacc-runtime-library-routines/accdetach

API routines for target platforms.

.. toctree::
  :maxdepth: 2

  openacc-runtime-library-routines/accgetcurrentcudadevice
  openacc-runtime-library-routines/accgetcurrentcudacontext
  openacc-runtime-library-routines/accgetcudastream
  openacc-runtime-library-routines/accsetcudastream

API routines for the OpenACC Profiling Interface.

.. toctree::
  :maxdepth: 2

  openacc-runtime-library-routines/accprofregister
  openacc-runtime-library-routines/accprofunregister
  openacc-runtime-library-routines/accproflookup
  openacc-runtime-library-routines/accregisterlibrary