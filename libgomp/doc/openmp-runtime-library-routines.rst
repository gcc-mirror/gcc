..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _runtime-library-routines:

OpenMP Runtime Library Routines
-------------------------------

The runtime routines described here are defined by Section 3 of the OpenMP
specification in version 4.5.  The routines are structured in following
three parts:

Control threads, processors and the parallel environment.  They have C
linkage, and do not throw exceptions.

.. toctree::
  :maxdepth: 2

  openmp-runtime-library-routines/ompgetactivelevel
  openmp-runtime-library-routines/ompgetancestorthreadnum
  openmp-runtime-library-routines/ompgetcancellation
  openmp-runtime-library-routines/ompgetdefaultdevice
  openmp-runtime-library-routines/ompgetdevicenum
  openmp-runtime-library-routines/ompgetdynamic
  openmp-runtime-library-routines/ompgetinitialdevice
  openmp-runtime-library-routines/ompgetlevel
  openmp-runtime-library-routines/ompgetmaxactivelevels
  openmp-runtime-library-routines/ompgetmaxtaskpriority
  openmp-runtime-library-routines/ompgetmaxteams
  openmp-runtime-library-routines/ompgetmaxthreads
  openmp-runtime-library-routines/ompgetnested
  openmp-runtime-library-routines/ompgetnumdevices
  openmp-runtime-library-routines/ompgetnumprocs
  openmp-runtime-library-routines/ompgetnumteams
  openmp-runtime-library-routines/ompgetnumthreads
  openmp-runtime-library-routines/ompgetprocbind
  openmp-runtime-library-routines/ompgetschedule
  openmp-runtime-library-routines/ompgetsupportedactivelevels
  openmp-runtime-library-routines/ompgetteamnum
  openmp-runtime-library-routines/ompgetteamsize
  openmp-runtime-library-routines/ompgetteamsthreadlimit
  openmp-runtime-library-routines/ompgetthreadlimit
  openmp-runtime-library-routines/ompgetthreadnum
  openmp-runtime-library-routines/ompinparallel
  openmp-runtime-library-routines/ompinfinal
  openmp-runtime-library-routines/ompisinitialdevice
  openmp-runtime-library-routines/ompsetdefaultdevice
  openmp-runtime-library-routines/ompsetdynamic
  openmp-runtime-library-routines/ompsetmaxactivelevels
  openmp-runtime-library-routines/ompsetnested
  openmp-runtime-library-routines/ompsetnumteams
  openmp-runtime-library-routines/ompsetnumthreads
  openmp-runtime-library-routines/ompsetschedule
  openmp-runtime-library-routines/ompsetteamsthreadlimit

Initialize, set, test, unset and destroy simple and nested locks.

.. toctree::
  :maxdepth: 2

  openmp-runtime-library-routines/ompinitlock
  openmp-runtime-library-routines/ompsetlock
  openmp-runtime-library-routines/omptestlock
  openmp-runtime-library-routines/ompunsetlock
  openmp-runtime-library-routines/ompdestroylock
  openmp-runtime-library-routines/ompinitnestlock
  openmp-runtime-library-routines/ompsetnestlock
  openmp-runtime-library-routines/omptestnestlock
  openmp-runtime-library-routines/ompunsetnestlock
  openmp-runtime-library-routines/ompdestroynestlock

Portable, thread-based, wall clock timer.

.. toctree::
  :maxdepth: 2

  openmp-runtime-library-routines/ompgetwtick
  openmp-runtime-library-routines/ompgetwtime

Support for event objects.

.. toctree::
  :maxdepth: 2

  openmp-runtime-library-routines/ompfulfillevent