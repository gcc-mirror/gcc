..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _implementing-critical-construct:

Implementing CRITICAL construct
*******************************

Without a specified name,

.. code-block:: c++

    void GOMP_critical_start (void);
    void GOMP_critical_end (void);

so that we don't get COPY relocations from libgomp to the main
application.

With a specified name, use omp_set_lock and omp_unset_lock with
name being transformed into a variable declared like

.. code-block:: c++

    omp_lock_t gomp_critical_user_<name> __attribute__((common))

Ideally the ABI would specify that all zero is a valid unlocked
state, and so we wouldn't need to initialize this at
startup.