..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: replacement functions, functions, replacement

.. _replacement-functions:

Replacement Functions
*********************

Some functions have extremely limited implementations on different
platforms.  Other functions are tedious to use correctly; for example,
proper use of ``malloc`` calls for the return value to be checked and
appropriate action taken if memory has been exhausted.  A group of
'replacement functions' is available in ``libiberty`` to address these issues
for some of the most commonly used subroutines.

All of these functions are declared in the :samp:`libiberty.h` header
file.  Many of the implementations will use preprocessor macros set by
GNU Autoconf, if you decide to make use of that program.  Some of these
functions may call one another.

.. toctree::
  :maxdepth: 2


.. index:: memory allocation

.. _memory-allocation:

Memory Allocation
^^^^^^^^^^^^^^^^^

The functions beginning with the letter :samp:`x` are wrappers around
standard functions; the functions provided by the system environment
are called and their results checked before the results are passed back
to client code.  If the standard functions fail, these wrappers will
terminate the program.  Thus, these versions can be used with impunity.

.. index:: exit handlers

.. _exit-handlers:

Exit Handlers
^^^^^^^^^^^^^

The existence and implementation of the ``atexit`` routine varies
amongst the flavors of Unix.  ``libiberty`` provides an unvarying dependable
implementation via ``xatexit`` and ``xexit``.

.. index:: error reporting

.. _error-reporting:

Error Reporting
^^^^^^^^^^^^^^^

These are a set of routines to facilitate programming with the system
``errno`` interface.  The ``libiberty`` source file :samp:`strerror.c`
contains a good deal of documentation for these functions.
