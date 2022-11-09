..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _implementing-threadprivate-construct:

Implementing THREADPRIVATE construct
************************************

In _most_ cases we can map this directly to ``__thread``.  Except
that OMP allows constructors for C++ objects.  We can either
refuse to support this (how often is it used?) or we can
implement something akin to .ctors.

Even more ideally, this ctor feature is handled by extensions
to the main pthreads library.  Failing that, we can have a set
of entry points to register ctor functions to be called.
