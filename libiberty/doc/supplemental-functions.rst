..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: supplemental functions, functions, supplemental, functions, missing

.. _supplemental-functions:

Supplemental Functions
**********************

Certain operating systems do not provide functions which have since
become standardized, or at least common.  For example, the Single
Unix Specification Version 2 requires that the ``basename``
function be provided, but an OS which predates that specification
might not have this function.  This should not prevent well-written
code from running on such a system.

Similarly, some functions exist only among a particular 'flavor'
or 'family' of operating systems.  As an example, the ``bzero``
function is often not present on systems outside the BSD-derived
family of systems.

Many such functions are provided in ``libiberty``.  They are quickly
listed here with little description, as systems which lack them
become less and less common.  Each function :samp:`{foo}` is implemented
in :samp:`{foo}.c` but not declared in any ``libiberty`` header file; more
comments and caveats for each function's implementation are often
available in the source file.  Generally, the function can simply
be declared as ``extern``.
