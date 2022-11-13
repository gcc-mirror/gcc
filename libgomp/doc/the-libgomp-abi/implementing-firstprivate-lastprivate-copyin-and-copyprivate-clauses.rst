..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _implementing-firstprivate-lastprivate-copyin-and-copyprivate-clauses:

Implementing FIRSTPRIVATE LASTPRIVATE COPYIN and COPYPRIVATE clauses
********************************************************************

This seems simple enough for PARALLEL blocks.  Create a private
struct for communicating between the parent and subfunction.
In the parent, copy in values for scalar and "small" structs;
copy in addresses for others TREE_ADDRESSABLE types.  In the
subfunction, copy the value into the local variable.

It is not clear what to do with bare FOR or SECTION blocks.
The only thing I can figure is that we do something like:

.. code-block:: c++

  #pragma omp for firstprivate(x) lastprivate(y)
  for (int i = 0; i < n; ++i)
    body;

which becomes

.. code-block:: c++

  {
    int x = x, y;

    // for stuff

    if (i == n)
      y = y;
  }

where the "x=x" and "y=y" assignments actually have different
uids for the two variables, i.e. not something you could write
directly in C.  Presumably this only makes sense if the "outer"
x and y are global variables.

COPYPRIVATE would work the same way, except the structure
broadcast would have to happen via SINGLE machinery instead.