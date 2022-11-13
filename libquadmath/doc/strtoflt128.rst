..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _strtoflt128:

strtoflt128 --- Convert from string
***********************************

The function ``strtoflt128`` converts a string into a
``__float128`` number.

Syntax:
  ``__float128 strtoflt128 (const char *s, char **sp)``

Arguments:
  .. list-table::

     * - :samp:`{s}`
       - input string
     * - :samp:`{sp}`
       - the address of the next character in the string

  The argument :samp:`{sp}` contains, if not ``NULL``, the address of the
  next character following the parts of the string, which have been read.

Example:
  .. code-block:: c++

    #include <quadmath.h>

    int main ()
    {
      __float128 r;

      r = strtoflt128 ("1.2345678", NULL);

      return 0;
    }