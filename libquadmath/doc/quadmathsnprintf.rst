..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _quadmath_snprintf:

quadmath_snprintf --- Convert to string
***************************************

The function ``quadmath_snprintf`` converts a ``__float128`` floating-point
number into a string.  It is a specialized alternative to ``snprintf``, where
the format string is restricted to a single conversion specifier with ``Q``
modifier and conversion specifier ``e``, ``E``, ``f``, ``F``, ``g``,
``G``, ``a`` or ``A``, with no extra characters before or after the
conversion specifier.  The ``%m$`` or ``*m$`` style must not be used in
the format.

Syntax:
  ``int quadmath_snprintf (char *s, size_t size, const char *format, ...)``

Arguments:
  .. list-table::

     * - :samp:`{s}`
       - output string
     * - :samp:`{size}`
       - byte size of the string, including trailing NUL
     * - :samp:`{format}`
       - conversion specifier string

Note:
  On some targets when supported by the C library hooks are installed
  for ``printf`` family of functions, so that ``printf ("%Qe", 1.2Q);``
  etc. works too.

Example:
  .. code-block:: c++

    #include <quadmath.h>
    #include <stdlib.h>
    #include <stdio.h>

    int main ()
    {
      __float128 r;
      int prec = 20;
      int width = 46;
      char buf[128];

      r = 2.0q;
      r = sqrtq (r);
      int n = quadmath_snprintf (buf, sizeof buf, "%+-#*.20Qe", width, r);
      if ((size_t) n < sizeof buf)
        printf ("%s\n", buf);
        /* Prints: +1.41421356237309504880e+00 */
      quadmath_snprintf (buf, sizeof buf, "%Qa", r);
      if ((size_t) n < sizeof buf)
        printf ("%s\n", buf);
        /* Prints: 0x1.6a09e667f3bcc908b2fb1366ea96p+0 */
      n = quadmath_snprintf (NULL, 0, "%+-#46.*Qe", prec, r);
      if (n > -1)
        {
          char *str = malloc (n + 1);
          if (str)
            {
              quadmath_snprintf (str, n + 1, "%+-#46.*Qe", prec, r);
              printf ("%s\n", str);
              /* Prints: +1.41421356237309504880e+00 */
            }
          free (str);
        }
      return 0;
    }
