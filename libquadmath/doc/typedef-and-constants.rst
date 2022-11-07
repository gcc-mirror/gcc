..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _typedef-and-constants:

Typedef and constants
---------------------

The following data type has been defined via ``typedef``.

* :samp:`{__complex128}`:  ``__float128``-based complex number

The following macros are defined, which give the numeric limits of the
``__float128`` data type.

* :samp:`{FLT128_MAX}`:  largest finite number
* :samp:`{FLT128_MIN}`:  smallest positive number with full precision
* :samp:`{FLT128_EPSILON}`:  difference between 1 and the next larger representable number
* :samp:`{FLT128_DENORM_MIN}`:  smallest positive denormalized number
* :samp:`{FLT128_MANT_DIG}`:  number of digits in the mantissa (bit precision)
* :samp:`{FLT128_MIN_EXP}`:  maximal negative exponent
* :samp:`{FLT128_MAX_EXP}`:  maximal positive exponent
* :samp:`{FLT128_DIG}`:  number of decimal digits in the mantissa
* :samp:`{FLT128_MIN_10_EXP}`:  maximal negative decimal exponent
* :samp:`{FLT128_MAX_10_EXP}`:  maximal positive decimal exponent

The following mathematical constants of type ``__float128`` are defined.

* :samp:`{M_Eq}`:  the constant e (Euler's number)
* :samp:`{M_LOG2Eq}`:  binary logarithm of 2
* :samp:`{M_LOG10Eq}`:  common, decimal logarithm of 2
* :samp:`{M_LN2q}`:  natural logarithm of 2
* :samp:`{M_LN10q}`:  natural logarithm of 10
* :samp:`{M_PIq}`:  pi
* :samp:`{M_PI_2q}`:  pi divided by two
* :samp:`{M_PI_4q}`:  pi divided by four
* :samp:`{M_1_PIq}`:  one over pi
* :samp:`{M_2_PIq}`:  one over two pi
* :samp:`{M_2_SQRTPIq}`:  two over square root of pi
* :samp:`{M_SQRT2q}`:  square root of 2
* :samp:`{M_SQRT1_2q}`:  one over square root of 2