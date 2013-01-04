/* Wrapper for feraiseexcept.  This file is in the public domain.
   Contributed by Alexandre Oliva <aoliva@redhat.com>
   See QUADMATH_FERAISEEXCEPT in configure.ac for more information.  */

int
__quadmath_feraiseexcept (int xcpt)
{
  feraiseexcept (xcpt);
}
