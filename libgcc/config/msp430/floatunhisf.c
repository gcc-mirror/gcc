/* Public domain.  */
typedef int HItype __attribute__ ((mode (HI)));
typedef unsigned int UHItype __attribute__ ((mode (HI)));
typedef float SFtype __attribute__ ((mode (SF)));

extern SFtype __floatunsisf (unsigned long);

SFtype
__floatunhisf (UHItype u)
{
  return __floatunsisf ((unsigned long)u);
}
