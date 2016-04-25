/* Public domain.  */
typedef int HItype __attribute__ ((mode (HI)));
typedef unsigned int UHItype __attribute__ ((mode (HI)));
typedef float DFtype __attribute__ ((mode (DF)));

extern DFtype __floatunsidf (unsigned long);

DFtype __floatunhidf (UHItype);

DFtype
__floatunhidf (UHItype u)
{
  return __floatunsidf ((unsigned long)u);
}
