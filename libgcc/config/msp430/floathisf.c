/* Public domain.  */
typedef int HItype __attribute__ ((mode (HI)));
typedef float SFtype __attribute__ ((mode (SF)));

extern SFtype __floatsisf (unsigned long);

SFtype
__floathisf (HItype u)
{
  return __floatsisf ((unsigned long)u);
}
