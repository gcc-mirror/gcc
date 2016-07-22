/* Public domain.  */
extern double __floatsidf (long);

double __floathidf (int);

double
__floathidf (int u)
{
  return __floatsidf ((long)u);
}
