/* Public domain.  */
int
__mspabi_cmpf (float x, float y)
{
  if (x < y)
    return -1;
  if (x > y)
    return 1;
  return 0;
}
int
__mspabi_cmpd (double x, double y)
{
  if (x < y)
    return -1;
  if (x > y)
    return 1;
  return 0;
}
