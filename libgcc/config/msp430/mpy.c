/* Public domain.  */
extern int __mulhi3 (int, int);

int
__mulhi3 (int x, int y)
{
  volatile int rv = 0;

  while (y > 0)
    {
      rv += x;
      y --;
    }
  return rv;
}
