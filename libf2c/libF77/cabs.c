#undef abs
#include <math.h>
double
f__cabs (double real, double imag)
{
  double temp;

  if (real < 0)
    real = -real;
  if (imag < 0)
    imag = -imag;
  if (imag > real)
    {
      temp = real;
      real = imag;
      imag = temp;
    }
  if ((real + imag) == real)
    return (real);

  temp = imag / real;
  temp = real * sqrt (1.0 + temp * temp);	/*overflow!! */
  return (temp);
}
