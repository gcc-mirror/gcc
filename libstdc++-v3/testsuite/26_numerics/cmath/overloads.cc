// PR 3181
// Origin: pete@toyon.com

#include <cmath>

int main()
{
  int i = -1;
  int j = 9;
  double ans;
  ans = std::acos(i);
  ans = std::asin(i);
  ans = std::atan(i);
  ans = std::atan2(i, j);
  ans = std::cos(i);
  ans = std::cosh(i);
  ans = std::exp(i);
  ans = std::fabs(i);
  ans = std::floor(i);
  ans = std::log(i);
  ans = std::log10(i);
  ans = std::sqrt(i);
  ans = std::sin(i);
  ans = std::sinh(j);
  ans = std::tan(i);
  ans = std::tanh(i);
}
