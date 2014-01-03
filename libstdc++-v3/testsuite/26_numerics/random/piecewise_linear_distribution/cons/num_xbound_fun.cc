// { dg-options "-std=c++0x" }
// { dg-require-cstdint "" }
//
// 2008-12-03  Edward M. Smith-Rowland <3dw4rd@verizon.net>
//
// Copyright (C) 2008-2014 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// 26.4.8.5.3 Class template piecewise_linear_distribution 
// [rand.dist.samp.plinear]
// 26.4.2.4 Concept RandomNumberDistribution [rand.concept.dist]

#include <random>
#include <cmath>
#include <testsuite_hooks.h>

struct cosine_distribution
{
    cosine_distribution(double x0, double lambda)
    : _M_x0(x0), _M_lambda(lambda)
    { }

    double
    operator()(double x)
    {
      if (x - _M_x0 < -_M_lambda / 4)
        return 0.0;
      else if (x - _M_x0 > _M_lambda / 4)
        return 0.0;
      else
	{
	  const double pi = 3.14159265358979323846;
	  return std::cos(2 * pi * (x - _M_x0) / _M_lambda);
	}
    }

private:
    double _M_x0;
    double _M_lambda;
};

void
test01()
{
  bool test __attribute__((unused)) = true;

  cosine_distribution cd(1.5, 3.0);
  std::piecewise_linear_distribution<> u(21, -10.0, 10.0, cd);
  std::vector<double> interval = u.intervals();
  std::vector<double> density = u.densities();
  VERIFY( interval.size() == 22 );
  VERIFY( density.size() == 22 );
}

int main()
{
  test01();
  return 0;
}
