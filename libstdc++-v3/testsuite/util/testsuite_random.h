// -*- C++ -*-

// Copyright (C) 2011 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the terms
// of the GNU General Public License as published by the Free Software
// Foundation; either version 3, or (at your option) any later
// version.

// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

/**
 * @file testsuite_random.h
 */

#ifndef _GLIBCXX_TESTSUITE_RANDOM_H
#define _GLIBCXX_TESTSUITE_RANDOM_H

#include <cmath>
#include <testsuite_hooks.h>

namespace __gnu_test
{
  // Adapted for libstdc++ from GNU gsl-1.14/randist/test.c
  // Copyright (C) 1996, 1997, 1998, 1999, 2000, 2007, 2010
  // James Theiler, Brian Gough
  template<unsigned long BINS = 100,
	   unsigned long N = 100000,
	   typename Distribution, typename Pdf>
    void
    testDiscreteDist(Distribution& f, Pdf pdf)
    {
      bool test __attribute__((unused)) = true;
      double count[BINS], p[BINS];

      for (unsigned long i = 0; i < BINS; i++)
	count[i] = 0;

      for (unsigned long i = 0; i < N; i++)
	{
	  auto r = f();
	  if (r >= 0 && r < BINS)
	    count[r]++;
	}

      for (unsigned long i = 0; i < BINS; i++)
	p[i] = pdf(i);

      for (unsigned long i = 0; i < BINS; i++)
	{
	  bool status_i;
	  double d = std::abs(count[i] - N * p[i]);

	  if (p[i] != 0)
	    {
	      double s = d / std::sqrt(N * p[i]);
	      status_i = (s > 5) && (d > 1);
	    }
	  else
	    status_i = (count[i] != 0);

	  VERIFY( !status_i );
	}
    }

  inline double
  bernoulli_pdf(int k, double p)
  {
    if (k == 0)
      return 1 - p;
    else if (k == 1)
      return p;
    else
      return 0;
  }

#ifdef _GLIBCXX_USE_C99_MATH_TR1
  inline double
  binomial_pdf(int k, double p, int n)
  {
    if (k < 0 || k > n)
      return 0;
    else
      {
	double q;

	if (p == 0) 
	  q = (k == 0) ? 1 : 0;
	else if (p == 1)
	  q = (k == n) ? 1 : 0;
	else
	  {
	    double ln_Cnk = (std::lgamma(n + 1) - std::lgamma(k + 1)
			     - std::lgamma(n - k + 1));
	    q = ln_Cnk + k * std::log(p) + (n - k) * std::log1p(-p);
	    q = std::exp(q);
	  }

	return q;
      }
  }
#endif

  inline double
  geometric_pdf(int k, double p)
  {
    if (k < 0)
      return 0;
    else if (k == 0)
      return p;
    else
      return p * std::pow(1 - p, k);
  }
} // namespace __gnu_test

#endif // #ifndef _GLIBCXX_TESTSUITE_RANDOM_H
