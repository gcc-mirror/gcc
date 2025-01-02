// -*- C++ -*-

// Copyright (C) 2011-2025 Free Software Foundation, Inc.
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
#include <initializer_list>
#include <system_error>
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
      double count[BINS], p[BINS];

      for (unsigned long i = 0; i < BINS; i++)
	count[i] = 0;

      for (unsigned long i = 0; i < N; i++)
	{
	  auto r = f();
	  if (r >= 0 && (unsigned long)r < BINS)
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
      return 0.0;
  }

#ifdef _GLIBCXX_USE_C99_MATH_FUNCS
  inline double
  binomial_pdf(int k, int n, double p)
  {
    if (k < 0 || k > n)
      return 0.0;
    else
      {
	double q;

	if (p == 0.0)
	  q = (k == 0) ? 1.0 : 0.0;
	else if (p == 1.0)
	  q = (k == n) ? 1.0 : 0.0;
	else
	  {
	    double ln_Cnk = (std::lgamma(n + 1.0) - std::lgamma(k + 1.0)
			     - std::lgamma(n - k + 1.0));
	    q = ln_Cnk + k * std::log(p) + (n - k) * std::log1p(-p);
	    q = std::exp(q);
	  }

	return q;
      }
  }
#endif

  inline double
  discrete_pdf(int k, std::initializer_list<double> wl)
  {
    if (!wl.size())
      {
	static std::initializer_list<double> one = { 1.0 };
	wl = one;
      }

    if (k < 0 || (std::size_t)k >= wl.size())
      return 0.0;
    else
      {
	double sum = 0.0;
	for (auto it = wl.begin(); it != wl.end(); ++it)
	  sum += *it;
	return wl.begin()[k] / sum;
      }
  }

  inline double
  geometric_pdf(int k, double p)
  {
    if (k < 0)
      return 0.0;
    else if (k == 0)
      return p;
    else
      return p * std::pow(1 - p, k);
  }

#ifdef _GLIBCXX_USE_C99_MATH_FUNCS
  inline double
  negative_binomial_pdf(int k, int n, double p)
  {
    if (k < 0)
      return 0.0;
    else
      {
	double f = std::lgamma(k + (double)n);
	double a = std::lgamma(n);
	double b = std::lgamma(k + 1.0);

	return std::exp(f - a - b) * std::pow(p, n) * std::pow(1 - p, k);
      }
  }

  inline double
  poisson_pdf(int k, double mu)
  {
    if (k < 0)
      return 0.0;
    else
      {
	double lf = std::lgamma(k + 1.0);
	return std::exp(std::log(mu) * k - lf - mu);
      }
  }
#endif

  inline double
  uniform_int_pdf(int k, int a, int b)
  {
    if (k < 0 || k < a || k > b)
      return 0.0;
    else
      return 1.0 / (b - a + 1.0);
  }

#ifdef _GLIBCXX_USE_C99_MATH_FUNCS
  inline double
  lbincoef(int n, int k)
  {
    return std::lgamma(double(1 + n))
         - std::lgamma(double(1 + k))
         - std::lgamma(double(1 + n - k));
  }

  inline double
  hypergeometric_pdf(int k, int N, int K, int n)
  {
    if (k < 0 || k < std::max(0, n - (N - K)) || k > std::min(K, n))
      return 0.0;
    else
      return lbincoef(K, k) + lbincoef(N - K, n - k) - lbincoef(N, n);
  }
#endif

  // Check whether TOKEN can construct a std::random_device successfully.
  inline bool
  random_device_available(const std::string& token) noexcept
  {
    try {
      std::random_device dev(token);
      return true;
    } catch (const std::system_error& /* See PR libstdc++/105081 */) {
      return false;
    }
  }

} // namespace __gnu_test

#endif // #ifndef _GLIBCXX_TESTSUITE_RANDOM_H
