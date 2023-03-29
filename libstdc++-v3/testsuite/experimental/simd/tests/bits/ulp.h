// Copyright (C) 2020-2023 Free Software Foundation, Inc.
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

#ifndef ULP_H
#define ULP_H

#include <cmath>
#include <experimental/simd>
#include <type_traits>
#include <cfenv>

namespace vir {
  namespace test {
    template <typename T, typename R = typename T::value_type>
      R
      value_type_impl(int);

    template <typename T>
      T
      value_type_impl(float);

    template <typename T>
      using value_type_t = decltype(value_type_impl<T>(int()));

    template <typename T>
      inline T
      ulp_distance(const T& val_, const T& ref_)
      {
        if constexpr (std::is_floating_point_v<value_type_t<T>>)
          {
            const int fp_exceptions = std::fetestexcept(FE_ALL_EXCEPT);
            T val = val_;
            T ref = ref_;

            T diff = T();

            using std::abs;
            using std::fpclassify;
            using std::frexp;
            using std::isnan;
            using std::isinf;
            using std::ldexp;
            using std::max;
            using std::experimental::where;
            using TT = value_type_t<T>;

            where(ref == 0, val) = abs(val);
            where(ref == 0, diff) = 1;
            where(ref == 0, ref) = std::__norm_min_v<TT>;
            where(isinf(ref) && ref == val, ref)
              = 0; // where(val_ == ref_) = 0 below will fix it up

            where(val == 0, ref) = abs(ref);
            where(val == 0, diff) += 1;
            where(val == 0, val) = std::__norm_min_v<TT>;

            using I = decltype(fpclassify(std::declval<T>()));
            I exp = {};
            frexp(ref, &exp);
            // lower bound for exp must be min_exponent to scale the resulting
            // difference from a denormal correctly
            exp = max(exp, I(std::__min_exponent_v<TT>));
            diff += ldexp(abs(ref - val), std::__digits_v<TT> - exp);
            where(val_ == ref_ || (isnan(val_) && isnan(ref_)), diff) = T();
            std::feclearexcept(FE_ALL_EXCEPT ^ fp_exceptions);
            return diff;
          }
        else
          {
            if (val_ > ref_)
              return val_ - ref_;
            else
              return ref_ - val_;
          }
      }

    template <typename T>
      inline T
      ulp_distance_signed(const T& _val, const T& _ref)
      {
        using std::copysign;
        return copysign(ulp_distance(_val, _ref), _val - _ref);
      }
  } // namespace test
} // namespace vir

#endif // ULP_H
