// The template and inlines for the -*- C++ -*- numeric_limits classes.

// Copyright (C) 2000 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

// Note: this is not a conforming implementation.
// Written by Gabriel Dos Reis <Gabriel.Dos-Reis@cmla.ens-cachan.fr>

//
// ISO 14882:1998
// 18.2.1
//

#ifndef _CPP_NUMERIC_LIMITS
#define _CPP_NUMERIC_LIMITS 1

#include <bits/c++config.h>
#include <bits/std_cfloat.h>
#include <bits/std_climits.h>
#if defined( _GLIBCPP_USE_WCHAR_T) && defined(_GLIBCPP_HAS_WCHAR_MIN_MAX)
#include <bits/std_cwchar.h>
#endif

namespace std {

    enum float_round_style {
        round_indeterminate       = -1,
        round_toward_zero         = 0,
        round_to_nearest          = 1,
        round_toward_infinity     = 2,
        round_toward_neg_infinity = 3
    };

    enum float_denorm_style {
        denorm_indeterminate = -1,
        denorm_absent        = 0,
        denorm_present       = 1
    };

    template<typename _T> struct numeric_limits {
        static const bool is_specialized = false;

        static _T min() throw() { return static_cast<_T>(0); }
        static _T max() throw() { return static_cast<_T>(0); }

        static const int digits = 0;
        static const int digits10 = 0;
        static const bool is_signed = false;
        static const bool is_integer = false;
        static const bool is_exact = false;
        static const int radix = 0;

        static _T epsilon() throw() { return static_cast<_T>(0); }
        static _T round_error() throw() { return static_cast<_T>(0); }

        static const int min_exponent = 0;
        static const int min_exponent10 = 0;
        static const int max_exponent = 0;
        static const int max_exponent10 = 0;

        static const bool has_infinity = false;
        static const bool has_quiet_NaN = false;
        static const bool has_signaling_NaN = false;
        static const float_denorm_style has_denorm = denorm_absent;
        static const bool has_denorm_loss = false;

        static _T infinity() throw()  { return static_cast<_T>(0); }
        static _T quiet_NaN() throw() { return static_cast<_T>(0); }
        static _T signaling_NaN() throw() { return static_cast<_T>(0); }
        static _T denorm_min() throw() { return static_cast<_T>(0); }

        static const bool is_iec559 = false;
        static const bool is_bounded = false;
        static const bool is_modulo = false;

        static const bool traps = false;
        static const bool tinyness_before = false;
        static const float_round_style round_style = round_toward_zero;
    };

    template<typename _T> _T __limits_infinity();
    template<typename _T> _T __limits_quiet_NaN();
    template<typename _T> _T __limits_signaling_NaN();
    template<typename _T> _T __limits_denorm_min();

    template<> struct numeric_limits<bool> {
        static const bool is_specialized = true;

        static bool min() throw()
        { return false; }
        static bool max() throw()
        { return true; }

        static const int digits = 8;
        static const int digits10 = 2;
        static const bool is_signed = false;
        static const bool is_integer = true;
        static const bool is_exact = true;
        static const int radix = 2;
        static bool epsilon() throw()
        { return 0; }
        static bool round_error() throw()
        { return 0; }

        static const int min_exponent = 0;
        static const int min_exponent10 = 0;
        static const int max_exponent = 0;
        static const int max_exponent10 = 0;

        static const bool has_infinity = false;
        static const bool has_quiet_NaN = false;
        static const bool has_signaling_NaN = false;
        static const float_denorm_style has_denorm = denorm_absent;
        static const bool has_denorm_loss = false;

        static bool infinity() throw()
        { return static_cast<bool>(0); }
        static bool quiet_NaN() throw()
        { return static_cast<bool>(0); }
        static bool signaling_NaN() throw()
        { return static_cast<bool>(0); }
        static bool denorm_min() throw()
        { return static_cast<bool>(0); }

        static const bool is_iec559 = true;
        static const bool is_bounded = true;
        static const bool is_modulo = true;

        static const bool traps = false;
        static const bool tinyness_before = false;
        static const float_round_style round_style = round_toward_zero;
    };

    template<> struct numeric_limits<char> {
        static const bool is_specialized = true;

        static char min() throw()
        { return CHAR_MIN; }
        static char max() throw()
        { return CHAR_MAX; }

        static const int digits = 7;
        static const int digits10 = 2;
        static const bool is_signed = true;
        static const bool is_integer = true;
        static const bool is_exact = true;
        static const int radix = 2;
        static char epsilon() throw()
        { return 0; }
        static char round_error() throw()
        { return 0; }

        static const int min_exponent = 0;
        static const int min_exponent10 = 0;
        static const int max_exponent = 0;
        static const int max_exponent10 = 0;

        static const bool has_infinity = false;
        static const bool has_quiet_NaN = false;
        static const bool has_signaling_NaN = false;
        static const float_denorm_style has_denorm = denorm_absent;
        static const bool has_denorm_loss = false;

        static char infinity() throw()
        { return static_cast<char>(0); }
        static char quiet_NaN() throw()
        { return static_cast<char>(0); }
        static char signaling_NaN() throw()
        { return static_cast<char>(0); }
        static char denorm_min() throw()
        { return static_cast<char>(0); }

        static const bool is_iec559 = false;
        static const bool is_bounded = true;
        static const bool is_modulo = false;

        static const bool traps = false;
        static const bool tinyness_before = false;
        static const float_round_style round_style = round_toward_zero;
    };

    template<> struct numeric_limits<signed char> {
        static const bool is_specialized = true;

        static signed char min() throw()
        { return SCHAR_MIN; }
        static signed char max() throw()
        { return SCHAR_MAX; }

        static const int digits = 7;
        static const int digits10 = 2;
        static const bool is_signed = true;
        static const bool is_integer = true;
        static const bool is_exact = true;
        static const int radix = 2;
        static signed char epsilon() throw()
        { return 0; }
        static signed char round_error() throw()
        { return 0; }

        static const int min_exponent = 0;
        static const int min_exponent10 = 0;
        static const int max_exponent = 0;
        static const int max_exponent10 = 0;

        static const bool has_infinity = false;
        static const bool has_quiet_NaN = false;
        static const bool has_signaling_NaN = false;
        static const float_denorm_style has_denorm = denorm_absent;
        static const bool has_denorm_loss = false;

        static signed char infinity() throw()
        { return static_cast<signed char>(0); }
        static signed char quiet_NaN() throw()
        { return static_cast<signed char>(0); }
        static signed char signaling_NaN() throw()
        { return static_cast<signed char>(0); }
        static signed char denorm_min() throw()
        { return static_cast<signed char>(0); }

        static const bool is_iec559 = false;
        static const bool is_bounded = true;
        static const bool is_modulo = false;

        static const bool traps = false;
        static const bool tinyness_before = false;
        static const float_round_style round_style = round_toward_zero;
    };

    template<> struct numeric_limits<unsigned char> {
        static const bool is_specialized = true;

        static unsigned char min() throw()
        { return 0; }
        static unsigned char max() throw()
        { return UCHAR_MAX; }

        static const int digits = 8;
        static const int digits10 = 2;
        static const bool is_signed = false;
        static const bool is_integer = true;
        static const bool is_exact = true;
        static const int radix = 2;
        static unsigned char epsilon() throw()
        { return 0; }
        static unsigned char round_error() throw()
        { return 0; }

        static const int min_exponent = 0;
        static const int min_exponent10 = 0;
        static const int max_exponent = 0;
        static const int max_exponent10 = 0;

        static const bool has_infinity = false;
        static const bool has_quiet_NaN = false;
        static const bool has_signaling_NaN = false;
        static const float_denorm_style has_denorm = denorm_absent;
        static const bool has_denorm_loss = false;

        static unsigned char infinity() throw()
        { return static_cast<unsigned char>(0); }
        static unsigned char quiet_NaN() throw()
        { return static_cast<unsigned char>(0); }
        static unsigned char signaling_NaN() throw()
        { return static_cast<unsigned char>(0); }
        static unsigned char denorm_min() throw()
        { return static_cast<unsigned char>(0); }

        static const bool is_iec559 = false;
        static const bool is_bounded = true;
        static const bool is_modulo = true;

        static const bool traps = true;
        static const bool tinyness_before = false;
        static const float_round_style round_style = round_toward_zero;
    };

#if defined( _GLIBCPP_USE_WCHAR_T) && defined(_GLIBCPP_HAS_WCHAR_MIN_MAX)
    template<> struct numeric_limits<wchar_t> {
        static const bool is_specialized = true;

        static wchar_t min() throw()
        { return WCHART_MIN; }
        static wchar_t max() throw()
        { return WCHART_MAX; }

        static const int digits = 31;
        static const int digits10 = 9;
        static const bool is_signed = true;
        static const bool is_integer = true;
        static const bool is_exact = true;
        static const int radix = 2;
        static wchar_t epsilon() throw()
        { return 0; }
        static wchar_t round_error() throw()
        { return 0; }

        static const int min_exponent = 0;
        static const int min_exponent10 = 0;
        static const int max_exponent = 0;
        static const int max_exponent10 = 0;

        static const bool has_infinity = false;
        static const bool has_quiet_NaN = false;
        static const bool has_signaling_NaN = false;
        static const float_denorm_style has_denorm = denorm_absent;
        static const bool has_denorm_loss = false;

        static wchar_t infinity() throw()
        { return static_cast<wchar_t>(0); }
        static wchar_t quiet_NaN() throw()
        { return static_cast<wchar_t>(0); }
        static wchar_t signaling_NaN() throw()
        { return static_cast<wchar_t>(0); }
        static wchar_t denorm_min() throw()
        { return static_cast<wchar_t>(0); }

        static const bool is_iec559 = false;
        static const bool is_bounded = true;
        static const bool is_modulo = false;

        static const bool traps = false;
        static const bool tinyness_before = false;
        static const float_round_style round_style = round_toward_zero;
    };
#endif

    template<> struct numeric_limits<short> {
        static const bool is_specialized = true;

        static short min() throw()
        { return SHRT_MIN; }
        static short max() throw()
        { return SHRT_MAX; }

        static const int digits = 15;
        static const int digits10 = 4;
        static const bool is_signed = true;
        static const bool is_integer = true;
        static const bool is_exact = true;
        static const int radix = 2;
        static short epsilon() throw()
        { return 0; }
        static short round_error() throw()
        { return 0; }

        static const int min_exponent = 0;
        static const int min_exponent10 = 0;
        static const int max_exponent = 0;
        static const int max_exponent10 = 0;

        static const bool has_infinity = false;
        static const bool has_quiet_NaN = false;
        static const bool has_signaling_NaN = false;
        static const float_denorm_style has_denorm = denorm_absent;
        static const bool has_denorm_loss = false;

        static short infinity() throw()
        { return static_cast<short>(0); }
        static short quiet_NaN() throw()
        { return static_cast<short>(0); }
        static short signaling_NaN() throw()
        { return static_cast<short>(0); }
        static short denorm_min() throw()
        { return static_cast<short>(0); }

        static const bool is_iec559 = false;
        static const bool is_bounded = true;
        static const bool is_modulo = false;

        static const bool traps = false;
        static const bool tinyness_before = false;
        static const float_round_style round_style = round_toward_zero;
    };

    template<> struct numeric_limits<unsigned short> {
        static const bool is_specialized = true;

        static unsigned short min() throw()
        { return 0; }
        static unsigned short max() throw()
        { return USHRT_MAX; }

        static const int digits = 16;
        static const int digits10 = 4;
        static const bool is_signed = false;
        static const bool is_integer = true;
        static const bool is_exact = true;
        static const int radix = 2;
        static unsigned short epsilon() throw()
        { return 0; }
        static unsigned short round_error() throw()
        { return 0; }

        static const int min_exponent = 0;
        static const int min_exponent10 = 0;
        static const int max_exponent = 0;
        static const int max_exponent10 = 0;

        static const bool has_infinity = false;
        static const bool has_quiet_NaN = false;
        static const bool has_signaling_NaN = false;
        static const float_denorm_style has_denorm = denorm_absent;
        static const bool has_denorm_loss = false;

        static unsigned short infinity() throw()
        { return static_cast<unsigned short>(0); }
        static unsigned short quiet_NaN() throw()
        { return static_cast<unsigned short>(0); }
        static unsigned short signaling_NaN() throw()
        { return static_cast<unsigned short>(0); }
        static unsigned short denorm_min() throw()
        { return static_cast<unsigned short>(0); }

        static const bool is_iec559 = false;
        static const bool is_bounded = true;
        static const bool is_modulo = true;

        static const bool traps = true;
        static const bool tinyness_before = false;
        static const float_round_style round_style = round_toward_zero;
    };

    template<> struct numeric_limits<int> {
        static const bool is_specialized = true;

        static int min() throw()
        { return INT_MIN; }
        static int max() throw()
        { return INT_MAX; }

        static const int digits = 31;
        static const int digits10 = 9;
        static const bool is_signed = true;
        static const bool is_integer = true;
        static const bool is_exact = true;
        static const int radix = 2;
        static int epsilon() throw()
        { return 0; }
        static int round_error() throw()
        { return 0; }

        static const int min_exponent = 0;
        static const int min_exponent10 = 0;
        static const int max_exponent = 0;
        static const int max_exponent10 = 0;

        static const bool has_infinity = false;
        static const bool has_quiet_NaN = false;
        static const bool has_signaling_NaN = false;
        static const float_denorm_style has_denorm = denorm_absent;
        static const bool has_denorm_loss = false;

        static int infinity() throw()
        { return static_cast<int>(0); }
        static int quiet_NaN() throw()
        { return static_cast<int>(0); }
        static int signaling_NaN() throw()
        { return static_cast<int>(0); }
        static int denorm_min() throw()
        { return static_cast<int>(0); }

        static const bool is_iec559 = true;
        static const bool is_bounded = true;
        static const bool is_modulo = false;

        static const bool traps = false;
        static const bool tinyness_before = false;
        static const float_round_style round_style = round_toward_zero;
    };

    template<> struct numeric_limits<unsigned int> {
        static const bool is_specialized = true;

        static unsigned int min() throw()
        { return 0; }
        static unsigned int max() throw()
        { return UINT_MAX; }

        static const int digits = 32;
        static const int digits10 = 9;
        static const bool is_signed = false;
        static const bool is_integer = true;
        static const bool is_exact = true;
        static const int radix = 2;
        static unsigned int epsilon() throw()
        { return 0; }
        static unsigned int round_error() throw()
        { return 0; }

        static const int min_exponent = 0;
        static const int min_exponent10 = 0;
        static const int max_exponent = 0;
        static const int max_exponent10 = 0;

        static const bool has_infinity = false;
        static const bool has_quiet_NaN = false;
        static const bool has_signaling_NaN = false;
        static const float_denorm_style has_denorm = denorm_absent;
        static const bool has_denorm_loss = false;

        static unsigned int infinity() throw()
        { return static_cast<unsigned int>(0); }
        static unsigned int quiet_NaN() throw()
        { return static_cast<unsigned int>(0); }
        static unsigned int signaling_NaN() throw()
        { return static_cast<unsigned int>(0); }
        static unsigned int denorm_min() throw()
        { return static_cast<unsigned int>(0); }

        static const bool is_iec559 = true;
        static const bool is_bounded = true;
        static const bool is_modulo = true;

        static const bool traps = true;
        static const bool tinyness_before = false;
        static const float_round_style round_style = round_toward_zero;
    };

    template<> struct numeric_limits<long> {
        static const bool is_specialized = true;

        static long min() throw()
        { return LONG_MIN; }
        static long max() throw()
        { return LONG_MAX; }

        static const int digits = 31;
        static const int digits10 = 9;
        static const bool is_signed = true;
        static const bool is_integer = true;
        static const bool is_exact = true;
        static const int radix = 2;
        static long epsilon() throw()
        { return 0; }
        static long round_error() throw()
        { return 0; }

        static const int min_exponent = 0;
        static const int min_exponent10 = 0;
        static const int max_exponent = 0;
        static const int max_exponent10 = 0;

        static const bool has_infinity = false;
        static const bool has_quiet_NaN = false;
        static const bool has_signaling_NaN = false;
        static const float_denorm_style has_denorm = denorm_absent;
        static const bool has_denorm_loss = false;

        static long infinity() throw()
        { return static_cast<long>(0); }
        static long quiet_NaN() throw()
        { return static_cast<long>(0); }
        static long signaling_NaN() throw()
        { return static_cast<long>(0); }
        static long denorm_min() throw()
        { return static_cast<long>(0); }

        static const bool is_iec559 = true;
        static const bool is_bounded = true;
        static const bool is_modulo = false;

        static const bool traps = false;
        static const bool tinyness_before = false;
        static const float_round_style round_style = round_toward_zero;
    };

    template<> struct numeric_limits<unsigned long> {
        static const bool is_specialized = true;

        static unsigned long min() throw()
        { return 0; }
        static unsigned long max() throw()
        { return ULONG_MAX; }

        static const int digits = 32;
        static const int digits10 = 9;
        static const bool is_signed = false;
        static const bool is_integer = true;
        static const bool is_exact = true;
        static const int radix = 2;
        static unsigned long epsilon() throw()
        { return 0; }
        static unsigned long round_error() throw()
        { return 0; }

        static const int min_exponent = 0;
        static const int min_exponent10 = 0;
        static const int max_exponent = 0;
        static const int max_exponent10 = 0;

        static const bool has_infinity = false;
        static const bool has_quiet_NaN = false;
        static const bool has_signaling_NaN = false;
        static const float_denorm_style has_denorm = denorm_absent;
        static const bool has_denorm_loss = false;

        static unsigned long infinity() throw()
        { return static_cast<unsigned long>(0); }
        static unsigned long quiet_NaN() throw()
        { return static_cast<unsigned long>(0); }
        static unsigned long signaling_NaN() throw()
        { return static_cast<unsigned long>(0); }
        static unsigned long denorm_min() throw()
        { return static_cast<unsigned long>(0); }

        static const bool is_iec559 = true;
        static const bool is_bounded = true;
        static const bool is_modulo = true;

        static const bool traps = true;
        static const bool tinyness_before = false;
        static const float_round_style round_style = round_toward_zero;
    };

    template<> struct numeric_limits<float> {
        static const bool is_specialized = true;

        static float min() throw()
        { return FLT_MIN; }
        static float max() throw()
        { return FLT_MAX; }

        static const int digits = FLT_MANT_DIG;
        static const int digits10 = FLT_DIG;
        static const bool is_signed = true;
        static const bool is_integer = false;
        static const bool is_exact = false;
        static const int radix = FLT_RADIX;
        static float epsilon() throw()
        { return FLT_EPSILON; }
        static float round_error() throw()
        { return FLT_ROUNDS; }

        static const int min_exponent = FLT_MIN_EXP;
        static const int min_exponent10 = FLT_MIN_10_EXP;
        static const int max_exponent = FLT_MAX_EXP;
        static const int max_exponent10 = FLT_MAX_10_EXP;

        static const bool has_infinity = false;
        static const bool has_quiet_NaN = false;
        static const bool has_signaling_NaN = false;
        static const float_denorm_style has_denorm = denorm_absent;
        static const bool has_denorm_loss = false;

        static float infinity() throw()
        { return static_cast<float>(0); }
        static float quiet_NaN() throw()
        { return static_cast<float>(0); }
        static float signaling_NaN() throw()
        { return static_cast<float>(0); }
        static float denorm_min() throw()
        { return static_cast<float>(0); }

        static const bool is_iec559 = false;
        static const bool is_bounded = true;
        static const bool is_modulo = false;

        static const bool traps = false;
        static const bool tinyness_before = false;
        static const float_round_style round_style = round_toward_zero;
    };

    template<> struct numeric_limits<double> {
        static const bool is_specialized = true;

        static double min() throw()
        { return DBL_MIN; }
        static double max() throw()
        { return DBL_MAX; }

        static const int digits = DBL_MANT_DIG;
        static const int digits10 = DBL_DIG;
        static const bool is_signed = true;
        static const bool is_integer = false;
        static const bool is_exact = false;
        static const int radix = 2;
        static double epsilon() throw()
        { return DBL_EPSILON; }
        static double round_error() throw()
        { return 1.0; }

        static const int min_exponent = DBL_MIN_EXP;
        static const int min_exponent10 = DBL_MIN_10_EXP;
        static const int max_exponent = DBL_MAX_EXP;
        static const int max_exponent10 = DBL_MAX_10_EXP;

        static const bool has_infinity = false;
        static const bool has_quiet_NaN = false;
        static const bool has_signaling_NaN = false;
        static const float_denorm_style has_denorm = denorm_absent;
        static const bool has_denorm_loss = false;

        static double infinity() throw()
        { return static_cast<double>(0); }
        static double quiet_NaN() throw()
        { return static_cast<double>(0); }
        static double signaling_NaN() throw()
        { return static_cast<double>(0); }
        static double denorm_min() throw()
        { return static_cast<double>(0); }

        static const bool is_iec559 = false;
        static const bool is_bounded = true;
        static const bool is_modulo = false;

        static const bool traps = false;
        static const bool tinyness_before = false;
        static const float_round_style round_style = round_toward_zero;
    };

    template<> struct numeric_limits<long double> {
        static const bool is_specialized = true;

        static double min() throw()
        { return LDBL_MIN; }
        static double max() throw()
        { return LDBL_MAX; }

        static const int digits = LDBL_MANT_DIG;
        static const int digits10 = LDBL_DIG;
        static const bool is_signed = true;
        static const bool is_integer = false;
        static const bool is_exact = false;
        static const int radix = 2;
        static double epsilon() throw()
        { return LDBL_EPSILON; }
        static double round_error() throw()
        { return 1.0L; }

        static const int min_exponent = LDBL_MIN_EXP;
        static const int min_exponent10 = LDBL_MIN_10_EXP;
        static const int max_exponent = LDBL_MAX_EXP;
        static const int max_exponent10 = LDBL_MAX_10_EXP;

        static const bool has_infinity = false;
        static const bool has_quiet_NaN = false;
        static const bool has_signaling_NaN = false;
        static const float_denorm_style has_denorm = denorm_absent;
        static const bool has_denorm_loss = false;

        static double infinity() throw()
        { return static_cast<double>(0); }
        static double quiet_NaN() throw()
        { return static_cast<double>(0); }
        static double signaling_NaN() throw()
        { return static_cast<double>(0); }
        static double denorm_min() throw()
        { return static_cast<double>(0); }

        static const bool is_iec559 = false;
        static const bool is_bounded = true;
        static const bool is_modulo = false;

        static const bool traps = false;
        static const bool tinyness_before = false;
        static const float_round_style round_style = round_toward_zero;
    };

} // namespace std

#endif // _CPP_NUMERIC_LIMITS
