// Copyright (C) 1999, 2000 Free Software Foundation, Inc.
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

//
// Written by Gabriel Dos Reis <gdr@egcs.cygnus.com>
// 
// Note: This program outputs speciliazations of ISO C++ class template
// numeric_limits<> as described in 18.2.1.
// Do not compile with optimization turned on.
//

#include <bits/c++config.h>

//
// Force Linux <limits.h> to define the *LONG_LONG*
//
#if __linux__ && _GLIBCPP_USE_LONG_LONG
# ifndef __USE_GNU
#  define __USE_GNU 1
# endif
# ifndef _GNU_SOURCE
#  define _GNU_SOURCE 1
# endif
#endif

#include <limits.h>
#include <float.h>
#include <stdio.h>
#include <signal.h>
#include <setjmp.h>
#include <math.h>
#ifdef _GLIBCPP_USE_WCHAR_T
#include <wchar.h>
#endif


const char tab[] = "    ";
const char tab2[] = "        ";
const char* bool_alpha[] = { "false", "true" };
const double log10_of_two = .30102999566398119;
const int bits_per_byte = CHAR_BIT;
const int integer_base_rep = 2;


//
// numeric_limits members are all static (as it is usually the case for
// traits) and of three kinds: predicates, values and functions.
// Actually there is no harm to think of values and functions as being
// of the same kind.  Their main purposes are to denote values.
//


//
// Integer types: bool, char, signed char, unsigned char, wchar_t,
// short, unsigned short, int, unsigned, long, unsigned long,
// and possibly long long and unsigned long long
//
// Here ISO 14882 disagrees with LIA-1 in stating bool to be an
// integer type.  Therefore itn't suprising to see ambiguity in the
// interpretation of some members.  Need to keep track of the discusion
// in LWG on this topic.
//
// Integer types are first promoted to int or long before the actual
// arithmetical operations are carried out.  Therefore testing whether
// traps occur amounts -- for integer types -- to test whether traps
// occur for int, unsigned, long, unsigned long. Furthermore
// overflow cannot happen for unsigned integer types.

jmp_buf env;

void signal_handler(int sig) 
{ 
#ifdef __CYGWIN__
  static sigset_t x;
  signal (sig, signal_handler);
  sigemptyset (&x);
  sigprocmask(SIG_SETMASK, &x, NULL);
#endif /* __CYGWIN__ */
  longjmp(env, sig); 
}

template<typename Operation>
bool trapping(const Operation& op)
{
    if (setjmp(env) == 0) op();
    else return true;
    return false;
}

template<typename T> struct division_by_zero {
    void operator() () const
    {
        volatile T zero = T();
        volatile T one = T(1);
        volatile T infinity = one / zero;
    }
};

template<typename T> struct overflow {
    void operator() () const
    {
        T i = T(1);
        T j = T();
        while (i>j) {
            j = i;
            i = i * 2 + 1;
        }
    }
};

template<typename T> struct underflow {};

// traps
template<typename T> void traps()
{
    signal(SIGFPE, signal_handler);
    bool trap_flag = trapping(division_by_zero<T>());
    signal(SIGFPE, signal_handler);
    trap_flag = trap_flag && trapping(overflow<T>());
    const char* p = bool_alpha[trap_flag];
    printf("%s%s = %s;\n", tab2, "static const bool traps", p);    
}

#define SPECIALIZE_TRAPPING(T)                                          \
template<> void traps< T >()                                            \
{                                                                       \
    signal(SIGFPE, signal_handler);                                     \
    const char* p = bool_alpha[trapping(division_by_zero<T>())];        \
    printf("%s%s = %s;\n", tab2, "static const bool traps", p);         \
}

SPECIALIZE_TRAPPING(unsigned char);
SPECIALIZE_TRAPPING(unsigned short);
SPECIALIZE_TRAPPING(unsigned int);
SPECIALIZE_TRAPPING(unsigned long);
#if _GLIBCPP_USE_LONG_LONG
SPECIALIZE_TRAPPING(unsigned long long);
#endif

#undef SPECIALIZE_TRAPPING

template<typename T> struct type_name_trait {
    static const char type_name[];
    static const char trait_name[];
};

#define DEFINED_TYPE_NAME(T)                                            \
const char type_name_trait< T >::type_name[] = #T;                      \
const char type_name_trait< T >::trait_name[] = "numeric_limits<" #T ">";

DEFINED_TYPE_NAME(bool);
DEFINED_TYPE_NAME(char);
DEFINED_TYPE_NAME(signed char);
DEFINED_TYPE_NAME(unsigned char);
DEFINED_TYPE_NAME(wchar_t);
DEFINED_TYPE_NAME(short);
DEFINED_TYPE_NAME(unsigned short);
DEFINED_TYPE_NAME(int);
DEFINED_TYPE_NAME(unsigned int);
DEFINED_TYPE_NAME(long);
DEFINED_TYPE_NAME(unsigned long);
#ifdef _GLIBCPP_USE_LONG_LONG
DEFINED_TYPE_NAME(long long);
DEFINED_TYPE_NAME(unsigned long long);
#endif
DEFINED_TYPE_NAME(float);
DEFINED_TYPE_NAME(double);
DEFINED_TYPE_NAME(long double);

#undef DEFINED_TYPE_NAME

// declarator
template<typename T> struct declarator : type_name_trait<T> {
    typedef type_name_trait<T> base;
    static void start()
    {
        printf("%s%s %s %s\n", tab, "template<> struct",
               base::trait_name, "{");
    }

    static void end()
    {
        printf("%s};\n\n", tab);
    }
};


//
// Predicates
// 
template<typename T> struct predicate {
    static const bool is_signed;
    static const bool is_integer;
    static const bool is_exact;

    static const bool has_infinity;
    static const bool has_quiet_nan;
    static const bool has_signaling_nan;
    static const bool has_denorm;
    static const bool has_denorm_loss;

    static const bool is_iec559;
    static const bool is_bounded;

    static const bool traps;
};

template<typename T>
const bool predicate<T>::is_signed = T(-1) < 0;

// Non integer types should specialize this
template<typename T>
const bool predicate<T>::is_integer = true;

// Non exact types should specialize this;
template<typename T>
const bool predicate<T>::is_exact = true;

#define SPECIALIZE_EXACTNESS(T)						\
const bool predicate< T >::is_integer = false;				\
const bool predicate< T >::is_exact = false

SPECIALIZE_EXACTNESS(float);
SPECIALIZE_EXACTNESS(double);
SPECIALIZE_EXACTNESS(long double);

#undef SPECIALIZE_EXACTNESS


template<typename T>
const bool predicate<T>::has_infinity = false;

template<typename T>
const bool predicate<T>::has_quiet_nan = false;

template<typename T>
const bool predicate<T>::has_signaling_nan = false;

template<typename T>
const bool predicate<T>::has_denorm = false;

template<typename T>
const bool predicate<T>::has_denorm_loss = false;



// Each type conforming to IEC559 specifications should specialize this.
template<typename T>
const bool predicate<T>::is_iec559 = false;

#define SPECIALIZE_IEC559(T)						\
const bool predicate< T >::is_iec559 = true

SPECIALIZE_IEC559(bool);
SPECIALIZE_IEC559(int);
SPECIALIZE_IEC559(unsigned int);
SPECIALIZE_IEC559(long);
SPECIALIZE_IEC559(unsigned long);
#ifdef _GLIBCPP_USE_LONG_LONG
SPECIALIZE_IEC559(long long);
SPECIALIZE_IEC559(unsigned long long);
#endif

#undef SPECIALIZE_IEC559

//
// Values
// 

template<typename T> struct value {
    static const char min[];
    static const char max[];

    static const int digits;
    static const int digits10;
    
    static const int radix;
    static const char epsilon[];
    static const char round_error[];

    static const int min_exponent;
    static const int min_exponent10;
    static const int max_exponent;
    static const int max_exponent10;
};

#define DEFINE_EXTREMA(T, m, M)  DO_DEFINE_EXTREMA(T, m, M)
#define DO_DEFINE_EXTREMA(T, m, M)					\
const char value< T >::min[] = #m;					\
const char value< T >::max[] = #M

DEFINE_EXTREMA(bool, false, true);
DEFINE_EXTREMA(char, CHAR_MIN, CHAR_MAX);
DEFINE_EXTREMA(signed char, SCHAR_MIN, SCHAR_MAX);
DEFINE_EXTREMA(unsigned char, 0, UCHAR_MAX);
#ifdef _GLIBCPP_USE_WCHAR_T
DEFINE_EXTREMA(wchar_t, WCHAR_MIN, WCHAR_MAX);
#endif
DEFINE_EXTREMA(short, SHRT_MIN, SHRT_MAX);
DEFINE_EXTREMA(unsigned short, 0, USHRT_MAX);
DEFINE_EXTREMA(int, INT_MIN, INT_MAX);
DEFINE_EXTREMA(unsigned int, 0, UINT_MAX);
DEFINE_EXTREMA(long, LONG_MIN, LONG_MAX);
DEFINE_EXTREMA(unsigned long, 0, ULONG_MAX);
#ifdef _GLIBCPP_USE_LONG_LONG
DEFINE_EXTREMA(long long, LONG_LONG_MIN, LONG_LONG_MAX);
DEFINE_EXTREMA(unsigned long long, 0, ULONG_LONG_MAX);
#endif
DEFINE_EXTREMA(float, FLT_MIN, FLT_MAX);
DEFINE_EXTREMA(double, DBL_MIN, DBL_MAX);
DEFINE_EXTREMA(long double, LDBL_MIN, LDBL_MAX);

#undef DEFINE_EXTREMA
#undef DO_DEFINE_EXTREMA

// Non integer types should specialize this
template<typename T>
const int value<T>::digits =
      bits_per_byte * sizeof(T) - int(predicate<T>::is_signed);

// Non integer types should specialize this.  Always two for
// integer types.
template<typename T>
const int value<T>::radix = 2;

#define SPECIALIZE_DIGITS(T, D, D10)					\
const int value< T >::digits = D;					\
const int value< T >::digits10 = D10

SPECIALIZE_DIGITS(float, FLT_MANT_DIG, FLT_DIG);
SPECIALIZE_DIGITS(double, DBL_MANT_DIG, DBL_DIG);
SPECIALIZE_DIGITS(long double, LDBL_MANT_DIG, LDBL_DIG);

#undef SPECIALIZE_DIGITS


#define SPECIALIZE_RADIX(T, R) const int value< T >::radix = R

SPECIALIZE_RADIX(float, FLT_RADIX);
SPECIALIZE_RADIX(double, FLT_RADIX);
SPECIALIZE_RADIX(long double, FLT_RADIX);

#undef SPECIALIZE_RADIX

// Non integer types should specialize this.  
// Unfortunately, systems that don't deal with weak linking correctly
// (Ie, hpux and aix), cannot use this sophisticated yet sane method. So,
// explicitly instantiate all the data members here so that they will
// be happy.

// sophisticated, sane method
#if 0
template<typename T>
const char value<T>::epsilon[] = "0";
#endif

#define SPECIALIZE_EPSILON(T, E) DO_SPECIALIZE_EPSILON(T, E)
#define DO_SPECIALIZE_EPSILON(T, E) const char value< T >::epsilon[] = #E

// unsophisticated, gross method
#if 1
SPECIALIZE_EPSILON(bool, 0);
SPECIALIZE_EPSILON(char, 0);
SPECIALIZE_EPSILON(unsigned char, 0);
SPECIALIZE_EPSILON(signed char, 0);
SPECIALIZE_EPSILON(wchar_t, 0);
SPECIALIZE_EPSILON(short, 0);
SPECIALIZE_EPSILON(unsigned short, 0);
SPECIALIZE_EPSILON(int, 0);
SPECIALIZE_EPSILON(unsigned int, 0);
SPECIALIZE_EPSILON(long, 0);
SPECIALIZE_EPSILON(unsigned long, 0);
SPECIALIZE_EPSILON(long long, 0);
SPECIALIZE_EPSILON(unsigned long long, 0);
#endif

SPECIALIZE_EPSILON(float, FLT_EPSILON);
SPECIALIZE_EPSILON(double, DBL_EPSILON);
SPECIALIZE_EPSILON(long double, LDBL_EPSILON);

#undef DO_SPECIALIZE_EPSILON
#undef SPECIALIZE_EPSILON


// Non integer types should specialize this.  
// Unfortunately, systems that don't deal with weak linking correctly
// (Ie, hpux and aix), cannot use this sophisticated yet sane method. So,
// explicitly instantiate all the data members here so that they will
// be happy.

// sophisticated, sane method
#if 0
template<typename T>
const char value<T>::round_error[] = "0";
#endif

#define SPECIALIZE_ROUND_ERROR(T, R) const char value< T >::round_error[] = #R
// unsophisticated, gross method
#if 1
SPECIALIZE_ROUND_ERROR(bool, 0);
SPECIALIZE_ROUND_ERROR(char, 0);
SPECIALIZE_ROUND_ERROR(unsigned char, 0);
SPECIALIZE_ROUND_ERROR(signed char, 0);
SPECIALIZE_ROUND_ERROR(wchar_t, 0);
SPECIALIZE_ROUND_ERROR(short, 0);
SPECIALIZE_ROUND_ERROR(unsigned short, 0);
SPECIALIZE_ROUND_ERROR(int, 0);
SPECIALIZE_ROUND_ERROR(unsigned int, 0);
SPECIALIZE_ROUND_ERROR(long, 0);
SPECIALIZE_ROUND_ERROR(unsigned long, 0);
SPECIALIZE_ROUND_ERROR(long long, 0);
SPECIALIZE_ROUND_ERROR(unsigned long long, 0);
#endif

SPECIALIZE_ROUND_ERROR(float, 1.0f);
SPECIALIZE_ROUND_ERROR(double, 1.0);
SPECIALIZE_ROUND_ERROR(long double, 1.0L);

#undef SPECIALIZE_ROUND_ERROR


template<typename T>
const int value<T>::min_exponent = 0;

template<typename T>
const int value<T>::min_exponent10 = 0;

template<typename T>
const int value<T>::max_exponent = 0;

template<typename T>
const int value<T>::max_exponent10 = 0;

#define SPECIALIZE_EXPONENTS(T, m, m10, M, M10)				\
const int value< T >::min_exponent = m;					\
const int value< T >::min_exponent10 = m10;				\
const int value< T >::max_exponent = M;					\
const int value< T >::max_exponent10 = M10

SPECIALIZE_EXPONENTS(float, FLT_MIN_EXP, FLT_MIN_10_EXP,
                     FLT_MAX_EXP, FLT_MAX_10_EXP);
SPECIALIZE_EXPONENTS(double, DBL_MIN_EXP, DBL_MIN_10_EXP,
                     DBL_MAX_EXP, DBL_MAX_10_EXP);
SPECIALIZE_EXPONENTS(long double, LDBL_MIN_EXP, LDBL_MIN_10_EXP,
                     LDBL_MAX_EXP, LDBL_MAX_10_EXP);

#undef SPECIALIZE_EXPONENTS

//
// Functions to output predicates and values.
//

template<typename T> void is_signed()
{
    printf("%s%s = %s;\n", tab2, "static const bool is_signed",
           bool_alpha[predicate<T>::is_signed]);
}

// a fundamental type is modulo iff it isn't signed
template<typename T> void is_modulo()
{
    printf("%s%s = %s;\n", tab2, "static const bool is_modulo",
           bool_alpha[! predicate<T>::is_signed]);
}

template<typename T>
void min()
{
    printf("%s%s%s%s\n%s%s%s%s\n", tab2, "static ", declarator<T>::type_name,
           " min() throw()", tab2, "{ return ", value<T>::min, "; }");
}

template<typename T>
void max()
{
    printf("%s%s%s%s\n%s%s%s%s\n", tab2, "static ", declarator<T>::type_name,
           " max() throw()", tab2, "{ return ", value<T>::max, "; }");
}

template<typename T>
void is_integer()
{
    printf("%s%s = %s;\n", tab2, "static const bool is_integer",
           bool_alpha[predicate<T>::is_integer]);        
}

template<typename T>
void is_exact()
{
    printf("%s%s = %s;\n", tab2, "static const bool is_exact",
           bool_alpha[predicate<T>::is_exact]);    
}

template<typename T>
void digits()
{
    printf("%s%s = %d;\n", tab2, "static const int digits",
           value<T>::digits);
}

template<typename T>
void digits10()
{
    printf("%s%s = %d;\n", tab2, "static const int digits10",
           int(log10_of_two * value<T>::digits));
}

template<typename T>
void radix()
{
    printf("%s%s = %d;\n", tab2, "static const int radix",
           value<T>::radix);
}

template<typename T>
void epsilon()
{
    printf("%s%s %s %s\n%s%s %s%s\n", tab2, "static",
           declarator<T>::type_name, "epsilon() throw()",
           tab2, "{ return", value<T>::epsilon, "; }");
}

template<typename T>
void round_error()
{
    printf("%s%s %s %s\n%s%s %s%s\n", tab2, "static",
           declarator<T>::type_name, "round_error() throw()",
           tab2, "{ return", value<T>::round_error, "; }");
}

template<typename T>
void min_exponent()
{
    printf("%s%s = %d;\n", tab2, "static const int min_exponent",
           value<T>::min_exponent);    
}

template<typename T>
void min_exponent10()
{
    printf("%s%s = %d;\n", tab2, "static const int min_exponent10",
           value<T>::min_exponent10);    
}

template<typename T>
void max_exponent()
{
    printf("%s%s = %d;\n", tab2, "static const int max_exponent",
           value<T>::max_exponent);    
}

template<typename T>
void max_exponent10()
{
    printf("%s%s = %d;\n", tab2, "static const int max_exponent10",
           value<T>::max_exponent10);    
}

template<typename T>
void has_infinity()
{
    printf("%s%s = %s;\n", tab2, "static const bool has_infinity",
           bool_alpha[predicate<T>::has_infinity]);
}

template<typename T>
void has_quiet_nan()
{
    printf("%s%s = %s;\n", tab2, "static const bool has_quiet_NaN",
           bool_alpha[predicate<T>::has_quiet_nan]);
}

template<typename T>
void has_signaling_nan()
{
    printf("%s%s = %s;\n", tab2, "static const bool has_signaling_NaN",
           bool_alpha[predicate<T>::has_signaling_nan]);
}

template<typename T>
void has_denorm_loss()
{
    printf("%s%s = %s;\n", tab2, "static const bool has_denorm_loss",
           bool_alpha[predicate<T>::has_denorm_loss]);
}

template<typename T> struct infinity_trait {
    static void has_denorm()
    {
        printf("%s%s;\n", tab2, "static const float_denorm_style "
               "has_denorm = denorm_absent");
    }

    static void infinity()
    {
        printf("%s%s %s %s\n%s%s%s%s\n", tab2, "static",
               declarator<T>::type_name, "infinity() throw()",
               tab2, "{ return static_cast<", declarator<T>::type_name, 
	       ">(0); }");
    }

    static void quiet_NaN()
    {
        printf("%s%s %s %s\n%s%s%s%s\n", tab2, "static",
               declarator<T>::type_name, "quiet_NaN() throw()",
               tab2, "{ return static_cast<", declarator<T>::type_name, 
	       ">(0); }");
    }

    static void signaling_NaN()
    {
        printf("%s%s %s %s\n%s%s%s%s\n", tab2, "static",
               declarator<T>::type_name, "signaling_NaN() throw()",
               tab2, "{ return static_cast<", declarator<T>::type_name, 
	       ">(0); }");
    }

    static void denorm_min()
    {
        printf("%s%s %s %s\n%s%s%s%s\n", tab2, "static",
               declarator<T>::type_name, "denorm_min() throw()",
               tab2, "{ return static_cast<", declarator<T>::type_name, 
	       ">(0); }");
    }
};


template<typename T>
void is_iec559()
{
    printf("%s%s = %s;\n", tab2, "static const bool is_iec559",
           bool_alpha[predicate<T>::is_iec559]);
}

// tinyness_before
template<typename T>
void tinyness_before()
{
    printf("%s%s;\n", tab2, "static const bool tinyness_before = false");
}

// round style
template<typename T>
void round_style()
{
    printf("%s%s;\n", tab2, "static const float_round_style "
           "round_style = round_toward_zero");    
}



// type traits
template<typename T> struct type_trait {
    
    type_trait()
    {
        declarator<T>::start();
        printf("%s%s;\n\n", tab2, "static const bool is_specialized = true");
        min<T>();
        max<T>();
        printf("\n");
        digits<T>();
        digits10<T>();
        is_signed<T>();
        is_integer<T>();
        is_exact<T>();
        radix<T>();
        epsilon<T>();
        round_error<T>();
        printf("\n");
        min_exponent<T>();
        min_exponent10<T>();
        max_exponent<T>();
        max_exponent10<T>();
        printf("\n");
        has_infinity<T>();
        has_quiet_nan<T>();
        has_signaling_nan<T>();
        infinity_trait<T>::has_denorm();
        has_denorm_loss<T>();
        printf("\n");
        infinity_trait<T>::infinity();
        infinity_trait<T>::quiet_NaN();
        infinity_trait<T>::signaling_NaN();
        infinity_trait<T>::denorm_min();
        printf("\n");
        is_iec559<T>();
        printf("%s%s;\n", tab2, "static const bool is_bounded = true");
        is_modulo<T>();
        printf("\n");
        traps<T>();
        tinyness_before<T>();
        round_style<T>();
        declarator<T>::end();
    }
};

int main()
{
    type_trait<bool>();

    type_trait<char>();
    type_trait<signed char>();
    type_trait<unsigned char>();
#if defined( _GLIBCPP_USE_WCHAR_T) && defined(_GLIBCPP_HAS_WCHAR_MIN_MAX)
    type_trait<wchar_t>();
#endif
    
    type_trait<short>();
    type_trait<unsigned short>();

    type_trait<int>();
    type_trait<unsigned int>();

    type_trait<long>();
    type_trait<unsigned long>();

#ifdef _GLIBCPP_USE_LONG_LONG
    type_trait<long long>();
    type_trait<unsigned long long>();
#endif

    type_trait<float>();
    type_trait<double>();
    type_trait<long double>();

    // x86/linux gets this weirdness for the min/max functions:
    // static long double min() throw()
    // { return (__extension__ ((union __convert_long_double) 
    // {__convert_long_double_i: {0x0, 0x80000000, 0x1, 0x0}})
    // .__convert_long_double_d); }
}

