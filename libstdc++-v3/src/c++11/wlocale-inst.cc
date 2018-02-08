// Locale support -*- C++ -*-

// Copyright (C) 1999-2018 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

//
// ISO C++ 14882: 22.1  Locales
//

// Instantiate locales using COW std::wstring ABI
#define _GLIBCXX_USE_CXX11_ABI 0
#include <bits/c++config.h>

#ifdef _GLIBCXX_USE_WCHAR_T
#define C wchar_t
#include "locale-inst.cc"

// XXX GLIBCXX_ABI Deprecated
#if defined _GLIBCXX_LONG_DOUBLE_COMPAT

#pragma GCC diagnostic ignored "-Wattribute-alias"

#define _GLIBCXX_LDBL_COMPAT(dbl, ldbl) \
  extern "C" void ldbl (void) __attribute__ ((alias (#dbl), weak))

_GLIBCXX_LDBL_COMPAT(_ZNKSt17__gnu_cxx_ldbl1287num_getIwSt19istreambuf_iteratorIwSt11char_traitsIwEEE14_M_extract_intIjEES4_S4_S4_RSt8ios_baseRSt12_Ios_IostateRT_,
		     _ZNKSt7num_getIwSt19istreambuf_iteratorIwSt11char_traitsIwEEE14_M_extract_intIjEES3_S3_S3_RSt8ios_baseRSt12_Ios_IostateRT_);
_GLIBCXX_LDBL_COMPAT(_ZNKSt17__gnu_cxx_ldbl1287num_getIwSt19istreambuf_iteratorIwSt11char_traitsIwEEE14_M_extract_intIlEES4_S4_S4_RSt8ios_baseRSt12_Ios_IostateRT_,
		     _ZNKSt7num_getIwSt19istreambuf_iteratorIwSt11char_traitsIwEEE14_M_extract_intIlEES3_S3_S3_RSt8ios_baseRSt12_Ios_IostateRT_);
_GLIBCXX_LDBL_COMPAT(_ZNKSt17__gnu_cxx_ldbl1287num_getIwSt19istreambuf_iteratorIwSt11char_traitsIwEEE14_M_extract_intImEES4_S4_S4_RSt8ios_baseRSt12_Ios_IostateRT_,
		     _ZNKSt7num_getIwSt19istreambuf_iteratorIwSt11char_traitsIwEEE14_M_extract_intImEES3_S3_S3_RSt8ios_baseRSt12_Ios_IostateRT_);
_GLIBCXX_LDBL_COMPAT(_ZNKSt17__gnu_cxx_ldbl1287num_getIwSt19istreambuf_iteratorIwSt11char_traitsIwEEE14_M_extract_intItEES4_S4_S4_RSt8ios_baseRSt12_Ios_IostateRT_,
		     _ZNKSt7num_getIwSt19istreambuf_iteratorIwSt11char_traitsIwEEE14_M_extract_intItEES3_S3_S3_RSt8ios_baseRSt12_Ios_IostateRT_);
_GLIBCXX_LDBL_COMPAT(_ZNKSt17__gnu_cxx_ldbl1287num_getIwSt19istreambuf_iteratorIwSt11char_traitsIwEEE14_M_extract_intIxEES4_S4_S4_RSt8ios_baseRSt12_Ios_IostateRT_,
		     _ZNKSt7num_getIwSt19istreambuf_iteratorIwSt11char_traitsIwEEE14_M_extract_intIxEES3_S3_S3_RSt8ios_baseRSt12_Ios_IostateRT_);
_GLIBCXX_LDBL_COMPAT(_ZNKSt17__gnu_cxx_ldbl1287num_getIwSt19istreambuf_iteratorIwSt11char_traitsIwEEE14_M_extract_intIyEES4_S4_S4_RSt8ios_baseRSt12_Ios_IostateRT_,
		     _ZNKSt7num_getIwSt19istreambuf_iteratorIwSt11char_traitsIwEEE14_M_extract_intIyEES3_S3_S3_RSt8ios_baseRSt12_Ios_IostateRT_);
_GLIBCXX_LDBL_COMPAT(_ZNKSt17__gnu_cxx_ldbl1287num_putIwSt19ostreambuf_iteratorIwSt11char_traitsIwEEE13_M_insert_intIlEES4_S4_RSt8ios_basewT_,
		     _ZNKSt7num_putIwSt19ostreambuf_iteratorIwSt11char_traitsIwEEE13_M_insert_intIlEES3_S3_RSt8ios_basewT_);
_GLIBCXX_LDBL_COMPAT(_ZNKSt17__gnu_cxx_ldbl1287num_putIwSt19ostreambuf_iteratorIwSt11char_traitsIwEEE13_M_insert_intImEES4_S4_RSt8ios_basewT_,
		     _ZNKSt7num_putIwSt19ostreambuf_iteratorIwSt11char_traitsIwEEE13_M_insert_intImEES3_S3_RSt8ios_basewT_);
_GLIBCXX_LDBL_COMPAT(_ZNKSt17__gnu_cxx_ldbl1287num_putIwSt19ostreambuf_iteratorIwSt11char_traitsIwEEE13_M_insert_intIxEES4_S4_RSt8ios_basewT_,
		     _ZNKSt7num_putIwSt19ostreambuf_iteratorIwSt11char_traitsIwEEE13_M_insert_intIxEES3_S3_RSt8ios_basewT_);
_GLIBCXX_LDBL_COMPAT(_ZNKSt17__gnu_cxx_ldbl1287num_putIwSt19ostreambuf_iteratorIwSt11char_traitsIwEEE13_M_insert_intIyEES4_S4_RSt8ios_basewT_,
		     _ZNKSt7num_putIwSt19ostreambuf_iteratorIwSt11char_traitsIwEEE13_M_insert_intIyEES3_S3_RSt8ios_basewT_);
_GLIBCXX_LDBL_COMPAT(_ZNKSt17__gnu_cxx_ldbl1287num_putIwSt19ostreambuf_iteratorIwSt11char_traitsIwEEE15_M_insert_floatIdEES4_S4_RSt8ios_basewcT_,
		     _ZNKSt7num_putIwSt19ostreambuf_iteratorIwSt11char_traitsIwEEE15_M_insert_floatIdEES3_S3_RSt8ios_basewcT_);
_GLIBCXX_LDBL_COMPAT(_ZNKSt7num_putIwSt19ostreambuf_iteratorIwSt11char_traitsIwEEE15_M_insert_floatIdEES3_S3_RSt8ios_basewcT_,
		     _ZNKSt7num_putIwSt19ostreambuf_iteratorIwSt11char_traitsIwEEE15_M_insert_floatIeEES3_S3_RSt8ios_basewcT_);
_GLIBCXX_LDBL_COMPAT(_ZNKSt17__gnu_cxx_ldbl1289money_getIwSt19istreambuf_iteratorIwSt11char_traitsIwEEE10_M_extractILb0EEES4_S4_S4_RSt8ios_baseRSt12_Ios_IostateRSs,
		     _ZNKSt9money_getIwSt19istreambuf_iteratorIwSt11char_traitsIwEEE10_M_extractILb0EEES3_S3_S3_RSt8ios_baseRSt12_Ios_IostateRSs);
_GLIBCXX_LDBL_COMPAT(_ZNKSt17__gnu_cxx_ldbl1289money_getIwSt19istreambuf_iteratorIwSt11char_traitsIwEEE10_M_extractILb1EEES4_S4_S4_RSt8ios_baseRSt12_Ios_IostateRSs,
		     _ZNKSt9money_getIwSt19istreambuf_iteratorIwSt11char_traitsIwEEE10_M_extractILb1EEES3_S3_S3_RSt8ios_baseRSt12_Ios_IostateRSs);
_GLIBCXX_LDBL_COMPAT(_ZNKSt17__gnu_cxx_ldbl1289money_putIwSt19ostreambuf_iteratorIwSt11char_traitsIwEEE9_M_insertILb0EEES4_S4_RSt8ios_basewRKSbIwS3_SaIwEE,
		     _ZNKSt9money_putIwSt19ostreambuf_iteratorIwSt11char_traitsIwEEE9_M_insertILb0EEES3_S3_RSt8ios_basewRKSbIwS2_SaIwEE);
_GLIBCXX_LDBL_COMPAT(_ZNKSt17__gnu_cxx_ldbl1289money_putIwSt19ostreambuf_iteratorIwSt11char_traitsIwEEE9_M_insertILb1EEES4_S4_RSt8ios_basewRKSbIwS3_SaIwEE,
		     _ZNKSt9money_putIwSt19ostreambuf_iteratorIwSt11char_traitsIwEEE9_M_insertILb1EEES3_S3_RSt8ios_basewRKSbIwS2_SaIwEE);

#endif // _GLIBCXX_LONG_DOUBLE_COMPAT
#endif
