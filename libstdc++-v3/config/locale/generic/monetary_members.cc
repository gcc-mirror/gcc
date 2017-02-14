// std::moneypunct implementation details, generic version -*- C++ -*-

// Copyright (C) 2001-2017 Free Software Foundation, Inc.
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
// ISO C++ 14882: 22.2.6.3.2  moneypunct virtual functions
//

// Written by Benjamin Kosnik <bkoz@redhat.com>

#include <locale>

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

// This file might be compiled twice, but we only want to define the members
// of money_base once.
#if ! _GLIBCXX_USE_CXX11_ABI

  // Construct and return valid pattern consisting of some combination of:
  // space none symbol sign value
  money_base::pattern
  money_base::_S_construct_pattern(char, char, char) throw()
  { return _S_default_pattern; }
#endif

  template<>
    void
    moneypunct<char, true>::_M_initialize_moneypunct(__c_locale, const char*)
    {
      // "C" locale.
      if (!_M_data)
	_M_data = new __moneypunct_cache<char, true>;

      _M_data->_M_decimal_point = '.';
      _M_data->_M_thousands_sep = ',';
      _M_data->_M_grouping = "";
      _M_data->_M_grouping_size = 0;
      _M_data->_M_curr_symbol = "";
      _M_data->_M_curr_symbol_size = 0;
      _M_data->_M_positive_sign = "";
      _M_data->_M_positive_sign_size = 0;
      _M_data->_M_negative_sign = "";
      _M_data->_M_negative_sign_size = 0;
      _M_data->_M_frac_digits = 0;
      _M_data->_M_pos_format = money_base::_S_default_pattern;
      _M_data->_M_neg_format = money_base::_S_default_pattern;

      for (size_t __i = 0; __i < money_base::_S_end; ++__i)
	_M_data->_M_atoms[__i] = money_base::_S_atoms[__i];
    }

  template<>
    void
    moneypunct<char, false>::_M_initialize_moneypunct(__c_locale, const char*)
    {
      // "C" locale.
      if (!_M_data)
	_M_data = new __moneypunct_cache<char, false>;

      _M_data->_M_decimal_point = '.';
      _M_data->_M_thousands_sep = ',';
      _M_data->_M_grouping = "";
      _M_data->_M_grouping_size = 0;
      _M_data->_M_curr_symbol = "";
      _M_data->_M_curr_symbol_size = 0;
      _M_data->_M_positive_sign = "";
      _M_data->_M_positive_sign_size = 0;
      _M_data->_M_negative_sign = "";
      _M_data->_M_negative_sign_size = 0;
      _M_data->_M_frac_digits = 0;
      _M_data->_M_pos_format = money_base::_S_default_pattern;
      _M_data->_M_neg_format = money_base::_S_default_pattern;

      for (size_t __i = 0; __i < money_base::_S_end; ++__i)
	_M_data->_M_atoms[__i] = money_base::_S_atoms[__i];
    }

  template<>
    moneypunct<char, true>::~moneypunct()
    { delete _M_data; }

  template<>
    moneypunct<char, false>::~moneypunct()
    { delete _M_data; }

#ifdef _GLIBCXX_USE_WCHAR_T
  template<>
    void
    moneypunct<wchar_t, true>::_M_initialize_moneypunct(__c_locale,
							const char*)
    {
      // "C" locale
      if (!_M_data)
	_M_data = new __moneypunct_cache<wchar_t, true>;

      _M_data->_M_decimal_point = L'.';
      _M_data->_M_thousands_sep = L',';
      _M_data->_M_grouping = "";
      _M_data->_M_grouping_size = 0;
      _M_data->_M_curr_symbol = L"";
      _M_data->_M_curr_symbol_size = 0;
      _M_data->_M_positive_sign = L"";
      _M_data->_M_positive_sign_size = 0;
      _M_data->_M_negative_sign = L"";
      _M_data->_M_negative_sign_size = 0;
      _M_data->_M_frac_digits = 0;
      _M_data->_M_pos_format = money_base::_S_default_pattern;
      _M_data->_M_neg_format = money_base::_S_default_pattern;

      for (size_t __i = 0; __i < money_base::_S_end; ++__i)
	_M_data->_M_atoms[__i] =
	  static_cast<wchar_t>(money_base::_S_atoms[__i]);
    }

  template<>
    void
    moneypunct<wchar_t, false>::_M_initialize_moneypunct(__c_locale,
							 const char*)
    {
      // "C" locale
      if (!_M_data)
	_M_data = new __moneypunct_cache<wchar_t, false>;

      _M_data->_M_decimal_point = L'.';
      _M_data->_M_thousands_sep = L',';
      _M_data->_M_grouping = "";
      _M_data->_M_grouping_size = 0;
      _M_data->_M_curr_symbol = L"";
      _M_data->_M_curr_symbol_size = 0;
      _M_data->_M_positive_sign = L"";
      _M_data->_M_positive_sign_size = 0;
      _M_data->_M_negative_sign = L"";
      _M_data->_M_negative_sign_size = 0;
      _M_data->_M_frac_digits = 0;
      _M_data->_M_pos_format = money_base::_S_default_pattern;
      _M_data->_M_neg_format = money_base::_S_default_pattern;

      for (size_t __i = 0; __i < money_base::_S_end; ++__i)
	_M_data->_M_atoms[__i] =
	  static_cast<wchar_t>(money_base::_S_atoms[__i]);
    }

  template<>
    moneypunct<wchar_t, true>::~moneypunct()
    { delete _M_data; }

  template<>
    moneypunct<wchar_t, false>::~moneypunct()
    { delete _M_data; }
#endif

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace
