// std::moneypunct implementation details, GNU version -*- C++ -*-

// Copyright (C) 2001 Free Software Foundation, Inc.
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
// ISO C++ 14882: 22.2.6.3.2  moneypunct virtual functions
//

// Written by Benjamin Kosnik <bkoz@redhat.com>

#include <locale>

namespace std
{
  // Construct and return valid pattern consisting of some combination of:
  // space none symbol sign value
  money_base::pattern
  money_base::_S_construct_pattern(char __preceeds, char __space, char __posn)
  { 
    pattern __ret;

    // This insanely complicated routine attempts to construct a valid
    // pattern for use with monyepunct. A couple of invariants:

    // if (__preceeds) symbol -> value
    // else value -> symbol
    
    // if (__space) space
    // else none

    // none == never first
    // space never first or last

    // Any elegant implementations of this are welcome.
    switch (__posn)
      {
      case 0:
      case 1:
	// 1 The sign precedes the value and symbol.
	if (__space)
	  {
	    // Pattern starts with sign.
	    if (__preceeds)
	      {
		__ret.field[1] = symbol;
		__ret.field[2] = space;
		__ret.field[3] = value;
	      }
	    else
	      {
		__ret.field[1] = value;
		__ret.field[2] = space;
		__ret.field[3] = symbol;
	      }
	    __ret.field[0] = sign;
	  }
	else
	  {
	    // Pattern starts with sign and ends with none.
	    if (__preceeds)
	      {
		__ret.field[1] = symbol;
		__ret.field[2] = value;
	      }
	    else
	      {
		__ret.field[1] = value;
		__ret.field[2] = symbol;
	      }
	    __ret.field[0] = sign;
	    __ret.field[3] = none;
	  }
	break;
      case 2:
	// 2 The sign follows the value and symbol.
	if (__space)
	  {
	    // Pattern either ends with sign.
	    if (__preceeds)
	      {
		__ret.field[0] = symbol;
		__ret.field[1] = space;
		__ret.field[2] = value;
	      }
	    else
	      {
		__ret.field[0] = value;
		__ret.field[1] = space;
		__ret.field[2] = symbol;
	      }
	    __ret.field[3] = sign;
	  }
	else
	  {
	    // Pattern ends with sign then none.
	    if (__preceeds)
	      {
		__ret.field[0] = symbol;
		__ret.field[1] = value;
	      }
	    else
	      {
		__ret.field[0] = value;
		__ret.field[1] = symbol;
	      }
	    __ret.field[2] = sign;
	    __ret.field[3] = none;
	  }
	break;
      case 3:
	// 3 The sign immediately precedes the symbol.
	if (__space)
	  {
	    // Have space.
	    if (__preceeds)
	      {
		__ret.field[0] = sign;
		__ret.field[1] = symbol;
		__ret.field[2] = space;
		__ret.field[3] = value;
	      }
	    else
	      {
		__ret.field[0] = value;
		__ret.field[1] = space;
		__ret.field[2] = sign;
		__ret.field[3] = symbol;
	      }
	  }
	else
	  {
	    // Have none.
	    if (__preceeds)
	      {
		__ret.field[0] = sign;
		__ret.field[1] = symbol;
		__ret.field[2] = value;
	      }
	    else
	      {
		__ret.field[0] = value;
		__ret.field[1] = sign;
		__ret.field[2] = symbol;
	      }
	    __ret.field[3] = none;
	  }
	break;
      case 4:
	// 4 The sign immediately follows the symbol. 
	if (__space)
	  {
	    // Have space.
	    if (__preceeds)
	      {
		__ret.field[0] = symbol;
		__ret.field[1] = sign;
		__ret.field[2] = space;
		__ret.field[3] = value;
	      }
	    else
	      {
		__ret.field[0] = value;
		__ret.field[1] = space;
		__ret.field[2] = symbol;
		__ret.field[3] = sign;
	      }
	  }
	else
	  {
	    // Have none.
	    if (__preceeds)
	      {
		__ret.field[0] = symbol;
		__ret.field[1] = sign;
		__ret.field[2] = value;
	      }
	    else
	      {
		__ret.field[0] = value;
		__ret.field[1] = symbol;
		__ret.field[2] = sign;
	      }
	    __ret.field[3] = none;
	  }
	break;
      default:
	;
      }
    return __ret;
  }

  template<> 
    void
    moneypunct<char, true>::_M_initialize_moneypunct(__c_locale __cloc)
    {
      if (!__cloc)
	{
	  // "C" locale
	  _M_decimal_point = '.';
	  _M_thousands_sep = ',';
	  _M_grouping = "";
	  _M_curr_symbol = string_type();
	  _M_positive_sign = string_type();
	  _M_negative_sign = string_type();
	  _M_frac_digits = 0;
	  _M_pos_format = money_base::_S_default_pattern;
	  _M_neg_format = money_base::_S_default_pattern;
	}
      else
	{
	  // Named locale.
	  _M_decimal_point = *(__nl_langinfo_l(__MON_DECIMAL_POINT, __cloc));
	  _M_thousands_sep = *(__nl_langinfo_l(__MON_THOUSANDS_SEP, __cloc));
	  _M_grouping = __nl_langinfo_l(__MON_GROUPING, __cloc);
	  _M_positive_sign = __nl_langinfo_l(__POSITIVE_SIGN, __cloc);
	  _M_negative_sign = __nl_langinfo_l(__NEGATIVE_SIGN, __cloc);

	  // _Intl == true
	  _M_curr_symbol = __nl_langinfo_l(__INT_CURR_SYMBOL, __cloc);
	  _M_frac_digits = *(__nl_langinfo_l(__INT_FRAC_DIGITS, __cloc));
	  char __ppreceeds = *(__nl_langinfo_l(__INT_P_CS_PRECEDES, __cloc));
	  char __pspace = *(__nl_langinfo_l(__INT_P_SEP_BY_SPACE, __cloc));
	  char __pposn = *(__nl_langinfo_l(__INT_P_SIGN_POSN, __cloc));
	  _M_pos_format = _S_construct_pattern(__ppreceeds, __pspace, __pposn);
	  char __npreceeds = *(__nl_langinfo_l(__INT_N_CS_PRECEDES, __cloc));
	  char __nspace = *(__nl_langinfo_l(__INT_N_SEP_BY_SPACE, __cloc));
	  char __nposn = *(__nl_langinfo_l(__INT_N_SIGN_POSN, __cloc));
	  if (!__nposn)
	    _M_negative_sign = "()";
	  _M_neg_format = _S_construct_pattern(__npreceeds, __nspace, __nposn);
	}
    }

  template<> 
    void
    moneypunct<char, false>::_M_initialize_moneypunct(__c_locale __cloc)
    {
      if (!__cloc)
	{
	  // "C" locale
	  _M_decimal_point = '.';
	  _M_thousands_sep = ',';
	  _M_grouping = "";
	  _M_curr_symbol = string_type();
	  _M_positive_sign = string_type();
	  _M_negative_sign = string_type();
	  _M_frac_digits = 0;
	  _M_pos_format = money_base::_S_default_pattern;
	  _M_neg_format = money_base::_S_default_pattern;
	}
      else
	{
	  // Named locale.
	  _M_decimal_point = *(__nl_langinfo_l(__MON_DECIMAL_POINT, __cloc));
	  _M_thousands_sep = *(__nl_langinfo_l(__MON_THOUSANDS_SEP, __cloc));
	  _M_grouping = __nl_langinfo_l(__MON_GROUPING, __cloc);
	  _M_positive_sign = __nl_langinfo_l(__POSITIVE_SIGN, __cloc);
	  _M_negative_sign = __nl_langinfo_l(__NEGATIVE_SIGN, __cloc);

	  // _Intl == false
	  _M_curr_symbol = __nl_langinfo_l(__CURRENCY_SYMBOL, __cloc);
	  _M_frac_digits = *(__nl_langinfo_l(__FRAC_DIGITS, __cloc));
	  char __ppreceeds = *(__nl_langinfo_l(__P_CS_PRECEDES, __cloc));
	  char __pspace = *(__nl_langinfo_l(__P_SEP_BY_SPACE, __cloc));
	  char __pposn = *(__nl_langinfo_l(__P_SIGN_POSN, __cloc));
	  _M_pos_format = _S_construct_pattern(__ppreceeds, __pspace, __pposn);
	  char __npreceeds = *(__nl_langinfo_l(__N_CS_PRECEDES, __cloc));
	  char __nspace = *(__nl_langinfo_l(__N_SEP_BY_SPACE, __cloc));
	  char __nposn = *(__nl_langinfo_l(__N_SIGN_POSN, __cloc));
	  if (!__nposn)
	    _M_negative_sign = "()";
	  _M_neg_format = _S_construct_pattern(__npreceeds, __nspace, __nposn);
	}
    }

#ifdef _GLIBCPP_USE_WCHAR_T
  template<> 
    void
    moneypunct<wchar_t, true>::_M_initialize_moneypunct(__c_locale __cloc)
    {
      if (!__cloc)
	{
	  // "C" locale
	  _M_decimal_point = L'.';
	  _M_thousands_sep = L',';
	  _M_grouping = "";
	  _M_curr_symbol = string_type();
	  _M_positive_sign = string_type();
	  _M_negative_sign = string_type();
	  _M_frac_digits = 0;
	  _M_pos_format = money_base::_S_default_pattern;
	  _M_neg_format = money_base::_S_default_pattern;
	}
      else
	{
	  // Named locale.
	  _M_decimal_point = reinterpret_cast<wchar_t>(__nl_langinfo_l(_NL_NUMERIC_DECIMAL_POINT_WC, __cloc));
	  _M_thousands_sep = reinterpret_cast<wchar_t>(__nl_langinfo_l(_NL_NUMERIC_THOUSANDS_SEP_WC,__cloc));
	  _M_grouping = __nl_langinfo_l(GROUPING, __cloc);
	  _M_positive_sign = reinterpret_cast<wchar_t*>(__nl_langinfo_l(__POSITIVE_SIGN, __cloc));
	  _M_negative_sign = reinterpret_cast<wchar_t*>(__nl_langinfo_l(__NEGATIVE_SIGN, __cloc));

	  // _Intl == true.
	  _M_curr_symbol = reinterpret_cast<wchar_t*>(__nl_langinfo_l(__INT_CURR_SYMBOL, __cloc));
	  _M_frac_digits = *(__nl_langinfo_l(__INT_FRAC_DIGITS, __cloc));
	  char __ppreceeds = *(__nl_langinfo_l(__INT_P_CS_PRECEDES, __cloc));
	  char __pspace = *(__nl_langinfo_l(__INT_P_SEP_BY_SPACE, __cloc));
	  char __pposn = *(__nl_langinfo_l(__INT_P_SIGN_POSN, __cloc));
	  _M_pos_format = _S_construct_pattern(__ppreceeds, __pspace, __pposn);
	  char __npreceeds = *(__nl_langinfo_l(__INT_N_CS_PRECEDES, __cloc));
	  char __nspace = *(__nl_langinfo_l(__INT_N_SEP_BY_SPACE, __cloc));
	  char __nposn = *(__nl_langinfo_l(__INT_N_SIGN_POSN, __cloc));
	  if (!__nposn)
	    _M_negative_sign = L"()";
	  _M_neg_format = _S_construct_pattern(__npreceeds, __nspace, __nposn);
	}
    }

  template<> 
    void
    moneypunct<wchar_t, false>::_M_initialize_moneypunct(__c_locale __cloc)
    {
      if (!__cloc)
	{
	  // "C" locale
	  _M_decimal_point = L'.';
	  _M_thousands_sep = L',';
	  _M_grouping = "";
	  _M_curr_symbol = string_type();
	  _M_positive_sign = string_type();
	  _M_negative_sign = string_type();
	  _M_frac_digits = 0;
	  _M_pos_format = money_base::_S_default_pattern;
	  _M_neg_format = money_base::_S_default_pattern;
	}
      else
	{
	  // Named locale.
	  _M_decimal_point = reinterpret_cast<wchar_t>(__nl_langinfo_l(_NL_NUMERIC_DECIMAL_POINT_WC, __cloc));
	  _M_thousands_sep = reinterpret_cast<wchar_t>(__nl_langinfo_l(_NL_NUMERIC_THOUSANDS_SEP_WC,__cloc));
	  _M_grouping = __nl_langinfo_l(GROUPING, __cloc);
	  _M_positive_sign = reinterpret_cast<wchar_t*>(__nl_langinfo_l(__POSITIVE_SIGN, __cloc));
	  _M_negative_sign = reinterpret_cast<wchar_t*>(__nl_langinfo_l(__NEGATIVE_SIGN, __cloc));

	  // _Intl == false.
	  _M_curr_symbol = reinterpret_cast<wchar_t*>(__nl_langinfo_l(__CURRENCY_SYMBOL, __cloc));
	  _M_frac_digits = *(__nl_langinfo_l(__FRAC_DIGITS, __cloc));
	  char __ppreceeds = *(__nl_langinfo_l(__P_CS_PRECEDES, __cloc));
	  char __pspace = *(__nl_langinfo_l(__P_SEP_BY_SPACE, __cloc));
	  char __pposn = *(__nl_langinfo_l(__P_SIGN_POSN, __cloc));
	  _M_pos_format = _S_construct_pattern(__ppreceeds, __pspace, __pposn);
	  char __npreceeds = *(__nl_langinfo_l(__N_CS_PRECEDES, __cloc));
	  char __nspace = *(__nl_langinfo_l(__N_SEP_BY_SPACE, __cloc));
	  char __nposn = *(__nl_langinfo_l(__N_SIGN_POSN, __cloc));
	  if (!__nposn)
	    _M_negative_sign = L"()";
	  _M_neg_format = _S_construct_pattern(__npreceeds, __nspace, __nposn);
	}
    }
#endif
}
