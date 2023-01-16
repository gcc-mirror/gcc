// std::moneypunct implementation details, DragonFly version -*- C++ -*-

// Copyright (C) 2015-2023 Free Software Foundation, Inc.
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
// Modified for DragonFly by John Marino <gnugcc@marino.st>

#include <locale>
#include <cstring>
#include <xlocale.h>

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

// This file might be compiled twice, but we only want to define the members
// of money_base once.
#if ! _GLIBCXX_USE_CXX11_ABI

  // Construct and return valid pattern consisting of some combination of:
  // space none symbol sign value
  money_base::pattern
  money_base::_S_construct_pattern(char __precedes, char __space,
				   char __posn) throw()
  {
    pattern __ret;

    // This insanely complicated routine attempts to construct a valid
    // pattern for use with moneypunct. A couple of invariants:

    // if (__precedes) symbol -> value
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
	__ret.field[0] = sign;
	if (__space)
	  {
	    // Pattern starts with sign.
	    if (__precedes)
	      {
		__ret.field[1] = symbol;
		__ret.field[3] = value;
	      }
	    else
	      {
		__ret.field[1] = value;
		__ret.field[3] = symbol;
	      }
	    __ret.field[2] = space;
	  }
	else
	  {
	    // Pattern starts with sign and ends with none.
	    if (__precedes)
	      {
		__ret.field[1] = symbol;
		__ret.field[2] = value;
	      }
	    else
	      {
		__ret.field[1] = value;
		__ret.field[2] = symbol;
	      }
	    __ret.field[3] = none;
	  }
	break;
      case 2:
	// 2 The sign follows the value and symbol.
	if (__space)
	  {
	    // Pattern either ends with sign.
	    if (__precedes)
	      {
		__ret.field[0] = symbol;
		__ret.field[2] = value;
	      }
	    else
	      {
		__ret.field[0] = value;
		__ret.field[2] = symbol;
	      }
	    __ret.field[1] = space;
	    __ret.field[3] = sign;
	  }
	else
	  {
	    // Pattern ends with sign then none.
	    if (__precedes)
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
	if (__precedes)
	  {
	    __ret.field[0] = sign;
	    __ret.field[1] = symbol;
	    if (__space)
	      {
		__ret.field[2] = space;
		__ret.field[3] = value;
	      }
	    else
	      {
		__ret.field[2] = value;
		__ret.field[3] = none;
	      }
	  }
	else
	  {
	    __ret.field[0] = value;
	    if (__space)
	      {
		__ret.field[1] = space;
		__ret.field[2] = sign;
		__ret.field[3] = symbol;
	      }
	    else
	      {
		__ret.field[1] = sign;
		__ret.field[2] = symbol;
		__ret.field[3] = none;
	      }
	  }
	break;
      case 4:
	// 4 The sign immediately follows the symbol.
	if (__precedes)
	  {
	    __ret.field[0] = symbol;
	    __ret.field[1] = sign;
	    if (__space)
	      {
		__ret.field[2] = space;
		__ret.field[3] = value;
	      }
	    else
	      {
		__ret.field[2] = value;
		__ret.field[3] = none;
	      }
	  }
	else
	  {
	    __ret.field[0] = value;
	    if (__space)
	      {
		__ret.field[1] = space;
		__ret.field[2] = symbol;
		__ret.field[3] = sign;
	      }
	    else
	      {
		__ret.field[1] = symbol;
		__ret.field[2] = sign;
		__ret.field[3] = none;
	      }
	  }
	break;
      default:
	__ret = pattern();
      }
    return __ret;
  }
#endif

  template<>
    void
    moneypunct<char, true>::_M_initialize_moneypunct(__c_locale __cloc,
						     const char*)
    {
      if (!_M_data)
	_M_data = new __moneypunct_cache<char, true>;

      if (!__cloc)
	{
	  // "C" locale
	  _M_data->_M_decimal_point = '.';
	  _M_data->_M_thousands_sep = ',';
	  _M_data->_M_grouping = "";
	  _M_data->_M_grouping_size = 0;
	  _M_data->_M_use_grouping = false;
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
      else
	{
	  // Named locale.
	  lconv* lc = localeconv_l((locale_t) __cloc);

	  // Check for NULL, which implies no fractional digits.
	  if (lc->mon_decimal_point == NULL ||
	      lc->mon_decimal_point[0] == '\0')
	    {
	      // Like in "C" locale.
	      _M_data->_M_frac_digits = 0;
	      _M_data->_M_decimal_point = '.';
	    }
	  else
	    {
	      _M_data->_M_decimal_point = lc->mon_decimal_point[0];
	      _M_data->_M_frac_digits = lc->int_frac_digits;
	    }

	  const char* __cgroup = lc->mon_grouping;
	  const char* __cpossign = lc->positive_sign;
	  const char* __cnegsign = lc->negative_sign;
	  // _Intl == true
	  const char* __ccurr = lc->int_curr_symbol;

	  char* __group = 0;
	  char* __ps = 0;
	  char* __ns = 0;
	  const char __nposn = lc->int_n_sign_posn;
	  __try
	    {
	      size_t __len;

	      // Check for NULL, which implies no grouping.
	      if (lc->mon_thousands_sep == NULL ||
	          lc->mon_thousands_sep[0] == '\0')
		{
		  // Like in "C" locale.
		  _M_data->_M_grouping = "";
		  _M_data->_M_grouping_size = 0;
		  _M_data->_M_use_grouping = false;
		  _M_data->_M_thousands_sep = ',';
		}
	      else
		{
	          _M_data->_M_thousands_sep = lc->mon_thousands_sep[0];

		  __len = strlen(__cgroup);
		  if (__len)
		    {
		      __group = new char[__len + 1];
		      memcpy(__group, __cgroup, __len + 1);
		      _M_data->_M_grouping = __group;
		    }
		  else
		    {
		      _M_data->_M_grouping = "";
		      _M_data->_M_use_grouping = false;
		    }
		  _M_data->_M_grouping_size = __len;
		}

	      __len = strlen(__cpossign);
	      if (__len)
		{
		  __ps = new char[__len + 1];
		  memcpy(__ps, __cpossign, __len + 1);
		  _M_data->_M_positive_sign = __ps;
		}
	      else
		_M_data->_M_positive_sign = "";
	      _M_data->_M_positive_sign_size = __len;

	      if (!__nposn)
		{
		  _M_data->_M_negative_sign = "()";
		  _M_data->_M_negative_sign_size = 2;
		}
	      else
		{
		  __len = strlen(__cnegsign);
		  if (__len)
		    {
		      __ns = new char[__len + 1];
		      memcpy(__ns, __cnegsign, __len + 1);
		      _M_data->_M_negative_sign = __ns;
		    }
		  else
		    _M_data->_M_negative_sign = "";
		  _M_data->_M_negative_sign_size = __len;
		}

	      __len = strlen(__ccurr);
	      if (__len)
		{
		  char* __curr = new char[__len + 1];
		  memcpy(__curr, __ccurr, __len + 1);
		  _M_data->_M_curr_symbol = __curr;
		}
	      else
		_M_data->_M_curr_symbol = "";
	      _M_data->_M_curr_symbol_size = __len;
	    }
	  __catch(...)
	    {
	      delete _M_data;
	      _M_data = 0;
	      delete [] __group;
	      delete [] __ps;
	      delete [] __ns;
	      __throw_exception_again;
	    }

	  char __pprecedes = lc->int_p_cs_precedes;
	  char __pspace = lc->int_p_sep_by_space;
	  char __pposn = lc->int_p_sign_posn;
	  _M_data->_M_pos_format = _S_construct_pattern(__pprecedes, __pspace,
							__pposn);
	  char __nprecedes = lc->int_n_cs_precedes;
	  char __nspace = lc->int_n_sep_by_space;
	  _M_data->_M_neg_format = _S_construct_pattern(__nprecedes, __nspace,
							__nposn);
	}
    }

  template<>
    void
    moneypunct<char, false>::_M_initialize_moneypunct(__c_locale __cloc,
						      const char*)
    {
      if (!_M_data)
	_M_data = new __moneypunct_cache<char, false>;

      if (!__cloc)
	{
	  // "C" locale
	  _M_data->_M_decimal_point = '.';
	  _M_data->_M_thousands_sep = ',';
	  _M_data->_M_grouping = "";
	  _M_data->_M_grouping_size = 0;
	  _M_data->_M_use_grouping = false;
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
      else
	{
	  // Named locale.
	  lconv* lc = localeconv_l((locale_t) __cloc);

	  // Check for NULL, which implies no fractional digits.
	  if (lc->mon_decimal_point == NULL ||
	      lc->mon_decimal_point[0] == '\0')
	    {
	      // Like in "C" locale.
	      _M_data->_M_frac_digits = 0;
	      _M_data->_M_decimal_point = '.';
	    }
	  else
	    {
	      _M_data->_M_decimal_point = lc->mon_decimal_point[0];
	      _M_data->_M_frac_digits = lc->frac_digits;
            }

	  const char* __cgroup = lc->mon_grouping;
	  const char* __cpossign = lc->positive_sign;
	  const char* __cnegsign = lc->negative_sign;
	  // _Intl == false
	  const char* __ccurr = lc->currency_symbol;

	  char* __group = 0;
	  char* __ps = 0;
	  char* __ns = 0;
	  const char __nposn = lc->n_sign_posn;
	  __try
	    {
	      size_t __len;

	      // Check for NULL, which implies no grouping.
	      if (lc->mon_thousands_sep == NULL ||
	          lc->mon_thousands_sep[0] == '\0')
		{
		  // Like in "C" locale.
		  _M_data->_M_grouping = "";
		  _M_data->_M_grouping_size = 0;
		  _M_data->_M_use_grouping = false;
		  _M_data->_M_thousands_sep = ',';
		}
	      else
		{
	          _M_data->_M_thousands_sep = lc->mon_thousands_sep[0];

		  __len = strlen(__cgroup);
		  if (__len)
		    {
		      __group = new char[__len + 1];
		      memcpy(__group, __cgroup, __len + 1);
		      _M_data->_M_grouping = __group;
		    }
		  else
		    {
		      _M_data->_M_grouping = "";
		      _M_data->_M_use_grouping = false;
		    }
		  _M_data->_M_grouping_size = __len;
		}

	      __len = strlen(__cpossign);
	      if (__len)
		{
		  __ps = new char[__len + 1];
		  memcpy(__ps, __cpossign, __len + 1);
		  _M_data->_M_positive_sign = __ps;
		}
	      else
		_M_data->_M_positive_sign = "";
	      _M_data->_M_positive_sign_size = __len;

	      if (!__nposn)
		{
		  _M_data->_M_negative_sign = "()";
		  _M_data->_M_negative_sign_size = 2;
		}
	      else
		{
		  __len = strlen(__cnegsign);
		  if (__len)
		    {
		      __ns = new char[__len + 1];
		      memcpy(__ns, __cnegsign, __len + 1);
		      _M_data->_M_negative_sign = __ns;
		    }
		  else
		    _M_data->_M_negative_sign = "";
		  _M_data->_M_negative_sign_size = __len;
		}

	      __len = strlen(__ccurr);
	      if (__len)
		{
		  char* __curr = new char[__len + 1];
		  memcpy(__curr, __ccurr, __len + 1);
		  _M_data->_M_curr_symbol = __curr;
		}
	      else
		_M_data->_M_curr_symbol = "";
	      _M_data->_M_curr_symbol_size = __len;
	    }
	  __catch(...)
	    {
	      delete _M_data;
	      _M_data = 0;
	      delete [] __group;
	      delete [] __ps;
	      delete [] __ns;
	      __throw_exception_again;
	    }

	  char __pprecedes = lc->p_cs_precedes;
	  char __pspace = lc->p_sep_by_space;
	  char __pposn = lc->p_sign_posn;
	  _M_data->_M_pos_format = _S_construct_pattern(__pprecedes, __pspace,
							__pposn);
	  char __nprecedes = lc->n_cs_precedes;
	  char __nspace = lc->n_sep_by_space;
	  _M_data->_M_neg_format = _S_construct_pattern(__nprecedes, __nspace,
							__nposn);
	}
    }

  template<>
    moneypunct<char, true>::~moneypunct()
    {
      if (_M_data->_M_grouping_size)
	delete [] _M_data->_M_grouping;
      if (_M_data->_M_positive_sign_size)
	delete [] _M_data->_M_positive_sign;
      if (_M_data->_M_negative_sign_size
          && strcmp(_M_data->_M_negative_sign, "()") != 0)
	delete [] _M_data->_M_negative_sign;
      if (_M_data->_M_curr_symbol_size)
	delete [] _M_data->_M_curr_symbol;
      delete _M_data;
    }

  template<>
    moneypunct<char, false>::~moneypunct()
    {
      if (_M_data->_M_grouping_size)
	delete [] _M_data->_M_grouping;
      if (_M_data->_M_positive_sign_size)
	delete [] _M_data->_M_positive_sign;
      if (_M_data->_M_negative_sign_size
          && strcmp(_M_data->_M_negative_sign, "()") != 0)
	delete [] _M_data->_M_negative_sign;
      if (_M_data->_M_curr_symbol_size)
	delete [] _M_data->_M_curr_symbol;
      delete _M_data;
    }

#ifdef _GLIBCXX_USE_WCHAR_T
  template<>
    void
    moneypunct<wchar_t, true>::_M_initialize_moneypunct(__c_locale __cloc,
							const char*)
    {
      if (!_M_data)
	_M_data = new __moneypunct_cache<wchar_t, true>;

      if (!__cloc)
	{
	  // "C" locale
	  _M_data->_M_decimal_point = L'.';
	  _M_data->_M_thousands_sep = L',';
	  _M_data->_M_grouping = "";
	  _M_data->_M_grouping_size = 0;
	  _M_data->_M_use_grouping = false;
	  _M_data->_M_curr_symbol = L"";
	  _M_data->_M_curr_symbol_size = 0;
	  _M_data->_M_positive_sign = L"";
	  _M_data->_M_positive_sign_size = 0;
	  _M_data->_M_negative_sign = L"";
	  _M_data->_M_negative_sign_size = 0;
	  _M_data->_M_frac_digits = 0;
	  _M_data->_M_pos_format = money_base::_S_default_pattern;
	  _M_data->_M_neg_format = money_base::_S_default_pattern;

	  // Use ctype::widen code without the facet...
	  for (size_t __i = 0; __i < money_base::_S_end; ++__i)
	    _M_data->_M_atoms[__i] =
	      static_cast<wchar_t>(money_base::_S_atoms[__i]);
	}
      else
	{
	  __c_locale __old = (__c_locale)uselocale((locale_t)__cloc);
	  // Named locale.
	  lconv* lc = localeconv_l((locale_t) __cloc);

	  // Check for NULL, which implies no fractional digits.
	  if (lc->mon_decimal_point == NULL ||
	      lc->mon_decimal_point[0] == '\0')
	    {
	      // Like in "C" locale.
	      _M_data->_M_frac_digits = 0;
	      _M_data->_M_decimal_point = L'.';
	    }
	  else
	    {
	      _M_data->_M_frac_digits = lc->int_frac_digits;
	      _M_data->_M_decimal_point = (wchar_t)lc->mon_decimal_point[0];
            }

	  const char* __cgroup = lc->mon_grouping;
	  const char* __cpossign = lc->positive_sign;
	  const char* __cnegsign = lc->negative_sign;
	  const char* __ccurr = lc->int_curr_symbol;

	  char* __group = 0;
	  wchar_t* __wcs_ps = 0;
	  wchar_t* __wcs_ns = 0;
	  const char __nposn = lc->int_n_sign_posn;
	  __try
	    {
	      size_t __len;

	      // Check for NULL, which implies no grouping.
	      if (lc->mon_thousands_sep == NULL ||
	          lc->mon_thousands_sep[0] == '\0')
		{
		  // Like in "C" locale.
		  _M_data->_M_grouping = "";
		  _M_data->_M_grouping_size = 0;
		  _M_data->_M_use_grouping = false;
		  _M_data->_M_thousands_sep = L',';
		}
	      else
		{
		  _M_data->_M_thousands_sep =
			(wchar_t)lc->mon_thousands_sep[0];
		  __len = strlen(__cgroup);
		  if (__len)
		    {
		      __group = new char[__len + 1];
		      memcpy(__group, __cgroup, __len + 1);
		      _M_data->_M_grouping = __group;
		    }
		  else
		    {
		      _M_data->_M_grouping = "";
		      _M_data->_M_use_grouping = false;
		    }
		  _M_data->_M_grouping_size = __len;
		}

	      mbstate_t __state;
	      __len = strlen(__cpossign);
	      if (__len)
		{
		  memset(&__state, 0, sizeof(mbstate_t));
		  __wcs_ps = new wchar_t[__len + 1];
		  mbsrtowcs(__wcs_ps, &__cpossign, __len + 1, &__state);
		  _M_data->_M_positive_sign = __wcs_ps;
		}
	      else
		_M_data->_M_positive_sign = L"";
	      _M_data->_M_positive_sign_size =
		wcslen(_M_data->_M_positive_sign);

	      __len = strlen(__cnegsign);
	      if (!__nposn)
		_M_data->_M_negative_sign = L"()";
	      else if (__len)
		{
		  memset(&__state, 0, sizeof(mbstate_t));
		  __wcs_ns = new wchar_t[__len + 1];
		  mbsrtowcs(__wcs_ns, &__cnegsign, __len + 1, &__state);
		  _M_data->_M_negative_sign = __wcs_ns;
		}
	      else
		_M_data->_M_negative_sign = L"";
	      _M_data->_M_negative_sign_size =
		wcslen(_M_data->_M_negative_sign);

	      // _Intl == true.
	      __len = strlen(__ccurr);
	      if (__len)
		{
		  memset(&__state, 0, sizeof(mbstate_t));
		  wchar_t* __wcs = new wchar_t[__len + 1];
		  mbsrtowcs(__wcs, &__ccurr, __len + 1, &__state);
		  _M_data->_M_curr_symbol = __wcs;
		}
	      else
		_M_data->_M_curr_symbol = L"";
	      _M_data->_M_curr_symbol_size = wcslen(_M_data->_M_curr_symbol);
	    }
	  __catch(...)
	    {
	      delete _M_data;
	      _M_data = 0;
	      delete [] __group;
	      delete [] __wcs_ps;
	      delete [] __wcs_ns;
	      uselocale((locale_t)__old);
	      __throw_exception_again;
	    }

	  char __pprecedes = lc->int_p_cs_precedes;
	  char __pspace = lc->int_p_sep_by_space;
	  char __pposn = lc->int_p_sign_posn;
	  _M_data->_M_pos_format = _S_construct_pattern(__pprecedes, __pspace,
							__pposn);
	  char __nprecedes = lc->int_n_cs_precedes;
	  char __nspace = lc->int_n_sep_by_space;
	  _M_data->_M_neg_format = _S_construct_pattern(__nprecedes, __nspace,
							__nposn);

	  uselocale((locale_t)__old);
	}
    }

  template<>
  void
  moneypunct<wchar_t, false>::_M_initialize_moneypunct(__c_locale __cloc,
						       const char*)
  {
    if (!_M_data)
      _M_data = new __moneypunct_cache<wchar_t, false>;

    if (!__cloc)
	{
	  // "C" locale
	  _M_data->_M_decimal_point = L'.';
	  _M_data->_M_thousands_sep = L',';
	  _M_data->_M_grouping = "";
          _M_data->_M_grouping_size = 0;
	  _M_data->_M_use_grouping = false;
	  _M_data->_M_curr_symbol = L"";
	  _M_data->_M_curr_symbol_size = 0;
	  _M_data->_M_positive_sign = L"";
	  _M_data->_M_positive_sign_size = 0;
	  _M_data->_M_negative_sign = L"";
	  _M_data->_M_negative_sign_size = 0;
	  _M_data->_M_frac_digits = 0;
	  _M_data->_M_pos_format = money_base::_S_default_pattern;
	  _M_data->_M_neg_format = money_base::_S_default_pattern;

	  // Use ctype::widen code without the facet...
	  for (size_t __i = 0; __i < money_base::_S_end; ++__i)
	    _M_data->_M_atoms[__i] =
	      static_cast<wchar_t>(money_base::_S_atoms[__i]);
	}
      else
	{
	  __c_locale __old = (__c_locale)uselocale((locale_t)__cloc);
	  // Named locale.
	  lconv* lc = localeconv_l((locale_t) __cloc);

	  // Check for NULL, which implies no fractional digits.
	  if (lc->mon_decimal_point == NULL ||
	      lc->mon_decimal_point[0] == '\0')
	    {
	      // Like in "C" locale.
	      _M_data->_M_frac_digits = 0;
	      _M_data->_M_decimal_point = L'.';
	    }
	  else
	    {
	      _M_data->_M_frac_digits = lc->frac_digits;
	      _M_data->_M_decimal_point = (wchar_t)lc->mon_decimal_point[0];
            }

	  const char* __cgroup = lc->mon_grouping;
	  const char* __cpossign = lc->positive_sign;
	  const char* __cnegsign = lc->negative_sign;
	  const char* __ccurr = lc->currency_symbol;

	  char* __group = 0;
	  wchar_t* __wcs_ps = 0;
	  wchar_t* __wcs_ns = 0;
	  const char __nposn = lc->n_sign_posn;
	  __try
            {
	      size_t __len;

	      // Check for NULL, which implies no grouping.
	      if (lc->mon_thousands_sep == NULL ||
	          lc->mon_thousands_sep[0] == '\0')
		{
		  // Like in "C" locale.
		  _M_data->_M_grouping = "";
		  _M_data->_M_grouping_size = 0;
		  _M_data->_M_use_grouping = false;
		  _M_data->_M_thousands_sep = L',';
		}
	      else
		{
		  _M_data->_M_thousands_sep =
			(wchar_t)lc->mon_thousands_sep[0];
		  __len = strlen(__cgroup);
		  if (__len)
		    {
		      __group = new char[__len + 1];
		      memcpy(__group, __cgroup, __len + 1);
		      _M_data->_M_grouping = __group;
		    }
		  else
		    {
		      _M_data->_M_grouping = "";
		      _M_data->_M_use_grouping = false;
		    }
		  _M_data->_M_grouping_size = __len;
		}

              mbstate_t __state;
              __len = strlen(__cpossign);
              if (__len)
                {
		  memset(&__state, 0, sizeof(mbstate_t));
		  __wcs_ps = new wchar_t[__len + 1];
		  mbsrtowcs(__wcs_ps, &__cpossign, __len + 1, &__state);
		  _M_data->_M_positive_sign = __wcs_ps;
		}
	      else
		_M_data->_M_positive_sign = L"";
              _M_data->_M_positive_sign_size =
		wcslen(_M_data->_M_positive_sign);

	      __len = strlen(__cnegsign);
	      if (!__nposn)
		_M_data->_M_negative_sign = L"()";
	      else if (__len)
		{
		  memset(&__state, 0, sizeof(mbstate_t));
		  __wcs_ns = new wchar_t[__len + 1];
		  mbsrtowcs(__wcs_ns, &__cnegsign, __len + 1, &__state);
		  _M_data->_M_negative_sign = __wcs_ns;
		}
	      else
		_M_data->_M_negative_sign = L"";
              _M_data->_M_negative_sign_size =
		wcslen(_M_data->_M_negative_sign);

	      // _Intl == true.
	      __len = strlen(__ccurr);
	      if (__len)
		{
		  memset(&__state, 0, sizeof(mbstate_t));
		  wchar_t* __wcs = new wchar_t[__len + 1];
		  mbsrtowcs(__wcs, &__ccurr, __len + 1, &__state);
		  _M_data->_M_curr_symbol = __wcs;
		}
	      else
		_M_data->_M_curr_symbol = L"";
              _M_data->_M_curr_symbol_size = wcslen(_M_data->_M_curr_symbol);
	    }
          __catch(...)
	    {
	      delete _M_data;
              _M_data = 0;
	      delete [] __group;
	      delete [] __wcs_ps;
	      delete [] __wcs_ns;
	      uselocale((locale_t)__old);
              __throw_exception_again;
	    }

	  char __pprecedes = lc->p_cs_precedes;
	  char __pspace = lc->p_sep_by_space;
	  char __pposn = lc->p_sign_posn;
	  _M_data->_M_pos_format = _S_construct_pattern(__pprecedes, __pspace,
	                                                __pposn);
	  char __nprecedes = lc->n_cs_precedes;
	  char __nspace = lc->n_sep_by_space;
	  _M_data->_M_neg_format = _S_construct_pattern(__nprecedes, __nspace,
	                                                __nposn);

	  uselocale((locale_t)__old);
	}
    }

  template<>
    moneypunct<wchar_t, true>::~moneypunct()
    {
      if (_M_data->_M_grouping_size)
	delete [] _M_data->_M_grouping;
      if (_M_data->_M_positive_sign_size)
	delete [] _M_data->_M_positive_sign;
      if (_M_data->_M_negative_sign_size
          && wcscmp(_M_data->_M_negative_sign, L"()") != 0)
	delete [] _M_data->_M_negative_sign;
      if (_M_data->_M_curr_symbol_size)
	delete [] _M_data->_M_curr_symbol;
      delete _M_data;
    }

  template<>
    moneypunct<wchar_t, false>::~moneypunct()
    {
      if (_M_data->_M_grouping_size)
	delete [] _M_data->_M_grouping;
      if (_M_data->_M_positive_sign_size)
	delete [] _M_data->_M_positive_sign;
      if (_M_data->_M_negative_sign_size
          && wcscmp(_M_data->_M_negative_sign, L"()") != 0)
	delete [] _M_data->_M_negative_sign;
      if (_M_data->_M_curr_symbol_size)
	delete [] _M_data->_M_curr_symbol;
      delete _M_data;
    }
#endif

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace
