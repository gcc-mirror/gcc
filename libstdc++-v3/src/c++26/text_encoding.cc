// Definitions for <text_encoding> -*- C++ -*-

// Copyright The GNU Toolchain Authors.
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

#include <text_encoding>
#include <locale>

#ifdef _GLIBCXX_USE_NL_LANGINFO_L
#include <locale.h>
#if __has_include(<xlocale.h>)
# include <xlocale.h>
#endif
#include <langinfo.h>

#if __CHAR_BIT__ == 8
namespace std
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

text_encoding
__locale_encoding(const char* name)
{
  text_encoding enc;
  if (locale_t loc = ::newlocale(LC_CTYPE_MASK, name, (locale_t)0))
    {
      if (const char* codeset = ::nl_langinfo_l(CODESET, loc))
	{
	  string_view s(codeset);
	  if (s.size() < text_encoding::max_name_length)
	    enc = text_encoding(s);
	}
      ::freelocale(loc);
    }
  return enc;
}

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std

std::text_encoding
std::text_encoding::environment()
{
  return std::__locale_encoding("");
}

bool
std::text_encoding::_M_is_environment() const
{
  bool matched = false;
  if (locale_t loc = ::newlocale(LC_CTYPE_MASK, "", (locale_t)0))
    {
      if (const char* codeset = ::nl_langinfo_l(CODESET, loc))
	{
	  string_view sv(codeset);
	  for (auto alias : aliases())
	    if (__unicode::__charset_alias_match(alias, sv))
	      {
		matched = true;
		break;
	      }
	}
      ::freelocale(loc);
    }
  return matched;
}

std::text_encoding
std::locale::encoding() const
{
  return std::__locale_encoding(name().c_str());
}
#endif // CHAR_BIT == 8

#endif // _GLIBCXX_USE_NL_LANGINFO_L
