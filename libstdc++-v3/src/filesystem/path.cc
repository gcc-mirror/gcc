// Class experimental::filesystem::path -*- C++ -*-

// Copyright (C) 2014-2023 Free Software Foundation, Inc.
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

#ifndef _GLIBCXX_USE_CXX11_ABI
# define _GLIBCXX_USE_CXX11_ABI 1
#endif

#include <experimental/filesystem>

namespace fs = std::experimental::filesystem;
using fs::path;

fs::filesystem_error::~filesystem_error() = default;

constexpr path::value_type path::preferred_separator [[gnu::used]];

path&
path::remove_filename()
{
  if (_M_type == _Type::_Multi)
    {
      if (!_M_cmpts.empty())
	{
	  auto cmpt = std::prev(_M_cmpts.end());
	  _M_pathname.erase(cmpt->_M_pos);
	  _M_cmpts.erase(cmpt);
	  _M_trim();
	}
    }
  else
    clear();
  return *this;
}

path&
path::replace_filename(const path& replacement)
{
  remove_filename();
  operator/=(replacement);
  return *this;
}

#ifdef _GLIBCXX_FILESYSTEM_IS_WINDOWS
const fs::path::value_type dot = L'.';
#else
const fs::path::value_type dot = '.';
#endif

path&
path::replace_extension(const path& replacement)
{
  auto ext = _M_find_extension();
  if (ext.first && ext.second != string_type::npos)
    {
      if (ext.first == &_M_pathname)
	_M_pathname.erase(ext.second);
      else
	{
	  const auto& back = _M_cmpts.back();
	  if (ext.first != &back._M_pathname)
	    _GLIBCXX_THROW_OR_ABORT(
		std::logic_error("path::replace_extension failed"));
	  _M_pathname.erase(back._M_pos + ext.second);
	}
    }
  if (!replacement.empty() && replacement.native()[0] != dot)
    _M_pathname += dot;
  _M_pathname += replacement.native();
  _M_split_cmpts();
  return *this;
}

namespace
{
  template<typename Iter1, typename Iter2>
    int do_compare(Iter1 begin1, Iter1 end1, Iter2 begin2, Iter2 end2)
    {
      int cmpt = 1;
      while (begin1 != end1 && begin2 != end2)
	{
	  if (begin1->native() < begin2->native())
	    return -cmpt;
	  if (begin1->native() > begin2->native())
	    return +cmpt;
	  ++begin1;
	  ++begin2;
	  ++cmpt;
	}
      if (begin1 == end1)
	{
	  if (begin2 == end2)
	    return 0;
	  return -cmpt;
	}
      return +cmpt;
    }
}

int
path::compare(const path& p) const noexcept
{
  struct CmptRef
  {
    const path* ptr;
    const string_type& native() const noexcept { return ptr->native(); }
  };

  if (_M_type == _Type::_Multi && p._M_type == _Type::_Multi)
    return do_compare(_M_cmpts.begin(), _M_cmpts.end(),
		      p._M_cmpts.begin(), p._M_cmpts.end());
  else if (_M_type == _Type::_Multi)
    {
      CmptRef c[1] = { { &p } };
      return do_compare(_M_cmpts.begin(), _M_cmpts.end(), c, c+1);
    }
  else if (p._M_type == _Type::_Multi)
    {
      CmptRef c[1] = { { this } };
      return do_compare(c, c+1, p._M_cmpts.begin(), p._M_cmpts.end());
    }
  else
    return _M_pathname.compare(p._M_pathname);
}

path
path::root_name() const
{
  path __ret;
  if (_M_type == _Type::_Root_name)
    __ret = *this;
  else if (_M_cmpts.size() && _M_cmpts.begin()->_M_type == _Type::_Root_name)
    __ret = *_M_cmpts.begin();
  return __ret;
}

path
path::root_directory() const
{
  path __ret;
  if (_M_type == _Type::_Root_dir)
    __ret = *this;
  else if (!_M_cmpts.empty())
    {
      auto __it = _M_cmpts.begin();
      if (__it->_M_type == _Type::_Root_name)
        ++__it;
      if (__it != _M_cmpts.end() && __it->_M_type == _Type::_Root_dir)
        __ret = *__it;
    }
  return __ret;
}


path
path::root_path() const
{
  path __ret;
  if (_M_type == _Type::_Root_name || _M_type == _Type::_Root_dir)
    __ret = *this;
  else if (!_M_cmpts.empty())
    {
      auto __it = _M_cmpts.begin();
      if (__it->_M_type == _Type::_Root_name)
        {
          __ret = *__it++;
          if (__it != _M_cmpts.end() && __it->_M_type == _Type::_Root_dir)
            {
              __ret._M_pathname += preferred_separator;
              __ret._M_split_cmpts();
            }
        }
      else if (__it->_M_type == _Type::_Root_dir)
        __ret = *__it;
    }
  return __ret;
}

path
path::relative_path() const
{
  path __ret;
  if (_M_type == _Type::_Filename)
    __ret = *this;
  else if (!_M_cmpts.empty())
    {
      auto __it = _M_cmpts.begin();
      if (__it->_M_type == _Type::_Root_name)
        ++__it;
      if (__it != _M_cmpts.end() && __it->_M_type == _Type::_Root_dir)
        ++__it;
      if (__it != _M_cmpts.end())
        __ret.assign(_M_pathname.substr(__it->_M_pos));
    }
  return __ret;
}

path
path::parent_path() const
{
  path __ret;
  if (_M_cmpts.size() < 2)
    return __ret;
  for (auto __it = _M_cmpts.begin(), __end = std::prev(_M_cmpts.end());
       __it != __end; ++__it)
    {
      __ret /= *__it;
    }
  return __ret;
}

bool
path::has_root_name() const
{
  if (_M_type == _Type::_Root_name)
    return true;
  if (!_M_cmpts.empty() && _M_cmpts.begin()->_M_type == _Type::_Root_name)
    return true;
  return false;
}

bool
path::has_root_directory() const
{
  if (_M_type == _Type::_Root_dir)
    return true;
  if (!_M_cmpts.empty())
    {
      auto __it = _M_cmpts.begin();
      if (__it->_M_type == _Type::_Root_name)
        ++__it;
      if (__it != _M_cmpts.end() && __it->_M_type == _Type::_Root_dir)
        return true;
    }
  return false;
}

bool
path::has_root_path() const
{
  if (_M_type == _Type::_Root_name || _M_type == _Type::_Root_dir)
    return true;
  if (!_M_cmpts.empty())
    {
      auto __type = _M_cmpts.front()._M_type;
      if (__type == _Type::_Root_name || __type == _Type::_Root_dir)
        return true;
    }
  return false;
}

bool
path::has_relative_path() const
{
  if (_M_type == _Type::_Filename)
    return true;
  if (!_M_cmpts.empty())
    {
      auto __it = _M_cmpts.begin();
      if (__it->_M_type == _Type::_Root_name)
        ++__it;
      if (__it != _M_cmpts.end() && __it->_M_type == _Type::_Root_dir)
        ++__it;
      if (__it != _M_cmpts.end())
        return true;
    }
  return false;
}


bool
path::has_parent_path() const
{
  return _M_cmpts.size() > 1;
}

bool
path::has_filename() const
{
  return !empty();
}

std::pair<const path::string_type*, std::size_t>
path::_M_find_extension() const
{
  const string_type* s = nullptr;

  if (_M_type != _Type::_Multi)
    s = &_M_pathname;
  else if (!_M_cmpts.empty())
    {
      const auto& c = _M_cmpts.back();
      if (c._M_type == _Type::_Filename)
	s = &c._M_pathname;
    }

  if (s)
    {
      if (auto sz = s->size())
	{
	  if (sz <= 2 && (*s)[0] == dot)
	    {
	      if (sz == 1 || (*s)[1] == dot)  // filename is "." or ".."
		return { s, string_type::npos };
	      else
		return { s, 0 };  // filename is like ".?"
	    }
	  return { s, s->rfind(dot) };
	}
    }
  return {};
}

void
path::_M_split_cmpts()
{
  _M_type = _Type::_Multi;
  _M_cmpts.clear();

  // Use const-reference to access _M_pathname, to avoid "leaking" COW string.
  const auto& pathname = _M_pathname;

  if (pathname.empty())
    return;

  {
    // Approximate count of components, to reserve space in _M_cmpts vector:
    int count = 1;
    bool saw_sep_last = _S_is_dir_sep(pathname[0]);
    bool saw_non_sep = !saw_sep_last;
    for (value_type c : pathname)
      {
       if (_S_is_dir_sep(c))
         saw_sep_last = true;
       else if (saw_sep_last)
         {
           ++count;
           saw_sep_last = false;
           saw_non_sep = true;
         }
      }
    if (saw_non_sep && saw_sep_last)
      ++count; // empty filename after trailing slash
    if (count > 1)
      _M_cmpts.reserve(count);
  }

  size_t pos = 0;
  const size_t len = pathname.size();

  // look for root name or root directory
  if (_S_is_dir_sep(pathname[0]))
    {
      // look for root name, such as "//" or "//foo"
      if (len > 1 && pathname[1] == pathname[0])
	{
	  if (len == 2)
	    {
	      // entire path is just "//"
	      _M_type = _Type::_Root_name;
	      return;
	    }

	  if (!_S_is_dir_sep(pathname[2]))
	    {
	      // got root name, find its end
	      pos = 3;
	      while (pos < len && !_S_is_dir_sep(pathname[pos]))
		++pos;
	      if (pos == len)
		{
		  _M_type = _Type::_Root_name;
		  return;
		}
	      _M_add_root_name(pos);
	      _M_add_root_dir(pos);
	    }
	  else
	    {
	      // got something like "///foo" which is just a root directory
	      // composed of multiple redundant directory separators
	      _M_add_root_dir(0);
	    }
	}
      else if (len == 1) // got root directory only
	{
	  _M_type = _Type::_Root_dir;
	  return;
	}
      else // got root directory
	_M_add_root_dir(0);
      ++pos;
    }
#ifdef _GLIBCXX_FILESYSTEM_IS_WINDOWS
  else if (len > 1 && pathname[1] == L':')
    {
      // got disk designator
      if (len == 2)
	{
	  _M_type = _Type::_Root_name;
	  return;
	}
      _M_add_root_name(2);
      if (len > 2 && _S_is_dir_sep(pathname[2]))
	_M_add_root_dir(2);
      pos = 2;
    }
#endif
  else
    {
      size_t n = 1;
      for (; n < pathname.size() && !_S_is_dir_sep(pathname[n]); ++n)
	{ }
      if (n == pathname.size())
	{
	  _M_type = _Type::_Filename;
	  return;
	}
    }

  size_t back = pos;
  while (pos < len)
    {
      if (_S_is_dir_sep(pathname[pos]))
	{
	  if (back != pos)
	    _M_add_filename(back, pos - back);
	  back = ++pos;
	}
      else
	++pos;
    }

  if (back != pos)
    _M_add_filename(back, pos - back);
  else if (_S_is_dir_sep(pathname.back()))
    {
      // [path.itr]/8
      // "Dot, if one or more trailing non-root slash characters are present."
      if (_M_cmpts.back()._M_type == _Type::_Filename)
	{
	  const auto& last = _M_cmpts.back();
	  pos = last._M_pos + last._M_pathname.size();
	  _M_cmpts.emplace_back(string_type(1, dot), _Type::_Filename, pos);
	}
    }

  _M_trim();
}

void
path::_M_add_root_name(size_t n)
{
  _M_cmpts.emplace_back(_M_pathname.substr(0, n), _Type::_Root_name, 0);
}

void
path::_M_add_root_dir(size_t pos)
{
  _M_cmpts.emplace_back(_M_pathname.substr(pos, 1), _Type::_Root_dir, pos);
}

void
path::_M_add_filename(size_t pos, size_t n)
{
  _M_cmpts.emplace_back(_M_pathname.substr(pos, n), _Type::_Filename, pos);
}

void
path::_M_trim()
{
  if (_M_cmpts.size() == 1)
    {
      _M_type = _M_cmpts.front()._M_type;
      _M_cmpts.clear();
    }
}

path::string_type
path::_S_convert_loc(const char* __first, const char* __last,
		     const std::locale& __loc)
{
#if _GLIBCXX_USE_WCHAR_T
  auto& __cvt = std::use_facet<codecvt<wchar_t, char, mbstate_t>>(__loc);
  basic_string<wchar_t> __ws;
  if (!__str_codecvt_in_all(__first, __last, __ws, __cvt))
    _GLIBCXX_THROW_OR_ABORT(filesystem_error(
	  "Cannot convert character sequence",
	  std::make_error_code(errc::illegal_byte_sequence)));
#ifdef _GLIBCXX_FILESYSTEM_IS_WINDOWS
  return __ws;
#else
  return _Cvt<wchar_t>::_S_convert(__ws.data(), __ws.data() + __ws.size());
#endif
#else
  return {__first, __last};
#endif
}

std::size_t
fs::hash_value(const path& p) noexcept
{
  // [path.non-member]
  // "If for two paths, p1 == p2 then hash_value(p1) == hash_value(p2)."
  // Equality works as if by traversing the range [begin(), end()), meaning
  // e.g. path("a//b") == path("a/b"), so we cannot simply hash _M_pathname
  // but need to iterate over individual elements. Use the hash_combine from
  // http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2014/n3876.pdf
  size_t seed = 0;
  for (const auto& x : p)
    {
      seed ^= std::hash<path::string_type>()(x.native()) + 0x9e3779b9
	+ (seed<<6) + (seed>>2);
    }
  return seed;
}

#include <experimental/string_view>

std::string
fs::filesystem_error::_M_gen_what()
{
  const std::string pstr1 = _M_path1.u8string();
  const std::string pstr2 = _M_path2.u8string();
  experimental::string_view s = this->system_error::what();
  const size_t len = 18 + s.length()
    + (pstr1.length() || pstr2.length() ? pstr1.length() + 3 : 0)
    + (pstr2.length() ? pstr2.length() + 3 : 0);
  std::string w;
  w.reserve(len);
  w = "filesystem error: ";
  w.append(s.data(), s.length());
  if (!pstr1.empty())
    {
      w += " [";
      w += pstr1;
      w += ']';
    }
  if (!pstr2.empty())
    {
      if (pstr1.empty())
	w += " []";
      w += " [";
      w += pstr2;
      w += ']';
    }
  return w;
}
