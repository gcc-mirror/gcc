// Class filesystem::path -*- C++ -*-

// Copyright (C) 2014-2017 Free Software Foundation, Inc.
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

#include <filesystem>
#ifdef _GLIBCXX_FILESYSTEM_IS_WINDOWS
# include <algorithm>
#endif

namespace fs = std::filesystem;
using fs::path;

fs::filesystem_error::~filesystem_error() = default;

constexpr path::value_type path::preferred_separator;

path&
path::remove_filename()
{
  if (_M_type == _Type::_Multi)
    {
      if (!_M_cmpts.empty())
	{
	  auto cmpt = std::prev(_M_cmpts.end());
	  if (cmpt->_M_type == _Type::_Filename && !cmpt->empty())
	    {
	      _M_pathname.erase(cmpt->_M_pos);
	      auto prev = std::prev(cmpt);
	      if (prev->_M_type == _Type::_Root_dir
		  || prev->_M_type == _Type::_Root_name)
		{
		  _M_cmpts.erase(cmpt);
		  _M_trim();
		}
	      else
		cmpt->clear();
	    }
	}
    }
  else if (_M_type == _Type::_Filename)
    clear();
  if (!empty() && _M_pathname.back() != '/')
    throw 1;
  return *this;
}

path&
path::replace_filename(const path& replacement)
{
  remove_filename();
  operator/=(replacement);
  return *this;
}

path&
path::replace_extension(const path& replacement)
{
  auto ext = _M_find_extension();
  // Any existing extension() is removed
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
   // If replacement is not empty and does not begin with a dot character,
   // a dot character is appended
  if (!replacement.empty() && replacement.native()[0] != '.')
    _M_pathname += '.';
  operator+=(replacement);
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

  if (empty() && p.empty())
    return 0;
  else if (_M_type == _Type::_Multi && p._M_type == _Type::_Multi)
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
    {
      __ret._M_type = _Type::_Root_dir;
      __ret._M_pathname.assign(1, preferred_separator);
    }
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
  if (_M_type == _Type::_Root_name)
    __ret = *this;
  else if (_M_type == _Type::_Root_dir)
    {
      __ret._M_pathname.assign(1, preferred_separator);
      __ret._M_type = _Type::_Root_dir;
    }
  else if (!_M_cmpts.empty())
    {
      auto __it = _M_cmpts.begin();
      if (__it->_M_type == _Type::_Root_name)
        {
          __ret = *__it++;
          if (__it != _M_cmpts.end() && __it->_M_type == _Type::_Root_dir)
	    __ret /= *__it;
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
  if (!has_relative_path())
    __ret = *this;
  else if (_M_cmpts.size() >= 2)
    {
      for (auto __it = _M_cmpts.begin(), __end = std::prev(_M_cmpts.end());
	   __it != __end; ++__it)
	{
	  __ret /= *__it;
	}
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
  if (!has_relative_path())
    return !empty();
  return _M_cmpts.size() >= 2;
}

bool
path::has_filename() const
{
  if (empty())
    return false;
  if (_M_type == _Type::_Filename)
    return !_M_pathname.empty();
  if (_M_type == _Type::_Multi)
    {
      if (_M_pathname.back() == preferred_separator)
	return false;
      return _M_cmpts.back().has_filename();
    }
  return false;
}

namespace
{
#ifdef _GLIBCXX_FILESYSTEM_IS_WINDOWS
  inline bool is_dot(wchar_t c) { return c == L'.'; }
#else
  inline bool is_dot(char c) { return c == '.'; }
#endif

  inline bool is_dot(const fs::path& path)
  {
    const auto& filename = path.native();
    return filename.size() == 1 && is_dot(filename[0]);
  }

  inline bool is_dotdot(const fs::path& path)
  {
    const auto& filename = path.native();
    return filename.size() == 2 && is_dot(filename[0]) && is_dot(filename[1]);
  }
} // namespace

path
path::lexically_normal() const
{
  /*
  C++17 [fs.path.generic] p6
  - If the path is empty, stop.
  - Replace each slash character in the root-name with a preferred-separator.
  - Replace each directory-separator with a preferred-separator.
  - Remove each dot filename and any immediately following directory-separator.
  - As long as any appear, remove a non-dot-dot filename immediately followed
    by a directory-separator and a dot-dot filename, along with any immediately
    following directory-separator.
  - If the last filename is dot-dot, remove any trailing directory-separator.
  - If the path is empty, add a dot.
  */
  path ret;
  // If the path is empty, stop.
  if (empty())
    return ret;
  for (auto& p : *this)
    {
#ifdef _GLIBCXX_FILESYSTEM_IS_WINDOWS
      // Replace each slash character in the root-name
      if (p.is_root_name())
	{
	  string_type s = p.native();
	  std::replace(s.begin(), s.end(), L'/', L'\\');
	  ret /= s;
	  continue;
	}
#endif
      if (is_dotdot(p))
	{
	  if (ret.has_filename() && !is_dotdot(ret.filename()))
	    ret.remove_filename();
	  else
	    ret /= p;
	}
      else if (is_dot(p))
	ret /= path();
      else
	ret /= p;
    }

  if (ret._M_cmpts.size() >= 2)
    {
      auto back = std::prev(ret.end());
      // If the last filename is dot-dot, ...
      if (back->empty() && is_dotdot(*std::prev(back)))
	// ... remove any trailing directory-separator.
	ret = ret.parent_path();
    }
  // If the path is empty, add a dot.
  else if (ret.empty())
    ret = ".";

  return ret;
}

path
path::lexically_relative(const path& base) const
{
  path ret;
  if (root_name() != base.root_name())
    return ret;
  if (is_absolute() != base.is_absolute())
    return ret;
  if (!has_root_directory() && base.has_root_directory())
    return ret;
  auto [a, b] = std::mismatch(begin(), end(), base.begin(), base.end());
  if (a == end() && b == base.end())
    ret = ".";
  else
  {
    int n = 0;
    for (; b != base.end(); ++b)
    {
      const path& p = *b;
      if (is_dotdot(p))
	--n;
      else if (!is_dot(p))
	++n;
    }
    if (n >= 0)
    {
      const path dotdot("..");
      while (n--)
	ret /= dotdot;
      for (; a != end(); ++a)
	ret /= *a;
    }
  }
  return ret;
}

path
path::lexically_proximate(const path& base) const
{
  path rel = lexically_relative(base);
  if (rel.empty())
    rel = *this;
  return rel;
}

std::pair<const path::string_type*, std::size_t>
path::_M_find_extension() const
{
  const std::string* s = nullptr;

  if (_M_type == _Type::_Filename)
    s = &_M_pathname;
  else if (_M_type == _Type::_Multi && !_M_cmpts.empty())
    {
      const auto& c = _M_cmpts.back();
      if (c._M_type == _Type::_Filename)
	s = &c._M_pathname;
    }

  if (s)
    {
      if (auto sz = s->size())
	{
	  if (sz <= 2 && (*s)[0] == '.')
	    return { s, string_type::npos };
	  const auto pos = s->rfind('.');
	  return { s, pos ? pos : string_type::npos };
	}
    }
  return {};
}

void
path::_M_split_cmpts()
{
  _M_type = _Type::_Multi;
  _M_cmpts.clear();

  if (_M_pathname.empty())
    return;

  size_t pos = 0;
  const size_t len = _M_pathname.size();

  // look for root name or root directory
  if (_S_is_dir_sep(_M_pathname[0]))
    {
#ifdef __CYGWIN__
      // look for root name, such as "//foo"
      if (len > 2 && _M_pathname[1] == _M_pathname[0])
	{
	  if (!_S_is_dir_sep(_M_pathname[2]))
	    {
	      // got root name, find its end
	      pos = 3;
	      while (pos < len && !_S_is_dir_sep(_M_pathname[pos]))
		++pos;
	      _M_add_root_name(pos);
	      if (pos < len) // also got root directory
		_M_add_root_dir(pos);
	    }
	  else
	    {
	      // got something like "///foo" which is just a root directory
	      // composed of multiple redundant directory separators
	      _M_add_root_dir(0);
	    }
	}
      else
#endif
        {
	  // got root directory
	  if (_M_pathname.find_first_not_of('/') == string_type::npos)
	    {
	      // entire path is just slashes
	      _M_type = _Type::_Root_dir;
	      return;
	    }
	  _M_add_root_dir(0);
	  ++pos;
	}
    }
#ifdef _GLIBCXX_FILESYSTEM_IS_WINDOWS
  else if (len > 1 && _M_pathname[1] == L':')
    {
      // got disk designator
      _M_add_root_name(2);
      if (len > 2 && _S_is_dir_sep(_M_pathname[2]))
	_M_add_root_dir(2);
      pos = 2;
    }
#endif

  size_t back = pos;
  while (pos < len)
    {
      if (_S_is_dir_sep(_M_pathname[pos]))
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
  else if (_S_is_dir_sep(_M_pathname.back()))
    {
      // [fs.path.itr]/4
      // An empty element, if trailing non-root directory-separator present.
      if (_M_cmpts.back()._M_type == _Type::_Filename)
	{
	  const auto& last = _M_cmpts.back();
	  pos = last._M_pos + last._M_pathname.size();
	  _M_cmpts.emplace_back(string_type(), _Type::_Filename, pos);
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
  if (!__str_codecvt_in(__first, __last, __ws, __cvt))
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

namespace std
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
namespace filesystem
{
  string
  fs_err_concat(const string& __what, const string& __path1,
		  const string& __path2)
  {
    const size_t __len = 18 + __what.length()
      + (__path1.length() ? __path1.length() + 3 : 0)
      + (__path2.length() ? __path2.length() + 3 : 0);
    string __ret;
    __ret.reserve(__len);
    __ret = "filesystem error: ";
    __ret += __what;
    if (!__path1.empty())
      {
	__ret += " [";
	__ret += __path1;
	__ret += ']';
      }
    if (!__path2.empty())
      {
	__ret += " [";
	__ret += __path2;
	__ret += ']';
      }
    return __ret;
  }

_GLIBCXX_BEGIN_NAMESPACE_CXX11

  std::string filesystem_error::_M_gen_what()
  {
    return fs_err_concat(system_error::what(), _M_path1.native(),
			 _M_path2.native());
  }

_GLIBCXX_END_NAMESPACE_CXX11

} // filesystem
_GLIBCXX_END_NAMESPACE_VERSION
} // std
