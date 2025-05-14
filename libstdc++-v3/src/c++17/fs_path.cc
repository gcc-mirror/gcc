// Class filesystem::path -*- C++ -*-

// Copyright (C) 2014-2025 Free Software Foundation, Inc.
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

#ifdef __CYGWIN__
// Interpret "//x" as a root-name, not root-dir + filename
# define SLASHSLASH_IS_ROOTNAME 1
#endif

#include <filesystem>
#include <algorithm>
#include <array>
#include <bits/stl_uninitialized.h>
#include <ext/numeric_traits.h> // __gnu_cxx::__int_traits

namespace fs = std::filesystem;
using fs::path;

static inline bool is_dir_sep(path::value_type ch)
{
#ifdef _GLIBCXX_FILESYSTEM_IS_WINDOWS
    return ch == L'/' || ch == path::preferred_separator;
#else
    return ch == '/';
#endif
}

#ifdef _GLIBCXX_FILESYSTEM_IS_WINDOWS
static inline bool is_disk_designator(std::wstring_view s)
{
  return s.length() == 2 && s[1] == L':';
}
#endif

struct path::_Parser
{
  using string_view_type = std::basic_string_view<value_type>;

  struct cmpt
  {
    string_view_type str;
    _Type type = _Type::_Multi;

    bool valid() const { return type != _Type::_Multi; }
  };

  string_view_type input;
  string_view_type::size_type pos = 0;
  size_t origin;
  _Type last_type = _Type::_Multi;

  _Parser(string_view_type s, size_t o = 0) : input(s), origin(o) { }

  pair<cmpt, cmpt> root_path() noexcept
  {
    pos = 0;
    pair<cmpt, cmpt> root;

    const size_t len = input.size();

    // look for root name or root directory
    if (len && is_dir_sep(input[0]))
      {
#if SLASHSLASH_IS_ROOTNAME
	// look for root name, such as "//foo"
	if (len > 2 && input[1] == input[0])
	  {
	    if (!is_dir_sep(input[2]))
	      {
		// got root name, find its end
		pos = 3;
		while (pos < len && !is_dir_sep(input[pos]))
		  ++pos;
		root.first.str = input.substr(0, pos);
		root.first.type = _Type::_Root_name;

		if (pos < len) // also got root directory
		  {
		    root.second.str = input.substr(pos, 1);
		    root.second.type = _Type::_Root_dir;
		    ++pos;
		  }
	      }
	    else
	      {
		// got something like "///foo" which is just a root directory
		// composed of multiple redundant directory separators
		root.first.str = input.substr(0, 1);
		root.first.type = _Type::_Root_dir;
		pos += 2;
	      }
	  }
	else
#endif
	  {
	    root.first.str = input.substr(0, 1);
	    root.first.type = _Type::_Root_dir;
	    ++pos;
	  }
	// Find the start of the first filename
	while (pos < len && is_dir_sep(input[pos]))
	  ++pos;
      }
#ifdef _GLIBCXX_FILESYSTEM_IS_WINDOWS
    else if (is_disk_designator(input.substr(0, 2)))
      {
	// got disk designator
	root.first.str = input.substr(0, 2);
	root.first.type = _Type::_Root_name;
	if (len > 2 && is_dir_sep(input[2]))
	  {
	    root.second.str = input.substr(2, 1);
	    root.second.type = _Type::_Root_dir;
	  }
	pos = input.find_first_not_of(L"/\\", 2);
      }
#endif

    if (root.second.valid())
      last_type = root.second.type;
    else
      last_type = root.first.type;

    return root;
  }

  cmpt next() noexcept
  {
#ifdef _GLIBCXX_FILESYSTEM_IS_WINDOWS
    string_view_type sep = L"/\\";
#else
    char sep = '/';
#endif

    const int last_pos = pos;

    cmpt f;
    if (pos != input.npos)
      {
	pos = input.find_first_not_of(sep, pos);
	if (pos != input.npos)
	  {
	    const auto end = input.find_first_of(sep, pos);
	    f.str = input.substr(pos, end - pos);
	    f.type = _Type::_Filename;
	    pos = end;
	  }
	else if (last_type == _Type::_Filename
	    || (last_pos == 0 && !input.empty()))
	  {
	    // [fs.path.itr]/4 An empty element, if trailing non-root
	    // directory-separator present.
	    __glibcxx_assert(is_dir_sep(input.back()));
	    f.str = input.substr(input.length(), 0);
	    f.type = _Type::_Filename;
	  }
      }
    last_type = f.type;
    return f;
  }

  string_view_type::size_type
  offset(const cmpt& c) const noexcept
  { return origin + c.str.data() - input.data(); }
};

inline
path::path(basic_string_view<value_type> __str, _Type __type)
: _M_pathname(__str)
{
  __glibcxx_assert(__type != _Type::_Multi);
  _M_cmpts.type(__type);
}

inline
path::_Cmpt::_Cmpt(basic_string_view<value_type> __s, _Type __t, size_t __pos)
: path(__s, __t), _M_pos(__pos)
{ }

struct path::_List::_Impl
{
  using value_type = _Cmpt;

  _Impl(int cap) : _M_size(0), _M_capacity(cap) { }

  alignas(value_type) int _M_size;
  int _M_capacity;

  using iterator = value_type*;
  using const_iterator = const value_type*;

  iterator begin() { return reinterpret_cast<value_type*>(this + 1); }
  iterator end() { return begin() + size(); }

  const_iterator begin() const
  { return reinterpret_cast<const value_type*>(this + 1); }
  const_iterator end() const { return begin() + size(); }

  const value_type& front() const { return *begin(); }
  const value_type& back() const { return end()[-1]; }

  int size() const { return _M_size; }
  int capacity() const { return _M_capacity; }
  bool empty() const { return _M_size == 0; }

  void clear() { std::destroy_n(begin(), _M_size); _M_size = 0; }

  void pop_back()
  {
    back().~_Cmpt();
    --_M_size;
  }

  void _M_erase_from(const_iterator pos)
  {
    iterator first = begin() + (pos - begin());
    iterator last = end();
    std::destroy(first, last);
    _M_size -= last - first;
  }

  unique_ptr<_Impl, _Impl_deleter> copy() const
  {
    const auto n = size();
    void* p = ::operator new(sizeof(_Impl) + n * sizeof(value_type));
    unique_ptr<_Impl, _Impl_deleter> newptr(::new (p) _Impl{n});
    std::uninitialized_copy_n(begin(), n, newptr->begin());
    newptr->_M_size = n;
    return newptr;
  }

  // Clear the lowest two bits from the pointer (i.e. remove the _Type value)
  static _Impl* notype(_Impl* p)
  {
    constexpr uintptr_t mask = ~(uintptr_t)0x3;
    return reinterpret_cast<_Impl*>(reinterpret_cast<uintptr_t>(p) & mask);
  }
};

void path::_List::_Impl_deleter::operator()(_Impl* p) const noexcept
{
  p = _Impl::notype(p);
  if (p)
    {
      __glibcxx_assert(p->_M_size <= p->_M_capacity);
      p->clear();
      ::operator delete(p, sizeof(*p) + p->_M_capacity * sizeof(value_type));
    }
}

path::_List::_List() : _M_impl(reinterpret_cast<_Impl*>(_Type::_Filename)) { }

path::_List::_List(const _List& other)
{
  if (!other.empty())
    _M_impl = other._M_impl->copy();
  else
    type(other.type());
}

path::_List&
path::_List::operator=(const _List& other)
{
  if (!other.empty())
    {
      // copy in-place if there is capacity
      const int newsize = other._M_impl->size();
      auto impl = _Impl::notype(_M_impl.get());
      if (impl && impl->capacity() >= newsize)
	{
	  const int oldsize = impl->_M_size;
	  auto to = impl->begin();
	  auto from = other._M_impl->begin();
	  const int minsize = std::min(newsize, oldsize);
	  for (int i = 0; i < minsize; ++i)
	    to[i]._M_pathname.reserve(from[i]._M_pathname.length());
	  if (newsize > oldsize)
	    {
	      std::uninitialized_copy_n(from + oldsize, newsize - oldsize,
					to + oldsize);
	      impl->_M_size = newsize;
	    }
	  else if (newsize < oldsize)
	    impl->_M_erase_from(impl->begin() + newsize);
	  std::copy_n(from, minsize, to);
	  type(_Type::_Multi);
	}
      else
	_M_impl = other._M_impl->copy();
    }
  else
    {
      clear();
      type(other.type());
    }
  return *this;
}

inline void
path::_List::type(_Type t) noexcept
{
  auto val = reinterpret_cast<uintptr_t>(_Impl::notype(_M_impl.release()));
  _M_impl.reset(reinterpret_cast<_Impl*>(val | (unsigned char)t));
}

inline int
path::_List::size() const noexcept
{
  if (auto* ptr = _Impl::notype(_M_impl.get()))
    return ptr->size();
  return 0;
}

inline int
path::_List::capacity() const noexcept
{
  if (auto* ptr = _Impl::notype(_M_impl.get()))
    return ptr->capacity();
  return 0;
}

inline bool
path::_List::empty() const noexcept
{
  return size() == 0;
}

inline auto
path::_List::begin() noexcept
-> iterator
{
  __glibcxx_assert(!empty());
  if (auto* ptr = _Impl::notype(_M_impl.get()))
    return ptr->begin();
  return nullptr;
}

inline auto
path::_List::end() noexcept
-> iterator
{
  __glibcxx_assert(!empty());
  if (auto* ptr = _Impl::notype(_M_impl.get()))
    return ptr->end();
  return nullptr;
}

auto
path::_List::begin() const noexcept
-> const_iterator
{
  __glibcxx_assert(!empty());
  if (auto* ptr = _Impl::notype(_M_impl.get()))
    return ptr->begin();
  return nullptr;
}

auto
path::_List::end() const noexcept
-> const_iterator
{
  __glibcxx_assert(!empty());
  if (auto* ptr = _Impl::notype(_M_impl.get()))
    return ptr->end();
  return nullptr;
}

inline auto
path::_List::front() noexcept
-> value_type&
{
  return *_M_impl->begin();
}

inline auto
path::_List::back() noexcept
-> value_type&
{
  return _M_impl->begin()[_M_impl->size() - 1];
}

inline auto
path::_List::front() const noexcept
-> const value_type&
{
  return *_M_impl->begin();
}

inline auto
path::_List::back() const noexcept
-> const value_type&
{
  return _M_impl->begin()[_M_impl->size() - 1];
}

inline void
path::_List::pop_back()
{
  __glibcxx_assert(size() > 0);
  _M_impl->pop_back();
}

inline void
path::_List::_M_erase_from(const_iterator pos)
{
  _M_impl->_M_erase_from(pos);
}

inline void
path::_List::clear()
{
  if (auto ptr = _Impl::notype(_M_impl.get()))
    ptr->clear();
}

void
path::_List::reserve(int newcap, bool exact = false)
{
  // __glibcxx_assert(type() == _Type::_Multi);

  _Impl* curptr = _Impl::notype(_M_impl.get());

  int curcap = curptr ? curptr->capacity() : 0;

  if (curcap < newcap)
    {
      if (!exact)
	{
	  const int nextcap = curcap + curcap / 2;
	  if (newcap < nextcap)
	    newcap = nextcap;
	}

      using __gnu_cxx::__int_traits;
      // Nobody should need paths with this many components.
      if (newcap >= __int_traits<int>::__max / 4)
	std::__throw_bad_alloc();

      size_t bytes;
      if constexpr (__int_traits<int>::__max >= __int_traits<size_t>::__max)
	{
	  size_t components;
	  if (__builtin_mul_overflow(newcap, sizeof(value_type), &components)
		|| __builtin_add_overflow(sizeof(_Impl), components, &bytes))
	    std::__throw_bad_alloc();
	}
      else // This won't overflow, even for 20-bit size_t on msp430.
	bytes = sizeof(_Impl) + newcap * sizeof(value_type);

      void* p = ::operator new(bytes);
      std::unique_ptr<_Impl, _Impl_deleter> newptr(::new(p) _Impl{newcap});
      const int cursize = curptr ? curptr->size() : 0;
      if (cursize)
	{
	  std::uninitialized_move_n(curptr->begin(), cursize, newptr->begin());
	  newptr->_M_size = cursize;
	}
      std::swap(newptr, _M_impl);
    }
}

path&
path::operator=(const path& p)
{
  if (&p == this) [[__unlikely__]]
    return *this;

  _M_pathname.reserve(p._M_pathname.length());
  _M_cmpts = p._M_cmpts;	// might throw
  _M_pathname = p._M_pathname;	// won't throw because we reserved enough space
  return *this;
}

path&
path::operator/=(const path& __p)
{
#ifdef _GLIBCXX_FILESYSTEM_IS_WINDOWS
  if (__p.is_absolute()
      || (__p.has_root_name() && __p.root_name() != root_name()))
    return operator=(__p);

  basic_string_view<value_type> __lhs = _M_pathname;
  bool __add_sep = false;

  if (__p.has_root_directory())
    {
      // Remove any root directory and relative path
      if (_M_type() != _Type::_Root_name)
	{
	  if (!_M_cmpts.empty()
	      && _M_cmpts.front()._M_type() == _Type::_Root_name)
	    __lhs = _M_cmpts.front()._M_pathname;
	  else
	    __lhs = {};
	}
    }
  else if (has_filename() || (!has_root_directory() && is_absolute()))
    __add_sep = true;

  basic_string_view<value_type> __rhs = __p._M_pathname;
  // Omit any root-name from the generic format pathname:
  if (__p._M_type() == _Type::_Root_name)
    __rhs = {};
  else if (!__p._M_cmpts.empty()
      && __p._M_cmpts.front()._M_type() == _Type::_Root_name)
    __rhs.remove_prefix(__p._M_cmpts.front()._M_pathname.size());

  const size_t __len = __lhs.size() + (int)__add_sep + __rhs.size();
  const int __maxcmpts = _M_cmpts.size() + __p._M_cmpts.size();
  if (_M_pathname.capacity() < __len || _M_cmpts.capacity() < __maxcmpts)
    {
      // Construct new path and swap (strong exception-safety guarantee).
      string_type __tmp;
      __tmp.reserve(__len);
      __tmp = __lhs;
      if (__add_sep)
	__tmp += preferred_separator;
      __tmp += __rhs;
      path __newp = std::move(__tmp);
      swap(__newp);
    }
  else
    {
      _M_pathname = __lhs;
      if (__add_sep)
	_M_pathname += preferred_separator;
      _M_pathname += __rhs;
      __try
	{
	  _M_split_cmpts();
	}
      __catch (...)
	{
	  __try
	    {
	      // try to restore original state
	      _M_pathname.resize(__lhs.length());
	      _M_split_cmpts();
	    }
	  __catch (...)
	    {
	      // give up, basic exception safety guarantee only:
	      clear();
	      __throw_exception_again;
	    }
	}
    }
#else
  // POSIX version is simpler than the specification in the standard,
  // as any path with root-name or root-dir is absolute.

  if (__p.is_absolute() || this->empty())
    {
      return operator=(__p);
    }

  using string_view_type = basic_string_view<value_type>;

  string_view_type sep;
  if (has_filename())
    sep = { &preferred_separator, 1 };  // need to add a separator
#if SLASHSLASH_IS_ROOTNAME
  else if (_M_type() == _Type::_Root_name) // root-name with no root-dir
    sep = { &preferred_separator, 1 };  // need to add a separator
#endif
  else if (__p.empty())
    return *this;			    // nothing to do

  const auto orig_pathlen = _M_pathname.length();
  const auto orig_size = _M_cmpts.size();
  const auto orig_type = _M_type();

  int capacity = 0;
  if (_M_type() == _Type::_Multi)
    capacity += _M_cmpts.size();
  else if (!empty())
    capacity += 1;
  if (__p._M_type() == _Type::_Multi)
    capacity += __p._M_cmpts.size();
  else if (!__p.empty() || !sep.empty())
    capacity += 1;
#if SLASHSLASH_IS_ROOTNAME
  if (orig_type == _Type::_Root_name)
    ++capacity; // Need to insert root-directory after root-name
#endif

  _M_pathname.reserve(_M_pathname.length() + sep.length()
		      + __p._M_pathname.length());

  __try
    {
      _M_pathname += sep;
      const auto basepos = _M_pathname.length();
      _M_pathname += __p.native();

      _M_cmpts.type(_Type::_Multi);
      _M_cmpts.reserve(capacity);
      _Cmpt* output = _M_cmpts._M_impl->end();

      if (orig_type == _Type::_Multi)
	{
	  // Remove empty final component
	  if (_M_cmpts._M_impl->back().empty())
	    {
	      _M_cmpts.pop_back();
	      --output;
	    }
	}
      else if (orig_pathlen != 0)
	{
	  // Create single component from original path
	  string_view_type s(_M_pathname.data(), orig_pathlen);
	  ::new(output++) _Cmpt(s, orig_type, 0);
	  ++_M_cmpts._M_impl->_M_size;
#if SLASHSLASH_IS_ROOTNAME
	  if (orig_type == _Type::_Root_name)
	    {
	      ::new(output++) _Cmpt(sep, _Type::_Root_dir,
				    orig_pathlen + sep.length());
	      ++_M_cmpts._M_impl->_M_size;
	    }
#endif
	}

      if (__p._M_type() == _Type::_Multi)
	{
	  for (auto& c : *__p._M_cmpts._M_impl)
	    {
	      ::new(output++) _Cmpt(c._M_pathname, _Type::_Filename,
				    c._M_pos + basepos);
	      ++_M_cmpts._M_impl->_M_size;
	    }
	}
      else if (!__p.empty() || !sep.empty())
	{
	  __glibcxx_assert(__p._M_type() == _Type::_Filename);
	  ::new(output) _Cmpt(__p._M_pathname, __p._M_type(), basepos);
	  ++_M_cmpts._M_impl->_M_size;
	}
    }
  __catch (...)
    {
      _M_pathname.resize(orig_pathlen);
      if (orig_type == _Type::_Multi)
	_M_cmpts._M_erase_from(_M_cmpts.begin() + orig_size);
      else
	_M_cmpts.clear();
      _M_cmpts.type(orig_type);
      __throw_exception_again;
    }
#endif
  return *this;
}

// [fs.path.append]
void
path::_M_append(basic_string_view<value_type> s)
{
  _Parser parser(s);
  auto root_path = parser.root_path();

#ifdef _GLIBCXX_FILESYSTEM_IS_WINDOWS
  bool is_absolute = root_path.second.type == _Type::_Root_dir;
  bool has_root_name = root_path.first.type == _Type::_Root_name;
  if (is_absolute || (has_root_name && root_path.first.str != root_name()))
    {
      operator=(s);
      return;
    }

  basic_string_view<value_type> lhs = _M_pathname;
  bool add_sep = false;

  bool has_root_directory = root_path.first.type == _Type::_Root_dir
    || root_path.second.type == _Type::_Root_dir;

  if (has_root_directory)
    {
      // Remove any root directory and relative path
      if (_M_type() != _Type::_Root_name)
	{
	  if (!_M_cmpts.empty()
	      && _M_cmpts.front()._M_type() == _Type::_Root_name)
	    lhs = _M_cmpts.front()._M_pathname;
	  else
	    lhs = {};
	}
    }
  else if (has_filename() || (!has_root_directory && is_absolute))
    add_sep = true;

  basic_string_view<value_type> rhs = s;
  // Omit any root-name from the generic format pathname:
  if (has_root_name)
    rhs.remove_prefix(root_path.first.str.length());

  // Construct new path and swap (strong exception-safety guarantee).
  string_type tmp;
  tmp.reserve(lhs.size() + (int)add_sep + rhs.size());
  tmp = lhs;
  if (add_sep)
    tmp += preferred_separator;
  tmp += rhs;
  path newp = std::move(tmp);
  swap(newp);
#else

  bool is_absolute = root_path.first.type == _Type::_Root_dir
    || root_path.second.type == _Type::_Root_dir;
  if (is_absolute || this->empty())
    {
      operator=(s);
      return;
    }

  const auto orig_pathlen = _M_pathname.length();
  const auto orig_size = _M_cmpts.size();
  const auto orig_type = _M_type();

  basic_string_view<value_type> sep;
  if (has_filename())
    sep = { &preferred_separator, 1 };  // need to add a separator
#if SLASHSLASH_IS_ROOTNAME
  else if (_M_type() == _Type::_Root_name) // root-name with no root-dir
    sep = { &preferred_separator, 1 };  // need to add a separator
#endif
  else if (s.empty())
    return;			    // nothing to do

  // Copy the input into _M_pathname:
  _M_pathname += s;
  _M_pathname.insert(orig_pathlen, sep);
  // Update s to refer to the new copy (this ensures s is not a dangling
  // reference to deallocated characters, in the case where it was referring
  // into _M_pathname or a member of _M_cmpts).
  s = _M_pathname;
  const auto orig_pathname = s.substr(0, orig_pathlen);
  s.remove_prefix(orig_pathlen + sep.length());

  parser.input = s; // reset parser to use updated string view
  const auto basepos = orig_pathname.length() + sep.length();
  parser.origin = basepos;

  std::array<_Parser::cmpt, 64> buf;
  auto next = buf.begin();

  int capacity = 0;
  if (_M_type() == _Type::_Multi)
    capacity += _M_cmpts.size();
  else if (!empty())
    capacity += 1;

  auto cmpt = parser.next();
  if (cmpt.valid())
    {
      do
	{
	  *next++ = cmpt;
	  cmpt = parser.next();
	}
      while (cmpt.valid() && next != buf.end());

      capacity += next - buf.begin();
      if (cmpt.valid()) // filled buffer before parsing whole input
	{
	  ++capacity;
	  _Parser parser2(parser);
	  while (parser2.next().valid())
	    ++capacity;
	}
    }
  else if (!sep.empty())
    ++capacity;

#if SLASHSLASH_IS_ROOTNAME
  if (orig_type == _Type::_Root_name)
    ++capacity; // Need to insert root-directory after root-name
#endif

  __try
    {
      _M_cmpts.type(_Type::_Multi);
      _M_cmpts.reserve(capacity);
      _Cmpt* output = _M_cmpts._M_impl->end();

      if (orig_type == _Type::_Multi)
	{
	  // Remove empty final component
	  if (_M_cmpts._M_impl->back().empty())
	    {
	      _M_cmpts.pop_back();
	      --output;
	    }
	}
      else if (orig_pathlen != 0)
	{
	  // Create single component from original path
	  ::new(output++) _Cmpt(orig_pathname, orig_type, 0);
	  ++_M_cmpts._M_impl->_M_size;

#if SLASHSLASH_IS_ROOTNAME
	  if (!sep.empty() && orig_type == _Type::_Root_name)
	    {
	      ::new(output++) _Cmpt(sep, _Type::_Root_dir,
				    orig_pathlen + sep.length());
	      ++_M_cmpts._M_impl->_M_size;
	    }
#endif
	}

      if (next != buf.begin())
	{
	  for (auto it = buf.begin(); it != next; ++it)
	    {
	      auto c = *it;
	      ::new(output++) _Cmpt(c.str, c.type, parser.offset(c));
	      ++_M_cmpts._M_impl->_M_size;
	    }
	  while (cmpt.valid())
	    {
	      ::new(output++) _Cmpt(cmpt.str, cmpt.type, parser.offset(cmpt));
	      ++_M_cmpts._M_impl->_M_size;
	      cmpt = parser.next();
	    }
	}
      else if (!sep.empty())
	{
	  // Empty filename at the end:
	  ::new(output) _Cmpt({}, _Type::_Filename, basepos);
	  ++_M_cmpts._M_impl->_M_size;
	}
    }
  __catch (...)
    {
      _M_pathname.resize(orig_pathlen);
      if (orig_type == _Type::_Multi)
	_M_cmpts._M_erase_from(_M_cmpts.begin() + orig_size);
      else
	_M_cmpts.clear();
      _M_cmpts.type(orig_type);
      __throw_exception_again;
    }
#endif
}

// [fs.path.concat]
path&
path::operator+=(const path& p)
{
  if (p.empty())
    return *this;

  if (this->empty())
    {
      operator=(p);
      return *this;
    }

  // Handle p += p which would otherwise access dangling pointers after
  // reallocating _M_cmpts and _M_pathname.
  if (&p == this) [[unlikely]]
    return *this += p.native();
  // Handle p += *i where i is in [p.begin(),p.end()), for the same reason.
  if (_M_type() == _Type::_Multi && p._M_type() != _Type::_Multi)
    for (const path& cmpt : *this)
      if (&cmpt == &p) [[unlikely]]
	return *this += p.native();

#if _GLIBCXX_FILESYSTEM_IS_WINDOWS
  if (_M_type() == _Type::_Root_name
      || (_M_type() == _Type::_Filename && _M_pathname.size() == 1))
    {
      // Handle path("C") += path(":") and path("C:") += path("/x")
      // FIXME: do this more efficiently
      *this = path(_M_pathname + p._M_pathname);
      return *this;
    }
#endif
#if SLASHSLASH_IS_ROOTNAME
  if (_M_type() == _Type::_Root_dir)
    {
      // Handle path("/") += path("/x") and path("//") += path("x")
      // FIXME: do this more efficiently
      *this = path(_M_pathname + p._M_pathname);
      return *this;
    }
#endif

  const auto orig_pathlen = _M_pathname.length();
  const auto orig_type = _M_type();
  const auto orig_size = _M_cmpts.size();
  int orig_filenamelen = -1;
  basic_string_view<value_type> extra;

  // Ensure that '_M_pathname += p._M_pathname' won't throw:
  _M_pathname.reserve(orig_pathlen + p._M_pathname.length());

  _Cmpt c;
  _Cmpt* it = nullptr;
  _Cmpt* last = nullptr;
  if (p._M_type() == _Type::_Multi)
    {
      it = p._M_cmpts._M_impl->begin();
      last = p._M_cmpts._M_impl->end();
    }
  else
    {
      c = _Cmpt(p._M_pathname, p._M_type(), 0);
      it = &c;
      last = it + 1;
    }

  if (it->_M_type() == _Type::_Filename)
    {
      // See if there's a filename or root-name at the end of the original path
      // that we can add to.
      if (_M_type() == _Type::_Filename
#if SLASHSLASH_IS_ROOTNAME
	  || _M_type() == _Type::_Root_name
#endif
	  )
	{
	  if (p._M_type() == _Type::_Filename)
	    {
	      // Simplest case where we just add the whole of p to the
	      // original path.
	      _M_pathname += p._M_pathname;
	      return *this;
	    }
	  // Only the first component of s should be appended, do so below:
	  extra = it->_M_pathname;
	  ++it;
	}
      else if (_M_type() == _Type::_Multi
	  && _M_cmpts.back()._M_type() == _Type::_Filename)
	{
	  auto& back = _M_cmpts.back();
	  if (p._M_type() == _Type::_Filename)
	    {
	      basic_string_view<value_type> s = p._M_pathname;
	      back._M_pathname += s;
	      _M_pathname += s;
	      return *this;
	    }

	  orig_filenamelen = back._M_pathname.length();
	  back._M_pathname += it->_M_pathname;
	  extra = it->_M_pathname;
	  ++it;
	}
    }
  else if (is_dir_sep(_M_pathname.back()) && _M_type() == _Type::_Multi
      && _M_cmpts.back()._M_type() == _Type::_Filename)
    orig_filenamelen = 0; // current path has empty filename at end

  int capacity = 0;
  if (_M_type() == _Type::_Multi)
    capacity += _M_cmpts.size();
  else
    capacity += 1;
  if (p._M_type() == _Type::_Multi)
    capacity += p._M_cmpts.size();
  else
    capacity += 1;

  __try
    {
      _M_cmpts.type(_Type::_Multi);
      _M_cmpts.reserve(capacity);
      _Cmpt* output = _M_cmpts._M_impl->end();

      if (orig_type != _Type::_Multi)
	{
	  // Create single component from original path
	  auto ptr = ::new(output++) _Cmpt({}, orig_type, 0);
	  ++_M_cmpts._M_impl->_M_size;
	  ptr->_M_pathname.reserve(_M_pathname.length() + extra.length());
	  ptr->_M_pathname = _M_pathname;
	  ptr->_M_pathname += extra;

#if SLASHSLASH_IS_ROOTNAME
	  if (orig_type == _Type::_Root_name)
	    {
	      basic_string_view<value_type> s(p._M_pathname);
	      ::new(output++) _Cmpt(s.substr(extra.length(), 1),
		  _Type::_Root_dir, orig_pathlen + extra.length());
	      ++_M_cmpts._M_impl->_M_size;
	    }
#endif
	}
      else if (orig_filenamelen == 0 && it != last)
	{
	  // Remove empty filename at end of original path.
	  _M_cmpts.pop_back();
	  --output;
	}

      if (it != last && it->_M_type() == _Type::_Root_name)
	{
	  basic_string_view<value_type> s = it->_M_pathname;
	  auto pos = orig_pathlen;
#if SLASHSLASH_IS_ROOTNAME
	  s.remove_prefix(2);
	  pos += 2;
#endif
	  ::new(output++) _Cmpt(s, _Type::_Filename, pos);
	  ++_M_cmpts._M_impl->_M_size;
	  ++it;
	}

      if (it != last && it->_M_type() == _Type::_Root_dir)
	++it;

      while (it != last)
	{
	  auto pos = it->_M_pos + orig_pathlen;
	  ::new(output++) _Cmpt(it->_M_pathname, _Type::_Filename, pos);
	  ++_M_cmpts._M_impl->_M_size;
	  ++it;
	}

      _M_pathname += p._M_pathname;

      if (is_dir_sep(_M_pathname.back()))
	{
	  ::new(output++) _Cmpt({}, _Type::_Filename, _M_pathname.length());
	  ++_M_cmpts._M_impl->_M_size;
	}
      }
  __catch (...)
    {
      _M_pathname.resize(orig_pathlen);
      if (orig_type == _Type::_Multi)
	{
	  if (_M_cmpts.size() > orig_size)
	    _M_cmpts._M_erase_from(_M_cmpts.begin() + orig_size);
	  if (orig_filenamelen != -1)
	    {
	      if (_M_cmpts.size() == orig_size)
		{
		  auto& back = _M_cmpts.back();
		  back._M_pathname.resize(orig_filenamelen);
		  if (orig_filenamelen == 0)
		    back._M_pos = orig_pathlen;
		}
	      else
		{
		  auto output = _M_cmpts._M_impl->end();
		  ::new(output) _Cmpt({}, _Type::_Filename, orig_pathlen);
		  ++_M_cmpts._M_impl->_M_size;
		}
	    }
	}
      else
	_M_cmpts.clear();
      _M_cmpts.type(orig_type);
      __throw_exception_again;
    }
  return *this;
}

// [fs.path.concat]
void
path::_M_concat(basic_string_view<value_type> s)
{
  if (s.empty())
    return;

  if (this->empty())
    {
      operator=(s);
      return;
    }

#if _GLIBCXX_FILESYSTEM_IS_WINDOWS
  if (_M_type() == _Type::_Root_name
      || (_M_type() == _Type::_Filename && _M_pathname.size() == 1))
    {
      // Handle path("C") += ":" and path("C:") += "/x"
      // FIXME: do this more efficiently
      *this = path(_M_pathname + string_type(s));
      return;
    }
#endif
#if SLASHSLASH_IS_ROOTNAME
  if (_M_type() == _Type::_Root_dir)
    {
      // Handle path("/") += "/x" and path("//") += "x"
      // FIXME: do this more efficiently
      *this = path(_M_pathname + string_type(s));
      return;
    }
#endif

  const auto orig_pathlen = _M_pathname.length();
  const auto orig_type = _M_type();
  const auto orig_size = _M_cmpts.size();
  int orig_filenamelen = -1;
  basic_string_view<value_type> extra;

  // Copy the input into _M_pathname:
  _M_pathname += s;
  // Update s to refer to the new copy (this ensures s is not a dangling
  // reference to deallocated characters, in the case where it was referring
  // into _M_pathname or a member of _M_cmpts).
  s = _M_pathname;
  const auto orig_pathname = s.substr(0, orig_pathlen);
  s.remove_prefix(orig_pathlen);

  _Parser parser(s, orig_pathlen);
  auto cmpt = parser.next();

  if (cmpt.str.data() == s.data())
    {
      // See if there's a filename or root-name at the end of the original path
      // that we can add to.
      if (_M_type() == _Type::_Filename
#if SLASHSLASH_IS_ROOTNAME
	  || _M_type() == _Type::_Root_name
#endif
	  )
	{
	  if (cmpt.str.length() == s.length())
	    {
	      // Simplest case where we just need to add the whole of s
	      // to the original path, which was already done above.
	      return;
	    }
	  // Only the first component of s should be appended, do so below:
	  extra = cmpt.str;
	  cmpt = {}; // so we don't process it again
	}
      else if (_M_type() == _Type::_Multi
	  && _M_cmpts.back()._M_type() == _Type::_Filename)
	{
	  auto& back = _M_cmpts.back();
	  if (cmpt.str.length() == s.length())
	    {
	      back._M_pathname += s;
	      return;
	    }

	  orig_filenamelen = back._M_pathname.length();
	  back._M_pathname += cmpt.str;
	  extra = cmpt.str;
	  cmpt = {};
	}
    }
  else if (is_dir_sep(orig_pathname.back()) && _M_type() == _Type::_Multi
      && _M_cmpts.back()._M_type() == _Type::_Filename)
    orig_filenamelen = 0; // original path had empty filename at end

  std::array<_Parser::cmpt, 64> buf;
  auto next = buf.begin();

  if (cmpt.valid())
    *next++ = cmpt;

  cmpt = parser.next();
  while (cmpt.valid() && next != buf.end())
    {
      *next++ = cmpt;
      cmpt = parser.next();
    }

  int capacity = 0;
  if (_M_type() == _Type::_Multi)
    capacity += _M_cmpts.size();
  else
    capacity += 1;

  capacity += next - buf.begin();

  if (cmpt.valid()) // filled buffer before parsing whole input
    {
      ++capacity;
      _Parser parser2(parser);
      while (parser2.next().valid())
	++capacity;
    }

#if SLASHSLASH_IS_ROOTNAME
  if (orig_type == _Type::_Root_name)
    ++capacity; // Need to insert root-directory after root-name
#endif

  __try
    {
      _M_cmpts.type(_Type::_Multi);
      _M_cmpts.reserve(capacity);
      _Cmpt* output = _M_cmpts._M_impl->end();
      auto it = buf.begin();

      if (orig_type != _Type::_Multi)
	{
	  // Create single component from original path
	  auto p = ::new(output++) _Cmpt({}, orig_type, 0);
	  ++_M_cmpts._M_impl->_M_size;
	  p->_M_pathname.reserve(orig_pathname.length() + extra.length());
	  p->_M_pathname = orig_pathname;
	  p->_M_pathname += extra;

#if SLASHSLASH_IS_ROOTNAME
	  if (orig_type == _Type::_Root_name)
	    {
	      ::new(output++) _Cmpt(s.substr(extra.length(), 1),
		  _Type::_Root_dir, orig_pathlen + extra.length());
	      ++_M_cmpts._M_impl->_M_size;
	    }
#endif
	}
      else if (orig_filenamelen == 0 && extra.empty())
	{
	  // Replace empty filename at end of original path.
	  std::prev(output)->_M_pathname = it->str;
	  std::prev(output)->_M_pos = parser.offset(*it);
	  ++it;
	}

      while (it != next)
	{
	  ::new(output++) _Cmpt(it->str, _Type::_Filename, parser.offset(*it));
	  ++_M_cmpts._M_impl->_M_size;
	  ++it;
	}

      if (next == buf.end())
	{
	  while (cmpt.valid())
	    {
	      auto pos = parser.offset(cmpt);
	      ::new(output++) _Cmpt(cmpt.str, _Type::_Filename, pos);
	      ++_M_cmpts._M_impl->_M_size;
	      cmpt = parser.next();
	    }
	}
    }
  __catch (...)
    {
      _M_pathname.resize(orig_pathlen);
      if (orig_type == _Type::_Multi)
	{
	  _M_cmpts._M_erase_from(_M_cmpts.begin() + orig_size);
	  if (orig_filenamelen != -1)
	    {
	      auto& back = _M_cmpts.back();
	      back._M_pathname.resize(orig_filenamelen);
	      if (orig_filenamelen == 0)
		back._M_pos = orig_pathlen;
	    }
	}
      else
	_M_cmpts.clear();
      _M_cmpts.type(orig_type);
      __throw_exception_again;
    }
}

path&
path::remove_filename()
{
  if (_M_type() == _Type::_Multi)
    {
      if (!_M_cmpts.empty())
	{
	  auto cmpt = std::prev(_M_cmpts.end());
	  if (cmpt->_M_type() == _Type::_Filename && !cmpt->empty())
	    {
	      _M_pathname.erase(cmpt->_M_pos);
	      auto prev = std::prev(cmpt);
	      if (prev->_M_type() == _Type::_Root_dir
		  || prev->_M_type() == _Type::_Root_name)
		{
		  _M_cmpts.pop_back();
		  if (_M_cmpts.size() == 1)
		    {
		      _M_cmpts.type(_M_cmpts.front()._M_type());
		      _M_cmpts.clear();
		    }
		}
	      else
		cmpt->clear();
	    }
	}
    }
  else if (_M_type() == _Type::_Filename)
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
  // Any existing extension() is removed
  if (ext.first && ext.second != string_type::npos)
    {
      if (ext.first == &_M_pathname)
	_M_pathname.erase(ext.second);
      else
	{
	  auto& back = _M_cmpts.back();
	  __glibcxx_assert( ext.first == &back._M_pathname );
	  back._M_pathname.erase(ext.second);
	  _M_pathname.erase(back._M_pos + ext.second);
	}
    }
   // If replacement is not empty and does not begin with a dot character,
   // a dot character is appended
  if (!replacement.empty() && replacement.native()[0] != dot)
    operator+=(".");
  operator+=(replacement);
  return *this;
}

int
path::compare(const path& p) const noexcept
{
  if (_M_pathname == p._M_pathname)
    return 0;

  basic_string_view<value_type> lroot, rroot;
  if (_M_type() == _Type::_Root_name)
    lroot = _M_pathname;
  else if (_M_type() == _Type::_Multi
      && _M_cmpts.front()._M_type() == _Type::_Root_name)
    lroot = _M_cmpts.front()._M_pathname;
  if (p._M_type() == _Type::_Root_name)
    rroot = p._M_pathname;
  else if (p._M_type() == _Type::_Multi
      && p._M_cmpts.front()._M_type() == _Type::_Root_name)
    rroot = p._M_cmpts.front()._M_pathname;
  if (int rootNameComparison = lroot.compare(rroot))
    return rootNameComparison;

  if (!this->has_root_directory() && p.has_root_directory())
    return -1;
  else if (this->has_root_directory() && !p.has_root_directory())
    return +1;

  using Iterator = const _Cmpt*;
  Iterator begin1, end1, begin2, end2;
  if (_M_type() == _Type::_Multi)
    {
      begin1 = _M_cmpts.begin();
      end1 = _M_cmpts.end();
      // Find start of this->relative_path()
      while (begin1 != end1 && begin1->_M_type() != _Type::_Filename)
	++begin1;
    }
  else
    begin1 = end1 = nullptr;

  if (p._M_type() == _Type::_Multi)
    {
      begin2 = p._M_cmpts.begin();
      end2 = p._M_cmpts.end();
      // Find start of p.relative_path()
      while (begin2 != end2 && begin2->_M_type() != _Type::_Filename)
	++begin2;
    }
  else
    begin2 = end2 = nullptr;

  if (_M_type() == _Type::_Filename)
    {
      if (p._M_type() == _Type::_Filename)
	return native().compare(p.native());
      else if (begin2 != end2)
	{
	  if (int ret = native().compare(begin2->native()))
	    return ret;
	  else
	    return ++begin2 == end2 ? 0 : -1;
	}
      else
	return +1;
    }
  else if (p._M_type() == _Type::_Filename)
    {
      if (begin1 != end1)
	{
	  if (int ret = begin1->native().compare(p.native()))
	    return ret;
	  else
	    return ++begin1 == end1 ? 0 : +1;
	}
      else
	return -1;
    }

  int count = 1;
  while (begin1 != end1 && begin2 != end2)
    {
      if (int i = begin1->native().compare(begin2->native()))
	return i;
      ++begin1;
      ++begin2;
      ++count;
    }
  if (begin1 == end1)
    {
      if (begin2 == end2)
	return 0;
      return -count;
    }
  return count;
}

int
path::compare(basic_string_view<value_type> s) const noexcept
{
  if (_M_pathname == s)
    return 0;

  _Parser parser(s);

  basic_string_view<value_type> lroot, rroot;
  if (_M_type() == _Type::_Root_name)
    lroot = _M_pathname;
  else if (_M_type() == _Type::_Multi
      && _M_cmpts.front()._M_type() == _Type::_Root_name)
    lroot = _M_cmpts.front()._M_pathname;
  auto root_path = parser.root_path();
  if (root_path.first.type == _Type::_Root_name)
    rroot = root_path.first.str;
  if (int rootNameComparison = lroot.compare(rroot))
    return rootNameComparison;

  const bool has_root_dir = root_path.first.type == _Type::_Root_dir
    || root_path.second.type == _Type::_Root_dir;
  if (!this->has_root_directory() && has_root_dir)
    return -1;
  else if (this->has_root_directory() && !has_root_dir)
    return +1;

  using Iterator = const _Cmpt*;
  Iterator begin1, end1;
  if (_M_type() == _Type::_Filename)
    {
      auto cmpt = parser.next();
      if (cmpt.valid())
	{
	  if (int ret = this->native().compare(cmpt.str))
	    return ret;
	  return parser.next().valid() ? -1 : 0;
	}
      else
	return +1;
    }
  else if (_M_type() == _Type::_Multi)
    {
      begin1 = _M_cmpts.begin();
      end1 = _M_cmpts.end();
      while (begin1 != end1 && begin1->_M_type() != _Type::_Filename)
	++begin1;
    }
  else
    begin1 = end1 = nullptr;

  int count = 1;
  auto cmpt = parser.next();
  while (begin1 != end1 && cmpt.valid())
    {
      if (int i = begin1->native().compare(cmpt.str))
	return i;
      ++begin1;
      cmpt = parser.next();
      ++count;
    }
  if (begin1 == end1)
    {
      if (!cmpt.valid())
	return 0;
      return -count;
    }
  return +count;
}

path
path::root_name() const
{
  path __ret;
  if (_M_type() == _Type::_Root_name)
    __ret = *this;
  else if (_M_cmpts.size() && _M_cmpts.begin()->_M_type() == _Type::_Root_name)
    __ret = *_M_cmpts.begin();
  return __ret;
}

path
path::root_directory() const
{
  path __ret;
  if (_M_type() == _Type::_Root_dir)
    {
      __ret._M_cmpts.type(_Type::_Root_dir);
      __ret._M_pathname.assign(1, preferred_separator);
    }
  else if (!_M_cmpts.empty())
    {
      auto __it = _M_cmpts.begin();
      if (__it->_M_type() == _Type::_Root_name)
        ++__it;
      if (__it != _M_cmpts.end() && __it->_M_type() == _Type::_Root_dir)
        __ret = *__it;
    }
  return __ret;
}

path
path::root_path() const
{
  path __ret;
  if (_M_type() == _Type::_Root_name)
    __ret = *this;
  else if (_M_type() == _Type::_Root_dir)
    {
      __ret._M_pathname.assign(1, preferred_separator);
      __ret._M_cmpts.type(_Type::_Root_dir);
    }
  else if (!_M_cmpts.empty())
    {
      auto __it = _M_cmpts.begin();
      if (__it->_M_type() == _Type::_Root_name)
        {
          __ret = *__it++;
          if (__it != _M_cmpts.end() && __it->_M_type() == _Type::_Root_dir)
	    __ret /= *__it;
        }
      else if (__it->_M_type() == _Type::_Root_dir)
        __ret = *__it;
    }
  return __ret;
}

path
path::relative_path() const
{
  path __ret;
  if (_M_type() == _Type::_Filename)
    __ret = *this;
  else if (!_M_cmpts.empty())
    {
      auto __it = _M_cmpts.begin();
      if (__it->_M_type() == _Type::_Root_name)
        ++__it;
      if (__it != _M_cmpts.end() && __it->_M_type() == _Type::_Root_dir)
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
      const auto parent = std::prev(_M_cmpts.end(), 2);
      const auto len = parent->_M_pos + parent->_M_pathname.length();
      __ret.assign(_M_pathname.substr(0, len));
    }
  return __ret;
}

bool
path::has_root_name() const noexcept
{
  if (_M_type() == _Type::_Root_name)
    return true;
  if (!_M_cmpts.empty() && _M_cmpts.begin()->_M_type() == _Type::_Root_name)
    return true;
  return false;
}

bool
path::has_root_directory() const noexcept
{
  if (_M_type() == _Type::_Root_dir)
    return true;
  if (!_M_cmpts.empty())
    {
      auto __it = _M_cmpts.begin();
      if (__it->_M_type() == _Type::_Root_name)
        ++__it;
      if (__it != _M_cmpts.end() && __it->_M_type() == _Type::_Root_dir)
        return true;
    }
  return false;
}

bool
path::has_root_path() const noexcept
{
  if (_M_type() == _Type::_Root_name || _M_type() == _Type::_Root_dir)
    return true;
  if (!_M_cmpts.empty())
    {
      auto __type = _M_cmpts.front()._M_type();
      if (__type == _Type::_Root_name || __type == _Type::_Root_dir)
        return true;
    }
  return false;
}

bool
path::has_relative_path() const noexcept
{
  if (_M_type() == _Type::_Filename && !_M_pathname.empty())
    return true;
  if (!_M_cmpts.empty())
    {
      auto __it = _M_cmpts.begin();
      if (__it->_M_type() == _Type::_Root_name)
        ++__it;
      if (__it != _M_cmpts.end() && __it->_M_type() == _Type::_Root_dir)
        ++__it;
      if (__it != _M_cmpts.end() && !__it->_M_pathname.empty())
        return true;
    }
  return false;
}


bool
path::has_parent_path() const noexcept
{
  if (!has_relative_path())
    return !empty();
  return _M_cmpts.size() >= 2;
}

bool
path::has_filename() const noexcept
{
  if (empty())
    return false;
  if (_M_type() == _Type::_Filename)
    return !_M_pathname.empty();
  if (_M_type() == _Type::_Multi)
    {
      if (_M_pathname.back() == preferred_separator)
	return false;
      return _M_cmpts.back().has_filename();
    }
  return false;
}

namespace
{
  inline bool is_dot(fs::path::value_type c) { return c == dot; }

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
  - If there is a root-directory, remove all dot-dot filenames and any
    directory-separators immediately following them.
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
      if (p._M_type() == _Type::_Root_name || p._M_type() == _Type::_Root_dir)
	{
	  string_type s = p.native();
	  std::replace(s.begin(), s.end(), L'/', L'\\');
	  ret /= s;
	  continue;
	}
#endif
      if (is_dotdot(p))
	{
	  if (ret.has_filename())
	    {
	      // remove a non-dot-dot filename immediately followed by /..
	      if (!is_dotdot(ret.filename()))
		ret.remove_filename();
	      else
		ret /= p;
	    }
	  else if (!ret.has_relative_path())
	    {
	      // remove a dot-dot filename immediately after root-directory
	      if (!ret.has_root_directory())
		ret /= p;
	    }
	  else
	    {
	      // Got a path with a relative path (i.e. at least one non-root
	      // element) and no filename at the end (i.e. empty last element),
	      // so must have a trailing slash. See what is before it.
	      auto elem = ret._M_cmpts.end() - 2;
	      if (elem->has_filename() && !is_dotdot(*elem))
		{
		  // Remove the filename before the trailing slash
		  // (equiv. to ret = ret.parent_path().remove_filename())

		  if (elem == ret._M_cmpts.begin())
		    ret.clear();
		  else
		    {
		      ret._M_pathname.erase(elem->_M_pos);
		      // Remove empty filename at the end:
		      ret._M_cmpts.pop_back();
		      // If we still have a trailing non-root dir separator
		      // then leave an empty filename at the end:
		      if (std::prev(elem)->_M_type() == _Type::_Filename)
			elem->clear();
		      else // remove the component completely:
			ret._M_cmpts.pop_back();
		    }
		}
	      else
		// Append the ".." to something ending in "../" which happens
		// when normalising paths like ".././.." and "../a/../.."
		ret /= p;
	    }
	}
      else if (is_dot(p))
	ret /= path();
#if SLASHSLASH_IS_ROOTNAME
      else if (p._M_type() == _Type::_Root_dir)
	ret += '/'; // using operator/=('/') would replace whole of ret
#endif
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
#ifdef _GLIBCXX_FILESYSTEM_IS_WINDOWS
  // _GLIBCXX_RESOLVE_LIB_DEFECTS
  // 3070. path::lexically_relative causes surprising results if a filename
  // can also be a root-name
  if (!empty())
    for (auto& p : _M_cmpts)
      if (p._M_type() == _Type::_Filename && is_disk_designator(p.native()))
	return ret;
  if (!base.empty())
    for (auto i = b, end = base.end(); i != end; ++i)
      if (i->_M_type() == _Type::_Filename && is_disk_designator(i->native()))
	return ret;
#endif
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
      else if (!p.empty() && !is_dot(p))
	++n;
    }
    if (n == 0 && (a == end() || a->empty()))
      ret = ".";
    else if (n >= 0)
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
path::_M_find_extension() const noexcept
{
  const string_type* s = nullptr;

  if (_M_type() == _Type::_Filename)
    s = &_M_pathname;
  else if (_M_type() == _Type::_Multi && !_M_cmpts.empty())
    {
      const auto& c = _M_cmpts.back();
      if (c._M_type() == _Type::_Filename)
	s = &c._M_pathname;
    }

  if (s)
    {
      if (auto sz = s->size())
	{
	  if (sz <= 2 && (*s)[0] == dot)
	    return { s, string_type::npos };
	  if (const auto pos = s->rfind(dot))
	    return { s , pos };
	  return { s, string_type::npos };
	}
    }
  return {};
}

void
path::_M_split_cmpts()
{
  _M_cmpts.clear();

  if (_M_pathname.empty())
    {
      _M_cmpts.type(_Type::_Filename);
      return;
    }

  _Parser parser(_M_pathname);

  std::array<_Parser::cmpt, 64> buf;
  auto next = buf.begin();

  // look for root name or root directory
  auto root_path = parser.root_path();
  if (root_path.first.valid())
    {
      *next++ = root_path.first;
      if (root_path.second.valid())
	*next++ = root_path.second;
    }

  auto cmpt = parser.next();
  while (cmpt.valid())
    {
      do
	{
	  *next++ = cmpt;
	  cmpt = parser.next();
	}
      while (cmpt.valid() && next != buf.end());

      if (next == buf.end())
	{
	  _M_cmpts.type(_Type::_Multi);
	  _M_cmpts.reserve(_M_cmpts.size() + buf.size());
	  auto output = _M_cmpts._M_impl->end();
	  for (const auto& c : buf)
	    {
	      ::new(output++) _Cmpt(c.str, c.type, parser.offset(c));
	      ++_M_cmpts._M_impl->_M_size;
	    }
	  next = buf.begin();
	}
    }

  if (auto n = next - buf.begin())
    {
      if (n == 1 && _M_cmpts.empty())
	{
	  _M_cmpts.type(buf.front().type);
	  return;
	}

      _M_cmpts.type(_Type::_Multi);
      _M_cmpts.reserve(_M_cmpts.size() + n, true);
      auto output = _M_cmpts._M_impl->end();
      for (int i = 0; i < n; ++i)
	{
	  const auto& c = buf[i];
	  ::new(output++) _Cmpt(c.str, c.type, parser.offset(c));
	  ++_M_cmpts._M_impl->_M_size;
	}
    }
}

path::string_type
path::_S_convert_loc(const char* __first, const char* __last,
		     [[maybe_unused]] const std::locale& __loc)
{
#if _GLIBCXX_USE_WCHAR_T
  auto& __cvt = std::use_facet<codecvt<wchar_t, char, mbstate_t>>(__loc);
  basic_string<wchar_t> __ws;
  if (!__str_codecvt_in_all(__first, __last, __ws, __cvt))
    _GLIBCXX_THROW_OR_ABORT(filesystem_error(
	  "Cannot convert character sequence",
	  std::make_error_code(errc::illegal_byte_sequence)));
  return _S_convert(std::move(__ws));
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

struct fs::filesystem_error::_Impl
{
  _Impl(string_view what_arg, const path& p1, const path& p2)
  : path1(p1), path2(p2), what(make_what(what_arg, &p1, &p2))
  { }

  _Impl(string_view what_arg, const path& p1)
  : path1(p1), path2(), what(make_what(what_arg, &p1, nullptr))
  { }

  _Impl(string_view what_arg)
  : what(make_what(what_arg, nullptr, nullptr))
  { }

  static std::string
  make_what(string_view s, const path* p1, const path* p2)
  {
    const std::string pstr1 = p1 ? p1->u8string() : std::string{};
    const std::string pstr2 = p2 ? p2->u8string() : std::string{};
    const size_t len = 18 + s.length()
      + (pstr1.length() ? pstr1.length() + 3 : 0)
      + (pstr2.length() ? pstr2.length() + 3 : 0);
    std::string w;
    w.reserve(len);
    w = "filesystem error: ";
    w += s;
    if (p1)
      {
	w += " [";
	w += pstr1;
	w += ']';
	if (p2)
	  {
	    w += " [";
	    w += pstr2;
	    w += ']';
	  }
      }
    return w;
  }

  path path1;
  path path2;
  std::string what;
};

template class std::__shared_ptr<const fs::filesystem_error::_Impl>;

fs::filesystem_error::
filesystem_error(const string& what_arg, error_code ec)
: system_error(ec, what_arg),
  _M_impl(std::__make_shared<_Impl>(system_error::what()))
{ }

fs::filesystem_error::
filesystem_error(const string& what_arg, const path& p1, error_code ec)
: system_error(ec, what_arg),
  _M_impl(std::__make_shared<_Impl>(system_error::what(), p1))
{ }

fs::filesystem_error::
filesystem_error(const string& what_arg, const path& p1, const path& p2,
		 error_code ec)
: system_error(ec, what_arg),
  _M_impl(std::__make_shared<_Impl>(system_error::what(), p1, p2))
{ }

fs::filesystem_error::~filesystem_error() = default;

const fs::path&
fs::filesystem_error::path1() const noexcept
{ return _M_impl->path1; }

const fs::path&
fs::filesystem_error::path2() const noexcept
{ return _M_impl->path2; }

const char*
fs::filesystem_error::what() const noexcept
{ return _M_impl->what.c_str(); }
