// Class filesystem::path -*- C++ -*-

// Copyright (C) 2014-2015 Free Software Foundation, Inc.
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

/** @file experimental/fs_path.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{experimental/filesystem}
 */

#ifndef _GLIBCXX_EXPERIMENTAL_FS_PATH_H
#define _GLIBCXX_EXPERIMENTAL_FS_PATH_H 1

#if __cplusplus < 201103L
# include <bits/c++0x_warning.h>
#else

#include <utility>
#include <type_traits>
#include <vector>
#include <locale>
#include <iosfwd>
#include <codecvt>
#include <system_error>
#include <bits/stl_algobase.h>
#include <bits/quoted_string.h>
#include <bits/locale_conv.h>
#if __cplusplus >= 201402L
# include <experimental/string_view>
#endif

#if defined(_WIN32) && !defined(__CYGWIN__)
# define _GLIBCXX_FILESYSTEM_IS_WINDOWS 1
# include <algorithm>
#endif

namespace std _GLIBCXX_VISIBILITY(default)
{
namespace experimental
{
namespace filesystem
{
inline namespace v1
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
_GLIBCXX_BEGIN_NAMESPACE_CXX11

#if __cplusplus >= 201402L
  template<typename _CharT, typename _Traits = std::char_traits<_CharT>>
    using __basic_string_view
      = std::experimental::basic_string_view<_CharT, _Traits>;
#endif

  /**
   * @ingroup filesystem
   * @{
   */

  /// A filesystem path.
  class path
  {
    template<typename _CharT>
      struct __is_encoded_char : std::false_type { };

    template<typename _Iter,
	     typename _Iter_traits = std::iterator_traits<_Iter>>
      using __is_path_iter_src
	= __and_<__is_encoded_char<typename _Iter_traits::value_type>,
		 std::is_base_of<std::input_iterator_tag,
				 typename _Iter_traits::iterator_category>>;

    template<typename _Iter>
      static __is_path_iter_src<_Iter>
      __is_path_src(_Iter, int);

    template<typename _CharT, typename _Traits, typename _Alloc>
      static __is_encoded_char<_CharT>
      __is_path_src(const basic_string<_CharT, _Traits, _Alloc>&, int);

#if __cplusplus >= 201402L
    template<typename _CharT, typename _Traits>
      static __is_encoded_char<_CharT>
      __is_path_src(const __basic_string_view<_CharT, _Traits>&, int);
#endif

    template<typename _Unknown>
      static std::false_type
      __is_path_src(const _Unknown&, ...);

    template<typename _Tp1, typename _Tp2>
      struct __constructible_from;

    template<typename _Iter>
      struct __constructible_from<_Iter, _Iter>
      : __is_path_iter_src<_Iter>
      { };

    template<typename _Source>
      struct __constructible_from<_Source, void>
      : decltype(__is_path_src(std::declval<_Source>(), 0))
      { };

    template<typename _Tp1, typename _Tp2 = void>
      using _Path = typename
	std::enable_if<__and_<__not_<is_same<_Tp1, path>>,
			      __constructible_from<_Tp1, _Tp2>>::value,
		       path>::type;

    template<typename _Source>
      static _Source
      _S_range_begin(_Source __begin) { return __begin; }

    struct __null_terminated { };

    template<typename _Source>
      static __null_terminated
      _S_range_end(_Source) { return {}; }

    template<typename _CharT, typename _Traits, typename _Alloc>
      static const _CharT*
      _S_range_begin(const basic_string<_CharT, _Traits, _Alloc>& __str)
      { return __str.data(); }

    template<typename _CharT, typename _Traits, typename _Alloc>
      static const _CharT*
      _S_range_end(const basic_string<_CharT, _Traits, _Alloc>& __str)
      { return __str.data() + __str.size(); }

#if __cplusplus >= 201402L
    template<typename _CharT, typename _Traits>
      static const _CharT*
      _S_range_begin(const __basic_string_view<_CharT, _Traits>& __str)
      { return __str.data(); }

    template<typename _CharT, typename _Traits>
      static const _CharT*
      _S_range_end(const __basic_string_view<_CharT, _Traits>& __str)
      { return __str.data() + __str.size(); }
#endif

    template<typename _Tp,
	     typename _Iter = decltype(_S_range_begin(std::declval<_Tp>())),
	     typename _Val = typename std::iterator_traits<_Iter>::value_type>
      using __value_type_is_char
	= typename std::enable_if<std::is_same<_Val, char>::value>::type;

  public:
#ifdef _GLIBCXX_FILESYSTEM_IS_WINDOWS
    typedef wchar_t				value_type;
    static constexpr value_type			preferred_separator = L'\\';
#else
    typedef char				value_type;
    static constexpr value_type			preferred_separator = '/';
#endif
    typedef std::basic_string<value_type>	string_type;

    // constructors and destructor

    path() noexcept { }

    path(const path& __p) = default;

    path(path&& __p) noexcept
    : _M_pathname(std::move(__p._M_pathname)), _M_type(__p._M_type)
    {
      _M_split_cmpts();
      __p.clear();
    }

    path(string_type&& __source)
    : _M_pathname(std::move(__source))
    { _M_split_cmpts(); }

    template<typename _Source,
	     typename _Require = _Path<_Source>>
      path(_Source const& __source)
      : _M_pathname(_S_convert(_S_range_begin(__source),
			       _S_range_end(__source)))
      { _M_split_cmpts(); }

    template<typename _InputIterator,
	     typename _Require = _Path<_InputIterator, _InputIterator>>
      path(_InputIterator __first, _InputIterator __last)
      : _M_pathname(_S_convert(__first, __last))
      { _M_split_cmpts(); }

    template<typename _Source,
	     typename _Require = _Path<_Source>,
	     typename _Require2 = __value_type_is_char<_Source>>
      path(_Source const& __source, const locale& __loc)
      : _M_pathname(_S_convert_loc(_S_range_begin(__source),
				   _S_range_end(__source), __loc))
      { _M_split_cmpts(); }

    template<typename _InputIterator,
	     typename _Require = _Path<_InputIterator, _InputIterator>,
	     typename _Require2 = __value_type_is_char<_InputIterator>>
      path(_InputIterator __first, _InputIterator __last, const locale& __loc)
      : _M_pathname(_S_convert_loc(__first, __last, __loc))
      { _M_split_cmpts(); }

    ~path() = default;

    // assignments

    path& operator=(const path& __p) = default;
    path& operator=(path&& __p) noexcept;
    path& operator=(string_type&& __source);
    path& assign(string_type&& __source);

    template<typename _Source>
      _Path<_Source>&
      operator=(_Source const& __source)
      { return *this = path(__source); }

    template<typename _Source>
      _Path<_Source>&
      assign(_Source const& __source)
      { return *this = path(__source); }

    template<typename _InputIterator>
      _Path<_InputIterator, _InputIterator>&
      assign(_InputIterator __first, _InputIterator __last)
      { return *this = path(__first, __last); }

    // appends

    path& operator/=(const path& __p) { return _M_append(__p._M_pathname); }

    template <class _Source>
      _Path<_Source>&
      operator/=(_Source const& __source)
      { return append(__source); }

    template<typename _Source>
      _Path<_Source>&
      append(_Source const& __source)
      {
	return _M_append(_S_convert(_S_range_begin(__source),
				    _S_range_end(__source)));
      }

    template<typename _InputIterator>
      _Path<_InputIterator, _InputIterator>&
      append(_InputIterator __first, _InputIterator __last)
      { return _M_append(_S_convert(__first, __last)); }

    // concatenation

    path& operator+=(const path& __x);
    path& operator+=(const string_type& __x);
    path& operator+=(const value_type* __x);
    path& operator+=(value_type __x);
#if __cplusplus >= 201402L
    path& operator+=(__basic_string_view<value_type> __x);
#endif

    template<typename _Source>
      _Path<_Source>&
      operator+=(_Source const& __x) { return concat(__x); }

    template<typename _CharT>
      _Path<_CharT*, _CharT*>&
      operator+=(_CharT __x);

    template<typename _Source>
      _Path<_Source>&
      concat(_Source const& __x)
      { return *this += _S_convert(_S_range_begin(__x), _S_range_end(__x)); }

    template<typename _InputIterator>
      _Path<_InputIterator, _InputIterator>&
      concat(_InputIterator __first, _InputIterator __last)
      { return *this += _S_convert(__first, __last); }

    // modifiers

    void clear() noexcept { _M_pathname.clear(); _M_split_cmpts(); }

    path& make_preferred();
    path& remove_filename();
    path& replace_filename(const path& __replacement);
    path& replace_extension(const path& __replacement = path());

    void swap(path& __rhs) noexcept;

    // native format observers

    const string_type&  native() const noexcept { return _M_pathname; }
    const value_type*   c_str() const noexcept { return _M_pathname.c_str(); }
    operator string_type() const { return _M_pathname; }

    template<typename _CharT, typename _Traits = std::char_traits<_CharT>,
	     typename _Allocator = std::allocator<_CharT>>
      std::basic_string<_CharT, _Traits, _Allocator>
      string(const _Allocator& __a = _Allocator()) const;

    std::string    string() const;
#if _GLIBCXX_USE_WCHAR_T
    std::wstring   wstring() const;
#endif
    std::string    u8string() const;
    std::u16string u16string() const;
    std::u32string u32string() const;

    // generic format observers
    template<typename _CharT, typename _Traits = std::char_traits<_CharT>,
	     typename _Allocator = std::allocator<_CharT>>
      std::basic_string<_CharT, _Traits, _Allocator>
      generic_string(const _Allocator& __a = _Allocator()) const;

    std::string    generic_string() const;
#if _GLIBCXX_USE_WCHAR_T
    std::wstring   generic_wstring() const;
#endif
    std::string    generic_u8string() const;
    std::u16string generic_u16string() const;
    std::u32string generic_u32string() const;

    // compare

    int compare(const path& __p) const noexcept;
    int compare(const string_type& __s) const;
    int compare(const value_type* __s) const;
#if __cplusplus >= 201402L
    int compare(const __basic_string_view<value_type> __s) const;
#endif

    // decomposition

    path root_name() const;
    path root_directory() const;
    path root_path() const;
    path relative_path() const;
    path parent_path() const;
    path filename() const;
    path stem() const;
    path extension() const;

    // query

    bool empty() const noexcept { return _M_pathname.empty(); }
    bool has_root_name() const;
    bool has_root_directory() const;
    bool has_root_path() const;
    bool has_relative_path() const;
    bool has_parent_path() const;
    bool has_filename() const;
    bool has_stem() const;
    bool has_extension() const;
    bool is_absolute() const;
    bool is_relative() const { return !is_absolute(); }

    // iterators
    class iterator;
    typedef iterator const_iterator;

    iterator begin() const;
    iterator end() const;

  private:
    enum class _Type : unsigned char {
	_Multi, _Root_name, _Root_dir, _Filename
    };

    path(string_type __str, _Type __type) : _M_pathname(__str), _M_type(__type)
    {
      _GLIBCXX_DEBUG_ASSERT(!empty());
      _GLIBCXX_DEBUG_ASSERT(_M_type != _Type::_Multi);
    }

    enum class _Split { _Stem, _Extension };

    path& _M_append(const string_type& __str)
    {
      if (!_M_pathname.empty() && !_S_is_dir_sep(_M_pathname.back())
	  && !__str.empty() && !_S_is_dir_sep(__str.front()))
	_M_pathname += preferred_separator;
      _M_pathname += __str;
      _M_split_cmpts();
      return *this;
    }

    pair<const string_type*, size_t> _M_find_extension() const;

    template<typename _CharT>
      struct _Cvt;

    static string_type
    _S_convert(value_type* __src, __null_terminated)
    { return string_type(__src); }

    static string_type
    _S_convert(const value_type* __src, __null_terminated)
    { return string_type(__src); }

    template<typename _Iter>
      static string_type
      _S_convert(_Iter __first, _Iter __last)
      {
	using __value_type = typename std::iterator_traits<_Iter>::value_type;
	return _Cvt<typename remove_cv<__value_type>::type>::
	  _S_convert(__first, __last);
      }

    template<typename _InputIterator>
      static string_type
      _S_convert(_InputIterator __src, __null_terminated)
      {
	using _Tp = typename std::iterator_traits<_InputIterator>::value_type;
	std::basic_string<typename remove_cv<_Tp>::type> __tmp;
	for (; *__src != _Tp{}; ++__src)
	  __tmp.push_back(*__src);
	return _S_convert(__tmp.c_str(), __tmp.c_str() + __tmp.size());
      }

    static string_type
    _S_convert_loc(const char* __first, const char* __last,
		   const std::locale& __loc);

    template<typename _Iter>
      static string_type
      _S_convert_loc(_Iter __first, _Iter __last, const std::locale& __loc)
      {
	const std::string __str(__first, __last);
	return _S_convert_loc(__str.data(), __str.data()+__str.size(), __loc);
      }

    template<typename _InputIterator>
      static string_type
      _S_convert_loc(_InputIterator __src, __null_terminated,
		     const std::locale& __loc)
      {
	std::string __tmp;
	while (*__src != '\0')
	  __tmp.push_back(*__src++);
	return _S_convert_loc(__tmp.data(), __tmp.data()+__tmp.size(), __loc);
      }

    bool _S_is_dir_sep(value_type __ch)
    {
#ifdef _GLIBCXX_FILESYSTEM_IS_WINDOWS
      return __ch == L'/' || __ch == preferred_separator;
#else
      return __ch == '/';
#endif
    }

    void _M_split_cmpts();
    void _M_trim();
    void _M_add_root_name(size_t __n);
    void _M_add_root_dir(size_t __pos);
    void _M_add_filename(size_t __pos, size_t __n);

    string_type _M_pathname;

    struct _Cmpt;
    using _List = _GLIBCXX_STD_C::vector<_Cmpt>;
    _List _M_cmpts; // empty unless _M_type == _Type::_Multi
    _Type _M_type = _Type::_Multi;
  };

  inline void swap(path& __lhs, path& __rhs) noexcept { __lhs.swap(__rhs); }

  size_t hash_value(const path& __p) noexcept;

  /// Compare paths
  inline bool operator<(const path& __lhs, const path& __rhs) noexcept
  { return __lhs.compare(__rhs) < 0; }

  /// Compare paths
  inline bool operator<=(const path& __lhs, const path& __rhs) noexcept
  { return !(__rhs < __lhs); }

  /// Compare paths
  inline bool operator>(const path& __lhs, const path& __rhs) noexcept
  { return __rhs < __lhs; }

  /// Compare paths
  inline bool operator>=(const path& __lhs, const path& __rhs) noexcept
  { return !(__lhs < __rhs); }

  /// Compare paths
  inline bool operator==(const path& __lhs, const path& __rhs) noexcept
  { return __lhs.compare(__rhs) == 0; }

  /// Compare paths
  inline bool operator!=(const path& __lhs, const path& __rhs) noexcept
  { return !(__lhs == __rhs); }

  /// Append one path to another
  inline path operator/(const path& __lhs, const path& __rhs)
  { return path(__lhs) /= __rhs; }

  /// Write a path to a stream
  template<typename _CharT, typename _Traits>
    basic_ostream<_CharT, _Traits>&
    operator<<(basic_ostream<_CharT, _Traits>& __os, const path& __p)
    {
      auto __tmp = __p.string<_CharT, _Traits>();
      using __quoted_string
	= std::__detail::_Quoted_string<decltype(__tmp)&, _CharT>;
      __os << __quoted_string{__tmp, '"', '\\'};
      return __os;
    }

  /// Read a path from a stream
  template<typename _CharT, typename _Traits>
    basic_istream<_CharT, _Traits>&
    operator>>(basic_istream<_CharT, _Traits>& __is, path& __p)
    {
      basic_string<_CharT, _Traits> __tmp;
      using __quoted_string
	= std::__detail::_Quoted_string<decltype(__tmp)&, _CharT>;
      if (__is >> __quoted_string{ __tmp, '"', '\\' })
	__p = std::move(__tmp);
      return __is;
    }

  // TODO constrain with _Path<Source> and __value_type_is_char
  template<typename _Source>
    inline path
    u8path(const _Source& __source)
    {
#ifdef _GLIBCXX_FILESYSTEM_IS_WINDOWS
      return path{ path::string_type{__source} };
#else
      return path{ __source };
#endif
    }

  // TODO constrain with _Path<InputIterator, InputIterator> and __value_type_is_char
  template<typename _InputIterator>
    inline path
    u8path(_InputIterator __first, _InputIterator __last)
    {
#ifdef _GLIBCXX_FILESYSTEM_IS_WINDOWS
      return path{ path::string_type{__first, __last} };
#else
      return path{ __first, __last };
#endif
    }

  class filesystem_error : public std::system_error
  {
  public:
    filesystem_error(const string& __what_arg, error_code __ec)
    : system_error(__ec, __what_arg) { }

    filesystem_error(const string& __what_arg, const path& __p1,
		     error_code __ec)
    : system_error(__ec, __what_arg), _M_path1(__p1) { }

    filesystem_error(const string& __what_arg, const path& __p1,
		     const path& __p2, error_code __ec)
    : system_error(__ec, __what_arg), _M_path1(__p1), _M_path2(__p2)
    { }

    ~filesystem_error();

    const path& path1() const noexcept { return _M_path1; }
    const path& path2() const noexcept { return _M_path2; }
    const char* what() const noexcept { return _M_what.c_str(); }

  private:
    std::string _M_gen_what();

    path _M_path1;
    path _M_path2;
    std::string _M_what = _M_gen_what();
  };

  template<>
    struct path::__is_encoded_char<char> : std::true_type
    { using value_type = char; };

  template<>
    struct path::__is_encoded_char<wchar_t> : std::true_type
    { using value_type = wchar_t; };

  template<>
    struct path::__is_encoded_char<char16_t> : std::true_type
    { using value_type = char16_t; };

  template<>
    struct path::__is_encoded_char<char32_t> : std::true_type
    { using value_type = char32_t; };

  template<typename _Tp>
    struct path::__is_encoded_char<const _Tp> : __is_encoded_char<_Tp> { };

  struct path::_Cmpt : path
  {
    _Cmpt(string_type __s, _Type __t, size_t __pos)
      : path(std::move(__s), __t), _M_pos(__pos) { }

    _Cmpt() : _M_pos(-1) { }

    size_t _M_pos;
  };

  // specialize _Cvt for degenerate 'noconv' case
  template<>
    struct path::_Cvt<path::value_type>
    {
      template<typename _Iter>
	static string_type
	_S_convert(_Iter __first, _Iter __last)
	{ return string_type{__first, __last}; }
    };

  template<typename _CharT>
    struct path::_Cvt
    {
#ifdef _GLIBCXX_FILESYSTEM_IS_WINDOWS
      static string_type
      _S_wconvert(const char* __f, const char* __l, true_type)
      {
	using _Cvt = std::codecvt<wchar_t, char, mbstate_t>;
	const auto& __cvt = std::use_facet<_Cvt>(std::locale{});
	std::wstring __wstr;
	if (__str_codecvt_in(__f, __l, __wstr, __cvt))
	    return __wstr;
	_GLIBCXX_THROW_OR_ABORT(filesystem_error(
	      "Cannot convert character sequence",
	      std::make_error_code(errc::illegal_byte_sequence)));
      }

      static string_type
      _S_wconvert(const _CharT* __f, const _CharT* __l, false_type)
      {
	std::codecvt_utf8<_CharT> __cvt;
	std::string __str;
	if (__str_codecvt_out(__f, __l, __str, __cvt))
	  {
	    const char* __f2 = __str.data();
	    const char* __l2 = __f2 + __str.size();
	    std::codecvt_utf8<wchar_t> __wcvt;
	    std::wstring __wstr;
	    if (__str_codecvt_in(__f2, __l2, __wstr, __wcvt))
	      return __wstr;
	  }
	_GLIBCXX_THROW_OR_ABORT(filesystem_error(
	      "Cannot convert character sequence",
	      std::make_error_code(errc::illegal_byte_sequence)));
      }

      static string_type
      _S_convert(const _CharT* __f, const _CharT* __l)
      {
	return _S_wconvert(__f, __l, is_same<_CharT, char>{});
      }
#else
      static string_type
      _S_convert(const _CharT* __f, const _CharT* __l)
      {
	std::codecvt_utf8<_CharT> __cvt;
	std::string __str;
	if (__str_codecvt_out(__f, __l, __str, __cvt))
	  return __str;
	_GLIBCXX_THROW_OR_ABORT(filesystem_error(
	      "Cannot convert character sequence",
	      std::make_error_code(errc::illegal_byte_sequence)));
      }
#endif

      static string_type
      _S_convert(_CharT* __f, _CharT* __l)
      {
	return _S_convert(const_cast<const _CharT*>(__f),
			  const_cast<const _CharT*>(__l));
      }

      template<typename _Iter>
	static string_type
	_S_convert(_Iter __first, _Iter __last)
	{
	  const std::basic_string<_CharT> __str(__first, __last);
	  return _S_convert(__str.data(), __str.data() + __str.size());
	}

      template<typename _Iter, typename _Cont>
	static string_type
	_S_convert(__gnu_cxx::__normal_iterator<_Iter, _Cont> __first,
		  __gnu_cxx::__normal_iterator<_Iter, _Cont> __last)
	{ return _S_convert(__first.base(), __last.base()); }
    };

  /// An iterator for the components of a path
  class path::iterator
  {
  public:
    using difference_type	= std::ptrdiff_t;
    using value_type		= path;
    using reference		= const path&;
    using pointer		= const path*;
    using iterator_category	= std::bidirectional_iterator_tag;

    iterator() : _M_path(nullptr), _M_cur(), _M_at_end() { }

    iterator(const iterator&) = default;
    iterator& operator=(const iterator&) = default;

    reference operator*() const;
    pointer   operator->() const { return std::__addressof(**this); }

    iterator& operator++();
    iterator  operator++(int) { auto __tmp = *this; ++_M_cur; return __tmp; }

    iterator& operator--();
    iterator  operator--(int) { auto __tmp = *this; --_M_cur; return __tmp; }

    friend bool operator==(const iterator& __lhs, const iterator& __rhs)
    { return __lhs._M_equals(__rhs); }

    friend bool operator!=(const iterator& __lhs, const iterator& __rhs)
    { return !__lhs._M_equals(__rhs); }

  private:
    friend class path;

    iterator(const path* __path, path::_List::const_iterator __iter)
    : _M_path(__path), _M_cur(__iter), _M_at_end()
    { }

    iterator(const path* __path, bool __at_end)
    : _M_path(__path), _M_cur(), _M_at_end(__at_end)
    { }

    bool _M_equals(iterator) const;

    const path* 		_M_path;
    path::_List::const_iterator _M_cur;
    bool			_M_at_end;  // only used when type != _Multi
  };


  inline path&
  path::operator=(path&& __p) noexcept
  {
    _M_pathname = std::move(__p._M_pathname);
    _M_cmpts = std::move(__p._M_cmpts);
    _M_type = __p._M_type;
    __p.clear();
    return *this;
  }

  inline path&
  path::operator=(string_type&& __source)
  { return *this = path(std::move(__source)); }

  inline path&
  path::assign(string_type&& __source)
  { return *this = path(std::move(__source)); }

  inline path&
  path::operator+=(const path& __p)
  {
    return operator+=(__p.native());
  }

  inline path&
  path::operator+=(const string_type& __x)
  {
    _M_pathname += __x;
    _M_split_cmpts();
    return *this;
  }

  inline path&
  path::operator+=(const value_type* __x)
  {
    _M_pathname += __x;
    _M_split_cmpts();
    return *this;
  }

  inline path&
  path::operator+=(value_type __x)
  {
    _M_pathname += __x;
    _M_split_cmpts();
    return *this;
  }

#if __cplusplus >= 201402L
  inline path&
  path::operator+=(__basic_string_view<value_type> __x)
  {
    _M_pathname.append(__x.data(), __x.size());
    _M_split_cmpts();
    return *this;
  }
#endif

  template<typename _CharT>
    inline path::_Path<_CharT*, _CharT*>&
    path::operator+=(_CharT __x)
    {
      auto* __addr = std::__addressof(__x);
      return concat(__addr, __addr + 1);
    }

  inline path&
  path::make_preferred()
  {
#ifdef _GLIBCXX_FILESYSTEM_IS_WINDOWS
    std::replace(_M_pathname.begin(), _M_pathname.end(), L'/',
		 preferred_separator);
#endif
    return *this;
  }

  inline void path::swap(path& __rhs) noexcept
  {
    _M_pathname.swap(__rhs._M_pathname);
    _M_cmpts.swap(__rhs._M_cmpts);
    std::swap(_M_type, __rhs._M_type);
  }

  template<typename _CharT, typename _Traits, typename _Allocator>
    inline std::basic_string<_CharT, _Traits, _Allocator>
    path::string(const _Allocator& __a) const
    {
      if (is_same<_CharT, value_type>::value)
	return { _M_pathname.begin(), _M_pathname.end(), __a };

      const value_type* __first = _M_pathname.data();
      const value_type* __last = __first + _M_pathname.size();

#ifdef _GLIBCXX_FILESYSTEM_IS_WINDOWS
      using _CharAlloc = __alloc_rebind<_Allocator, char>;
      using _String = basic_string<char, char_traits<char>, _CharAlloc>;
      using _WString = basic_string<_CharT, _Traits, _Allocator>;

      // use codecvt_utf8<wchar_t> to convert native string to UTF-8
      codecvt_utf8<value_type> __cvt;
      _String __u8str{_CharAlloc{__a}};
      if (__str_codecvt_out(__first, __last, __u8str, __cvt))
	{
	  struct
	  {
	    const _String*
	    operator()(const _String& __from, _String&, true_type)
	    { return std::__addressof(__from); }

	    _WString*
	    operator()(const _String& __from, _WString& __to, false_type)
	    {
	      // use codecvt_utf8<_CharT> to convert UTF-8 to wide string
	      codecvt_utf8<_CharT> __cvt;
	      const char* __f = __from.data();
	      const char* __l = __f + __from.size();
	      if (__str_codecvt_in(__f, __l, __to, __cvt))
		return std::__addressof(__to);
	      return nullptr;
	    }
	  } __dispatch;
	  _WString __wstr;
	  if (auto* __p = __dispatch(__u8str, __wstr, is_same<_CharT, char>{}))
	    return *__p;
	}
#else
      codecvt_utf8<_CharT> __cvt;
      basic_string<_CharT, _Traits, _Allocator> __wstr{__a};
      if (__str_codecvt_in(__first, __last, __wstr, __cvt))
	return __wstr;
#endif
      _GLIBCXX_THROW_OR_ABORT(filesystem_error(
	    "Cannot convert character sequence",
	    std::make_error_code(errc::illegal_byte_sequence)));
    }

  inline std::string
  path::string() const { return string<char>(); }

#if _GLIBCXX_USE_WCHAR_T
  inline std::wstring
  path::wstring() const { return string<wchar_t>(); }
#endif

  inline std::string
  path::u8string() const
  {
#ifdef _GLIBCXX_FILESYSTEM_IS_WINDOWS
    std::string __str;
    // convert from native encoding to UTF-8
    codecvt_utf8<value_type> __cvt;
    const value_type* __first = _M_pathname.data();
    const value_type* __last = __first + _M_pathname.size();
    if (__str_codecvt_out(__first, __last, __str, __cvt))
      return __str;
    _GLIBCXX_THROW_OR_ABORT(filesystem_error(
	  "Cannot convert character sequence",
	  std::make_error_code(errc::illegal_byte_sequence)));
#else
    return _M_pathname;
#endif
  }

  inline std::u16string
  path::u16string() const { return string<char16_t>(); }

  inline std::u32string
  path::u32string() const { return string<char32_t>(); }

#ifndef _GLIBCXX_FILESYSTEM_IS_WINDOWS
  template<typename _CharT, typename _Traits, typename _Allocator>
    inline std::basic_string<_CharT, _Traits, _Allocator>
    path::generic_string(const _Allocator& __a) const
    { return string<_CharT, _Traits, _Allocator>(__a); }

  inline std::string
  path::generic_string() const { return string(); }

#if _GLIBCXX_USE_WCHAR_T
  inline std::wstring
  path::generic_wstring() const { return wstring(); }
#endif

  inline std::string
  path::generic_u8string() const { return u8string(); }

  inline std::u16string
  path::generic_u16string() const { return u16string(); }

  inline std::u32string
  path::generic_u32string() const { return u32string(); }
#endif

  inline int
  path::compare(const string_type& __s) const { return compare(path(__s)); }

  inline int
  path::compare(const value_type* __s) const { return compare(path(__s)); }

#if __cplusplus >= 201402L
  inline int
  path::compare(__basic_string_view<value_type> __s) const
  { return compare(path(__s)); }
#endif

  inline path
  path::filename() const { return empty() ? path() : *--end(); }

  inline path
  path::stem() const
  {
    auto ext = _M_find_extension();
    if (ext.first && ext.second != 0)
      return path{ext.first->substr(0, ext.second)};
    return {};
  }

  inline path
  path::extension() const
  {
    auto ext = _M_find_extension();
    if (ext.first && ext.second != string_type::npos)
      return path{ext.first->substr(ext.second)};
    return {};
  }

  inline bool
  path::has_stem() const
  {
    auto ext = _M_find_extension();
    return ext.first && ext.second != 0;
  }

  inline bool
  path::has_extension() const
  {
    auto ext = _M_find_extension();
    return ext.first && ext.second != string_type::npos;
  }

  inline bool
  path::is_absolute() const
  {
#ifdef _GLIBCXX_FILESYSTEM_IS_WINDOWS
    return has_root_name();
#else
    return has_root_directory();
#endif
  }

  inline path::iterator
  path::begin() const
  {
    if (_M_type == _Type::_Multi)
      return iterator(this, _M_cmpts.begin());
    return iterator(this, false);
  }

  inline path::iterator
  path::end() const
  {
    if (_M_type == _Type::_Multi)
      return iterator(this, _M_cmpts.end());
    return iterator(this, true);
  }

  inline path::iterator&
  path::iterator::operator++()
  {
    _GLIBCXX_DEBUG_ASSERT(_M_path != nullptr);
    if (_M_path->_M_type == _Type::_Multi)
      {
	_GLIBCXX_DEBUG_ASSERT(_M_cur != _M_path->_M_cmpts.end());
	++_M_cur;
      }
    else
      {
	_GLIBCXX_DEBUG_ASSERT(!_M_at_end);
	_M_at_end = true;
      }
    return *this;
  }

  inline path::iterator&
  path::iterator::operator--()
  {
    _GLIBCXX_DEBUG_ASSERT(_M_path != nullptr);
    if (_M_path->_M_type == _Type::_Multi)
      {
	_GLIBCXX_DEBUG_ASSERT(_M_cur != _M_path->_M_cmpts.begin());
	--_M_cur;
      }
    else
      {
	_GLIBCXX_DEBUG_ASSERT(_M_at_end);
	_M_at_end = false;
      }
    return *this;
  }

  inline path::iterator::reference
  path::iterator::operator*() const
  {
    _GLIBCXX_DEBUG_ASSERT(_M_path != nullptr);
    if (_M_path->_M_type == _Type::_Multi)
      {
	_GLIBCXX_DEBUG_ASSERT(_M_cur != _M_path->_M_cmpts.end());
	return *_M_cur;
      }
    return *_M_path;
  }

  inline bool
  path::iterator::_M_equals(iterator __rhs) const
  {
    if (_M_path != __rhs._M_path)
      return false;
    if (_M_path == nullptr)
      return true;
    if (_M_path->_M_type == path::_Type::_Multi)
      return _M_cur == __rhs._M_cur;
    return _M_at_end == __rhs._M_at_end;
  }

  // @} group filesystem
_GLIBCXX_END_NAMESPACE_CXX11
_GLIBCXX_END_NAMESPACE_VERSION
} // namespace v1
} // namespace filesystem
} // namespace experimental
} // namespace std

#endif // C++11

#endif // _GLIBCXX_EXPERIMENTAL_FS_PATH_H
