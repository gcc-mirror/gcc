// Filesystem directory utilities -*- C++ -*-

// Copyright (C) 2014-2018 Free Software Foundation, Inc.
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

/** @file include/bits/fs_dir.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{filesystem}
 */

#ifndef _GLIBCXX_FS_DIR_H
#define _GLIBCXX_FS_DIR_H 1

#if __cplusplus >= 201703L
# include <typeinfo>
# include <ext/concurrence.h>
# include <bits/unique_ptr.h>
# include <bits/shared_ptr.h>

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

namespace filesystem
{
  /**
   * @ingroup filesystem
   * @{
   */

  class file_status
  {
  public:
    // constructors and destructor
    file_status() noexcept : file_status(file_type::none) {}

    explicit
    file_status(file_type __ft, perms __prms = perms::unknown) noexcept
    : _M_type(__ft), _M_perms(__prms) { }

    file_status(const file_status&) noexcept = default;
    file_status(file_status&&) noexcept = default;
    ~file_status() = default;

    file_status& operator=(const file_status&) noexcept = default;
    file_status& operator=(file_status&&) noexcept = default;

    // observers
    file_type  type() const noexcept { return _M_type; }
    perms      permissions() const noexcept { return _M_perms; }

    // modifiers
    void       type(file_type __ft) noexcept { _M_type = __ft; }
    void       permissions(perms __prms) noexcept { _M_perms = __prms; }

  private:
    file_type	_M_type;
    perms	_M_perms;
  };

_GLIBCXX_BEGIN_NAMESPACE_CXX11

  struct _Dir;
  class directory_iterator;
  class recursive_directory_iterator;

  class directory_entry
  {
  public:
    // constructors and destructor
    directory_entry() noexcept = default;
    directory_entry(const directory_entry&) = default;
    directory_entry(directory_entry&&) noexcept = default;

    explicit
    directory_entry(const filesystem::path& __p)
    : _M_path(__p)
    { refresh(); }

    directory_entry(const filesystem::path& __p, error_code& __ec)
    : _M_path(__p)
    {
      refresh(__ec);
      if (__ec)
	_M_path.clear();
    }

    ~directory_entry() = default;

    // modifiers
    directory_entry& operator=(const directory_entry&) = default;
    directory_entry& operator=(directory_entry&&) noexcept = default;

    void
    assign(const filesystem::path& __p)
    {
      _M_path = __p;
      refresh();
    }

    void
    assign(const filesystem::path& __p, error_code& __ec)
    {
      _M_path = __p;
      refresh(__ec);
    }

    void
    replace_filename(const filesystem::path& __p)
    {
      _M_path.replace_filename(__p);
      refresh();
    }

    void
    replace_filename(const filesystem::path& __p, error_code& __ec)
    {
      _M_path.replace_filename(__p);
      refresh(__ec);
    }

    void
    refresh()
    { _M_type = symlink_status().type(); }

    void
    refresh(error_code& __ec) noexcept
    { _M_type = symlink_status(__ec).type(); }

    // observers
    const filesystem::path& path() const noexcept { return _M_path; }
    operator const filesystem::path& () const noexcept { return _M_path; }

    bool
    exists() const
    { return filesystem::exists(file_status{_M_file_type()}); }

    bool
    exists(error_code& __ec) const noexcept
    { return filesystem::exists(file_status{_M_file_type(__ec)}); }

    bool
    is_block_file() const
    { return _M_file_type() == file_type::block; }

    bool
    is_block_file(error_code& __ec) const noexcept
    { return _M_file_type(__ec) == file_type::block; }

    bool
    is_character_file() const
    { return _M_file_type() == file_type::character; }

    bool
    is_character_file(error_code& __ec) const noexcept
    { return _M_file_type(__ec) == file_type::character; }

    bool
    is_directory() const
    { return _M_file_type() == file_type::directory; }

    bool
    is_directory(error_code& __ec) const noexcept
    { return _M_file_type(__ec) == file_type::directory; }

    bool
    is_fifo() const
    { return _M_file_type() == file_type::fifo; }

    bool
    is_fifo(error_code& __ec) const noexcept
    { return _M_file_type(__ec) == file_type::fifo; }

    bool
    is_other() const
    { return filesystem::is_other(file_status{_M_file_type()}); }

    bool
    is_other(error_code& __ec) const noexcept
    { return filesystem::is_other(file_status{_M_file_type(__ec)}); }

    bool
    is_regular_file() const
    { return _M_file_type() == file_type::regular; }

    bool
    is_regular_file(error_code& __ec) const noexcept
    { return _M_file_type(__ec) == file_type::regular; }

    bool
    is_socket() const
    { return _M_file_type() == file_type::socket; }

    bool
    is_socket(error_code& __ec) const noexcept
    { return _M_file_type(__ec) == file_type::socket; }

    bool
    is_symlink() const
    {
      if (_M_type != file_type::none)
	return _M_type == file_type::symlink;
      return symlink_status().type() == file_type::symlink;
    }

    bool
    is_symlink(error_code& __ec) const noexcept
    {
      if (_M_type != file_type::none)
	return _M_type == file_type::symlink;
      return symlink_status(__ec).type() == file_type::symlink;
    }

    uintmax_t
    file_size() const
    { return filesystem::file_size(_M_path); }

    uintmax_t
    file_size(error_code& __ec) const noexcept
    { return filesystem::file_size(_M_path, __ec); }

    uintmax_t
    hard_link_count() const
    { return filesystem::hard_link_count(_M_path); }

    uintmax_t
    hard_link_count(error_code& __ec) const noexcept
    { return filesystem::hard_link_count(_M_path, __ec); }

    file_time_type
    last_write_time() const
    { return filesystem::last_write_time(_M_path); }


    file_time_type
    last_write_time(error_code& __ec) const noexcept
    { return filesystem::last_write_time(_M_path, __ec); }

    file_status
    status() const
    { return filesystem::status(_M_path); }

    file_status
    status(error_code& __ec) const noexcept
    { return filesystem::status(_M_path, __ec); }

    file_status
    symlink_status() const
    { return filesystem::symlink_status(_M_path); }

    file_status
    symlink_status(error_code& __ec) const noexcept
    { return filesystem::symlink_status(_M_path, __ec); }

    bool
    operator< (const directory_entry& __rhs) const noexcept
    { return _M_path < __rhs._M_path; }

    bool
    operator==(const directory_entry& __rhs) const noexcept
    { return _M_path == __rhs._M_path; }

    bool
    operator!=(const directory_entry& __rhs) const noexcept
    { return _M_path != __rhs._M_path; }

    bool
    operator<=(const directory_entry& __rhs) const noexcept
    { return _M_path <= __rhs._M_path; }

    bool
    operator> (const directory_entry& __rhs) const noexcept
    { return _M_path > __rhs._M_path; }

    bool
    operator>=(const directory_entry& __rhs) const noexcept
    { return _M_path >= __rhs._M_path; }

  private:
    friend class _Dir;
    friend class directory_iterator;
    friend class recursive_directory_iterator;

    directory_entry(const filesystem::path& __p, file_type __t)
    : _M_path(__p), _M_type(__t)
    { }

    // Equivalent to status().type() but uses cached value, if any.
    file_type
    _M_file_type() const
    {
      if (_M_type != file_type::none && _M_type != file_type::symlink)
	return _M_type;
      return status().type();
    }

    // Equivalent to status(__ec).type() but uses cached value, if any.
    file_type
    _M_file_type(error_code& __ec) const noexcept
    {
      if (_M_type != file_type::none && _M_type != file_type::symlink)
	{
	  __ec.clear();
	  return _M_type;
	}
      return status(__ec).type();
    }

    filesystem::path	_M_path;
    file_type		_M_type = file_type::none;
  };

  struct __directory_iterator_proxy
  {
    const directory_entry& operator*() const& noexcept { return _M_entry; }

    directory_entry operator*() && noexcept { return std::move(_M_entry); }

  private:
    friend class directory_iterator;
    friend class recursive_directory_iterator;

    explicit
    __directory_iterator_proxy(const directory_entry& __e) : _M_entry(__e) { }

    directory_entry _M_entry;
  };

  class directory_iterator
  {
  public:
    typedef directory_entry        value_type;
    typedef ptrdiff_t              difference_type;
    typedef const directory_entry* pointer;
    typedef const directory_entry& reference;
    typedef input_iterator_tag     iterator_category;

    directory_iterator() = default;

    explicit
    directory_iterator(const path& __p)
    : directory_iterator(__p, directory_options::none, nullptr) { }

    directory_iterator(const path& __p, directory_options __options)
    : directory_iterator(__p, __options, nullptr) { }

    directory_iterator(const path& __p, error_code& __ec)
    : directory_iterator(__p, directory_options::none, __ec) { }

    directory_iterator(const path& __p, directory_options __options,
		       error_code& __ec)
    : directory_iterator(__p, __options, &__ec) { }

    directory_iterator(const directory_iterator& __rhs) = default;

    directory_iterator(directory_iterator&& __rhs) noexcept = default;

    ~directory_iterator() = default;

    directory_iterator&
    operator=(const directory_iterator& __rhs) = default;

    directory_iterator&
    operator=(directory_iterator&& __rhs) noexcept = default;

    const directory_entry& operator*() const;
    const directory_entry* operator->() const { return &**this; }
    directory_iterator&    operator++();
    directory_iterator&    increment(error_code& __ec);

    __directory_iterator_proxy operator++(int)
    {
      __directory_iterator_proxy __pr{**this};
      ++*this;
      return __pr;
    }

  private:
    directory_iterator(const path&, directory_options, error_code*);

    friend bool
    operator==(const directory_iterator& __lhs,
               const directory_iterator& __rhs);

    friend class recursive_directory_iterator;

    std::shared_ptr<_Dir> _M_dir;
  };

  inline directory_iterator
  begin(directory_iterator __iter) noexcept
  { return __iter; }

  inline directory_iterator
  end(directory_iterator) noexcept
  { return directory_iterator(); }

  inline bool
  operator==(const directory_iterator& __lhs, const directory_iterator& __rhs)
  {
    return !__rhs._M_dir.owner_before(__lhs._M_dir)
      && !__lhs._M_dir.owner_before(__rhs._M_dir);
  }

  inline bool
  operator!=(const directory_iterator& __lhs, const directory_iterator& __rhs)
  { return !(__lhs == __rhs); }

  class recursive_directory_iterator
  {
  public:
    typedef directory_entry        value_type;
    typedef ptrdiff_t              difference_type;
    typedef const directory_entry* pointer;
    typedef const directory_entry& reference;
    typedef input_iterator_tag     iterator_category;

    recursive_directory_iterator() = default;

    explicit
    recursive_directory_iterator(const path& __p)
    : recursive_directory_iterator(__p, directory_options::none, nullptr) { }

    recursive_directory_iterator(const path& __p, directory_options __options)
    : recursive_directory_iterator(__p, __options, nullptr) { }

    recursive_directory_iterator(const path& __p, directory_options __options,
                                 error_code& __ec)
    : recursive_directory_iterator(__p, __options, &__ec) { }

    recursive_directory_iterator(const path& __p, error_code& __ec)
    : recursive_directory_iterator(__p, directory_options::none, &__ec) { }

    recursive_directory_iterator(
        const recursive_directory_iterator&) = default;

    recursive_directory_iterator(recursive_directory_iterator&&) = default;

    ~recursive_directory_iterator();

    // observers
    directory_options  options() const { return _M_options; }
    int                depth() const;
    bool               recursion_pending() const { return _M_pending; }

    const directory_entry& operator*() const;
    const directory_entry* operator->() const { return &**this; }

    // modifiers
    recursive_directory_iterator&
    operator=(const recursive_directory_iterator& __rhs) noexcept;
    recursive_directory_iterator&
    operator=(recursive_directory_iterator&& __rhs) noexcept;

    recursive_directory_iterator& operator++();
    recursive_directory_iterator& increment(error_code& __ec);

    __directory_iterator_proxy operator++(int)
    {
      __directory_iterator_proxy __pr{**this};
      ++*this;
      return __pr;
    }

    void pop();
    void pop(error_code&);

    void disable_recursion_pending() { _M_pending = false; }

  private:
    recursive_directory_iterator(const path&, directory_options, error_code*);

    friend bool
    operator==(const recursive_directory_iterator& __lhs,
               const recursive_directory_iterator& __rhs);

    struct _Dir_stack;
    std::shared_ptr<_Dir_stack> _M_dirs;
    directory_options _M_options = {};
    bool _M_pending = false;
  };

  inline recursive_directory_iterator
  begin(recursive_directory_iterator __iter) noexcept
  { return __iter; }

  inline recursive_directory_iterator
  end(recursive_directory_iterator) noexcept
  { return recursive_directory_iterator(); }

  inline bool
  operator==(const recursive_directory_iterator& __lhs,
             const recursive_directory_iterator& __rhs)
  {
    return !__rhs._M_dirs.owner_before(__lhs._M_dirs)
      && !__lhs._M_dirs.owner_before(__rhs._M_dirs);
  }

  inline bool
  operator!=(const recursive_directory_iterator& __lhs,
             const recursive_directory_iterator& __rhs)
  { return !(__lhs == __rhs); }

_GLIBCXX_END_NAMESPACE_CXX11

  // @} group filesystem
} // namespace filesystem

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std

#endif // C++17

#endif // _GLIBCXX_FS_DIR_H
