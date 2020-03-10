// <system_error> implementation file

// Copyright (C) 2007-2020 Free Software Foundation, Inc.
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


#define _GLIBCXX_USE_CXX11_ABI 1
#define __sso_string __sso_stringxxx
#include <cstring>
#include <system_error>
#include <bits/functexcept.h>
#include <limits>
#include <errno.h>
#undef __sso_string

namespace
{
  using std::string;

  struct generic_error_category : public std::error_category
  {
    virtual const char*
    name() const noexcept
    { return "generic"; }

    _GLIBCXX_DEFAULT_ABI_TAG
    virtual string
    message(int i) const
    {
      // XXX locale issues: how does one get or set loc.
      // _GLIBCXX_HAVE_STRERROR_L, strerror_l(i, cloc)
      return string(strerror(i));
    }
  };

  struct system_error_category : public std::error_category
  {
    virtual const char*
    name() const noexcept
    { return "system"; }

    _GLIBCXX_DEFAULT_ABI_TAG
    virtual string
    message(int i) const
    {
      // XXX locale issues: how does one get or set loc.
      // _GLIBCXX_HAVE_STRERROR_L, strerror_l(i, cloc)
      return string(strerror(i));
    }

    virtual std::error_condition
    default_error_condition(int ev) const noexcept
    {
      switch (ev)
      {
      // List of errno macros from [cerrno.syn].
      // C11 only defines EDOM, EILSEQ and ERANGE, the rest are from POSIX.
      // They expand to integer constant expressions with type int,
      // and distinct positive values, suitable for use in #if directives.
      // POSIX adds more macros (but they're not defined on all targets,
      // see config/os/*/error_constants.h), and POSIX allows
      // EAGAIN == EWOULDBLOCK and ENOTSUP == EOPNOTSUPP.

#ifdef E2BIG
      case E2BIG:
#endif
#ifdef EACCES
      case EACCES:
#endif
#ifdef EADDRINUSE
      case EADDRINUSE:
#endif
#ifdef EADDRNOTAVAIL
      case EADDRNOTAVAIL:
#endif
#ifdef EAFNOSUPPORT
      case EAFNOSUPPORT:
#endif
#ifdef EAGAIN
      case EAGAIN:
#endif
#ifdef EALREADY
      case EALREADY:
#endif
#ifdef EBADF
      case EBADF:
#endif
#ifdef EBADMSG
      case EBADMSG:
#endif
#ifdef EBUSY
      case EBUSY:
#endif
#ifdef ECANCELED
      case ECANCELED:
#endif
#ifdef ECHILD
      case ECHILD:
#endif
#ifdef ECONNABORTED
      case ECONNABORTED:
#endif
#ifdef ECONNREFUSED
      case ECONNREFUSED:
#endif
#ifdef ECONNRESET
      case ECONNRESET:
#endif
#ifdef EDEADLK
      case EDEADLK:
#endif
#ifdef EDESTADDRREQ
      case EDESTADDRREQ:
#endif
      case EDOM:
#ifdef EEXIST
      case EEXIST:
#endif
#ifdef EFAULT
      case EFAULT:
#endif
#ifdef EFBIG
      case EFBIG:
#endif
#ifdef EHOSTUNREACH
      case EHOSTUNREACH:
#endif
#ifdef EIDRM
      case EIDRM:
#endif
      case EILSEQ:
#ifdef EINPROGRESS
      case EINPROGRESS:
#endif
#ifdef EINTR
      case EINTR:
#endif
#ifdef EINVAL
      case EINVAL:
#endif
#ifdef EIO
      case EIO:
#endif
#ifdef EISCONN
      case EISCONN:
#endif
#ifdef EISDIR
      case EISDIR:
#endif
#ifdef ELOOP
      case ELOOP:
#endif
#ifdef EMFILE
      case EMFILE:
#endif
#ifdef EMLINK
      case EMLINK:
#endif
#ifdef EMSGSIZE
      case EMSGSIZE:
#endif
#ifdef ENAMETOOLONG
      case ENAMETOOLONG:
#endif
#ifdef ENETDOWN
      case ENETDOWN:
#endif
#ifdef ENETRESET
      case ENETRESET:
#endif
#ifdef ENETUNREACH
      case ENETUNREACH:
#endif
#ifdef ENFILE
      case ENFILE:
#endif
#ifdef ENOBUFS
      case ENOBUFS:
#endif
#ifdef ENODATA
      case ENODATA:
#endif
#ifdef ENODEV
      case ENODEV:
#endif
#ifdef ENOENT
      case ENOENT:
#endif
#ifdef ENOEXEC
      case ENOEXEC:
#endif
#ifdef ENOLCK
      case ENOLCK:
#endif
#ifdef ENOLINK
      case ENOLINK:
#endif
#ifdef ENOMEM
      case ENOMEM:
#endif
#ifdef ENOMSG
      case ENOMSG:
#endif
#ifdef ENOPROTOOPT
      case ENOPROTOOPT:
#endif
#ifdef ENOSPC
      case ENOSPC:
#endif
#ifdef ENOSR
      case ENOSR:
#endif
#ifdef ENOSTR
      case ENOSTR:
#endif
#ifdef ENOSYS
      case ENOSYS:
#endif
#ifdef ENOTCONN
      case ENOTCONN:
#endif
#ifdef ENOTDIR
      case ENOTDIR:
#endif
#if defined ENOTEMPTY && (!defined EEXIST || ENOTEMPTY != EEXIST)
      // AIX sometimes uses the same value for EEXIST and ENOTEMPTY
      case ENOTEMPTY:
#endif
#ifdef ENOTRECOVERABLE
      case ENOTRECOVERABLE:
#endif
#ifdef ENOTSOCK
      case ENOTSOCK:
#endif
#if defined ENOTSUP && (!defined ENOSYS || ENOTSUP != ENOSYS)
      // zTPF uses the same value for ENOSYS and ENOTSUP
      case ENOTSUP:
#endif
#ifdef ENOTTY
      case ENOTTY:
#endif
#ifdef ENXIO
      case ENXIO:
#endif
#if defined EOPNOTSUPP && (!defined ENOTSUP || EOPNOTSUPP != ENOTSUP)
      case EOPNOTSUPP:
#endif
#ifdef EOVERFLOW
      case EOVERFLOW:
#endif
#ifdef EOWNERDEAD
      case EOWNERDEAD:
#endif
#ifdef EPERM
      case EPERM:
#endif
#ifdef EPIPE
      case EPIPE:
#endif
#ifdef EPROTO
      case EPROTO:
#endif
#ifdef EPROTONOSUPPORT
      case EPROTONOSUPPORT:
#endif
#ifdef EPROTOTYPE
      case EPROTOTYPE:
#endif
      case ERANGE:
#ifdef EROFS
      case EROFS:
#endif
#ifdef ESPIPE
      case ESPIPE:
#endif
#ifdef ESRCH
      case ESRCH:
#endif
#ifdef ETIME
      case ETIME:
#endif
#ifdef ETIMEDOUT
      case ETIMEDOUT:
#endif
#ifdef ETXTBSY
      case ETXTBSY:
#endif
#if defined EWOULDBLOCK && (!defined EAGAIN || EWOULDBLOCK != EAGAIN)
      case EWOULDBLOCK:
#endif
#ifdef EXDEV
      case EXDEV:
#endif
        return std::error_condition(ev, std::generic_category());

      /* Additional system-dependent mappings from non-standard error codes
       * to one of the POSIX values above would go here, e.g.
      case EBLAH:
	return std::error_condition(EINVAL, std::generic_category());
       */

      default:
	return std::error_condition(ev, std::system_category());
      }
    }
  };

  const generic_error_category generic_category_instance{};
  const system_error_category system_category_instance{};
}

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  void
  __throw_system_error(int __i __attribute__((unused)))
  {
    _GLIBCXX_THROW_OR_ABORT(system_error(error_code(__i, generic_category())));
  }

  error_category::~error_category() noexcept = default;

  const error_category&
  _V2::system_category() noexcept { return system_category_instance; }

  const error_category&
  _V2::generic_category() noexcept { return generic_category_instance; }

  system_error::~system_error() noexcept = default;

  error_condition
  error_category::default_error_condition(int __i) const noexcept
  { return error_condition(__i, *this); }

  bool
  error_category::equivalent(int __i,
			     const error_condition& __cond) const noexcept
  { return default_error_condition(__i) == __cond; }

  bool
  error_category::equivalent(const error_code& __code, int __i) const noexcept
  { return *this == __code.category() && __code.value() == __i; }

  error_condition
  error_code::default_error_condition() const noexcept
  { return category().default_error_condition(value()); }

#if _GLIBCXX_USE_CXX11_ABI
  // Return error_category::message() as a COW string
  __cow_string
  error_category::_M_message(int i) const
  {
    string msg = this->message(i);
    return {msg.c_str(), msg.length()};
  }
#endif

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace
