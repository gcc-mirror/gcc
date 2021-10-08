// <system_error> implementation file

// Copyright (C) 2007-2021 Free Software Foundation, Inc.
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

#if defined(_WIN32) && !defined(__CYGWIN__)
#include <memory>
#include <windows.h>
#endif

namespace
{
  using std::string;

  template<typename T>
    struct constant_init
    {
      union {
	unsigned char unused;
	T obj;
      };
      constexpr constant_init() : obj() { }

      ~constant_init() { /* do nothing, union member is not destroyed */ }
    };

  struct generic_error_category final : public std::error_category
  {
    const char*
    name() const noexcept final
    { return "generic"; }

    _GLIBCXX_DEFAULT_ABI_TAG
    string
    message(int i) const final
    {
      // XXX locale issues: how does one get or set loc.
      // _GLIBCXX_HAVE_STRERROR_L, strerror_l(i, cloc)
      return string(strerror(i));
    }

    // Override this to avoid a virtual call to default_error_condition(i).
    bool
    equivalent(int i, const std::error_condition& cond) const noexcept final
    { return i == cond.value() && *this == cond.category(); }
  };

  __constinit constant_init<generic_error_category> generic_category_instance{};

  struct system_error_category final : public std::error_category
  {
    const char*
    name() const noexcept final
    { return "system"; }

    _GLIBCXX_DEFAULT_ABI_TAG
    string
    message(int i) const final
    {
#if defined(_WIN32) && !defined(__CYGWIN__)
      char* buf = nullptr;
      auto len
	= FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM
			| FORMAT_MESSAGE_ALLOCATE_BUFFER,
			nullptr,
			i,
			LANG_USER_DEFAULT,
			reinterpret_cast<LPTSTR>(&buf),
			0,
			nullptr);
      if (len > 0)
      {
	struct deleter {
	  void operator()(void* p) const { ::LocalFree(p); }
	};
	std::unique_ptr<char[], deleter> guard(buf);
	if (len > 3 && !__builtin_memcmp(buf + len - 3, ".\r\n", 3)) [[likely]]
	  len -= 3;
	return string(buf, len);
      }
      return string("Unknown error code");
#else
      // XXX locale issues: how does one get or set loc.
      // _GLIBCXX_HAVE_STRERROR_L, strerror_l(i, cloc)
      return string(strerror(i));
#endif
    }

    std::error_condition
    default_error_condition(int ev) const noexcept final
    {
      // Use generic category for all known POSIX errno values (including zero)
      // and system category otherwise.
      switch (ev)
      {
#if defined(_WIN32) && !defined(__CYGWIN__)
      case 0:
	return {0, generic_category_instance.obj};
	// Convert Windows error code into a corresponding POSIX errno value.
#define X(w, e) case ERROR_##w: return {e, generic_category_instance.obj};
	// This list is based on Cygwin's winsup/cygwin/errno.cc
	X (ACCESS_DENIED,		EACCES);
	X (ACTIVE_CONNECTIONS,		EAGAIN);
	X (ALREADY_EXISTS,		EEXIST);
	X (BAD_DEVICE,			ENODEV);
	X (BAD_EXE_FORMAT,		ENOEXEC);
	X (BAD_NETPATH,			ENOENT);
	X (BAD_NET_NAME,		ENOENT);
	X (BAD_NET_RESP,		ENOSYS);
	X (BAD_PATHNAME,		ENOENT);
	X (BAD_PIPE,			EINVAL);
	X (BAD_UNIT,			ENODEV);
	X (BAD_USERNAME,		EINVAL);
	X (BEGINNING_OF_MEDIA,		EIO);
	X (BROKEN_PIPE,			EPIPE);
	X (BUSY,			EBUSY);
	X (BUS_RESET,			EIO);
	X (CALL_NOT_IMPLEMENTED,	ENOSYS);
	X (CANCELLED,			EINTR);
	X (CANNOT_MAKE,			EPERM);
	X (CHILD_NOT_COMPLETE,		EBUSY);
	X (COMMITMENT_LIMIT,		EAGAIN);
	X (CONNECTION_REFUSED,		ECONNREFUSED);
	X (CRC,				EIO);
	X (DEVICE_DOOR_OPEN,		EIO);
	X (DEVICE_IN_USE,		EAGAIN);
	X (DEVICE_REQUIRES_CLEANING,	EIO);
	X (DEV_NOT_EXIST,		ENOENT);
	X (DIRECTORY,			ENOTDIR);
	X (DIR_NOT_EMPTY,		ENOTEMPTY);
	X (DISK_CORRUPT,		EIO);
#ifdef ENOSPC
	X (DISK_FULL,			ENOSPC);
#endif
	X (DS_GENERIC_ERROR,		EIO);
#ifdef ENOSPC
	X (END_OF_MEDIA,		ENOSPC);
#endif
	X (EOM_OVERFLOW,		EIO);
	X (EXE_MACHINE_TYPE_MISMATCH,	ENOEXEC);
	X (EXE_MARKED_INVALID,		ENOEXEC);
	X (FILEMARK_DETECTED,		EIO);
	X (FILENAME_EXCED_RANGE,	ENAMETOOLONG);
	X (FILE_CORRUPT,		EEXIST);
	X (FILE_EXISTS,			EEXIST);
	X (FILE_INVALID,		ENXIO);
	X (FILE_NOT_FOUND,		ENOENT);
#ifdef ENOSPC
	X (HANDLE_DISK_FULL,		ENOSPC);
#endif
	X (INVALID_ADDRESS,		EINVAL);
	X (INVALID_AT_INTERRUPT_TIME,	EINTR);
	X (INVALID_BLOCK_LENGTH,	EIO);
	X (INVALID_DATA,		EINVAL);
	X (INVALID_DRIVE,		ENODEV);
	X (INVALID_EA_NAME,		EINVAL);
	X (INVALID_EXE_SIGNATURE,	ENOEXEC);
	X (INVALID_HANDLE,		EBADF);
	X (INVALID_NAME,		ENOENT);
	X (INVALID_PARAMETER,		EINVAL);
	X (INVALID_SIGNAL_NUMBER,	EINVAL);
	X (IOPL_NOT_ENABLED,		ENOEXEC);
	X (IO_DEVICE,			EIO);
	X (IO_INCOMPLETE,		EAGAIN);
	X (IO_PENDING,			EAGAIN);
	X (LOCK_VIOLATION,		EBUSY);
	X (MAX_THRDS_REACHED,		EAGAIN);
	X (META_EXPANSION_TOO_LONG,	EINVAL);
	X (MOD_NOT_FOUND,		ENOENT);
	X (MORE_DATA,			EMSGSIZE);
	X (NEGATIVE_SEEK,		EINVAL);
	X (NETNAME_DELETED,		ENOENT);
	X (NOACCESS,			EFAULT);
	X (NONE_MAPPED,			EINVAL);
	X (NONPAGED_SYSTEM_RESOURCES,	EAGAIN);
	X (NOT_ENOUGH_MEMORY,		ENOMEM);
	X (NOT_ENOUGH_QUOTA,		EIO);
#ifdef EPERM
	X (NOT_OWNER,			EPERM);
#else
	X (NOT_OWNER,			EACCES);
#endif
	X (NOT_SAME_DEVICE,		EXDEV);
	X (NOT_SUPPORTED,		ENOSYS);
	X (NO_DATA,			EPIPE);
	X (NO_DATA_DETECTED,		EIO);
	X (NO_MORE_SEARCH_HANDLES,	ENFILE);
	X (NO_PROC_SLOTS,		EAGAIN);
	X (NO_SIGNAL_SENT,		EIO);
	X (NO_SYSTEM_RESOURCES,		EFBIG);
	X (NO_TOKEN,			EINVAL);
	X (OPEN_FAILED,			EIO);
	X (OPEN_FILES,			EAGAIN);
	X (OUTOFMEMORY,			ENOMEM);
	X (PAGED_SYSTEM_RESOURCES,	EAGAIN);
	X (PAGEFILE_QUOTA,		EAGAIN);
	X (PATH_NOT_FOUND,		ENOENT);
	X (PIPE_BUSY,			EBUSY);
	X (PIPE_CONNECTED,		EBUSY);
	X (POSSIBLE_DEADLOCK,		EDEADLK);
	X (PRIVILEGE_NOT_HELD,		EPERM);
	X (PROCESS_ABORTED,		EFAULT);
	X (PROC_NOT_FOUND,		ESRCH);
	X (SECTOR_NOT_FOUND,		EINVAL);
	X (SEEK,			EINVAL);
	X (SERVICE_REQUEST_TIMEOUT,	EBUSY);
	X (SETMARK_DETECTED,		EIO);
	X (SHARING_BUFFER_EXCEEDED,	ENOLCK);
	X (SHARING_VIOLATION,		EBUSY);
	X (SIGNAL_PENDING,		EBUSY);
	X (SIGNAL_REFUSED,		EIO);
	X (THREAD_1_INACTIVE,		EINVAL);
	X (TIMEOUT,			EBUSY);
	X (TOO_MANY_LINKS,		EMLINK);
	X (TOO_MANY_OPEN_FILES,		EMFILE);
	X (UNEXP_NET_ERR,		EIO);
	X (WORKING_SET_QUOTA,		EAGAIN);
	X (WRITE_PROTECT,		EROFS);
#undef X

#else
      // List of errno macros from [cerrno.syn].
      // C11 only defines EDOM, EILSEQ and ERANGE, the rest are from POSIX.
      // They expand to integer constant expressions with type int,
      // and distinct positive values, suitable for use in #if directives.
      // POSIX adds more macros (but they're not defined on all targets,
      // see config/os/.../error_constants.h), and POSIX allows
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
      case 0:
	return std::error_condition(ev, generic_category_instance.obj);

      /* Additional system-dependent mappings from non-standard error codes
       * to one of the POSIX values above would go here, e.g.
      case EBLAH:
	return std::error_condition(EINVAL, std::generic_category());
       */

#endif
      default:
	return std::error_condition(ev, *this);
      }
    }

    // Override this to avoid a virtual call to default_error_condition(i).
    bool
    equivalent(int i, const std::error_condition& cond) const noexcept final
    { return system_error_category::default_error_condition(i) == cond; }
  };

  __constinit constant_init<system_error_category> system_category_instance{};
}

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  void
  __throw_system_error(int __i __attribute__((unused)))
  {
    _GLIBCXX_THROW_OR_ABORT(system_error(__i, generic_category_instance.obj));
  }

  error_category::~error_category() = default;

  const error_category&
  _V2::system_category() noexcept { return system_category_instance.obj; }

  const error_category&
  _V2::generic_category() noexcept { return generic_category_instance.obj; }

  system_error::~system_error() = default;

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
