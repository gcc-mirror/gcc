/*
 * Copyright (c) 2021-2026 Symas Corporation
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * * Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * * Redistributions in binary form must reproduce the above
 *   copyright notice, this list of conditions and the following disclaimer
 *   in the documentation and/or other materials provided with the
 *   distribution.
 * * Neither the name of the Symas Corporation nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "config.h"

#include "io.h"
#include "cobol-endian.h"

#include <cassert>
#include <cerrno>
#include <cstdint>
#include <cstdio>
#include <cstdlib>

/*
 * The Cobol runtime support is responsible to set the file status
 * word appropriately to the application's expectations. This function
 * sets the defined file status register for the file to value of the
 * status parameter, except for FhErrno.  For FhErrno, it sets the
 * file status register to a value derived from the current value of
 * errno.  If the errno value is not accounted for, the high bit is
 * set to 1, and the rest to errno.
 */
extern "C"
file_status_t
__gg__file_status_word( enum file_status_t status,
                        int error_number) {
  file_status_t file_status_register;

  if( status != FsErrno ) {
    return status;
  }

  switch( error_number ) {
  case      0: file_status_register = FsSuccess;      break;
  case EACCES: file_status_register = FsNoAccess;     break;
  case EDQUOT: file_status_register = FsBoundary;     break;
  case EEXIST: file_status_register = FsNoAccess;     break;
  case EFAULT: file_status_register = FsNoFile;       break;
  case EFBIG:  file_status_register = FsBoundary;     break;
  case EINTR:  file_status_register = FsOsError;      break;
  case EINVAL: file_status_register = FsWrongType;    break;
  case EISDIR: file_status_register = FsWrongType;    break;
  case ELOOP:  file_status_register = FsOsError;      break;
  case EMFILE: file_status_register = FsOsError;      break;
  case ENAMETOOLONG:
               file_status_register = FsWrongType;    break;
  case ENFILE: file_status_register = FsOsError;      break;
  case ENODEV: file_status_register = FsNoFile;       break;
  case ENOENT: file_status_register = FsNoFile;       break;
  case ENOMEM: file_status_register = FsOsError;      break;
  case ENOSPC: file_status_register = FsBoundary;     break;
  case ENOTDIR: file_status_register = FsNoFile;      break;
  case ENXIO:  file_status_register = FsNoFile;       break;
  case EOPNOTSUPP:
               file_status_register = FsOsError;      break;
  case EOVERFLOW: file_status_register = FsBoundary;  break;
  case EPERM:  file_status_register = FsNoAccess;     break;
  case EROFS:  file_status_register = FsNoAccess;     break;
  case ETXTBSY: file_status_register= FsWrongType;    break;
  case EWOULDBLOCK:
               file_status_register = FsOsError;      break;
  default:
    file_status_register = FsOsError;
    break;
  }

  return file_status_register;
}

/*
 * This function is used by libgcobol_compat_gnu.
 * If status parameter is FsErrno, return the file_status_t for errno.
 * If status paramter is FsSuccess, return the file_status_t for errnum parameter.
 * The output is byte-swapped per MF specification.
 */
#include <cstring>
extern "C"
uint16_t
__compat_file_status_word( enum file_status_t status, int errnum) {
  /* In July of 2026, I had to make this routine work on a big-endian machine.

     By my recent standards, this routine is something of a mess.  It was
     returning a four-byte native binary file_status_t.  The only place this
     routine is called, however, is in cobrt-file-status.cbl, where
     the function call to __compat_file_status_word expects a return value
     of "01 FILE-STATUS PIC X(2) COMP-5.", which is a two-byte native binary
     value.  On a little-endian machine, this works because the return value is
     actually just sixteen bits in the low part half of the int, and the top
     two x00 bytes are discarded.

     That doesn't work on a big-endian architecture, where the actual return
     information is the top two bytes that are being discarded.

     So, I changed this routine to return a two-byte binary value, and adjusted
     the mangling accordingly.  Dubner, 2026-07-04  */

  switch( status ) {
  case FsErrno:
    errnum = errno;
    break;
  case FsSuccess:
    break;
  default:
    fprintf(stderr, "status is 0x%x, (%d)\n", status, status);
    assert( status == FsErrno || status == FsSuccess );
    break;
  }

  switch( errnum ) {
  case EACCES:
  case EPERM:
    status = FsCobRt037; // File access denied
    break;
  case EBADF:
    status = FsCobRt034; // Incorrect mode or file descriptor
    break;
  case EDQUOT:
  case ENOSPC:
    status = FsCobRt028; // No space on device
    break;
  case EFBIG:
  case EOVERFLOW:
    status = FsCobRt194; // File size too large
    break;
  case EINVAL:
    status = FsCobRt181; // Invalid parameter error
    break;
  case EIO:
    status = FsCobRt033; // Physical I-O error
    break;
  case EISDIR:
    status = FsCobRt021; // File is a directory
    break;
  case EMFILE:
    status = FsCobRt014; // Too many files open simultaneously
    break;
  case ENAMETOOLONG:
    status = FsCobRt188; // Filename too large
    break;
  case ENOENT:
    status = FsCobRt013; // File not found
    break;
  case ENOMEM:
    status = FsCobRt105; // Memory allocation error
    break;
  case EPIPE:
    status = FsCobRt042; // Attempt to write on broken pipe
    break;
  case EROFS:
    status = FsCobRt030; // File system is read-only
    break;
  default:
      status = FsCobRt000; // No defined mapping for errno value
  }

  // This function returns 9x status in the FsCobRt range.
  assert( FsCobRt000 <= status && status <= 0x09FF );

  static_assert(sizeof(status) == 4);

  uint16_t retval;

  if( !cobol_target_big_endian() ) // cppcheck-suppress knownConditionTrueFalse
    {
    // Arrange little-endian output per MF definition.
    retval = ((status & 0xF)<<8) + '9';
    }
  else
    {
    retval = (('9')<<8) + (status & 0xF);
    }

  return retval;
}
