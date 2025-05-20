/*
 * Copyright (c) 2021-2025 Symas Corporation
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

#include <cstdio>
#include <cstdlib>
#include <cerrno>
#include <cstdbool>
#include <cstdint>

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
    perror("What is this? ");
    fprintf(stderr, "__gg__file_status_word got an error_number "
          "%d, which it doesn't know how to handle\n", error_number);

    abort();
    break;
  }

  return file_status_register;
}
