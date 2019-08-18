/* File descriptor related functions.

   Copyright (C) 2019 Free Software Foundation, Inc.

   This file is part of the libiberty library.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street - Fifth Floor,
   Boston, MA 02110-1301, USA.  */

#include "config.h"
#include "ansidecl.h"
#include "libiberty.h"

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#if defined (_WIN32)
#define WIN32_LEAN_AND_MEAN
#include <windows.h> /* for GetFullPathName */
#endif
/* Return true when FD file descriptor exists.  */

int
is_valid_fd (int fd)
{
#if defined(_WIN32)
  HANDLE h = (HANDLE) _get_osfhandle (fd);
  return h != (HANDLE) -1;
#elif defined(F_GETFD)
  return fcntl (fd, F_GETFD) >= 0;
#else
  return dup2 (fd, fd) < 0;
#endif
}
