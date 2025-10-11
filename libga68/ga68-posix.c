/* Support run-time routines for the POSIX prelude.

   Copyright (C) 2025 Jose E. Marchesi.

   GCC is free software; you can redistribute it and/or modify it under the
   terms of the GNU General Public License as published by the Free Software
   Foundation; either version 3, or (at your option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
   details.

   Under Section 7 of GPL version 3, you are granted additional permissions
   described in the GCC Runtime Library Exception, version 3.1, as published by
   the Free Software Foundation.

   You should have received a copy of the GNU General Public License and a copy
   of the GCC Runtime Library Exception along with this program; see the files
   COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#include "ga68.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>  /* For open.  */
#include <unistd.h> /* For close and write.  */
#include <errno.h>  /* For errno.  */
#include <sys/socket.h>
#include <sys/stat.h> /* For struct stat */
#include <netinet/in.h>
#include <netdb.h> /* For gethostbyname.  */
#include <limits.h> /* For LLONG_MAX */

#define EOF_PSEUDO_CHARACTER -1

/* Some Unicode code points used in this file.  */

#define REPLACEMENT_CHARACTER 0xFFFD
#define NEWLINE 0x000A

/* Errno.  */

static int _libga68_errno;

/* Simple I/O based on POSIX file descriptors.  */

int
_libga68_posixerrno (void)
{
  return _libga68_errno;
}

void
_libga68_posixperror (uint32_t *s, size_t len, size_t stride)
{
  size_t u8len;
  uint8_t *u8str = _libga68_u32_to_u8 (s, len, stride, NULL, &u8len);

  const char *errstr = strerror (_libga68_errno);
  (void) write (2, u8str, u8len);
  (void) write (2, ": ", 2);
  (void) write (2, errstr, strlen (errstr));
  (void) write (2, "\n", 1);
}

uint32_t *
_libga68_posixstrerror (int errnum, size_t *len)
{
  const char *str = strerror (errnum);
  return _libga68_u8_to_u32 ((const uint8_t *)str, strlen (str), NULL, len);
}

/* Helper for _libga68_posixfopen.  */
static int
_libga68_open (const char *path, unsigned int flags)
{
  int fd = open (path, flags);
  _libga68_errno = errno;
  return fd;
}

#define FILE_O_DEFAULT 0x99999999
#define FILE_O_RDONLY  0x0
#define FILE_O_WRONLY  0x1
#define FILE_O_RDWR    0x2
#define FILE_O_TRUNC   0x8

int
_libga68_posixfopen (const uint32_t *pathname, size_t len, size_t stride,
		     unsigned int flags)
{
  int fd;
  int openflags = 0;
  size_t u8len;
  const uint8_t *u8pathname = _libga68_u32_to_u8 (pathname, len, stride, NULL,
						  &u8len);
  char *filepath = (char *) _libga68_malloc_internal (u8len + 1);
  memcpy (filepath, u8pathname, u8len);
  filepath[u8len] = '\0';

  /* Default mode: try read-write initially.
     If that fails, then try read-only.
     If that fails, then try write-only.  */
  if (flags == FILE_O_DEFAULT)
    {
      openflags = O_RDWR;
      if ((fd = _libga68_open (filepath, openflags)) < 0)
	{
	  openflags = O_RDONLY;
	  if ((fd = _libga68_open (filepath, openflags)) < 0)
	    {
	      openflags = O_WRONLY;
	      fd = _libga68_open (filepath, openflags);
	      _libga68_free_internal (filepath);
	      return fd;
	    }
	}
      _libga68_free_internal (filepath);
      return fd;
    }

  if (flags & FILE_O_RDONLY)
    openflags |= O_RDONLY;
  if (flags & FILE_O_WRONLY)
    openflags |= O_WRONLY;
  if (flags & FILE_O_RDWR)
    openflags |= O_RDWR;
  if (flags & FILE_O_TRUNC)
    openflags |= O_TRUNC;

  fd = _libga68_open (filepath, openflags);
  _libga68_free_internal (filepath);
  return fd;
}

int
_libga68_posixcreat (uint32_t *pathname, size_t len, size_t stride,
		     uint32_t mode)
{
  size_t u8len;
  uint8_t *u8pathname = _libga68_u32_to_u8 (pathname, len, stride, NULL, &u8len);
  u8pathname[u8len] = '\0';

  int res = creat (u8pathname, mode);
  _libga68_errno = errno;
  return res;
}

int
_libga68_posixclose (int fd)
{
  int res = close (fd);
  _libga68_errno = errno;
  return res;
}

/* Implementation of the posix prelude `posix argc'.  */

int
_libga68_posixargc (void)
{
  return _libga68_argc;
}

/* Implementation of the posix prelude `posix argv'.  */

uint32_t *
_libga68_posixargv (int n, size_t *len)
{
  if (n < 0 || n > _libga68_argc)
    {
      /* Return an empty string.  */
      *len = 0;
      return NULL;
    }
  else
    {
      char *arg = _libga68_argv[n - 1];
      return _libga68_u8_to_u32 (arg, strlen (arg), NULL, len);
    }
}

/* Implementation of the posix prelude `posix getenv'.  */

void
_libga68_posixgetenv (uint32_t *s, size_t len, size_t stride,
		      uint32_t **r, size_t *rlen)
{
  size_t varlen;
  char *varname = _libga68_u32_to_u8 (s, len, stride, NULL, &varlen);

  char *var = _libga68_malloc_internal (varlen + 1);
  memcpy (var, varname, varlen);
  var[varlen] = '\0';
  char *val = getenv (var);
  _libga68_free_internal (var);

  if (val == NULL)
    {
      /* Return an empty string.  */
      *r = NULL;
      *rlen = 0;
    }
  else
    *r = _libga68_u8_to_u32 (val, strlen (val), NULL, rlen);
}

/* Implementation of the posix prelude `posix puts'.  */

void
_libga68_posixputs (uint32_t *s, size_t len, size_t stride)
{
  (void) _libga68_posixfputs (1, s, len, stride);
}

/* Implementation of the posix prelude `posix fputs'.  */

int
_libga68_posixfputs (int fd, uint32_t *s, size_t len, size_t stride)
{
  size_t u8len;
  uint8_t *u8str = _libga68_u32_to_u8 (s, len, stride, NULL, &u8len);

  ssize_t ret = write (fd, u8str, u8len);
  _libga68_errno = errno;
  if (ret == -1)
    return 0;
  else
    return u8len;
}

/* Implementation of the posix prelude `posix putc'.  */

uint32_t
_libga68_posixfputc (int fd, uint32_t c)
{
  uint8_t u8[6];

  int u8len = _libga68_u8_uctomb (u8, c, 6);
  if (u8len < 0)
    return EOF_PSEUDO_CHARACTER;

  ssize_t ret = write (fd, &u8, u8len);
  if (ret == -1)
    return EOF_PSEUDO_CHARACTER;
  else
    return c;
}

/* Implementation of the posix prelude `posix putchar'.  */

uint32_t
_libga68_posixputchar (uint32_t c)
{
  return _libga68_posixfputc (1, c);
}

/* Implementation of the posix prelude `posix fgetc'.  */

uint32_t
_libga68_posixfgetc (int fd)
{
  /* We need to read one char (byte) at a time from FD, until we complete a
     full Unicode character.  Then we convert to UCS-4.  */

  uint8_t c;
  uint8_t u8c[6];
  size_t morechars = 0;
  size_t i;

  /* Read first UTF-8 character.  This gives us the total length of the
     character.  */
  if (read (fd, &c, 1) != 1)
    return EOF_PSEUDO_CHARACTER;

  if (c < 128)
    morechars = 0;
  else if (c < 224)
    morechars = 1;
  else if (c < 240)
    morechars = 2;
  else
    morechars = 3;

  u8c[0] = c;
  for (i = 0; i < morechars; ++i)
    {
      if (read (fd, &c, 1) != 1)
	return EOF_PSEUDO_CHARACTER;
      u8c[i + 1] = c;
    }

  uint32_t res;
  int num_units = morechars + 1;
  int length = _libga68_u8_mbtouc (&res, (const uint8_t *) &u8c, num_units);
  if (res == REPLACEMENT_CHARACTER || length != num_units)
    return REPLACEMENT_CHARACTER;
  else
    return res;
}

/* Implementation of the posix prelude `posix getchar'.  */

uint32_t
_libga68_posixgetchar (void)
{
  return _libga68_posixfgetc (0);
}

/* Implementation of the posix prelude `posix fgets'.  */

uint32_t *
_libga68_posixfgets (int fd, int nchars, size_t *len)
{
  uint32_t *res = NULL;
  int n = 0;
  uint32_t uc;

  if (nchars > 0)
    {
      /* Read exactly nchar or until EOF.  */
      res = _libga68_malloc (nchars * sizeof (uint32_t));
      do
	{
	  uc = _libga68_posixfgetc (fd);
	  if (uc == EOF_PSEUDO_CHARACTER)
	    break;
	  res[n++] = uc;
	}
      while (n < nchars);
    }
  else
    {
      /* Read until newline or EOF.  */
      size_t allocated = 80 * sizeof (uint32_t);
      res = _libga68_malloc (allocated);
      do
	{
	  uc = _libga68_posixfgetc (fd);
	  if (uc != EOF_PSEUDO_CHARACTER)
	    {
	      if (n % 80 == 0)
		res = _libga68_realloc (res, n * 80 * sizeof (uint32_t) + 80 * sizeof (uint32_t));
	      res[n++] = uc;
	    }
	}
      while (uc != NEWLINE && uc != EOF_PSEUDO_CHARACTER);
      if (n > 0)
	res = _libga68_realloc (res, n * 80 * sizeof (uint32_t));
    }

  *len = n;
  return res;
}

/* Implementation of the posix prelude `posix gets'.  */

uint32_t *
_libga68_posixgets (int nchars, size_t *len)
{
  return _libga68_posixfgets (0, nchars, len);
}

/* Implementation of the posix prelude `fconnect'.  */

int
_libga68_posixfconnect (uint32_t *str, size_t len, size_t stride,
			int port)
{
  size_t u8len;
  uint8_t *u8host = _libga68_u32_to_u8 (str, len, stride, NULL, &u8len);

  /* Create a stream socket.  */
  int fd = socket (AF_INET, SOCK_STREAM, 0);
  _libga68_errno = errno;
  if (fd < 0)
    goto error;

  /* Lookup the specified host.  */
  char *host = _libga68_malloc_internal (u8len + 1);
  memcpy (host, u8host, u8len);
  host[u8len] = '\0';
  struct hostent *server = gethostbyname (host);
  if (server == NULL)
    {
      _libga68_errno = h_errno;
      goto close_fd_and_error;
    }

  /* Connect the socket to the server.  */
  struct sockaddr_in serv_addr;
  memset (&serv_addr, 0, sizeof (serv_addr));
  serv_addr.sin_family = AF_INET;
  serv_addr.sin_port = htons (port);
  memcpy (&serv_addr.sin_addr.s_addr,
	  server->h_addr,
	  server->h_length);
  int res = connect (fd, (struct sockaddr *) &serv_addr,
		     sizeof (serv_addr));
  _libga68_errno = errno;
  if (res == -1)
    goto close_fd_and_error;

  _libga68_free_internal (host);
  return fd;

 close_fd_and_error:
  close (fd);
 error:
  _libga68_free_internal (host);
  return -1;
}

/* Implementation of the posix prelude `fsize'.  */

long long int
_libga68_posixfsize (int fd)
{
  struct stat stat;

  if (fstat (fd, &stat) == -1)
    {
      _libga68_errno = errno;
      return -1;
    }

  if (stat.st_size > LLONG_MAX)
    {
      _libga68_errno = EOVERFLOW;
      return -1;
    }

  return (long int) stat.st_size;
}

/* Implementation of the posix prelude `lseek'.  */
#define A68_SEEK_CUR 0
#define A68_SEEK_END 1
#define A68_SEEK_SET 2

long long int
_libga68_posixlseek (int fd, long long int offset, int whence)
{
  switch (whence)
    {
    case A68_SEEK_CUR:
      whence = SEEK_CUR;
      break;
    case A68_SEEK_END:
      whence = SEEK_END;
      break;
    case A68_SEEK_SET:
      whence = SEEK_SET;
      break;
    }

  long long int ret = (long long int) lseek(fd, offset, whence);
  _libga68_errno = errno;
  return ret;
}
