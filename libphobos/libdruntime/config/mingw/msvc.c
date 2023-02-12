/* Windows support code to wrap differences between different
   versions of the Microsoft C libaries.
   Copyright (C) 2021-2023 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#ifdef __MINGW32__
#include <_mingw.h>
#endif
#include <stdio.h>

/* The D runtime library defines stdin, stdout, and stderr as extern(C) symbols
   in the core.stdc.stdio module, and require initializing at start-up.  */
__attribute__((weakref ("stdin")))
static FILE *core_stdc_stdin;

__attribute__((weakref ("stdout")))
static FILE *core_stdc_stdout;

__attribute__((weakref ("stderr")))
static FILE *core_stdc_stderr;

/* Set to 1 if runtime is using libucrt.dll.  */
unsigned char msvcUsesUCRT;

void
init_msvc (void)
{
  core_stdc_stdin = stdin;
  core_stdc_stdout = stdout;
  core_stdc_stderr = stderr;

#if __MSVCRT_VERSION__ >= 0xE00
  msvcUsesUCRT = 1;
#endif
}

/* Phobos std.stdio module assumes these functions are present at link time,
   and not absent or macros.  */
#ifdef _fgetc_nolock
#undef _fgetc_nolock

int
_fgetc_nolock (FILE *fp)
{
  fp->_cnt--;
  if (fp->_cnt >= 0)
    {
      const int c = *fp->_ptr;
      fp->_ptr++;
      return c & 0xff;
    }
  else
    return _filbuf (fp);
}

#endif /* _fgetc_nolock */

#ifdef _fputc_nolock
#undef _fputc_nolock

int
_fputc_nolock (int c, FILE *fp)
{
  fp->_cnt--;
  if (fp->_cnt >= 0)
    {
      *fp->_ptr = (char) c;
      fp->_ptr++;
      return c & 0xff;
    }
  else
    return _flsbuf (c, fp);
}

#endif /* _fputc_nolock */

#ifdef rewind
#undef rewind

void
rewind (FILE *fp)
{
  fseek (fp, 0, SEEK_SET);
  fp->_flag &= ~_IOERR;
}

#endif /* rewind */

#ifdef clearerr
#undef clearerr

void
clearerr (FILE *fp)
{
  fp->_flag &= ~(_IOERR | _IOEOF);
}

#endif /* clearerr */

#ifdef feof
#undef feof

int
feof (FILE *fp)
{
  return fp->_flag & _IOEOF;
}

#endif /* feof */

#ifdef ferror
#undef ferror

int
ferror (FILE *fp)
{
  return fp->_flag & _IOERR;
}

#endif /* ferror */

#ifdef fileno
#undef fileno

int
fileno (FILE *fp)
{
  return fp->_file;
}

#endif /* fileno */

/* Phobos std.stdio module has a dependency on the UCRT library, so provide
   stubs that forward to the nearest equivalent.  */
#if __MSVCRT_VERSION__ < 0x800

wint_t
_fgetwc_nolock (FILE *fp)
{
  return fgetwc (fp);
}

wint_t
_fputwc_nolock (wchar_t c, FILE *fp)
{
    return fputwc(c, fp);
}

#endif /* __MSVCRT_VERSION__ < 0x800*/
