/* This is part of the iostream/stdio library, providing -*- C -*- I/O.
   Define ANSI C stdio on top of C++ iostreams.
   Copyright (C) 1991, 1994 Free Software Foundation

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.


This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*/

/*
 *	ANSI Standard: 4.9 INPUT/OUTPUT <stdio.h>
 */

#ifndef _STDIO_H
#define _STDIO_H
#define _STDIO_USES_IOSTREAM

#include <libio.h>

#ifndef NULL
#ifdef __cplusplus
#define NULL 0
#else
#define NULL (void*)0
#endif
#endif

#ifndef EOF
#define EOF (-1)
#endif
#ifndef BUFSIZ
#define BUFSIZ 1024
#endif

#define _IOFBF 0 /* Fully buffered. */
#define _IOLBF 1 /* Line buffered. */
#define _IONBF 2 /* No buffering. */

#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2

 /* define size_t.  Crud in case <sys/types.h> has defined it. */
#if !defined(_SIZE_T) && !defined(_T_SIZE_) && !defined(_T_SIZE)
#if !defined(__SIZE_T) && !defined(_SIZE_T_) && !defined(___int_size_t_h)
#if !defined(_GCC_SIZE_T) && !defined(_SIZET_)
#define _SIZE_T
#define _T_SIZE_
#define _T_SIZE
#define __SIZE_T
#define _SIZE_T_
#define ___int_size_t_h
#define _GCC_SIZE_T
#define _SIZET_
typedef _IO_size_t size_t;
#endif
#endif
#endif

typedef struct _IO_FILE FILE;
typedef _IO_fpos_t fpos_t;

#define FOPEN_MAX     _G_FOPEN_MAX
#define FILENAME_MAX _G_FILENAME_MAX
#ifndef TMP_MAX
#define TMP_MAX 999 /* Only limited by filename length */
#endif

#define L_ctermid     9
#define L_cuserid     9
#define P_tmpdir      "/tmp"
#define L_tmpnam      20

/* For use by debuggers. These are linked in if printf or fprintf are used. */
extern FILE *stdin, *stdout, *stderr; /* TODO */

#define stdin _IO_stdin
#define stdout _IO_stdout
#define stderr _IO_stderr

#ifdef __cplusplus
extern "C" {
#endif

#ifndef __P
#if defined(__STDC__) || defined(__cplusplus) || defined(c_plusplus)
#define __P(args) args
#else
#define __P(args) ()
#endif
#endif /*!__P*/

extern void clearerr __P((FILE*));
extern int fclose __P((FILE*));
extern int feof __P((FILE*));
extern int ferror __P((FILE*));
extern int fflush __P((FILE*));
extern int fgetc __P((FILE *));
extern int fgetpos __P((FILE* fp, fpos_t *pos));
extern char* fgets __P((char*, int, FILE*));
extern FILE* fopen __P((const char*, const char*));
extern int fprintf __P((FILE*, const char* format, ...));
extern int fputc __P((int, FILE*));
extern int fputs __P((const char *str, FILE *fp));
extern size_t fread __P((void*, size_t, size_t, FILE*));
extern FILE* freopen __P((const char*, const char*, FILE*));
extern int fscanf __P((FILE *fp, const char* format, ...));
extern int fseek __P((FILE* fp, long int offset, int whence));
extern int fsetpos __P((FILE* fp, const fpos_t *pos));
extern long int ftell __P((FILE* fp));
extern size_t fwrite __P((const void*, size_t, size_t, FILE*));
extern int getc __P((FILE *));
extern int getchar __P((void));
extern char* gets __P((char*));
extern void perror __P((const char *));
extern int printf __P((const char* format, ...));
extern int putc __P((int, FILE *));
extern int putchar __P((int));
extern int puts __P((const char *str));
extern int remove __P((const char*));
extern int rename __P((const char* _old, const char* _new));
extern void rewind __P((FILE*));
extern int scanf __P((const char* format, ...));
extern void setbuf __P((FILE*, char*));
extern void setlinebuf __P((FILE*));
extern void setbuffer __P((FILE*, char*, int));
extern int setvbuf __P((FILE*, char*, int mode, size_t size));
extern int sprintf __P((char*, const char* format, ...));
extern int sscanf __P((const char* string, const char* format, ...));
extern FILE* tmpfile __P((void));
extern char* tmpnam __P((char*));
extern int ungetc __P((int c, FILE* fp));
extern int vfprintf __P((FILE *fp, char const *fmt0, _IO_va_list));
extern int vprintf __P((char const *fmt, _IO_va_list));
extern int vsprintf __P((char* string, const char* format, _IO_va_list));

#ifndef __STRICT_ANSI__
extern int vfscanf __P((FILE*, const char *, _IO_va_list));
extern int vscanf __P((const char *, _IO_va_list));
extern int vsscanf __P((const char *, const char *, _IO_va_list));
#endif

#if !defined(__STRICT_ANSI__) || defined(_POSIX_SOURCE)
extern FILE *fdopen __P((int, const char *));
extern int fileno __P((FILE*));
extern FILE* popen __P((const char*, const char*));
extern int pclose __P((FILE*));
#endif

#ifdef __USE_GNU
extern _IO_ssize_t getdelim __P ((char **, size_t *, int, FILE*));
extern _IO_ssize_t getline __P ((char **, size_t *, FILE *));

extern int snprintf __P ((char *, size_t, const char *, ...));
extern int vsnprintf __P ((char *, size_t, const char *, _IO_va_list));
#endif

extern int __underflow __P((struct _IO_FILE*));
extern int __overflow __P((struct _IO_FILE*, int));

/* Handle locking of streams.  */
#if defined _REENTRANT || defined _THREAD_SAFE
extern void clearerr_locked __P ((FILE *));
extern void clearerr_unlocked __P ((FILE *));
extern int feof_locked __P ((FILE *));
extern int feof_unlocked __P ((FILE *));
extern int ferror_locked __P ((FILE*));
extern int ferror_unlocked __P ((FILE*));
extern int fileno_locked __P ((FILE *));
extern int fileno_unlocked __P ((FILE *));
extern void flockfile __P ((FILE *));
extern void funlockfile __P ((FILE *));
extern int ftrylockfile __P ((FILE *));
extern int fclose_unlocked __P ((FILE *));
extern int fflush_locked __P ((FILE *));
extern int fflush_unlocked __P ((FILE *));
extern size_t fread_unlocked __P ((void *, size_t, size_t, FILE *));
extern size_t fwrite_unlocked __P ((const void *, size_t, size_t, FILE *));

extern int fputc_locked __P ((int, FILE*));
extern int fputc_unlocked __P ((int, FILE*));
extern int getc_locked __P ((FILE *));
extern int getc_unlocked __P ((FILE *));
extern int getchar_locked __P ((void));
extern int getchar_unlocked __P ((void));
extern int putc_locked __P ((int, FILE *));
extern int putc_unlocked __P ((int, FILE *));
extern int putchar_locked __P ((int));
extern int putchar_unlocked __P ((int));

# define getc_unlocked(fp) _IO_getc_unlocked (fp)
# define getc_locked(fp) _IO_getc (fp)
# define getchar_unlocked() _IO_getc_unlocked (stdin)
# define getchar_locked() _IO_getc (stdin)
# define putchar_unlocked(c) _IO_putc_unlocked (c, stdout)
# define putchar_locked(c) _IO_putc (c, stdout)
#endif /* __USE_REENTRANT */

#define getc(fp) _IO_getc(fp)
#define putc(c, fp) _IO_putc(c, fp)
#define putchar(c) _IO_putc(c, stdout)
#define getchar() _IO_getc(stdin)

#ifdef __cplusplus
}
#endif

#endif /*!_STDIO_H*/
