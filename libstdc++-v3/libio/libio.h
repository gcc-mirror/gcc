/* Copyright (C) 1991, 92, 93, 94, 95, 97, 98 Free Software Foundation, Inc.
   This file is part of the GNU IO Library.
   Written by Per Bothner <bothner@cygnus.com>.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this library; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
   MA 02111-1307, USA.

   As a special exception, if you link this library with files
   compiled with a GNU compiler to produce an executable, this does
   not cause the resulting executable to be covered by the GNU General
   Public License.  This exception does not however invalidate any
   other reasons why the executable file might be covered by the GNU
   General Public License.  */

#ifndef _IO_STDIO_H
#define _IO_STDIO_H

#include <_G_config.h>
#define _IO_pos_t _G_fpos_t /* obsolete */
#define _IO_fpos_t _G_fpos_t
#define _IO_size_t _G_size_t
#define _IO_ssize_t _G_ssize_t
#define _IO_off_t _G_off_t
#define _IO_pid_t _G_pid_t
#define _IO_uid_t _G_uid_t
#define _IO_HAVE_SYS_WAIT _G_HAVE_SYS_WAIT
#define _IO_HAVE_ST_BLKSIZE _G_HAVE_ST_BLKSIZE
#define _IO_BUFSIZ _G_BUFSIZ
#define _IO_va_list _G_va_list
#if defined(_G_IO_IO_FILE_VERSION) && _G_IO_IO_FILE_VERSION == 0x20001
#define _IO_fpos64_t _G_fpos64_t
#define _IO_off64_t _G_off64_t
#endif

#ifdef _G_NEED_STDARG_H
/* This define avoids name pollution if we're using GNU stdarg.h */
# define __need___va_list
# include <stdarg.h>
# ifdef __GNUC_VA_LIST
#  undef _IO_va_list
#  define _IO_va_list __gnuc_va_list
# endif /* __GNUC_VA_LIST */
#endif

#ifndef __P
# if _G_HAVE_SYS_CDEFS
#  include <sys/cdefs.h>
# else
#  ifdef __STDC__
#   define __P(p) p
#  else
#   define __P(p) ()
#  endif
# endif
#endif /*!__P*/

#ifndef __PMT
# ifdef __STDC__
#  define __PMT(p) p
# else
#  define __PMT(p) ()
# endif
#endif /*!__P*/

/* For backward compatibility */
#ifndef _PARAMS
# define _PARAMS(protos) __P(protos)
#endif /*!_PARAMS*/

#ifndef __STDC__
# define const
#endif
#define _IO_UNIFIED_JUMPTABLES 1
#if !_G_HAVE_PRINTF_FP
# define _IO_USE_DTOA 1
#endif

#ifndef EOF
# define EOF (-1)
#endif
#ifndef NULL
# if defined __GNUG__ && \
    (__GNUC__ > 2 || (__GNUC__ == 2 && __GNUC_MINOR__ >= 8))
#  define NULL (__null)
# else
#  if !defined(__cplusplus)
#   define NULL ((void*)0)
#  else
#   define NULL (0)
#  endif
# endif
#endif

#define _IOS_INPUT	1
#define _IOS_OUTPUT	2
#define _IOS_ATEND	4
#define _IOS_APPEND	8
#define _IOS_TRUNC	16
#define _IOS_NOCREATE	32
#define _IOS_NOREPLACE	64
#define _IOS_BIN	128

/* Magic numbers and bits for the _flags field.
   The magic numbers use the high-order bits of _flags;
   the remaining bits are available for variable flags.
   Note: The magic numbers must all be negative if stdio
   emulation is desired. */

#define _IO_MAGIC 0xFBAD0000 /* Magic number */
#define _OLD_STDIO_MAGIC 0xFABC0000 /* Emulate old stdio. */
#define _IO_MAGIC_MASK 0xFFFF0000
#define _IO_USER_BUF 1 /* User owns buffer; don't delete it on close. */
#define _IO_UNBUFFERED 2
#define _IO_NO_READS 4 /* Reading not allowed */
#define _IO_NO_WRITES 8 /* Writing not allowd */
#define _IO_EOF_SEEN 0x10
#define _IO_ERR_SEEN 0x20
#define _IO_DELETE_DONT_CLOSE 0x40 /* Don't call close(_fileno) on cleanup. */
#define _IO_LINKED 0x80 /* Set if linked (using _chain) to streambuf::_list_all.*/
#define _IO_IN_BACKUP 0x100
#define _IO_LINE_BUF 0x200
#define _IO_TIED_PUT_GET 0x400 /* Set if put and get pointer logicly tied. */
#define _IO_CURRENTLY_PUTTING 0x800
#define _IO_IS_APPENDING 0x1000
#define _IO_IS_FILEBUF 0x2000
#define _IO_BAD_SEEN 0x4000

/* These are "formatting flags" matching the iostream fmtflags enum values. */
#define _IO_SKIPWS 01
#define _IO_LEFT 02
#define _IO_RIGHT 04
#define _IO_INTERNAL 010
#define _IO_DEC 020
#define _IO_OCT 040
#define _IO_HEX 0100
#define _IO_SHOWBASE 0200
#define _IO_SHOWPOINT 0400
#define _IO_UPPERCASE 01000
#define _IO_SHOWPOS 02000
#define _IO_SCIENTIFIC 04000
#define _IO_FIXED 010000
#define _IO_UNITBUF 020000
#define _IO_STDIO 040000
#define _IO_DONT_CLOSE 0100000
#define _IO_BOOLALPHA 0200000


struct _IO_jump_t;  struct _IO_FILE;

/* Handle lock.  */
#ifdef _IO_MTSAFE_IO
# if defined __GLIBC__ && __GLIBC__ >= 2
#  if __GLIBC_MINOR__ > 0
#   include <bits/stdio-lock.h>
#  else
#   include <stdio-lock.h>
#  endif
#  define _IO_LOCK_T _IO_lock_t *
# else
/*# include <comthread.h>*/
# endif
#else
# if defined(__GLIBC__) && __GLIBC__ >= 2
   typedef void _IO_lock_t;
#  define _IO_LOCK_T void *
# else
#  ifdef __linux__
    struct _IO_lock_t { void *ptr; short int field1; short int field2; };
#   define _IO_LOCK_T struct _IO_lock_t
#  else
    typedef void _IO_lock_t;
#   define _IO_LOCK_T void *
#  endif
# endif
#endif


/* A streammarker remembers a position in a buffer. */

struct _IO_marker {
  struct _IO_marker *_next;
  struct _IO_FILE *_sbuf;
  /* If _pos >= 0
 it points to _buf->Gbase()+_pos. FIXME comment */
  /* if _pos < 0, it points to _buf->eBptr()+_pos. FIXME comment */
  int _pos;
#if 0
    void set_streampos(streampos sp) { _spos = sp; }
    void set_offset(int offset) { _pos = offset; _spos = (streampos)(-2); }
  public:
    streammarker(streambuf *sb);
    ~streammarker();
    int saving() { return  _spos == -2; }
    int delta(streammarker&);
    int delta();
#endif
};

struct _IO_FILE {
  int _flags;		/* High-order word is _IO_MAGIC; rest is flags. */
#define _IO_file_flags _flags

  /* The following pointers correspond to the C++ streambuf protocol. */
  /* Note:  Tk uses the _IO_read_ptr and _IO_read_end fields directly. */
  char* _IO_read_ptr;	/* Current read pointer */
  char* _IO_read_end;	/* End of get area. */
  char* _IO_read_base;	/* Start of putback+get area. */
  char* _IO_write_base;	/* Start of put area. */
  char* _IO_write_ptr;	/* Current put pointer. */
  char* _IO_write_end;	/* End of put area. */
  char* _IO_buf_base;	/* Start of reserve area. */
  char* _IO_buf_end;	/* End of reserve area. */
  /* The following fields are used to support backing up and undo. */
  char *_IO_save_base; /* Pointer to start of non-current get area. */
  char *_IO_backup_base;  /* Pointer to first valid character of backup area */
  char *_IO_save_end; /* Pointer to end of non-current get area. */

  struct _IO_marker *_markers;

  struct _IO_FILE *_chain;

  int _fileno;
  int _blksize;
#ifdef _G_IO_IO_FILE_VERSION
  _IO_off_t _old_offset;
#else
  _IO_off_t _offset;
#endif

#define __HAVE_COLUMN /* temporary */
  /* 1+column number of pbase(); 0 is unknown. */
  unsigned short _cur_column;
  char _unused;
  char _shortbuf[1];

  /*  char* _save_gptr;  char* _save_egptr; */

#ifdef _IO_LOCK_T
  _IO_LOCK_T _lock;
#endif
#if defined(_G_IO_IO_FILE_VERSION) && _G_IO_IO_FILE_VERSION == 0x20001
  _IO_off64_t _offset;
  int _unused2[16];	/* Make sure we don't get into trouble again.  */
#endif
};

#ifndef __cplusplus
typedef struct _IO_FILE _IO_FILE;
#endif

#if defined(_G_IO_IO_FILE_VERSION) && _G_IO_IO_FILE_VERSION == 0x20001
#define _IO_stdin_ _IO_2_1_stdin_
#define _IO_stdout_ _IO_2_1_stdout_
#define _IO_stderr_ _IO_2_1_stderr_
#endif

struct _IO_FILE_plus;
extern struct _IO_FILE_plus _IO_stdin_, _IO_stdout_, _IO_stderr_;
#define _IO_stdin ((_IO_FILE*)(&_IO_stdin_))
#define _IO_stdout ((_IO_FILE*)(&_IO_stdout_))
#define _IO_stderr ((_IO_FILE*)(&_IO_stderr_))


/* Define the user-visible type, with user-friendly member names.  */
typedef struct
{
  _IO_ssize_t (*read) __PMT ((struct _IO_FILE *, void *, _IO_ssize_t));
  _IO_ssize_t (*write) __PMT ((struct _IO_FILE *, const void *, _IO_ssize_t));
  _IO_off_t (*seek) __PMT ((struct _IO_FILE *, _IO_off_t, int));
  int (*close) __PMT ((struct _IO_FILE *));
} _IO_cookie_io_functions_t;

/* Special file type for fopencookie function.  */
struct _IO_cookie_file
{
  struct _IO_FILE file;
  const void *vtable;
  void *cookie;
  _IO_cookie_io_functions_t io_functions;
};


#ifdef __cplusplus
extern "C" {
#endif

extern int __underflow __P ((_IO_FILE *));
extern int __uflow __P ((_IO_FILE *));
extern int __overflow __P ((_IO_FILE *, int));

#define _IO_getc_unlocked(_fp) \
       ((_fp)->_IO_read_ptr >= (_fp)->_IO_read_end ? __uflow (_fp) \
	: *(unsigned char *) (_fp)->_IO_read_ptr++)
#define _IO_peekc_unlocked(_fp) \
       ((_fp)->_IO_read_ptr >= (_fp)->_IO_read_end \
	  && __underflow (_fp) == EOF ? EOF \
	: *(unsigned char *) (_fp)->_IO_read_ptr)

#define _IO_putc_unlocked(_ch, _fp) \
   (((_fp)->_IO_write_ptr >= (_fp)->_IO_write_end) \
    ? __overflow (_fp, (unsigned char) (_ch)) \
    : (unsigned char) (*(_fp)->_IO_write_ptr++ = (_ch)))

#define _IO_feof_unlocked(__fp) (((__fp)->_flags & _IO_EOF_SEEN) != 0)
#define _IO_ferror_unlocked(__fp) (((__fp)->_flags & _IO_ERR_SEEN) != 0)

extern int _IO_getc __P ((_IO_FILE *__fp));
extern int _IO_putc __P ((int __c, _IO_FILE *__fp));
extern int _IO_feof __P ((_IO_FILE *__fp));
extern int _IO_ferror __P ((_IO_FILE *__fp));

extern int _IO_peekc_locked __P ((_IO_FILE *__fp));

/* This one is for Emacs. */
#define _IO_PENDING_OUTPUT_COUNT(_fp)	\
	((_fp)->_IO_write_ptr - (_fp)->_IO_write_base)

extern void _IO_flockfile __P ((_IO_FILE *));
extern void _IO_funlockfile __P ((_IO_FILE *));
extern int _IO_ftrylockfile __P ((_IO_FILE *));

#ifdef _IO_MTSAFE_IO
# define _IO_peekc(_fp) _IO_peekc_locked (_fp)
#else
# define _IO_peekc(_fp) _IO_peekc_unlocked (_fp)
# define _IO_flockfile(_fp) /**/
# define _IO_funlockfile(_fp) /**/
# define _IO_ftrylockfile(_fp) /**/
# define _IO_cleanup_region_start(_fct, _fp) /**/
# define _IO_cleanup_region_end(_Doit) /**/
#endif /* !_IO_MTSAFE_IO */


extern int _IO_vfscanf __P ((_IO_FILE *, const char *, _IO_va_list, int *));
extern int _IO_vfprintf __P ((_IO_FILE *, const char *, _IO_va_list));
extern _IO_ssize_t _IO_padn __P ((_IO_FILE *, int, _IO_ssize_t));
extern _IO_size_t _IO_sgetn __P ((_IO_FILE *, void *, _IO_size_t));

#if defined(_G_IO_IO_FILE_VERSION) && _G_IO_IO_FILE_VERSION == 0x20001
extern _IO_off64_t _IO_seekoff __P ((_IO_FILE *, _IO_off64_t, int, int));
extern _IO_off64_t _IO_seekpos __P ((_IO_FILE *, _IO_off64_t, int));
#else
extern _IO_off_t _IO_seekoff __P ((_IO_FILE *, _IO_off_t, int, int));
extern _IO_off_t _IO_seekpos __P ((_IO_FILE *, _IO_off_t, int));
#endif

extern void _IO_free_backup_area __P ((_IO_FILE *));

#ifdef __cplusplus
}
#endif

#endif /* _IO_STDIO_H */
