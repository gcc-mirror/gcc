/* termdep.h -- System things that terminal.c depends on.
   $Id: termdep.h,v 1.3 1996/10/02 22:23:52 karl Exp $

   This file is part of GNU Info, a program for reading online documentation
   stored in Info format.

   Copyright (C) 1993, 96 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

   Written by Brian Fox (bfox@ai.mit.edu). */

#if !defined (_TERMDEP_H_)
#  define _TERMDEP_H_

#if defined (HAVE_SYS_FCNTL_H)
#  include <sys/fcntl.h>
#else
#  include <fcntl.h>
#endif /* !HAVE_SYS_FCNTL_H */

#if defined (HAVE_SYS_FILE_H)
#  include <sys/file.h>
#endif /* HAVE_SYS_FILE_H */

#if defined (HAVE_STRINGS_H)
#  include <strings.h>
#else
#  if defined (HAVE_STRING_H)
#    include <string.h>
#  endif
#endif

#if defined (HAVE_TERMIOS_H)
#  include <termios.h>
#else
#  if defined (HAVE_TERMIO_H)
#    include <termio.h>
#    if defined (HAVE_SYS_PTEM_H)
#      if defined (M_UNIX) || !defined (M_XENIX)
#        include <sys/stream.h>
#        include <sys/ptem.h>
#        undef TIOCGETC
#      else /* M_XENIX */
#        define tchars tc
#      endif /* M_XENIX */
#    endif /* HAVE_SYS_PTEM_H */
#  else /* !HAVE_TERMIO_H */
#    include <sgtty.h>
#  endif /* !HAVE_TERMIO_H */
#endif /* !HAVE_TERMIOS_H */

#if defined (HAVE_SYS_TTOLD_H)
#  include <sys/ttold.h>
#endif /* HAVE_SYS_TTOLD_H */

#if !defined (HAVE_STRCHR)
#  undef strchr
#  undef strrchr
#  define strchr index
#  define strrchr rindex
#endif /* !HAVE_STRCHR */

#endif /* _TERMDEP_H_ */
