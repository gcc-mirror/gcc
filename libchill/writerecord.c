/* Implement Input/Output runtime actions for CHILL.
   Copyright (C) 1992,1993 Free Software Foundation, Inc.
   Author: Wilfried Moser, et al

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* As a special exception, if you link this library with other files,
   some of which are compiled with GCC, to produce an executable,
   this library does not by itself cause the resulting executable
   to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */

#include <setjmp.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>

#include "fileio.h"

static
void
doWrite( Access_Mode* the_access, void* buf, size_t nbyte )
{
  size_t nwrit;

  nwrit = write( the_access->association->handle, buf, nbyte );

  if( nwrit < nbyte )
  {
    the_access->association->syserrno = errno;
    RWEXCEPTION( WRITEFAIL, OS_IO_ERROR );
  }
}


void
__writerecord( Access_Mode*  the_access,
               signed long   the_index,
               char*         the_val_addr,
               unsigned long the_val_len,
               char*         file,
               int           line )

{
  Association_Mode* the_assoc;
  unsigned long  info;
  char*          actaddr;
  unsigned short actlen;
  off_t          filepos;

  if( !the_access )
    CHILLEXCEPTION( file, line, EMPTY, NULL_ACCESS );

  if( !(the_assoc = the_access->association) )
    CHILLEXCEPTION( file, line, NOTCONNECTED, IS_NOT_CONNECTED );

  /* Usage must no be ReadOnly */
  if( the_assoc->usage == ReadOnly )
    CHILLEXCEPTION( file, line, WRITEFAIL, BAD_USAGE );

  /*
   *  Positioning
   */
  if( TEST_FLAG( the_access, IO_INDEXED ) )
  {
    /* index expression must be within bounds of index mode */
    if( the_index < the_access->lowindex
        || the_access->highindex < the_index )
      CHILLEXCEPTION( file, line, RANGEFAIL, BAD_INDEX );
    filepos = the_access->base + 
              (the_index - the_access->lowindex) * the_access->reclength;

    if( lseek( the_assoc->handle, filepos, SEEK_SET ) == -1L )
      CHILLEXCEPTION( file, line, WRITEFAIL, LSEEK_FAILS );
  }

  if( (info = setjmp( __rw_exception )) )
    CHILLEXCEPTION( file, line, info>>16, info & 0xffff );

  if( TEST_FLAG( the_access, IO_TEXTIO ) )
  {
    if( TEST_FLAG( the_access, IO_INDEXED ) )
    {
      int nspace = the_access->reclength - the_val_len;
      memset( the_val_addr + 2 + the_val_len, ' ', nspace );
      actlen = the_access->reclength - 2;
      MOV2(the_val_addr,&actlen);
      doWrite( the_access, the_val_addr, the_access->reclength );
    }
    else
    { 
      if( the_assoc->ctl_pre )
	write( the_assoc->handle, &the_assoc->ctl_pre, 1 );
      MOV2(&actlen,the_val_addr);
      write( the_assoc->handle, the_val_addr + 2, actlen );
      if( the_assoc->ctl_post )
	write( the_assoc->handle, &the_assoc->ctl_post, 1 );
      the_assoc->ctl_pre  = '\0';
      the_assoc->ctl_post = '\n';
    }
  }
  else
  {
    switch( the_access->rectype )
    {
    case Fixed:
      if( TEST_FLAG( the_assoc, IO_VARIABLE ) )
      {
        actlen = the_access->reclength;
        doWrite( the_access, &actlen, sizeof(actlen) );
      }
      doWrite( the_access, the_val_addr, the_val_len );
      break;
    case VaryingChars:
      MOV2(&actlen,the_val_addr);
      if( actlen > the_access->reclength - 2 )
        CHILLEXCEPTION( file, line, RANGEFAIL, RECORD_TOO_LONG );
      actlen = TEST_FLAG( the_access, IO_INDEXED ) 
               ? the_access->reclength : actlen + 2;
      doWrite( the_access, the_val_addr, actlen );
      break;
    }
  }
}
