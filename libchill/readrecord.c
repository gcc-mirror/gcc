/* Implement Input/Output runtime actions for CHILL.
   Copyright (C) 1992, 1993, 1998 Free Software Foundation, Inc.
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
#include <stdlib.h>
#include <errno.h>
#include <sys/types.h>
#include <unistd.h>

#include "fileio.h"

#ifdef EOF
#undef EOF
#endif
#define EOF -1

static
Boolean
doRead( Access_Mode* the_access, void* buf, size_t nbyte )
{
  size_t nread;

  nread = read( the_access->association->handle, buf, nbyte );
  if( nread == nbyte )
  {
    CLR_FLAG( the_access, IO_OUTOFFILE );
    return True;
  }
  if( nread == 0 )
  {
    SET_FLAG( the_access, IO_OUTOFFILE );
    return False;
  }
  the_access->association->syserrno = errno;
  RWEXCEPTION( READFAIL, OS_IO_ERROR );
  /* no return */
}

static
int bgetc( int handle, readbuf_t* rbptr )
{
  if( rbptr->cur >= rbptr->len )
    {
      rbptr->len = read( handle, rbptr->buf, READBUFLEN );
      if( rbptr->len == 0 )
	return EOF;
      rbptr->cur = 0;
    }
  return rbptr->buf[rbptr->cur++];
}

static
void bungetc( readbuf_t* rbptr, int c )
{
  rbptr->buf[--rbptr->cur] = c;
}

void*
__readrecord( Access_Mode*  the_access,
              signed long   the_index,
              char*         the_buf_addr,
              char*         file,
              int           line )
{
  unsigned long  info;
  char*          actaddr;
  unsigned short actlen;
  off_t          filepos;
  unsigned short reclen;
  unsigned long  readlen;

  if( !the_access )
    CHILLEXCEPTION( file, line, EMPTY, NULL_ACCESS );

  if( !the_access->association )
    CHILLEXCEPTION( file, line, NOTCONNECTED, IS_NOT_CONNECTED );

  /* Usage must not be WriteOnly */
  if( the_access->association->usage == WriteOnly )
    CHILLEXCEPTION( file, line, READFAIL, BAD_USAGE );

  /* OUTOFFILE must not be True when connected for sequential read */
  if( !TEST_FLAG( the_access, IO_INDEXED )
      && TEST_FLAG( the_access, IO_OUTOFFILE ) )
    CHILLEXCEPTION( file, line, READFAIL, OUT_OF_FILE );

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

    if( lseek( the_access->association->handle, filepos, SEEK_SET ) == -1L )
      CHILLEXCEPTION( file, line, READFAIL, LSEEK_FAILS );
  }

  /* establish store loc */
  if( !(actaddr = the_buf_addr ))
  {
    /* if not yet allocated, do it now */
    if (!the_access->store_loc)
      if( !(the_access->store_loc = (char*)malloc( the_access->reclength ) ) )
	CHILLEXCEPTION( file, line, SPACEFAIL, STORE_LOC_ALLOC );
    actaddr = the_access->store_loc;
  }
  actlen  = the_access->reclength;

  if( (info = setjmp( __rw_exception )) )
    CHILLEXCEPTION( file, line, info>>16, info & 0xffff );

  if( TEST_FLAG( the_access, IO_TEXTIO ) )
  {
    readlen = actlen - 2;
    if( TEST_FLAG( the_access, IO_INDEXED ) )
    {
      if( ! doRead( the_access, &reclen, sizeof(reclen) ) )
        return NULL;
      if( reclen > readlen )
        CHILLEXCEPTION( file, line, RANGEFAIL, RECORD_TOO_LONG );
      if( ! doRead( the_access, actaddr + 2, reclen ) )
        CHILLEXCEPTION( file, line, READFAIL, RECORD_TOO_SHORT );
    }
    else
    { 
      Association_Mode *assoc = the_access->association;
      int              handle = assoc->handle;
      readbuf_t*       rbuf   = assoc->bufptr;
      char* cptr = actaddr+2;
      int   curr;

      reclen = 0;
      while( readlen-- )
      {
        curr = bgetc( handle, rbuf );
        if( curr == '\n' )
          goto end_of_line;
        if( curr == EOF )
	{
          if( !reclen )
            SET_FLAG( the_access, IO_OUTOFFILE );
          goto end_of_line;
	}
        *cptr++ = curr;
        reclen++;
      }
      if( (curr = bgetc( handle, rbuf )) != '\n' )
	{
	  bungetc( rbuf, curr );
	  CHILLEXCEPTION( file, line, RANGEFAIL, RECORD_TOO_LONG );
	}
end_of_line: ;
    }
    MOV2(actaddr,&reclen);
  }
  else
  {
    switch( the_access->rectype )
    {
    case Fixed:
      if( ! doRead( the_access, actaddr, actlen ) )
        return NULL;
      break;
    case VaryingChars:
      if( TEST_FLAG( the_access->association, IO_VARIABLE ) )
      {
        if( ! doRead( the_access, &reclen, sizeof(reclen) ) )
          return NULL;
        if( reclen > actlen - 2 )
          CHILLEXCEPTION( file, line, RANGEFAIL, RECORD_TOO_LONG );
        readlen = TEST_FLAG( the_access, IO_INDEXED ) ? actlen - 2 : reclen;
        if( ! doRead( the_access, actaddr + 2, readlen ) )
          CHILLEXCEPTION( file, line, READFAIL, RECORD_TOO_SHORT );
      }
      else
      {
        if( ! doRead( the_access, actaddr + 2, reclen = actlen - 2 ) )
          CHILLEXCEPTION( file, line, READFAIL, RECORD_TOO_SHORT );
      }
      MOV2(actaddr,&reclen);
      break;
    }
  }

  return actaddr;
}
