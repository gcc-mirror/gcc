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

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <limits.h>
#include <errno.h>

#include <string.h>
#include <stdlib.h>

#include "fileio.h"

#ifndef PATH_MAX
#ifdef _POSIX_PATH_MAX
#define PATH_MAX _POSIX_PATH_MAX
#else
#ifdef MAXPATHLEN
#define PATH_MAX MAXPATHLEN
#endif
#endif
#endif

static
void
GetSetAttributes( Association_Mode* the_assoc )
{
  struct stat statbuf;
  int retco;

  if( (retco = stat( the_assoc->pathname, &statbuf )) )
    return;

  if( S_ISREG(statbuf.st_mode) )
    {
      SET_FLAG( the_assoc, IO_EXISTING );
      if( !TEST_FLAG( the_assoc, IO_VARIABLE ) )
        SET_FLAG( the_assoc, IO_INDEXABLE );
    }
  else
    if( S_ISCHR(statbuf.st_mode) || S_ISFIFO(statbuf.st_mode) )
      {
	SET_FLAG( the_assoc, IO_EXISTING );
	CLR_FLAG( the_assoc, IO_INDEXABLE );
      }
  SET_FLAG( the_assoc, IO_SEQUENCIBLE );

  /* FIXME: File size and computation of number of records for outoffile ? */

  if( !access( the_assoc->pathname, R_OK ) )
    SET_FLAG( the_assoc, IO_READABLE );
  if( !access( the_assoc->pathname, W_OK ) )
    SET_FLAG( the_assoc, IO_WRITEABLE );
}

static
void 
makeName( Association_Mode* the_assoc, char* the_path, int the_path_len,
	 char* file, int line)
{
  int namlen;
  if( ! the_assoc->pathname && 
      ! (the_assoc->pathname = (char*)malloc( PATH_MAX )) )
    CHILLEXCEPTION( file, line, SPACEFAIL, PATHNAME_ALLOC );

  if( the_path[0] != DIRSEP )
    {
      if( !getcwd( the_assoc->pathname, PATH_MAX ) )
	{
	  the_assoc->syserrno = errno;
	  CHILLEXCEPTION( file, line, ASSOCIATEFAIL, GETCWD_FAILS );
	}
      namlen = strlen( the_assoc->pathname );
      the_assoc->pathname[namlen++] = DIRSEP;  
    }
  else
    namlen = 0;

  strncpy( the_assoc->pathname + namlen, the_path, the_path_len );
  the_assoc->pathname[namlen+the_path_len] = '\0';
}

/*
 * ASSOCIATE
 */
/* Caution: returns an Association mode location (!) */
Association_Mode*
__associate( Association_Mode* the_assoc,
	     char*             the_path,
	     int               the_path_len,
	     char*             the_mode,
	     int               the_mode_len,
	     char*             file,
	     int               line )
{
  if( !the_assoc )
    CHILLEXCEPTION( file, line, EMPTY, NULL_ASSOCIATION );

  if( TEST_FLAG(the_assoc, IO_ISASSOCIATED) )
    CHILLEXCEPTION( file, line, ASSOCIATEFAIL, IS_ASSOCIATED );

  /* clear all flags */
  the_assoc->flags = 0;

  if( ! the_path_len )
    CHILLEXCEPTION( file, line, ASSOCIATEFAIL, NO_PATH_NAME );

  makeName( the_assoc, the_path, the_path_len, file, line );
  GetSetAttributes( the_assoc );

  CLR_FLAG( the_assoc, IO_VARIABLE );
  if ( the_mode )
    {
      if( !strncmp( the_mode, "VARIABLE", 8 ) )
	{
	  SET_FLAG( the_assoc, IO_VARIABLE );
	  CLR_FLAG( the_assoc, IO_INDEXABLE );
	}
      else
	if( strlen( the_mode ) )
	  CHILLEXCEPTION( file, line, ASSOCIATEFAIL, INVALID_ASSOCIATION_MODE );
    }

  SET_FLAG( the_assoc, IO_ISASSOCIATED );
  return the_assoc;
}

/*
 *  DISSOCIATE
 */
void
__dissociate( Association_Mode* the_assoc, char* file, int line )
{
  if( !the_assoc )
    CHILLEXCEPTION( file, line, EMPTY, NULL_ASSOCIATION );

  if( !TEST_FLAG( the_assoc, IO_ISASSOCIATED ) )
    CHILLEXCEPTION( file, line, NOTASSOCIATED, IS_NOT_ASSOCIATED );

  if( the_assoc->access )
    __disconnect( the_assoc->access, file, line );

  the_assoc->access = NULL;
  CLR_FLAG( the_assoc, IO_ISASSOCIATED );

  /* free allocated memory */
  if (the_assoc->pathname)
    {
      free (the_assoc->pathname);
      the_assoc->pathname = 0;
    }
  if (the_assoc->bufptr)
    {
      free (the_assoc->bufptr);
      the_assoc->bufptr = 0;
    }
}

/*
 * CREATE
 */
void __create( Association_Mode* the_assoc, char* file, int line )
{
  if( !the_assoc )
    CHILLEXCEPTION( file, line, EMPTY, NULL_ASSOCIATION );

  if( !TEST_FLAG( the_assoc, IO_ISASSOCIATED ) )
    CHILLEXCEPTION( file, line, NOTASSOCIATED, IS_NOT_ASSOCIATED );

  if( TEST_FLAG( the_assoc, IO_EXISTING ) )
    CHILLEXCEPTION( file, line, CREATEFAIL, FILE_EXISTING );

  if( (the_assoc->handle = open( the_assoc->pathname, O_CREAT+O_TRUNC+O_WRONLY, 0666 ))
      == -1 )
      CHILLEXCEPTION( file, line, CREATEFAIL, CREATE_FAILS );

  the_assoc->usage = ReadWrite;
  GetSetAttributes( the_assoc );

  close( the_assoc->handle );
}

/*
 * MODIFY
 */
void
__modify( Association_Mode* the_assoc,
	  char*             the_path,
	  int               the_path_len,
	  char*             the_mode,
	  int               the_mode_len,
	  char*             file,
	  int               line )
{
  if( !the_assoc )
    CHILLEXCEPTION( file, line, EMPTY, NULL_ASSOCIATION );

  if( !TEST_FLAG( the_assoc, IO_ISASSOCIATED ) )
    CHILLEXCEPTION( file, line, NOTASSOCIATED, IS_NOT_ASSOCIATED );

  if( the_path_len )
    {
      char* oldname;

      if( ! (oldname = (char*)malloc( PATH_MAX )) )
	CHILLEXCEPTION( file, line, SPACEFAIL, PATHNAME_ALLOC );
      strcpy( oldname, the_assoc->pathname );

      makeName( the_assoc, the_path, the_path_len, file, line );

      if( rename( oldname, the_assoc->pathname ) )
	{
	  free( oldname );
	  CHILLEXCEPTION( file, line, MODIFYFAIL, RENAME_FAILS );
	}
      free( oldname );
    }
  else
    {
      /* FIXME: other options? */
    }
}

static
/*** char* DirMode[] = { "rb", "r+b", "r+b" }; ***/
int DirMode[] = { O_RDONLY, O_RDWR, O_RDWR };

static
/*** char* SeqMode [] = { "rb", "r+b", "r+b" }; ***/
int SeqMode[] = { O_RDONLY, O_RDWR, O_RDWR };

/*
 * CONNECT
 */
void
__connect( void*             the_transfer,
	   Association_Mode* the_assoc,
	   Usage_Mode        the_usage,
	   Where_Mode        the_where,
	   Boolean           with_index,
	   signed long       the_index,
	   char*             file,
	   int               line )
{
  Access_Mode*  the_access;
  off_t         filepos;
  off_t         savepos;
  char          dummy;
  unsigned long nbytes;
  int           oflag;

  if( !the_transfer )
    CHILLEXCEPTION( file, line, EMPTY, NULL_ACCESS );
  if( !the_assoc )
    CHILLEXCEPTION( file, line, EMPTY, NULL_ASSOCIATION );

  if( TEST_FLAG((Text_Mode*)the_transfer, IO_TEXTLOCATION ))
    {
      if( ! ((Text_Mode*)the_transfer)->access_sub )
	CHILLEXCEPTION( file, line, EMPTY, NO_ACCESS_SUBLOCATION );
      the_access = ((Text_Mode*)the_transfer)->access_sub;
      SET_FLAG( the_access, IO_TEXTIO );
    }
  else
    {
      the_access = (Access_Mode*)the_transfer;
      CLR_FLAG( the_access, IO_TEXTIO );
    }

  /* FIXME: This should be an (implementation-dependent) static check
     if( with_index && the_access->rectype > Fixed )
     CHILLEXCEPTION( file, line, CONNECTFAIL, IMPL_RESTRICTION );
     */

  if( ! TEST_FLAG(the_assoc, IO_ISASSOCIATED) )
    CHILLEXCEPTION( file, line, NOTASSOCIATED, IS_NOT_ASSOCIATED );

  if( ! TEST_FLAG( the_assoc, IO_EXISTING ) )
    CHILLEXCEPTION( file, line, CONNECTFAIL, NOT_EXISTING );

  if( ! TEST_FLAG( the_assoc, IO_READABLE ) &&
      ( the_usage = ReadOnly || the_usage == ReadWrite ) )    
    CHILLEXCEPTION( file, line, CONNECTFAIL, NOT_READABLE );

  if( ! TEST_FLAG( the_assoc, IO_WRITEABLE ) &&
      ( the_usage = WriteOnly || the_usage == ReadWrite ) )    
    CHILLEXCEPTION( file, line, CONNECTFAIL, NOT_WRITEABLE );

  if( ! TEST_FLAG( the_assoc, IO_INDEXABLE ) 
      && TEST_FLAG( the_access, IO_INDEXED ) )
    CHILLEXCEPTION( file, line, CONNECTFAIL, NOT_INDEXABLE );

  if( ! TEST_FLAG( the_assoc, IO_SEQUENCIBLE ) 
      && ! TEST_FLAG( the_access, IO_INDEXED ) )
    CHILLEXCEPTION( file, line, CONNECTFAIL, NOT_SEQUENCIBLE );

  if( the_where == Same && the_assoc->access == NULL )
    CHILLEXCEPTION( file, line, CONNECTFAIL, NO_CURRENT_POS );

  /* This dynamic condition is not checked for text connections. */
  if( ! TEST_FLAG( the_access, IO_TEXTIO ) )
    if( ! TEST_FLAG( the_assoc, IO_VARIABLE ) 
	&& the_access->rectype > Fixed 
	&& ( the_usage == WriteOnly || the_usage == ReadWrite ) )
      CHILLEXCEPTION( file, line, CONNECTFAIL, NOT_VARIABLE );
 
  if( TEST_FLAG( the_assoc, IO_VARIABLE )
      && the_access->rectype == Fixed 
      && ( the_usage == ReadOnly || the_usage == ReadWrite ) )
    CHILLEXCEPTION( file, line, CONNECTFAIL, NOT_FIXED );
 
  if( ! TEST_FLAG( the_access, IO_INDEXED ) && the_usage == ReadWrite )
    CHILLEXCEPTION( file, line, CONNECTFAIL, NOT_INDEXED );

  /* Access location may be connected to a different association. */
  if( the_access->association && the_access->association != the_assoc )
    __disconnect( the_access, file, line );

  /* Is the association location already connected? */
  if( the_assoc->access )
    {
      /* save position just in case we need it for the_where == Same */
      if( (savepos = lseek( the_assoc->handle, 0L, SEEK_CUR )) == -1L )
	CHILLEXCEPTION( file, line, CONNECTFAIL, LSEEK_FAILS );

      /* text: read correction, flush buffer */
      if( the_assoc->bufptr ){
	savepos -= the_assoc->bufptr->len - the_assoc->bufptr->cur;
	the_assoc->bufptr->len = the_assoc->bufptr->cur = 0;
      }

      /* implicit disconnect */
      __disconnect( the_assoc->access, file, line );
    }

  the_assoc->usage = the_usage;
  CLR_FLAG( the_access, IO_OUTOFFILE );
 
  if( TEST_FLAG( the_access, IO_INDEXED ) )
    {
      if( (the_assoc->handle = open( the_assoc->pathname, DirMode[the_usage] )) == -1 )
	CHILLEXCEPTION( file, line, CONNECTFAIL, OPEN_FAILS );

      /* Set base index. */
      switch( the_where )
	{
	case First: 
	  filepos = 0;
	  break;
	case Same: 
	  filepos = savepos;
	  break;
	case Last: 
	  if( lseek( the_assoc->handle, 0L, SEEK_END ) == -1L )
	    CHILLEXCEPTION( file, line, CONNECTFAIL, LSEEK_FAILS );
	  filepos = lseek( the_assoc->handle, 0L, SEEK_CUR );
	  break;
	}

      /* Set current index */
      if( with_index )
	{
	  if( the_index < the_access->lowindex
	      || the_access->highindex < the_index )
	    CHILLEXCEPTION( file, line, RANGEFAIL, BAD_INDEX );
	  filepos += (the_index - the_access->lowindex) * the_access->reclength;
	}
      if( lseek( the_assoc->handle, filepos, SEEK_SET ) == -1L )
	CHILLEXCEPTION( file, line, CONNECTFAIL, LSEEK_FAILS );
      the_access->base = filepos;
    }
  else
    {
      /* for association to text for reading: allocate buffer */
      if( TEST_FLAG((Text_Mode*)the_transfer, IO_TEXTLOCATION ) &&
	  the_usage == ReadOnly &&
	  !the_assoc->bufptr )
	{
	  if( ! (the_assoc->bufptr = (readbuf_t*)malloc( sizeof(readbuf_t) )) )
	    CHILLEXCEPTION( file, line, CONNECTFAIL, BUFFER_ALLOC ); 
	  memset (the_assoc->bufptr, 0, sizeof (readbuf_t));
	}
      if( (the_assoc->handle = open( the_assoc->pathname, SeqMode[the_usage] )) == -1 )
	CHILLEXCEPTION( file, line, CONNECTFAIL, OPEN_FAILS );

      /* Set base index. */
      switch( the_where )
	{
	case First: 
	  filepos = 0;
	  break;
	case Same: 
	  filepos = savepos;
	  break;
	case Last:
	  if( lseek( the_assoc->handle, 0L, SEEK_END ) == -1L )
	    CHILLEXCEPTION( file, line, CONNECTFAIL, LSEEK_FAILS );
	  filepos = lseek( the_assoc->handle, 0L, SEEK_CUR );
	  break;
	}

      /* file truncation for sequential, Write Only */
      /***************************** FIXME: cannot truncate at Same
	if( the_usage == WriteOnly )
	{
	if( fseek( the_assoc->file_ptr, filepos, SEEK_SET ) == -1L )
        CHILLEXCEPTION( file, line, CONNECTFAIL, FSEEK_FAILS );
	fclose( the_assoc->file_ptr );
	if( !(the_assoc->file_ptr = fopen( the_assoc->pathname, "ab" )) )
        CHILLEXCEPTION( file, line, CONNECTFAIL, OPEN_FAILS );
	}
	else
	***************************/
      if( (filepos = lseek( the_assoc->handle, filepos, SEEK_SET )) == -1L )
	CHILLEXCEPTION( file, line, CONNECTFAIL, LSEEK_FAILS );
    }

  the_access->association = the_assoc;
  the_assoc->access = the_access;
  /* for text: set carriage control default */
  if( TEST_FLAG((Text_Mode*)the_transfer, IO_TEXTLOCATION ) ){
    the_assoc->ctl_pre  = '\0';
    the_assoc->ctl_post = '\n';
  }
}

void
__disconnect( void* the_transfer, char* file, int line )
{
  Access_Mode* the_access;

  if( !the_transfer )
    CHILLEXCEPTION( file, line, EMPTY, NULL_ACCESS );

  if( TEST_FLAG((Text_Mode*)the_transfer, IO_TEXTLOCATION ))
    {
      the_access = ((Text_Mode*)the_transfer)->access_sub;
      CLR_FLAG( the_access, IO_TEXTIO );
    }
  else
    the_access = (Access_Mode*)the_transfer;

  if( !the_access->association )
    CHILLEXCEPTION( file, line, NOTCONNECTED, IS_NOT_CONNECTED );

  close( the_access->association->handle );
  /* FIXME: check result */

  if( the_access->store_loc )
    free( the_access->store_loc );
  the_access->store_loc           = NULL;
  the_access->association->access = NULL;
  the_access->association         = NULL;
}
