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

#ifndef _fileio_h_
#define _fileio_h_

#include <stdio.h>

#include "auxtypes.h"
#include "ioerror.h"
#include "iomodes.h"

#define DIRSEP '/'

#define TEST_FLAG(Xloc,Flag) (((Xloc)->flags) & (Flag))
#define SET_FLAG(Xloc,Flag)  (Xloc)->flags |= (Flag)
#define CLR_FLAG(Xloc,Flag)  (Xloc)->flags = ((Xloc)->flags & ~(Flag))

Boolean
__isassociated( Association_Mode* the_assoc, char* file, int line );

Boolean
__existing( Association_Mode* the_assoc, char* file, int line );

Boolean
__readable( Association_Mode* the_assoc, char* file, int line );

Boolean
__writeable( Association_Mode* the_assoc, char* file, int line );

Boolean
__indexable( Association_Mode* the_assoc, char* file, int line );

Boolean
__sequencible( Association_Mode* the_assoc, char* file, int line );

Boolean
__variable( Association_Mode* the_assoc, char* file, int line );

typedef signed long int Index_t;

Association_Mode*
__associate( Association_Mode* the_assoc,
             char*             the_path,
             int               the_path_len,
             char*             the_mode,
             int               the_mode_len,
             char*             file,
             int               line );

void
__dissociate( Association_Mode* the_assoc, char* file, int line );

void
__create( Association_Mode* the_assoc, char* file, int line );

void
__delete( Association_Mode* the_assoc, char* file, int line );

void
__modify( Association_Mode* the_assoc,
          char*             the_path,
          int               the_path_len,
          char*             the_mode,
          int               the_mode_len,
          char*             file,
          int               line );

void
__connect( void*             the_transfer, 
           Association_Mode* the_assoc,
           Usage_Mode        the_usage,
           Where_Mode        the_where,
           Boolean           with_index,
           signed long       the_index,
           char*             file,
           int               line );

void
__disconnect( void* the_transfer, char* file, int line );

Association_Mode*
__getassociation( void* the_transfer, char* file, int line );

Usage_Mode
__getusage( void* the_transfer, char* file, int line );

Boolean
__outoffile( void* the_transfer, char* file, int line );

void*
__readrecord( Access_Mode*  the_access,
              signed long   the_index,
              char*         the_buf_addr,
              char*         file,
              int           line );

void
__writerecord( Access_Mode*  the_access,
               signed long   the_index,
               char*         the_val_addr,
               unsigned long the_val_len,
               char*         file,
               int           line );

VarString*
__gettextrecord( Text_Mode* the_text, char* file, int line );

unsigned long
__gettextindex( Text_Mode* the_text, char* file, int line );

Access_Mode*
__gettextaccess( Text_Mode* the_text, char* file, int line );

Boolean
__eoln( Text_Mode* the_text, char* file, int line );

void
__settextrecord( Text_Mode* the_text,
                 VarString* the_text_rec,
                 char*      file,
                 int        line );

void
__settextindex( Text_Mode*  the_text,
                signed long the_text_index, 
                char*       file,
                int         line );

void
__settextaccess( Text_Mode*   the_text,
                 Access_Mode* the_access,
                 char*        file,
                 int          line );

#endif
