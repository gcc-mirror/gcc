/* GNU CHILL compiler regression test file
 Copyright (C) 1992, 1993 Free Software Foundation, Inc.
 
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
 the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA. */

#ifndef __rts_h_
#define __rts_h_

typedef enum
{
  UNUSED,
  Process,
  Signal,
  Buffer,
  Event,
  Synonym,
  Exception,
  LAST_AND_UNUSED,
} TaskingEnum;

typedef void (*EntryPoint) ();

typedef struct
{
  char       *name;
  short      *value;
  int         value_defined;
  EntryPoint  entry;
  unsigned char /*TaskingEnum*/ type;
} TaskingStruct;

typedef struct
{
  short ptype;
  short pcopy;
} INSTANCE;

#endif /* __rts_h_ */
