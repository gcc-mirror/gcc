/* Public attributes of the .gdb_index section.
   Copyright (C) 2012-2015 Free Software Foundation, Inc.

   This file is part of GDB.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* This file contains values for understanding the .gdb_index section
   needed by more than just GDB, e.g. readelf.  */

#ifndef GDB_INDEX_H
#define GDB_INDEX_H

/* Each symbol in .gdb_index refers to a set of CUs that defines the symbol.
   Each CU is represented by a 32 bit number that is the index of the CU in
   the CU table, plus some attributes of the use of the symbol in that CU.

   The values are defined such that if all the bits are zero, then no
   special meaning is assigned to any of them.  This is done to preserve
   compatibility with older indices.  The way this is done is to specify
   that if the GDB_INDEX_SYMBOL_KIND value is zero then all other attribute
   bits must be zero.

    0-23  CU index
   24-27  reserved
   28-30  symbol kind
   31     0 == global, 1 == static

   Bits 24-27 are reserved because it's easier to relax restrictions than
   it is to impose them after the fact.  At present 24 bits to represent
   the CU index is plenty.  If we need more bits for the CU index or for
   attributes then we have them.  */

/* Whether the symbol is in GLOBAL_BLOCK (== 0) or STATIC_BLOCK (== 1).  */
#define GDB_INDEX_SYMBOL_STATIC_SHIFT 31
#define GDB_INDEX_SYMBOL_STATIC_MASK 1
#define GDB_INDEX_SYMBOL_STATIC_VALUE(cu_index) \
  (((cu_index) >> GDB_INDEX_SYMBOL_STATIC_SHIFT) & GDB_INDEX_SYMBOL_STATIC_MASK)
#define GDB_INDEX_SYMBOL_STATIC_SET_VALUE(cu_index, value) \
  do { \
    (cu_index) |= (((value) & GDB_INDEX_SYMBOL_STATIC_MASK) \
		   << GDB_INDEX_SYMBOL_STATIC_SHIFT); \
  } while (0)

/* The kind of the symbol.
   We don't use GDB's internal values as these numbers are published
   so that other tools can build and read .gdb_index.  */

typedef enum {
  /* Special value to indicate no attributes are present.  */
  GDB_INDEX_SYMBOL_KIND_NONE = 0,
  GDB_INDEX_SYMBOL_KIND_TYPE = 1,
  GDB_INDEX_SYMBOL_KIND_VARIABLE = 2,
  GDB_INDEX_SYMBOL_KIND_FUNCTION = 3,
  GDB_INDEX_SYMBOL_KIND_OTHER = 4,
  /* We currently allocate 3 bits to record the symbol kind.
     Give the unused bits a value so gdb will print them sensibly.  */
  GDB_INDEX_SYMBOL_KIND_UNUSED5 = 5,
  GDB_INDEX_SYMBOL_KIND_UNUSED6 = 6,
  GDB_INDEX_SYMBOL_KIND_UNUSED7 = 7
} gdb_index_symbol_kind;

#define GDB_INDEX_SYMBOL_KIND_SHIFT 28
#define GDB_INDEX_SYMBOL_KIND_MASK 7
#define GDB_INDEX_SYMBOL_KIND_VALUE(cu_index) \
  ((gdb_index_symbol_kind) (((cu_index) >> GDB_INDEX_SYMBOL_KIND_SHIFT) \
			    & GDB_INDEX_SYMBOL_KIND_MASK))
#define GDB_INDEX_SYMBOL_KIND_SET_VALUE(cu_index, value) \
  do { \
    (cu_index) |= (((value) & GDB_INDEX_SYMBOL_KIND_MASK) \
		   << GDB_INDEX_SYMBOL_KIND_SHIFT); \
  } while (0)

#define GDB_INDEX_RESERVED_SHIFT 24
#define GDB_INDEX_RESERVED_MASK 15
#define GDB_INDEX_RESERVED_VALUE(cu_index) \
  (((cu_index) >> GDB_INDEX_RESERVED_SHIFT) & GDB_INDEX_RESERVED_MASK)

/* CU index.  */
#define GDB_INDEX_CU_BITSIZE 24
#define GDB_INDEX_CU_MASK ((1 << GDB_INDEX_CU_BITSIZE) - 1)
#define GDB_INDEX_CU_VALUE(cu_index) ((cu_index) & GDB_INDEX_CU_MASK)
#define GDB_INDEX_CU_SET_VALUE(cu_index, value) \
  do { \
    (cu_index) |= (value) & GDB_INDEX_CU_MASK; \
  } while (0)

#endif /* GDB_INDEX_H */
