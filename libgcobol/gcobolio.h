/*
 * Copyright (c) 2021-2025 Symas Corporation
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * * Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * * Redistributions in binary form must reproduce the above
 *   copyright notice, this list of conditions and the following disclaimer
 *   in the documentation and/or other materials provided with the
 *   distribution.
 * * Neither the name of the Symas Corporation nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
#ifndef GCOBOLIO_H_
#define GCOBOLIO_H_

#include <cstdio>

#include <map>
#include <unordered_map>
#include <vector>

// RUNTIME structures *must* match the ones created in structs.c and initialized
// and used in genapi.c.  It's actually not all that important to emphasize that
// fact, since the compiled executable will crash and burn quickly if they don't
// match precisely.

// Note that it must match the same structure in the GDB-COBOL debugger

typedef struct cblc_field_t
    {
    // This structure must match the code in structs.cc
    unsigned char *data;        // The runtime data. There is no null terminator
    size_t         capacity;    // The size of "data"
    size_t         allocated;   // The number of bytes available for capacity
    size_t   offset;            // Offset from our ancestor (see note below)
    char    *name;              // The null-terminated name of this variable
    char    *picture;           // The null-terminated picture string.
    char    *initial;           // The null_terminated initial value
    struct cblc_field_t *parent;// This field's immediate parent field
    size_t occurs_lower;        // non-zero for a table
    size_t occurs_upper;        // non-zero for a table
    unsigned long long attr;    // See cbl_field_attr_t
    signed char type;           // A one-byte copy of cbl_field_type_t
    signed char level;          // This variable's level in the naming heirarchy
    signed char digits;         // Digits specified in PIC string; e.g. 5 for 99v999
    signed char rdigits;        // Digits to the right of the decimal point. 3 for 99v999
    int    dummy;               // GCC seems to want an even number of 32-bit values
    } cblc_field_t;

/*
 * Implementation details
 */

class supplemental_t;

enum cblc_file_prior_op_t
  {
  file_op_none,
  file_op_open,
  file_op_start,
  file_op_read,
  file_op_write,
  file_op_rewrite,
  file_op_delete,
  file_op_close,
  };

/* end implementation details */

enum cblc_file_flags_t
    {
    file_flag_none_e          = 0x00000,
    file_flag_optional_e      = 0x00001,
    file_flag_existed_e       = 0x00002,
    file_name_quoted_e        = 0x00004,
    file_flag_initialized_e   = 0x00008,
    };

typedef struct cblc_file_t
    {
    // This structure must match the code in structs.cc
    char                *name;             // This is the name of the structure; might be the name of an environment variable
    size_t               symbol_table_index;  // of the related cbl_field_t structure
    char                *filename;         // The name of the file to be opened
    FILE                *file_pointer;     // The FILE *pointer
    cblc_field_t        *default_record;   // The record_area
    size_t               record_area_min;  // The size of the smallest 01 record in the FD
    size_t               record_area_max;  // The size of the largest  01 record in the FD
    cblc_field_t       **keys;             // For relative and indexed files.  The first is the primary key. Null-terminated.
    int                 *key_numbers;      // One per key -- each key has a number. This table is key_number + 1
    int                 *uniques;          // One per key
    cblc_field_t        *password;         //
    cblc_field_t        *status;           // This must exist, and is the cbl_field_t version of io_status
    cblc_field_t        *user_status;      // This might exist, and is another copy See 2014 standard, section 9.1.12
    cblc_field_t        *vsam_status;      //
    cblc_field_t        *record_length;    //
    supplemental_t      *supplemental;     //
    void                *implementation;   // reserved for any implementation
    size_t               reserve;          // From I-O section RESERVE clause
    long                 prior_read_location;   // Location of immediately preceding successful read
    cbl_file_org_t       org;              // from ORGANIZATION clause
    cbl_file_access_t    access;           // from ACCESS MODE clause
    int                  mode_char;        // 'r', 'w', '+', or 'a' from FILE OPEN statement
    int                  errnum;           // most recent errno; can't reuse "errno" as the name
    file_status_t        io_status;        // See 2014 standard, section 9.1.12
    int                  padding;          // Actually a char
    int                  delimiter;        // ends a record; defaults to '\n'.
    int                  flags;            // cblc_file_flags_t
    int                  recent_char;      // This is the most recent char sent to the file
    int                  recent_key;
    cblc_file_prior_op_t prior_op;         // run-time type is INT
    int                  dummy;
    } cblc_file_t;


/*  In various arithmetic routines implemented in libgcobol, it is oftent the
    case that complicates lists of variables need to be conveyed.  For example,
    "ADD A B C D GIVING E" and "ADD A TO B C D" are valid instructions.
    
    These treeplets (triplets of trees) were created to handle that.  */

extern cblc_field_t ** __gg__treeplet_1f;
extern size_t       *  __gg__treeplet_1o;
extern size_t       *  __gg__treeplet_1s;
extern cblc_field_t ** __gg__treeplet_2f;
extern size_t       *  __gg__treeplet_2o;
extern size_t       *  __gg__treeplet_2s;
extern cblc_field_t ** __gg__treeplet_3f;
extern size_t       *  __gg__treeplet_3o;
extern size_t       *  __gg__treeplet_3s;
extern cblc_field_t ** __gg__treeplet_4f;
extern size_t       *  __gg__treeplet_4o;
extern size_t       *  __gg__treeplet_4s;

extern int *        __gg__fourplet_flags;

#endif
