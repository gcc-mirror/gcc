/*
 * Copyright (c) 2021-2026 Symas Corporation
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
    signed char level;          // This variable's level in the naming hierarchy
    signed char digits;         // Digits specified in PIC string; e.g. 5 for 99v999
    signed char rdigits;        // Digits to the right of the decimal point. 3 for 99v999
    cbl_encoding_t encoding;    //
    int            alphabet;    // Same as cbl_field_t::codeset::language
    } cblc_field_t;

typedef struct cblc_referlet_t
    {
    cblc_field_t        *field;
    size_t               offset;
    size_t               size;
    } cblc_referlet_t;

typedef struct cblc_refer_t
    {
    cblc_field_t        *field;
    size_t               offset;
    size_t               size;
    int                  flags;
    } cblc_refer_t;

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
  file_op_remove,
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
    size_t               file_fpos;        // Calculated file position
    char                *buffer;           // read buffer
    size_t               buffer_pos;       // next character from the buffer
    size_t               buffer_len;       // number of characters in the buffer
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
    uint32_t             delimiter;        // ends a record; defaults to '\n'.
    int                  stride;           // Width of a character
    int                  flags;            // cblc_file_flags_t
    uint32_t             recent_char;      // This is the most recent char sent to the file
    int                  recent_key;
    cblc_file_prior_op_t prior_op;         // run-time type is INT
    cbl_encoding_t       encoding;         // We assume size int
    int                  alphabet;         // Actually cbl_encoding_t
    } cblc_file_t;

#define FILE_BUFFER_SIZE (64 * 1024)

/*
 * The following is a public API for gcobol I/O.
 *
 * The class gcobol_io_t may  be used by any library that provides an I/O
 * implementation.
 *
 * The library constructs the class, providing its own pointers, and supplies a
 * known function.  It calls __gg__file_io_funcs() to return the default
 * implementation, and overwrites it with one or more of its own functions.
 * gcobol-compiled programs call the functions through the pointers.
 *
 * This design makes it possible to run the same code, unaltered, simply by
 * relinking. It is commonly used, and is similar to the one used in GnuCOBOL.
 * One difference is that the file status is captured in the cblc_file_t,
 * whereas in GnuCOBOL it is the return value.
 *
 * There is no provision for using more than one implementation at
 * a time in the same program.
 */

class gcobol_io_t {
public:
  static const char constexpr marquee[64] = "libgcobol: gfileio.cc";

  typedef void (open_t)( cblc_file_t *file,
                   const char *filename,
                         int mode_char);
  typedef void (close_t)( cblc_file_t *file,
                          int how );
  typedef void (start_t)( cblc_file_t *file,
                          int relop, // needs enum
                          int first_last_key,
                          size_t length );
  typedef void (read_t)( cblc_file_t *file,
                         int where );
  typedef void (write_t)( cblc_file_t *file,
                          const unsigned char  *location,
                          size_t length,
                          int after,
                          int lines,
                          int is_random );
  typedef void (rewrite_t)(cblc_file_t *file,
                           size_t length, bool is_random );
  typedef void (delete_t)(cblc_file_t *file,
                          bool is_random );
  typedef void (remove_t)(cblc_file_t  *file,
                    const char         *filename);
  open_t      *Open;
  close_t     *Close;
  start_t     *Start;
  read_t      *Read;
  write_t     *Write;
  rewrite_t   *Rewrite;
  delete_t    *Delete;
  remove_t    *Remove;

  gcobol_io_t()
    : Open(NULL)
    , Close(NULL)
    , Start(NULL)
    , Read(NULL)
    , Write(NULL)
    , Rewrite(NULL)
    , Delete(NULL)
    , Remove(NULL)
  {}

  gcobol_io_t(   open_t      *Open,
                 close_t     *Close,
                 start_t     *Start,
                 read_t      *Read,
                 write_t     *Write,
                 rewrite_t   *Rewrite,
                 delete_t    *Delete,
                 remove_t    *Remove)
    : Open(Open)
    , Close(Close)
    , Start(Start)
    , Read(Read)
    , Write(Write)
    , Rewrite(Rewrite)
    , Delete(Delete)
    , Remove(Remove)
  {}
};

gcobol_io_t * __gg__file_io_funcs();

#endif
