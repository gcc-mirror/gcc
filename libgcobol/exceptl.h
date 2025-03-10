/*
 * Copyright (c) 2021-2025 Symas Corporation
 * All rights reserved.
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

#ifndef _CBL_EXCEPTC_H_
#define _CBL_EXCEPTC_H_

/*  This file contains declarations needed by the libgcobol compilation.  Some
    of the information here is required by the gcc/cobol compilation, and so it
    is safe to include in those files.  */

static const ec_type_t simon_says_important[] = {
  ec_argument_function_e,
  ec_bound_odo_e,
  ec_bound_ref_mod_e,
  ec_bound_subscript_e,
  ec_data_incompatible_e,
  ec_data_ptr_null_e,
  ec_size_overflow_e,
  ec_size_exponentiation_e,
  ec_size_truncation_e,
  ec_size_zero_divide_e,
  ec_program_not_found_e,
  ec_program_recursive_call_e,
  ec_program_arg_mismatch_e,
};

enum ec_disposition_t {
  ec_category_none_e,
  ec_category_fatal_e,
  ec_category_nonfatal_e,
  ec_category_implementor_e,

  // unimplemented equivalents
  uc_category_none_e =        0x80 + ec_category_none_e,
  uc_category_fatal_e =       0x80 + ec_category_fatal_e,
  uc_category_nonfatal_e =    0x80 + ec_category_nonfatal_e,
  uc_category_implementor_e = 0x80 + ec_category_implementor_e,
};

struct ec_descr_t {
  ec_type_t type;
  ec_disposition_t disposition;
  const cbl_name_t name;
  const char *description;

  bool operator==( ec_type_t type ) const {
    return this->type == type;
  }
};

extern ec_type_t ec_type_of( const cbl_name_t name );

extern ec_descr_t __gg__exception_table[];
extern ec_descr_t *__gg__exception_table_end;

/*  Inventory of exceptions:
    In except.hc::__gg__exception_table, unimplemented ECs have a uc_ disposition.

    ec_function_argument_e        ACOS
                                  ANNUITY
                                  ASIN
                                  LOG
                                  LOG10
                                  PRESENT-VALUE
                                  SQRT

    ec_sort_merge_file_open_e     FILE MERGE

    ec_bound_subscript_e          table subscript not an integer
                                  table subscript less than 1
                                  table subscript greater than occurs

    ec_bound_ref_mod_e            refmod start not an integer
                                  refmod start less than 1
                                  refmod start greater than variable size
                                  refmod length not an integer
                                  refmod length less than 1
                                  refmod start+length exceeds variable size

    ec_bound_odo_e                DEPENDING not an integer
                                  DEPENDING greater than occurs upper limit
                                  DEPENDING less    than occurs lower limit
                                  subscript greater than DEPENDING for sending item

    ec_size_zero_divide_e         For both fixed-point and floating-point division

    ec_size_truncation
    ec_size_exponentiation

 */

// SymException
struct cbl_exception_t {
  size_t program, file;
  ec_type_t type;
  cbl_file_mode_t mode;
};


struct cbl_declarative_t {
  enum { files_max = 16 };
  size_t section; // implies program
  bool global;
  ec_type_t type;
  uint32_t nfile, files[files_max];
  cbl_file_mode_t mode;

  cbl_declarative_t( cbl_file_mode_t mode = file_mode_none_e )
    : section(0), global(false), type(ec_none_e)
    , nfile(0)
    , mode(mode)
  {
    std::fill(files, files + COUNT_OF(files), 0);
  }
  cbl_declarative_t( ec_type_t type )
    : section(0), global(false), type(type)
    , nfile(0)
    , mode(file_mode_none_e)
  {
    std::fill(files, files + COUNT_OF(files), 0);
  }

  cbl_declarative_t( size_t section, ec_type_t type,
                     const std::list<size_t>& files,
                     cbl_file_mode_t mode, bool global = false )
    : section(section), global(global), type(type)
    , nfile(files.size())
    , mode(mode)
  {
    assert( files.size() <= COUNT_OF(this->files) );
    std::fill(this->files, this->files + COUNT_OF(this->files), 0);
    if( nfile > 0 ) {
      std::copy( files.begin(), files.end(), this->files );
    }
  }
  cbl_declarative_t( const cbl_declarative_t& that )
    : section(that.section), global(that.global), type(that.type)
    , nfile(that.nfile)
    , mode(that.mode)
  {
    std::fill(files, files + COUNT_OF(files), 0);
    if( nfile > 0 ) {
      std::copy( that.files, that.files + nfile, this->files );
    }
  }

  /*
   * Sort file names before file modes, and file modes before non-IO.
   */
  bool operator<( const cbl_declarative_t& that ) const {
    // file name declaratives first, in section order
    if( nfile != 0 ) {
      if( that.nfile != 0 ) return section < that.section;
      return true;
    }
    // file mode declaratives between file name declaratives and non-IO
    if( mode != file_mode_none_e ) {
      if( that.nfile != 0 ) return false;
      if( that.mode == file_mode_none_e ) return true;
      return section < that.section;
    }
    // all others by section, after names and modes
    if( that.nfile != 0 ) return false;
    if( that.mode != file_mode_none_e ) return false;
    return section < that.section;
  }

  // TRUE if there are no files to match, or the provided file is in the list.
  bool match_file( size_t file ) const {
    static const auto pend = files + nfile;

    return nfile == 0 || pend != std::find(files, files + nfile, file);
  }

  // USE Format 1 names a file mode, or at least one file, and not an EC.
  bool is_format_1() const {
    assert(type != ec_none_e || nfile > 0 || mode != file_mode_none_e);
    return nfile > 0 || mode != file_mode_none_e;
  }
};


/*
 * ec_status_t represents the runtime exception condition status for
 * any statement.  Prior to execution, the generated code
 * clears "type", and sets "source_file" and "lineno".
 *
 * If the statement includes some kind of ON ERROR
 * clause, the generated code sets "handled" to the exception type
 * handled by that clause, else it sets "handled" to ec_none_e.
 *
 * Post-execution, the generated code sets "type" to the appropriate
 * exception, if any.  The match-exception logic compares any raised
 * exception to the set of declaratives, and returns a symbol-table
 * index to the matching declarative, if any.
 */
class ec_status_t {
  char msg[132];
public:
  ec_type_t type, handled;
  cbl_name_t statement; // e.g., "ADD"
  size_t lineno;
  const char *source_file;

  ec_status_t()
    : type(ec_none_e)
    , handled(ec_none_e)
    , lineno(0)
    , source_file(NULL)
  {
    msg[0] = statement[0] = '\0';
  }

  ec_status_t& update();
  ec_status_t& enable( unsigned int mask );

  const char * exception_location() {
    snprintf(msg, sizeof(msg), "%s:%zu: '%s'", source_file, lineno, statement);
    return msg;
  }
  ec_type_t unhandled() const {
    return ec_type_t(static_cast<unsigned int>(type)
                     &
                     ~static_cast<unsigned int>(handled));
  }
};

#endif
