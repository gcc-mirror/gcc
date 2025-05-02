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

#endif
