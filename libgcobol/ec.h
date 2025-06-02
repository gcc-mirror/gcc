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

#ifndef _CBL_EC_H_
#define _CBL_EC_H_

#include <set>

#define  EC_ALL_E 0xFFFFFF00

enum ec_type_t {
  ec_none_e = 0x00000000,
  ec_all_e  = EC_ALL_E, // 0xFFFFFF00

  ec_argument_e = 0x00000100,
  ec_argument_function_e,
  ec_argument_imp_e,
  ec_argument_imp_command_e,
  ec_argument_imp_environment_e,

  ec_bound_e = 0x00000200,
  ec_bound_func_ret_value_e,
  ec_bound_imp_e,
  ec_bound_odo_e,
  ec_bound_overflow_e,
  ec_bound_ptr_e,
  ec_bound_ref_mod_e,
  ec_bound_set_e,
  ec_bound_subscript_e,
  ec_bound_table_limit_e,

  ec_data_e = 0x00000400,
  ec_data_conversion_e,
  ec_data_imp_e,
  ec_data_incompatible_e,
  ec_data_not_finite_e,
  ec_data_overflow_e,
  ec_data_ptr_null_e,

  ec_external_e = 0x00000800,
  ec_external_data_mismatch_e,
  ec_external_file_mismatch_e,
  ec_external_format_conflict_e,

  ec_flow_e = 0x00001000,
  ec_flow_global_exit_e,
  ec_flow_global_goback_e,
  ec_flow_imp_e,
  ec_flow_release_e,
  ec_flow_report_e,
  ec_flow_return_e,
  ec_flow_search_e,
  ec_flow_use_e,

  ec_function_e = 0x00002000,
  ec_function_not_found_e,
  ec_function_ptr_invalid_e,
  ec_function_ptr_null_e,

  ec_io_e = 0x00004000,
  ec_io_at_end_e,
  ec_io_invalid_key_e,
  ec_io_permanent_error_e,
  ec_io_logic_error_e,
  ec_io_record_operation_e,
  ec_io_file_sharing_e,
  ec_io_record_content_e,
  ec_io_imp_e,
  ec_io_eop_e,
  ec_io_eop_overflow_e,
  ec_io_linage_e,

  ec_imp_e = 0x00008000,
  ec_imp_suffix_e,

  ec_locale_e = 0x00010000,
  ec_locale_imp_e,
  ec_locale_incompatible_e,
  ec_locale_invalid_e,
  ec_locale_invalid_ptr_e,
  ec_locale_missing_e,
  ec_locale_size_e,

  ec_oo_e = 0x00020000,
  ec_oo_arg_omitted_e,
  ec_oo_conformance_e,
  ec_oo_exception_e,
  ec_oo_imp_e,
  ec_oo_method_e,
  ec_oo_null_e,
  ec_oo_resource_e,
  ec_oo_universal_e,

  ec_order_e = 0x00040000,
  ec_order_imp_e,
  ec_order_not_supported_e,

  ec_overflow_e = 0x00080000,
  ec_overflow_imp_e,
  ec_overflow_string_e,
  ec_overflow_unstring_e,

  ec_program_e = 0x00100000,
  ec_program_arg_mismatch_e,
  ec_program_arg_omitted_e,
  ec_program_cancel_active_e,
  ec_program_imp_e,
  ec_program_not_found_e,
  ec_program_ptr_null_e,
  ec_program_recursive_call_e,
  ec_program_resources_e,

  ec_raising_e = 0x00200000,
  ec_raising_imp_e,
  ec_raising_not_specified_e,

  ec_range_e = 0x00400000,
  ec_range_imp_e,
  ec_range_index_e,
  ec_range_inspect_size_e,
  ec_range_invalid_e,
  ec_range_perform_varying_e,
  ec_range_ptr_e,
  ec_range_search_index_e,
  ec_range_search_no_match_e,

  ec_report_e = 0x00800000,
  ec_report_active_e,
  ec_report_column_overlap_e,
  ec_report_file_mode_e,
  ec_report_imp_e,
  ec_report_inactive_e,
  ec_report_line_overlap_e,
  ec_report_not_terminated_e,
  ec_report_page_limit_e,
  ec_report_page_width_e,
  ec_report_sum_size_e,
  ec_report_varying_e,

  ec_screen_e = 0x01000000,
  ec_screen_field_overlap_e,
  ec_screen_imp_e,
  ec_screen_item_truncated_e,
  ec_screen_line_number_e,
  ec_screen_starting_column_e,

  ec_size_e = 0x02000000,
  ec_size_address_e,
  ec_size_exponentiation_e,
  ec_size_imp_e,
  ec_size_overflow_e,
  ec_size_truncation_e,
  ec_size_underflow_e,
  ec_size_zero_divide_e,

  ec_sort_merge_e = 0x04000000,
  ec_sort_merge_active_e,
  ec_sort_merge_file_open_e,
  ec_sort_merge_imp_e,
  ec_sort_merge_release_e,
  ec_sort_merge_return_e,
  ec_sort_merge_sequence_e,

  ec_storage_e = 0x08000000,
  ec_storage_imp_e,
  ec_storage_not_alloc_e,
  ec_storage_not_avail_e,

  ec_user_e = 0x10000000,
  ec_user_suffix_e,

  ec_validate_e = 0x20000000,
  ec_validate_content_e,
  ec_validate_format_e,
  ec_validate_imp_e,
  ec_validate_relation_e,
  ec_validate_varying_e,

  ec_continue_e = 0x30000000,
  ec_continue_less_than_zero,
};


// The following declarations are used by both gcc/cobol code and the libgcobol
// code

struct cblc_declarative_t
    {
    int format;
    int culprit;  //declarative_culprit_t
    int nfiles;
    };

/*  According to the standard, the first digit of the file operation status
    register is interpreted like this:

    EC-I-O-AT-END               '1'
    EC-I-O-INVALID-KEY          '2'
    EC-I-O-PERMANENT-ERROR      '3'
    EC-I-O-LOGIC-ERROR          '4'
    EC-I-O-RECORD-OPERATION     '5'
    EC-I-O-FILE-SHARING         '6'
    EC-I-O-IMP                  '9'

When the tens digit is '0', there are a number of conditions for
successful completion.  See section 9.1.12.1

    00      unqualified success
    02      duplicate key detected
    04      the data read were either too short or too long
    05      the operator couldn't find the tape
    07      somebody tried to rewind the card reader.

For now, I am going to treat the io_status as an integer 00 through 99.  I
anticipate mostly returning
    00 for ordinary success,
    04 for a mismatched record size
    10 for an end-of-file

*/

// This global variable is constantly being updated with the yylineno.  This is
// useful for creating error messages, and for handling EXCEPTION_CONDITIONS
extern int         __gg__exception_code;
extern int         __gg__exception_line_number;
extern int         __gg__exception_file_status;
extern const char *__gg__exception_file_name;
extern const char *__gg__exception_statement;
extern const char *__gg__exception_source_file;
extern const char *__gg__exception_program_id;
extern const char *__gg__exception_section;
extern const char *__gg__exception_paragraph;

extern "C" void __gg__set_exception_code( ec_type_t ec,
                                          int from_raise_statement=0);

#if 1
  static inline
  void exception_raise(ec_type_t ec_code) { __gg__set_exception_code(ec_code); }
#else
# define exception_raise(ec_code)do{__gg__set_exception_code(ec_code);}while(0);
#endif

#endif
