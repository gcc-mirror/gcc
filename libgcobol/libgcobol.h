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
#ifndef LIBGCOBOL_H_
#define LIBGCOBOL_H_

/*  Many of the routines declared here are called from the gcc/cobol code by
    means of explicit GENERIC calls, which is why they are defined as external
    "C".  Because there is no mechanism for checking the definitions, the caller
    and callee have to agree on parameter types and the types of returned
    values.

    Some are also called between source code modules in libgcobol, hence the
    need here for declarations. */

extern void __gg__mabort();


// The unnecessary abort() that follows is necessary to make cppcheck be 
// aware that massert() actually terminates processing after a failed
// malloc().
#define massert(p) if(!p){__gg__mabort();abort();}

extern "C" __int128 __gg__power_of_ten(int n);

extern "C" __int128 __gg__dirty_to_binary_source( const char *dirty,
                                                  int length,
                                                  int *rdigits);
extern "C" __int128 __gg__dirty_to_binary_internal( const char *dirty,
                                                    int length,
                                                    int *rdigits);
extern "C" __int128 __gg__binary_value_from_field(  int *rdigits,
                                                    cblc_field_t *var);

extern "C" int __gg__compare_2( cblc_field_t *left_side,
                                unsigned char   *left_location,
                                size_t  left_length,
                                int     left_attr,
                                int     left_flags,
                                cblc_field_t *right_side,
                                unsigned char   *right_location,
                                size_t  right_length,
                                int     right_attr,
                                int     right_flags,
                                int     second_time_through);
extern "C" void __gg__int128_to_field(cblc_field_t   *tgt,
                                      __int128        value,
                                      int             source_rdigits,
                                      enum cbl_round_t  rounded,
                                      int            *compute_error);
extern "C" void __gg__float128_to_field(cblc_field_t   *tgt,
                                        GCOB_FP128       value,
                                        enum cbl_round_t  rounded,
                                      int            *compute_error);
extern "C" void __gg__int128_to_qualified_field(cblc_field_t   *tgt,
                                size_t          offset,
                                size_t          length,
                                __int128        value,
                                int             source_rdigits,
                                enum cbl_round_t  rounded,
                                int            *compute_error);
extern "C" void __gg__float128_to_qualified_field(cblc_field_t   *tgt,
                                  size_t          tgt_offset,
                                  GCOB_FP128       value,
                                  enum cbl_round_t  rounded,
                                  int            *compute_error);
extern "C" void __gg__double_to_target( cblc_field_t *tgt,
                                        double tgt_value,
                                        cbl_round_t rounded);
extern "C" char __gg__get_decimal_separator();
extern "C" char __gg__get_decimal_point();
extern "C" char * __gg__get_default_currency_string();

struct cbl_timespec
  {
  /*  You keep using that word "portability".  I do not think it means what
      you think it means. */
  time_t  tv_sec;    // Seconds.
  long    tv_nsec;   // Nanoseconds.
  } ;

extern "C" void __gg__clock_gettime(struct cbl_timespec *tp);

extern "C" GCOB_FP128 __gg__float128_from_location(
                                        const cblc_field_t *var,
                                        const unsigned char *location);
extern "C" void __gg__adjust_dest_size(cblc_field_t *dest, size_t ncount);

extern "C" void __gg__realloc_if_necessary( char **dest,
                                            size_t *dest_size,
                                            size_t new_size);
extern "C" void __gg__set_exception_file(const cblc_file_t *file);
extern "C" void __gg__internal_to_console_in_place(char *loc, size_t length);
extern "C" __int128 __gg__binary_value_from_qualified_field(int     *rdigits,
                                                            const cblc_field_t *var,
                                                            size_t     offset,
                                                            size_t     size);
extern "C"  GCOB_FP128 __gg__float128_from_qualified_field(const cblc_field_t *field,
                                                          size_t offset,
                                                          size_t size);
extern "C"  __int128 __gg__integer_from_qualified_field(cblc_field_t *var,
                                                        size_t var_offset,
                                                        size_t var_size);
void __gg__abort(const char *msg);

#endif
