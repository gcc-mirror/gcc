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

#ifndef __VALCONV_H
#define __VALCONV_H

extern int    __gg__decimal_point        ;
extern int    __gg__decimal_separator    ;
extern int    __gg__quote_character      ;
extern int    __gg__low_value_character  ;
extern int    __gg__high_value_character ;
extern char **__gg__currency_signs       ;
extern int    __gg__default_currency_sign;
extern char  *__gg__ct_currency_signs[256];  // Compile-time currency signs


// All "ordinals" are zero-based ordinals.  The COBOL spec's ordinal values
// for ordinary ASCII/EBCDIC ranger from 1 to 256, so we call them zero through
// 255.  We use unsigned ints so that when an custom alphabet is described, we
// can make every unmentioned character have an ordinal greater than the final
// ordinal of the custom list.
struct alphabet_state
  {
  unsigned short collation[256];
  unsigned char low_char;
  unsigned char high_char;
  };

extern std::unordered_map<size_t, alphabet_state> __gg__alphabet_states;

extern "C"
  {
  void __gg__realloc_if_necessary(char **dest, size_t *dest_size, size_t new_size);
  void  __gg__alphabet_create(cbl_encoding_t encoding,
                              size_t alphabet_index,
                              const unsigned char *alphabet,
                              int low_char,
                              int high_char );
  bool __gg__string_to_numeric_edited(char * const dest,
                                      const char *source,       // ASCII
                                      int rdigits,
                                      int is_negative,
                                      const char *picture);
  void __gg__string_to_alpha_edited(char *dest,
                                    const char *source,
                                    int slength,
                                    const char *picture);
  void __gg__currency_sign_init();
  void __gg__currency_sign(int symbol, const char *sign);
  void __gg__remove_trailing_zeroes(char *p);
  }

#endif
