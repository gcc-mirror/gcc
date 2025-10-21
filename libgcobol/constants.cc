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

#include <fcntl.h>
#include <unistd.h>

#include <cctype>
#include <cerrno>
#include <cmath>
#include <cfenv>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <ctime>

#include <algorithm>
#include <unordered_map>
#include <vector>

#include "ec.h"
#include "io.h"
#include "common-defs.h"
#include "gcobolio.h"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wwrite-strings"
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"

// There are global variables that need to be initialized at the point where
// the very first PROGRAM-ID is executed.  This flag is used to make sure that
// initialization happens just once.
int __gg__globals_are_initialized = 0;

// We have a number of integer constants.  We need two macros, one for 1-digit
// names and a second for 2-digit names in order to match our mangling
// convention for variable names that start with a numeric:

//  4 becomes _1_4
//  _ indicates this is a mangled name
//  1 means it is one character long
//  _ terminates the 1
//  4 is the one-character name

#define INTEGER_CONSTANT1(a) \
unsigned char __gg__data_##a[1] = {(a)};  \
struct cblc_field_t __ggsr___1_##a = {    \
  .data           =  __gg__data_##a ,         \
  .capacity       = 1 ,                 \
  .allocated      = 1 ,                 \
  .offset         = 0 ,                 \
  .name           = #a ,                \
  .picture        = "" ,                \
  .initial        = #a ,                \
  .parent         = NULL,               \
  .occurs_lower   = 0 ,                 \
  .occurs_upper   = 0 ,                 \
  .attr           = global_e | constant_e , \
  .type           = FldLiteralN ,       \
  .level          = 0 ,                 \
  .digits         = 0 ,                 \
  .rdigits        = 0 ,                 \
  .dummy          = 0 ,                 \
  };

#define INTEGER_CONSTANT2(a) \
unsigned char __gg__data_##a[1] = {(a)};  \
struct cblc_field_t __ggsr___2_##a = {    \
  .data           = __gg__data_##a ,         \
  .capacity       = 1 ,                 \
  .allocate       = 1 ,                 \
  .offset         = 0 ,                 \
  .name           = #a ,                \
  .picture        = "" ,                \
  .initial        = #a ,                \
  .parent         = NULL,               \
  .occurs_lower   = 0 ,                 \
  .occurs_upper   = 0 ,                 \
  .attr           = global_e | constant_e , \
  .type           = FldLiteralN ,       \
  .level          = 0 ,                 \
  .digits         = 0 ,                 \
  .rdigits        = 0 ,                 \
  .encoding       = iconv_CP1252_e                   \
  .alphabet       = 0                   \
  };

unsigned char __gg__data_space[1] = {' '};
struct cblc_field_t __ggsr__space = {
  .data           = __gg__data_space ,
  .capacity       = sizeof(__gg__data_space) ,
  .allocated      = sizeof(__gg__data_space) ,
  .offset         = 0 ,
  .name           = "SPACE" ,
  .picture        = "" ,
  .initial        = (char *)space_value_e ,
  .parent         = NULL,
  .occurs_lower   = 0 ,
  .occurs_upper   = 0 ,
  .attr           = quoted_e | constant_e | space_value_e ,
  .type           = FldAlphanumeric ,
  .level          = 0 ,
  .digits         = 0 ,
  .rdigits        = 0 ,
  .encoding       = iconv_CP1252_e ,
  .alphabet       = 0 ,
  };

struct cblc_field_t __ggsr__spaces = {
  .data           = __gg__data_space ,
  .capacity       = sizeof(__gg__data_space) ,
  .allocated      = sizeof(__gg__data_space) ,
  .offset         = 0 ,
  .name           = "SPACES" ,
  .picture        = "" ,
  .initial        = (char *)space_value_e ,
  .parent         = NULL,
  .occurs_lower   = 0 ,
  .occurs_upper   = 0 ,
  .attr           = quoted_e | constant_e | space_value_e ,
  .type           = FldAlphanumeric ,
  .level          = 0 ,
  .digits         = 0 ,
  .rdigits        = 0 ,
  .encoding       = iconv_CP1252_e ,
  .alphabet       = 0 ,
  };

unsigned char __gg__data_low_values[1] = {'\0'};
struct cblc_field_t __ggsr__low_values = {
  .data           = __gg__data_low_values,
  .capacity       = 1 ,
  .allocated      = 1 ,
  .offset         = 0 ,
  .name           = "LOW_VALUES" ,
  .picture        = "" ,
  .initial        = (char *)low_value_e ,
  .parent         = NULL,
  .occurs_lower   = 0 ,
  .occurs_upper   = 0 ,
  .attr           = 0x281 ,
  .type           = FldAlphanumeric ,
  .level          = 0 ,
  .digits         = 0 ,
  .rdigits        = 0 ,
  .encoding       = iconv_CP1252_e ,
  .alphabet       = 0 ,
  };

unsigned char __gg__data_zeros[1] = {'0'};
struct cblc_field_t __ggsr__zeros = {
  .data           = __gg__data_zeros ,
  .capacity       = 1 ,
  .allocated      = 1 ,
  .offset         = 0 ,
  .name           = "ZEROS" ,
  .picture        = "" ,
  .initial        = (char *)zero_value_e ,
  .parent         = NULL ,
  .occurs_lower   = 0 ,
  .occurs_upper   = 0 ,
  .attr           = 0x83 ,
  .type           = FldAlphanumeric ,
  .level          = 0 ,
  .digits         = 0 ,
  .rdigits        = 0 ,
  .encoding       = iconv_CP1252_e ,
  .alphabet       = 0 ,
  };

unsigned char __gg__data_high_values[1] = {0xFF};
struct cblc_field_t __ggsr__high_values = {
  .data           = __gg__data_high_values ,
  .capacity       = 1 ,
  .allocated      = 1 ,
  .offset         = 0 ,
  .name           = "HIGH_VALUES" ,
  .picture        = "" ,
  .initial        = (char *)high_value_e ,
  .parent         = NULL,
  .occurs_lower   = 0 ,
  .occurs_upper   = 0 ,
  .attr           = 0x286 ,
  .type           = FldAlphanumeric ,
  .level          = 0 ,
  .digits         = 0 ,
  .rdigits        = 0 ,
  .encoding       = iconv_CP1252_e ,
  .alphabet       = 0 ,
  };

unsigned char __gg__data_quotes[1] = {0xFF};
struct cblc_field_t __ggsr__quotes = {
  .data           = __gg__data_quotes ,
  .capacity       = 1 ,
  .allocated      = 1 ,
  .offset         = 0 ,
  .name           = "QUOTES" ,
  .picture        = "" ,
  .initial        = (char *)quote_value_e ,
  .parent         = NULL,
  .occurs_lower   = 0 ,
  .occurs_upper   = 0 ,
  .attr           = 0x285 ,
  .type           = FldAlphanumeric ,
  .level          = 0 ,
  .digits         = 0 ,
  .rdigits        = 0 ,
  .encoding       = iconv_CP1252_e ,
  .alphabet       = 0 ,
  };

unsigned char __gg__data_nulls[8] = {0,0,0,0,0,0,0,0};
struct cblc_field_t __ggsr__nulls = {
  .data           = __gg__data_nulls ,
  .capacity       = 8 ,
  .allocated      = 8 ,
  .offset         = 0 ,
  .name           = "NULLS" ,
  .picture        = "" ,
  .initial        = "" ,
  .parent         = NULL,
  .occurs_lower   = 0 ,
  .occurs_upper   = 0 ,
  .attr           = quoted_e | constant_e ,
  .type           = FldPointer ,
  .level          = 0 ,
  .digits         = 0 ,
  .rdigits        = 0 ,
  .encoding       = iconv_CP1252_e ,
  .alphabet       = 0 ,
  };

unsigned char __gg__data__file_status[2] = {0,0};
struct cblc_field_t __ggsr___file_status = {
  .data           = __gg__data__file_status ,
  .capacity       = 2 ,
  .allocated      = 2 ,
  .offset         = 0 ,
  .name           = "_FILE_STATUS" ,
  .picture        = "" ,
  .initial        = "" ,
  .parent         = NULL,
  .occurs_lower   = 0 ,
  .occurs_upper   = 0 ,
  .attr           = 0x0 ,
  .type           = FldNumericDisplay ,
  .level          = 0 ,
  .digits         = 2 ,
  .rdigits        = 0 ,
  .encoding       = iconv_CP1252_e ,
  .alphabet       = 0 ,
  };


unsigned char __gg__data_linage_counter[2] = {0,0};
struct cblc_field_t __ggsr___14_linage_counter6 = {
  .data           = __gg__data_linage_counter ,
  .capacity       = 2 ,
  .allocated      = 2 ,
  .offset         = 0 ,
  .name           = "LINAGE-COUNTER" ,
  .picture        = "" ,
  .initial        = "" ,
  .parent         = NULL,
  .occurs_lower   = 0 ,
  .occurs_upper   = 0 ,
  .attr           = 0x0 ,
  .type           = FldNumericBin5 ,
  .level          = 0 ,
  .digits         = 4 ,
  .rdigits        = 0 ,
  .encoding       = iconv_CP1252_e ,
  .alphabet       = 0 ,
  };


unsigned char __gg__data_upsi_0[2] = {0,0};
struct cblc_field_t __ggsr__upsi_0 = {
  .data           = __gg__data_upsi_0 ,
  .capacity       = 2 ,
  .allocated      = 2 ,
  .offset         = 0 ,
  .name           = "UPSI-0" ,
  .picture        = "" ,
  .initial        = "" ,
  .parent         = NULL,
  .occurs_lower   = 0 ,
  .occurs_upper   = 0 ,
  .attr           = 0x0 ,
  .type           = FldNumericBin5 ,
  .level          = 0 ,
  .digits         = 4 ,
  .rdigits        = 0 ,
  .encoding       = iconv_CP1252_e ,
  .alphabet       = 0 ,
  };

short __gg__data_return_code = 0;
struct cblc_field_t __ggsr__return_code = {
  .data           = (unsigned char *)&__gg__data_return_code ,
  .capacity       = 2 ,
  .allocated      = 2 ,
  .offset         = 0 ,
  .name           = "RETURN-CODE" ,
  .picture        = "" ,
  .initial        = "" ,
  .parent         = NULL,
  .occurs_lower   = 0 ,
  .occurs_upper   = 0 ,
  .attr           = signable_e ,
  .type           = FldNumericBin5 ,
  .level          = 0 ,
  .digits         = 4 ,
  .rdigits        = 0 ,
  .encoding       = iconv_CP1252_e ,
  .alphabet       = 0 ,
  };

unsigned char __gg___data_dev_stdin[] = "/dev/stdin";
struct cblc_field_t __ggsr___dev_stdin = {
  .data           = __gg___data_dev_stdin ,
  .capacity       = sizeof(__gg___data_dev_stdin)-1 ,
  .allocated       = sizeof(__gg___data_dev_stdin)-1 ,
  .offset         = 0 ,
  .name           = "_dev_stdin" ,
  .picture        = "" ,
  .initial        = "/dev/stdin" ,
  .parent         = NULL,
  .occurs_lower   = 0 ,
  .occurs_upper   = 0 ,
  .attr           = 0x0 ,
  .type           = FldLiteralA ,
  .level          = 0 ,
  .digits         = 0 ,
  .rdigits        = 0 ,
  .encoding       = iconv_CP1252_e ,
  .alphabet       = 0 ,
  };

 unsigned char __gg___data_dev_stdout[] = "/dev/stdout";
struct cblc_field_t __ggsr___dev_stdout = {
  .data           = __gg___data_dev_stdout ,
  .capacity       = sizeof(__gg___data_dev_stdout)-1 ,
  .allocated      = sizeof(__gg___data_dev_stdout)-1 ,
  .offset         = 0 ,
  .name           = "_dev_stdout" ,
  .picture        = "" ,
  .initial        = "/dev/stdout" ,
  .parent         = NULL,
  .occurs_lower   = 0 ,
  .occurs_upper   = 0 ,
  .attr           = 0x0 ,
  .type           = FldLiteralA ,
  .level          = 0 ,
  .digits         = 0 ,
  .rdigits        = 0 ,
  .encoding       = iconv_CP1252_e ,
  .alphabet       = 0 ,
  };

unsigned char __gg___data_dev_stderr[] = "/dev/stderr";
struct cblc_field_t __ggsr___dev_stderr = {
  .data           = __gg___data_dev_stderr ,
  .capacity       = sizeof(__gg___data_dev_stderr)-1 ,
  .allocated      = sizeof(__gg___data_dev_stderr)-1 ,
  .offset         = 0 ,
  .name           = "_dev_stderr" ,
  .picture        = "" ,
  .initial        = "/dev/stderr" ,
  .parent         = NULL,
  .occurs_lower   = 0 ,
  .occurs_upper   = 0 ,
  .attr           = 0x0 ,
  .type           = FldLiteralA ,
  .level          = 0 ,
  .digits         = 0 ,
  .rdigits        = 0 ,
  .encoding       = iconv_CP1252_e ,
  .alphabet       = 0 ,
  };

unsigned char __gg___data_dev_null[] = "/dev/null";
struct cblc_field_t __ggsr___dev_null = {
  .data           = __gg___data_dev_null ,
  .capacity       = sizeof(__gg___data_dev_null)-1 ,
  .allocated      = sizeof(__gg___data_dev_null)-1 ,
  .offset         = 0 ,
  .name           = "_dev_null" ,
  .picture        = "" ,
  .initial        = "/dev/null" ,
  .parent         = NULL,
  .occurs_lower   = 0 ,
  .occurs_upper   = 0 ,
  .attr           = 0x0 ,
  .type           = FldLiteralA ,
  .level          = 0 ,
  .digits         = 0 ,
  .rdigits        = 0 ,
  .encoding       = iconv_CP1252_e ,
  .alphabet       = 0 ,
  };

unsigned char __gg__data_tally[] = {0,0};
struct cblc_field_t __ggsr___tally = {
  .data           = __gg__data_tally ,
  .capacity       = 4 ,
  .allocated      = 4 ,
  .offset         = 0 ,
  .name           = "_TALLY" ,
  .picture        = "" ,
  .initial        = "" ,
  .parent         = NULL,
  .occurs_lower   = 0 ,
  .occurs_upper   = 0 ,
  .attr           = global_e ,
  .type           = FldNumericBin5 ,
  .level          = 0 ,
  .digits         = 5 ,
  .rdigits        = 0 ,
  .encoding       = iconv_CP1252_e ,
  .alphabet       = 0 ,
  };

unsigned char __gg__data_argi[] = {0,0};
struct cblc_field_t __ggsr__argi = {
  .data           = __gg__data_argi ,
  .capacity       = 4 ,
  .allocated      = 4 ,
  .offset         = 0 ,
  .name           = "_ARGI" ,
  .picture        = "" ,
  .initial        = "" ,
  .parent         = NULL,
  .occurs_lower   = 0 ,
  .occurs_upper   = 0 ,
  .attr           = global_e ,
  .type           = FldNumericBin5 ,
  .level          = 0 ,
  .digits         = 5 ,
  .rdigits        = 0 ,
  .encoding       = iconv_CP1252_e ,
  .alphabet       = 0 ,
  };

/*
 * Special registers used by the XML parser
 */
// XML-CODE  PICTURE S9(9) USAGE BINARY VALUE ZERO   *> status of XML event
static int __gg__data_xml_code = 0;
struct cblc_field_t __ggsr__xml_code = {
  .data           = reinterpret_cast<unsigned char*>(&__gg__data_xml_code), 
  .capacity       = 4 ,
  .allocated      = 4 ,
  .offset         = 0 ,
  .name           = "XML-CODE" ,
  .picture        = "" ,
  .initial        = "" ,
  .parent         = NULL,
  .occurs_lower   = 0 ,
  .occurs_upper   = 0 ,
  .attr           = register_e,
  .type           = FldNumericBin5 ,
  .level          = 0 ,
  .digits         = 9 ,
  .rdigits        = 0 ,
  .encoding       = iconv_CP1252_e ,
  .alphabet       = 0 ,
  };

// XML-EVENT PICTURE X(30) USAGE DISPLAY VALUE SPACE *> name of XML event
static unsigned char __gg__data_xml_event[30];
struct cblc_field_t __ggsr__xml_event = {
  .data           = __gg__data_xml_event, 
  .capacity       = 30 ,
  .allocated      = 30 ,
  .offset         = 0 ,
  .name           = "XML-EVENT" ,
  .picture        = "" ,
  .initial        = NULL,
  .parent         = NULL, 
  .occurs_lower   = 0 ,
  .occurs_upper   = 0 ,
  .attr           = register_e ,
  .type           = FldAlphanumeric ,
  .level          = 0 ,
  .digits         = 0 ,
  .rdigits        = 0 ,
  .encoding       = iconv_CP1252_e ,
  .alphabet       = 0 ,
  };

// XML-INFORMATION  PICTURE S9(9) USAGE BINARY VALUE ZERO
static int __gg__data_xml_information = 0;
struct cblc_field_t __ggsr__xml_information = {
  .data           = reinterpret_cast<unsigned char*>(&__gg__data_xml_information), 
  .capacity       = 4 ,
  .allocated      = 4 ,
  .offset         = 0 ,
  .name           = "XML-INFORMATION" ,
  .picture        = "" ,
  .initial        = "" ,
  .parent         = NULL,
  .occurs_lower   = 0 ,
  .occurs_upper   = 0 ,
  .attr           = register_e,
  .type           = FldNumericBin5 ,
  .level          = 0 ,
  .digits         = 9 ,
  .rdigits        = 0 ,
  .encoding       = iconv_CP1252_e ,
  .alphabet       = 0 ,
  };

// XML-NAMESPACE  Variable-length based alphanumeric item
struct cblc_field_t __ggsr__xml_namespace = {
  .data           = nullptr ,
  .capacity       = 1 ,
  .allocated      = 1 ,
  .offset         = 0 ,
  .name           = "XML-NAMESPACE" ,
  .picture        = "" ,
  .initial        = "" ,
  .parent         = NULL,
  .occurs_lower   = 0 ,
  .occurs_upper   = 0 ,
  .attr           = register_e | based_e | any_length_e,
  .type           = FldAlphanumeric ,
  .level          = 0 ,
  .digits         = 0 ,
  .rdigits        = 0 ,
  .encoding       = iconv_CP1252_e ,
  .alphabet       = 0 ,
  };

// XML-NNAMESPACE Variable-length national item
struct cblc_field_t __ggsr__xml_nnamespace = {
  .data           = nullptr ,
  .capacity       = 1 ,
  .allocated      = 1 ,
  .offset         = 0 ,
  .name           = "XML-NNAMESPACE" ,
  .picture        = "" ,
  .initial        = "" ,
  .parent         = NULL,
  .occurs_lower   = 0 ,
  .occurs_upper   = 0 ,
  .attr           = register_e | based_e | any_length_e,
  .type           = FldAlphanumeric ,
  .level          = 0 ,
  .digits         = 0 ,
  .rdigits        = 0 ,
  .encoding       = iconv_CP1252_e ,
  .alphabet       = 0 ,
  };

// XML-NAMESPACE-PREFIX  Variable-length based alphanumeric item
struct cblc_field_t __ggsr__xml_namespace_prefix = {
  .data           = nullptr ,
  .capacity       = 1 ,
  .allocated      = 1 ,
  .offset         = 0 ,
  .name           = "XML-NAMESPACE-PREFIX" ,
  .picture        = "" ,
  .initial        = "" ,
  .parent         = NULL,
  .occurs_lower   = 0 ,
  .occurs_upper   = 0 ,
  .attr           = register_e | based_e | any_length_e,
  .type           = FldAlphanumeric ,
  .level          = 0 ,
  .digits         = 0 ,
  .rdigits        = 0 ,
  .encoding       = iconv_CP1252_e ,
  .alphabet       = 0 ,
  };

// XML-NNAMESPACE_PREFIX Variable-length national item
struct cblc_field_t __ggsr__xml_nnamespace_prefix = {
  .data           = nullptr ,
  .capacity       = 1 ,
  .allocated      = 1 ,
  .offset         = 0 ,
  .name           = "XML-NNAMESPACE-PREFIX" ,
  .picture        = "" ,
  .initial        = "" ,
  .parent         = NULL,
  .occurs_lower   = 0 ,
  .occurs_upper   = 0 ,
  .attr           = register_e | based_e | any_length_e,
  .type           = FldAlphanumeric ,
  .level          = 0 ,
  .digits         = 0 ,
  .rdigits        = 0 ,
  .encoding       = iconv_CP1252_e ,
  .alphabet       = 0 ,
  };

// XML-TEXT  Variable-length based alphanumeric item
struct cblc_field_t __ggsr__xml_text = {
  .data           = nullptr ,
  .capacity       = 1 ,
  .allocated      = 1 ,
  .offset         = 0 ,
  .name           = "XML-TEXT" ,
  .picture        = "" ,
  .initial        = "" ,
  .parent         = NULL,
  .occurs_lower   = 0 ,
  .occurs_upper   = 0 ,
  .attr           = register_e | based_e | any_length_e ,
  .type           = FldAlphanumeric ,
  .level          = 0 ,
  .digits         = 0 ,
  .rdigits        = 0 ,
  .encoding       = iconv_CP1252_e ,
  .alphabet       = 0 ,
  };

// XML-NTEXT Variable-length national item
struct cblc_field_t __ggsr__xml_ntext = {
  .data           = nullptr ,
  .capacity       = 1 ,
  .allocated      = 1 ,
  .offset         = 0 ,
  .name           = "XML-NTEXT" ,
  .picture        = "" ,
  .initial        = "" ,
  .parent         = NULL,
  .occurs_lower   = 0 ,
  .occurs_upper   = 0 ,
  .attr           = register_e | based_e | any_length_e,
  .type           = FldAlphanumeric ,
  .level          = 0 ,
  .digits         = 0 ,
  .rdigits        = 0 ,
  .encoding       = iconv_CP1252_e ,
  .alphabet       = 0 ,
  };

/* The following defines storage for the global DEBUG-ITEM:

     01 DEBUG-ITEM.
        02 DEBUG-LINE PIC X(6).
        02 FILLER PIC X VALUE SPACE.
        02 DEBUG-NAME PIC X(30).
        02 FILLER PIC X VALUE SPACE.
        02 DEBUG-SUB-1 PIC S9999 SIGN IS LEADING SEPARATE CHARACTER.
        02 FILLER PIC X VALUE SPACE.
        02 DEBUG-SUB-2 PIC S9999 SIGN IS LEADING SEPARATE CHARACTER.
        02 FILLER PIC X VALUE SPACE.
        02 DEBUG-SUB-3 PIC S9999 SIGN IS LEADING SEPARATE CHARACTER.
        02 FILLER PIC X VALUE SPACE.
        02 DEBUG-CONTENTS PIC X(76).
*/

unsigned char __gg__debug_item_data[132] = 
                                 "                                      "
                                 "+0000 +0000 +0000 "
                                 "                                      "
                                 "                                     ";
struct cblc_field_t __ggsr__debug_item = {
  .data           = __gg__debug_item_data ,
  .capacity       = 132 ,
  .allocated      = 132 ,
  .offset         = 0 ,
  .name           = "DEBUG-ITEM" ,
  .picture        = "" ,
  .initial        =              "                                      "
                                 "+0000 +0000 +0000 "
                                 "                                      "
                                 "                                     " ,
  .parent         = NULL ,
  .occurs_lower   = 0 ,
  .occurs_upper   = 0 ,
  .attr           = register_e ,
  .type           = FldGroup ,
  .level          = 01 ,
  .digits         = 0 ,
  .rdigits        = 0 ,
  .encoding       = iconv_CP1252_e ,
  .alphabet       = 0 ,
  };

struct cblc_field_t __ggsr__debug_line = {
  .data           = __gg__debug_item_data + 0 ,
  .capacity       = 6 ,
  .allocated      = 6 ,
  .offset         = 0 ,
  .name           = "DEBUG-LINE" ,
  .picture        = "" ,
  .initial        = NULL ,
  .parent         = &__ggsr__debug_item ,
  .occurs_lower   = 0 ,
  .occurs_upper   = 0 ,
  .attr           = register_e ,
  .type           = FldAlphanumeric ,
  .level          = 05 ,
  .digits         = 0 ,
  .rdigits        = 0 ,
  .encoding       = iconv_CP1252_e ,
  .alphabet       = 0 ,
  };

struct cblc_field_t __ggsr__debug_filler_1 = {
  .data           = __gg__debug_item_data + 6 ,
  .capacity       = 1 ,
  .allocated      = 1 ,
  .offset         = 6 ,
  .name           = "FILLER" ,
  .picture        = "" ,
  .initial        = NULL ,
  .parent         = &__ggsr__debug_item ,
  .occurs_lower   = 0 ,
  .occurs_upper   = 0 ,
  .attr           = register_e | filler_e ,
  .type           = FldAlphanumeric ,
  .level          = 05 ,
  .digits         = 0 ,
  .rdigits        = 0 ,
  .encoding       = iconv_CP1252_e ,
  .alphabet       = 0 ,
  };

struct cblc_field_t __ggsr__debug_name = {
  .data           = __gg__debug_item_data + 7 ,
  .capacity       = 30 ,
  .allocated      = 30 ,
  .offset         = 7 ,
  .name           = "DEBUG-NAME" ,
  .picture        = "" ,
  .initial        = NULL ,
  .parent         = &__ggsr__debug_item ,
  .occurs_lower   = 0 ,
  .occurs_upper   = 0 ,
  .attr           = register_e ,
  .type           = FldAlphanumeric ,
  .level          = 05 ,
  .digits         = 0 ,
  .rdigits        = 0 ,
  .encoding       = iconv_CP1252_e ,
  .alphabet       = 0 ,
  };

struct cblc_field_t __ggsr__debug_filler_2 = {
  .data           = __gg__debug_item_data + 37 ,
  .capacity       = 1 ,
  .allocated      = 1 ,
  .offset         = 37 ,
  .name           = "FILLER" ,
  .picture        = "" ,
  .initial        = NULL ,
  .parent         = &__ggsr__debug_item ,
  .occurs_lower   = 0 ,
  .occurs_upper   = 0 ,
  .attr           = register_e | filler_e ,
  .type           = FldAlphanumeric ,
  .level          = 05 ,
  .digits         = 0 ,
  .rdigits        = 0 ,
  .encoding       = iconv_CP1252_e ,
  .alphabet       = 0 ,
  };

struct cblc_field_t __ggsr__debug_sub_1 = {
  .data           = __gg__debug_item_data + 38 ,
  .capacity       = 5 ,
  .allocated      = 5 ,
  .offset         = 38 ,
  .name           = "DEBUG-SUB-1" ,
  .picture        = "" ,
  .initial        = NULL ,
  .parent         = &__ggsr__debug_item ,
  .occurs_lower   = 0 ,
  .occurs_upper   = 0 ,
  .attr           = signable_e | register_e | leading_e | separate_e ,
  .type           = FldNumericDisplay ,
  .level          = 05 ,
  .digits         = 4 ,
  .rdigits        = 0 ,
  .encoding       = iconv_CP1252_e ,
  .alphabet       = 0 ,
  };

struct cblc_field_t __ggsr__debug_filler_3 = {
  .data           = __gg__debug_item_data + 43,
  .capacity       =  1,
  .allocated      =  1,
  .offset         =  43,
  .name           = "FILLER" ,
  .picture        = "" ,
  .initial        = NULL ,
  .parent         = &__ggsr__debug_item ,
  .occurs_lower   = 0 ,
  .occurs_upper   = 0 ,
  .attr           = register_e | filler_e ,
  .type           = FldAlphanumeric ,
  .level          = 05 ,
  .digits         = 0 ,
  .rdigits        = 0 ,
  .encoding       = iconv_CP1252_e ,
  .alphabet       = 0 ,
  };

struct cblc_field_t __ggsr__debug_sub_2 = {
  .data           = __gg__debug_item_data + 44 ,
  .capacity       = 5 ,
  .allocated      = 5 ,
  .offset         = 44 ,
  .name           = "DEBUG-SUB-2" ,
  .picture        = "" ,
  .initial        = NULL ,
  .parent         = &__ggsr__debug_item ,
  .occurs_lower   = 0 ,
  .occurs_upper   = 0 ,
  .attr           = signable_e | register_e | leading_e | separate_e ,
  .type           = FldNumericDisplay ,
  .level          = 05 ,
  .digits         = 4 ,
  .rdigits        = 0 ,
  .encoding       = iconv_CP1252_e ,
  .alphabet       = 0 ,
  };

struct cblc_field_t __ggsr__debug_filler_4 = {
  .data           = __gg__debug_item_data +  49,
  .capacity       =  1,
  .allocated      =  1,
  .offset         =  49,
  .name           = "FILLER" ,
  .picture        = "" ,
  .initial        = NULL ,
  .parent         = &__ggsr__debug_item ,
  .occurs_lower   = 0 ,
  .occurs_upper   = 0 ,
  .attr           = register_e | filler_e ,
  .type           = FldAlphanumeric ,
  .level          = 05 ,
  .digits         = 0 ,
  .rdigits        = 0 ,
  .encoding       = iconv_CP1252_e ,
  .alphabet       = 0 ,
  };

struct cblc_field_t __ggsr__debug_sub_3 = {
  .data           = __gg__debug_item_data + 50 ,
  .capacity       = 5 ,
  .allocated      = 5 ,
  .offset         = 50 ,
  .name           = "DEBUG-SUB-3" ,
  .picture        = "" ,
  .initial        = NULL ,
  .parent         = &__ggsr__debug_item ,
  .occurs_lower   = 0 ,
  .occurs_upper   = 0 ,
  .attr           = signable_e | register_e | leading_e | separate_e ,
  .type           = FldNumericDisplay ,
  .level          = 05 ,
  .digits         = 4 ,
  .rdigits        = 0 ,
  .encoding       = iconv_CP1252_e ,
  .alphabet       = 0 ,
  };

struct cblc_field_t __ggsr__debug_filler_5 = {
  .data           = __gg__debug_item_data + 55,
  .capacity       =  1,
  .allocated      =  1,
  .offset         =  55,
  .name           = "FILLER" ,
  .picture        = "" ,
  .initial        = NULL ,
  .parent         = &__ggsr__debug_item ,
  .occurs_lower   = 0 ,
  .occurs_upper   = 0 ,
  .attr           = register_e | filler_e ,
  .type           = FldAlphanumeric ,
  .level          = 05 ,
  .digits         = 0 ,
  .rdigits        = 0 ,
  .encoding       = iconv_CP1252_e ,
  .alphabet       = 0 ,
  };

struct cblc_field_t __ggsr__debug_contents = {
  .data           = __gg__debug_item_data + 56 ,
  .capacity       = 76 ,
  .allocated      = 76 ,
  .offset         = 56 ,
  .name           = "DEBUG-CONTENTS" ,
  .picture        = "" ,
  .initial        = NULL ,
  .parent         = &__ggsr__debug_item ,
  .occurs_lower   = 0 ,
  .occurs_upper   = 0 ,
  .attr           = signable_e | register_e | leading_e | separate_e ,
  .type           = FldAlphanumeric ,
  .level          = 05 ,
  .digits         = 0 ,
  .rdigits        = 0 ,
  .encoding       = iconv_CP1252_e ,
  .alphabet       = 0 ,
  };

#pragma GCC diagnostic pop




