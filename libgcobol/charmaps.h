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

#ifndef CHARMAPS_H
#define CHARMAPS_H

#include <unistd.h>

/*  There are four distinct codeset domains in the COBOL compiler.
 *
 *  First is the codeset of the console.  Established by looking at what
 *  setlocale() reports, this can be either UTF-8 or some ASCII based code
 *  page.  (We assume CP1252).  Data coming from the console or the system,
 *  ACCEPT statements; redirected console input, getenv() and other system
 *  calls are in the "console" domain.
 *
 *  Second is the internal single-byte-coded codeset of the data, in memory,
 *  being manipulated by the generated code of the cobol executable.  The actual
 *  codeset of "internal" is either EBCDIC (in the form of Code Page 1140 or
 *  ASCII (Code Page 1252)
 *
 *  Third is the C++ source code of the GCOBOL compiler; this comment is
 *  in that environment.  We neither know, nor care, if this code is encoded in
 *  in UTF-8 (as is probable, in these enlighted days of 2022) or something like
 *  Code Page1252. We are going to regard it as "ascii" under the
 *  assumption that there is no reason for any character in the compiler's
 *  source code to have a code point outside of the plain vanilla 0x20 through
 *  0x7F range.
 *
 *  Fourth is the "raw" COBOL source code that is the input to the GCOBOL
 *  compiler.  This domain can be either UTF-8 or something like CodePage1252.
 *  Which encoding is relevant; The literal string MOVE "<euro>1234" is seven
 *  bytes long in UTF-8, and five bytes long in CP1252.  We start with an
 *  assumption that it is UTF-8 and switch to CP1252 upon encountering a byte
 *  sequence with values above 0x80 that can't be UTF-8.  We have provision for
 *  forcing it to be one or the other.  Codepoints in that domain are referenced
 *  as "raw".  Codepoint in the "raw" domain don't last long; they are be
 *  converted to either "ascii" or "internal" early on, as necessary.
 */


/*  Notes on character codesets:

    This library is implemented to handle "native" codesets of either ASCII (in
    the form of a single-byte-coded codeset like code page 1252) or EBCDIC (in
    the form of a single-byte-coded codeset like code page 1140).

    This C/C++ source code, however, is assumed to be an ASCII-based codeset,
    so that a character constant like a space is assumed to encode as 0x20.

    Furthermore, we assume that the codeset of the COBOL source code being
    compiled is also ASCII-based, even if it is actually UTF-8. Said another
    way, characters encoded between zero and 127 are regarded as ASCII.

    This means that we are not going to try to compile EBCDIC COBOL source code;
    any such will have to be externally converted to ASCII before feeding it
    through this compiler on an ASCII based Linux system.

    This situation is rife for confusion here in the source code for the
    library.

    To help reduce that confusion, we are going to eschew character constants
    in the C/C++ source code.  Instead, we use symbolic versions.  In general,
    "source_space" means 0x20, while "internal_space" will be either 0x20
    when using the ASCII-based native codeset, or it will be 0x40 when using
    the EBCDIC-based native codeset.

    Maintaining one's sanity while learning and working with this C/C++ code
    will require a firm grip on context.  You'll have to keep track of whether
    the character is being used to analyze the ASCII-based COBOL source, or
    whether the character in question is part of the native COBOL cobol data
    that is being analyzed or generated.

    For example, when a PICTURE string has in it a source_nine, the generated
    result in the variable is based on character_zero.

    Stay alert!    */

extern int    __gg__decimal_point        ;
extern int    __gg__decimal_separator    ;
extern int    __gg__quote_character      ;
extern int    __gg__low_value_character  ;
extern int    __gg__high_value_character ;
extern char **__gg__currency_signs       ;
extern int    __gg__default_currency_sign;
extern cbl_encoding_t __gg__display_encoding ;
extern cbl_encoding_t __gg__national_encoding ;
extern char  *__gg__ct_currency_signs[256];  // Compile-time currency signs

#define NULLCH ('\0')
#define DEGENERATE_HIGH_VALUE 0xFF
#define DEGENERATE_LOW_VALUE 0x00

#define ascii_A                ((uint8_t)('A'))
#define ascii_B                ((uint8_t)('B'))
#define ascii_C                ((uint8_t)('C'))
#define ascii_D                ((uint8_t)('D'))
#define ascii_E                ((uint8_t)('E'))
#define ascii_F                ((uint8_t)('F'))
#define ascii_G                ((uint8_t)('G'))
#define ascii_H                ((uint8_t)('H'))
#define ascii_I                ((uint8_t)('I'))
#define ascii_J                ((uint8_t)('J'))
#define ascii_K                ((uint8_t)('K'))
#define ascii_L                ((uint8_t)('L'))
#define ascii_M                ((uint8_t)('M'))
#define ascii_N                ((uint8_t)('N'))
#define ascii_O                ((uint8_t)('O'))
#define ascii_P                ((uint8_t)('P'))
#define ascii_Q                ((uint8_t)('Q'))
#define ascii_R                ((uint8_t)('R'))
#define ascii_S                ((uint8_t)('S'))
#define ascii_T                ((uint8_t)('T'))
#define ascii_U                ((uint8_t)('U'))
#define ascii_V                ((uint8_t)('V'))
#define ascii_W                ((uint8_t)('W'))
#define ascii_X                ((uint8_t)('X'))
#define ascii_Y                ((uint8_t)('Y'))
#define ascii_Z                ((uint8_t)('Z'))
#define ascii_a                ((uint8_t)('a'))
#define ascii_b                ((uint8_t)('b'))
#define ascii_c                ((uint8_t)('c'))
#define ascii_d                ((uint8_t)('d'))
#define ascii_e                ((uint8_t)('e'))
#define ascii_f                ((uint8_t)('f'))
#define ascii_g                ((uint8_t)('g'))
#define ascii_h                ((uint8_t)('h'))
#define ascii_i                ((uint8_t)('i'))
#define ascii_j                ((uint8_t)('j'))
#define ascii_k                ((uint8_t)('k'))
#define ascii_l                ((uint8_t)('l'))
#define ascii_m                ((uint8_t)('m'))
#define ascii_n                ((uint8_t)('n'))
#define ascii_o                ((uint8_t)('o'))
#define ascii_p                ((uint8_t)('p'))
#define ascii_q                ((uint8_t)('q'))
#define ascii_r                ((uint8_t)('r'))
#define ascii_s                ((uint8_t)('s'))
#define ascii_t                ((uint8_t)('t'))
#define ascii_u                ((uint8_t)('u'))
#define ascii_v                ((uint8_t)('v'))
#define ascii_w                ((uint8_t)('w'))
#define ascii_x                ((uint8_t)('x'))
#define ascii_y                ((uint8_t)('y'))
#define ascii_z                ((uint8_t)('z'))
#define ascii_space            ((uint8_t)(' '))
#define ascii_zero             ((uint8_t)('0'))
#define ascii_0                ((uint8_t)('0'))
#define ascii_1                ((uint8_t)('1'))
#define ascii_2                ((uint8_t)('2'))
#define ascii_3                ((uint8_t)('3'))
#define ascii_4                ((uint8_t)('4'))
#define ascii_5                ((uint8_t)('5'))
#define ascii_6                ((uint8_t)('6'))
#define ascii_7                ((uint8_t)('7'))
#define ascii_8                ((uint8_t)('8'))
#define ascii_9                ((uint8_t)('9'))
#define ascii_nine             ((uint8_t)('9'))
#define ascii_period           ((uint8_t)('.'))
#define ascii_colon            ((uint8_t)(':'))
#define ascii_comma            ((uint8_t)(','))
#define ascii_dollar_sign      ((uint8_t)('$'))
#define ascii_dquote           ((uint8_t)('"'))
#define ascii_oparen           ((uint8_t)('('))
#define ascii_caret            ((uint8_t)('^'))
#define ascii_slash            ((uint8_t)('/'))
#define ascii_plus             ((uint8_t)('+'))
#define ascii_minus            ((uint8_t)('-'))
#define ascii_hyphen           ((uint8_t)('-'))
#define ascii_underscore       ((uint8_t)('_'))
#define ascii_asterisk         ((uint8_t)('*'))
#define ascii_query            ((uint8_t)('?'))
#define ascii_cr               ((uint8_t)('\r'))
#define ascii_ff               ((uint8_t)('\f'))
#define ascii_newline          ((uint8_t)('\n'))
#define ascii_return           ((uint8_t)('\r'))

extern unsigned char __gg__data_space[1]       ;
extern unsigned char __gg__data_low_values[1]  ;
extern unsigned char __gg__data_zeros[1]       ;
extern unsigned char __gg__data_high_values[1] ;
extern unsigned char __gg__data_quotes[1]      ;
extern unsigned char __gg__data_upsi_0[2]      ;
extern short         __gg__data_return_code    ;

// These are the various hardcoded tables used for conversions.
extern const unsigned short __gg__one_to_one_values[256];
extern const unsigned short __gg__cp1252_to_cp1140_values[256];
extern const unsigned short __gg__cp1140_to_cp1252_values[256];

// These are the two standard collations.
extern const unsigned short __gg__cp1252_to_ebcdic_collation[256];
extern const unsigned short __gg__ebcdic_to_cp1252_collation[256];

const char * __gg__encoding_iconv_name( cbl_encoding_t encoding );
cbl_encoding_t __gg__encoding_iconv_type( const char *name );

char * __gg__iconverter(cbl_encoding_t from,
                        cbl_encoding_t to,
                  const char *str,
                        size_t length,
                        size_t *outlength);

#define DEFAULT_CHARMAP_SOURCE (iconv_CP1252_e)

class charmap_t
  {
  private:
    // This is the encoding of this character map
    cbl_encoding_t m_encoding;

    enum
      {
      sign_type_ascii,
      sign_type_ebcdic,
      } m_numeric_sign_type;

    // This map retains the ASCII-to-encoded value in m_encoding, so that iconv
    // need be called but once for each ASCII value.
    std::unordered_map<int, int>m_map_of_encodings;

    void determine_sign_type()
      {
      if( mapped_character(ascii_0) & 0x80 )
        {
        m_numeric_sign_type = sign_type_ebcdic;
        }
      else
        {
        m_numeric_sign_type = sign_type_ascii;
        }
      }

  public:
    explicit charmap_t(cbl_encoding_t e) : m_encoding(e)
      {
      determine_sign_type();
      }
    explicit charmap_t(uint16_t e) : m_encoding(static_cast<cbl_encoding_t>(e))
      {
      determine_sign_type();
      }

    int mapped_character(int ch)
      {
      // The assumption is that anybody calling this routine is providing
      // a single-byte character in the DEFAULT_CHARMAP_SOURCE encoding.  We
      // return the equivalent character in the m_encoding
      int retval;
      std::unordered_map<int, int>::const_iterator it =
                                                   m_map_of_encodings.find(ch);
      if( it != m_map_of_encodings.end() )
        {
        retval = it->second;
        }
      else
        {
        retval = 0;
        size_t outlength = 0;
        const char *mapped = __gg__iconverter(DEFAULT_CHARMAP_SOURCE,
                                              m_encoding,
                                              PTRCAST(char, &ch),
                                              1,
                                              &outlength);
        memcpy(&retval, mapped, outlength);
        m_map_of_encodings[ch] = retval;
        }
      return retval;
      }

    int decimal_point()
      {
      return mapped_character(__gg__decimal_point);
      }
    int decimal_separator()
      {
      return mapped_character(__gg__decimal_separator);
      }
    int quote_character()
      {
      return mapped_character(__gg__quote_character);
      }
    int low_value_character()
      {
      return mapped_character(__gg__low_value_character);
      }
    int high_value_character()
      {
      return mapped_character(__gg__high_value_character);
      }

    int figconst_character(cbl_figconst_t figconst)
      {
      int const_char = 0;  // Head off a compiler warning
      switch(figconst)
        {
        case normal_value_e :
          const_char = -1;
          break;
        case low_value_e    :
          const_char = low_value_character();
          break;
        case zero_value_e   :
          const_char = mapped_character(ascii_0);
          break;
        case space_value_e  :
          const_char = mapped_character(ascii_space);
          break;
        case quote_value_e  :
          const_char = quote_character();
          break;
        case high_value_e   :
          const_char = high_value_character();
          break;
        case null_value_e:
          const_char = '\0';
          break;
        default:
          abort();
          break;
        }
      return const_char;
      }

  bool
  is_digit_negative(int digit)
    {
    bool retval;
    switch(m_numeric_sign_type)
      {
      case sign_type_ascii:
        retval = !!(digit & NUMERIC_DISPLAY_SIGN_BIT_ASCII);
        break;

      case sign_type_ebcdic:
        retval = !!((~digit) & NUMERIC_DISPLAY_SIGN_BIT_EBCDIC);
        break;
      }
    return retval;
    }

  int
  set_digit_negative(int digit, bool is_negative)
    {
    switch(m_numeric_sign_type)
      {
      case sign_type_ascii:
        if( is_negative )
          {
          digit |= NUMERIC_DISPLAY_SIGN_BIT_ASCII;
          }
        else
          {
          digit &= ~NUMERIC_DISPLAY_SIGN_BIT_ASCII;
          }
        break;

      case sign_type_ebcdic:
        if( is_negative )
          {
          digit &= ~NUMERIC_DISPLAY_SIGN_BIT_EBCDIC;
          }
        else
          {
          digit |= NUMERIC_DISPLAY_SIGN_BIT_EBCDIC;
          }
        break;
      }
    return digit;
    }

  bool
  is_like_ebcdic() const
    {
    return m_numeric_sign_type == sign_type_ebcdic;
    }

  };

charmap_t *__gg__get_charmap(cbl_encoding_t encoding);

#endif