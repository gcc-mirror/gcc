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

#include <string>
#include <vector>

#include <unistd.h>
#include <limits.h>
#include <iconv.h>

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

typedef uint32_t cbl_char_t;
#define NOT_A_CHARACTER (0xbadbeef)

extern int    __gg__decimal_point        ;
extern int    __gg__decimal_separator    ;
extern int    __gg__quote_character      ;
extern int    __gg__low_value_character  ;
extern int    __gg__high_value_character ;
extern std::vector<std::string> __gg__currency_signs       ;
extern int    __gg__default_currency_sign;
extern cbl_encoding_t __gg__display_encoding ;
extern cbl_encoding_t __gg__national_encoding ;
extern cbl_char_t __gg__working_init;
extern cbl_char_t __gg__local_init;
extern uint32_t __gg__wsclear;

enum
  {
  /* HIGH-VALUE is an endless source of irritation.
  
     0xFF is the default value for COBOL since time immemorial.  Its use that
     way long predates the existence of code pages.  0xFF is a valid character
     in many code pages, which make a muddle of the original intent of a
     default value of 0xFF for high-value.

     We want older programs to continue to work.  And we want to use 0xFF for
     ascii and ebcdic, and it turns out that 0xFFFF works for UTF-16; it is
     specifically designed in UNICODE as a well-formed non-character.
     
     0xFFFFFFFF, however, is not readily usable in UTF-32.  It is not well-
     formed, and it is not a character.  Technically, the largest value in
     UTF-32 is the largest UNICODE code point, which is 0x10FFFF.  It's
     tempting to use that value as the UTF32 HIGH-VALUE, except that it doesn't
     map into a single 16-bit value in UTF-16 (it takes a pair of 16-bit
     values), and it doesn't map into anything sensible in ASCII or EBCDIC, and
     it takes multiple bytes in UTF-8.
     
     So, we are going to work with the following observations:
     
     0xFF   in CP1252 <==> 0x000000FF in UTF32
     0xFF   in CP1140 <==> 0x0000009F in UTF32
     0xFFFF in UTF-16 <==> 0x0000FFFF in UTF32

     Be it hereby acknowledged that not all possibilities for encoding inter-
     conversion have been explored, and we anticipate finding and eliminating
     HIGH-VALUE problems will be Whac-A-Mole territory for some time to come.
     
     Please use these constants for that kind of work, because otherwise
     finding anomalies will be even more frustrating than I currently
     anticipate.  Dubner, 2025-11-24  */
  DEFAULT_HIGH_VALUE_8  =       0xFF,
  DEFAULT_HIGH_VALUE_16 =     0x00FF,
  DEFAULT_HIGH_VALUE_32 = 0x000000FF,
  
  /* These values are used as figurative constants when interconverting from
     and encoding to UTF32.  Examine, for example, the implementation for
     the INSPECT statement: */
  ASCII_HIGH_VALUE_32   = 0x000000FF,
  EBCDIC_HIGH_VALUE_32  = 0x000000FF,
  UTF16_HIGH_VALUE_32   = 0x000000FF,
  UTF32_HIGH_VALUE_32   = 0x000000FF,

  REPLACEMENT_CHARACTER = 0xFFFD,
  };

#define NULLCH ('\0')
#define DEGENERATE_HIGH_VALUE 0xFF
#define DEGENERATE_LOW_VALUE 0x00

#define ascii_nul              ((uint8_t)('\0'))
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
#define ascii_bang             ((uint8_t)('!'))
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
extern cbl_encoding_t __gg__console_encoding;

// returns a pointer to a static buffer.  Beware!
char * __gg__iconverter(cbl_encoding_t from,
                        cbl_encoding_t to,
                  const void *str,
                        size_t length,
                        size_t *outlength = nullptr,     // Bytes produced
                        size_t *iconv_retval = nullptr);

// returns a malloced buffer.  Remember to free it.
char * __gg__miconverter(cbl_encoding_t from,
                         cbl_encoding_t to,
                   const void *str,
                         size_t length,
                         size_t *outlength = nullptr,     // Bytes produced
                         size_t *iconv_retval = nullptr);


#define DEFAULT_SOURCE_ENCODING (iconv_CP1252_e)
#define DEFAULT_32_ENCODING (iconv_UTF32LE_e)

class charmap_t;

charmap_t *__gg__get_charmap(cbl_encoding_t encoding);

class charmap_t
  {
  private:
    // This is the encoding of this character map
    cbl_encoding_t m_encoding;
    bool m_is_valid;
    bool m_is_big_endian;
    bool m_has_bom = false;
    bool m_is_like_utf8;
    uint8_t  m_stride; // Number of bytes between one character and the next

    enum
      {
      sign_type_ascii,
      sign_type_ebcdic,
      } m_numeric_sign_type;

    // This map retains the ASCII-to-encoded value in m_encoding, so that iconv
    // need be called but once for each ASCII value.
    std::unordered_map<cbl_char_t, cbl_char_t>m_map_of_encodings;

  public:
    explicit charmap_t(cbl_encoding_t e) : m_encoding(e)
      {
      // We are constructing a new charmap_t from an arbitrary encoding.  We
      // need to figure out how wide it is, its endianness, whether or not
      // it is EBCDIC-based, and so on.

      // We do that by converting "0" to the target encoding, and we analyze
      // what we get back.
      
      size_t outlength = 0;
      char challenge[] = "0";
      char response_[8];

      iconv_t cd = iconv_open(
                          __gg__encoding_iconv_name(m_encoding),
                          __gg__encoding_iconv_name(DEFAULT_SOURCE_ENCODING));
      char *inbuf  = challenge;
      char *outbuf = response_;
      size_t inbytesleft = 1;
      size_t outbytesleft = sizeof(response_);
      /*size_t nret = */ iconv( cd,
                            &inbuf,  &inbytesleft,
                            &outbuf, &outbytesleft);
      outlength = sizeof(response_) - outbytesleft;
      iconv_close(cd);
      
      const unsigned char *response = 
                                  reinterpret_cast<unsigned char *>(response_);
      
      unsigned char char_0 = 0x00;

      m_is_valid = false;
      m_has_bom  = false;
      m_is_big_endian = false;
      m_is_like_utf8 = false;

      if( outlength == 1 )
        {
        m_stride = 1;
        // This is our happy place:  A single-byte encoded character set.
        char_0 = response[0];
        }
      else if( outlength == 2 )
        {
        m_stride = 2;
        if( response[0] )
          {
          char_0 = response[0];
          }
        else if( response[1] )
          {
          m_is_big_endian = true;
          char_0 = response[1];
          }
        }
      else if( outlength == 4 )
        {
        // Check for the Byte Order Mark (BOM)
        if( response[0] == 0xFF && response[1] == 0xFE )
          {
          m_stride = 2;
          m_has_bom = true;
          char_0 = response[2];
          }
        else if( response[0] == 0xFE && response[1] == 0xFF )
          {
          m_stride = 2;
          m_has_bom = true;
          m_is_big_endian = true;
          char_0 = response[3];
          }
        else if( response[0] )
          {
          m_stride = 4;
          char_0 = response[0];
          }
        else
          {
          m_stride = 4;
          m_is_big_endian = true;
          char_0 = response[3];
          }
        }
      else if( outlength == 8 )
        {
        m_stride = 4;
        if( response[0] == 0xFF && response[1] == 0xFE )
          {
          char_0 = response[4];
          }
        else if( response[0] == 0xFE && response[1] == 0xFF )
          {
          m_is_big_endian = true;
          char_0 = response[7];
          }
        }

      // With everything else established, we now check the zero character.
      // We know about only 0x30 for ASCII and 0xF0 for EBCDIC.
      if( char_0 == 0x30 )
        {
        m_is_valid = true;
        m_numeric_sign_type = sign_type_ascii;
        }
      else if( char_0 == 0xF0 )
        {
        m_is_valid = true;
        m_numeric_sign_type = sign_type_ebcdic;
        }

      // Let's see if this encoding is UTF-8.  We will do that by converting
      // the single-byte CP1252 code for the Euro symbol to our encoding.
      cd = iconv_open(
                    __gg__encoding_iconv_name(iconv_CP1252_e),
                    __gg__encoding_iconv_name(m_encoding));
      challenge[0] = static_cast<char>(0x80);// This is the CP1252 Euro symbol.
      inbuf  = challenge;
      outbuf = response_;
      inbytesleft = 1;
      outbytesleft = sizeof(response_);
      iconv(cd,
            &inbuf,  &inbytesleft,
            &outbuf, &outbytesleft);
      outlength = sizeof(response_) - outbytesleft;
      iconv_close(cd);
      m_is_like_utf8 = (outlength == 3);
      }

    bool is_valid()      const { return m_is_valid     ; }
    bool is_big_endian() const { return m_is_big_endian; }
    bool has_bom()       const { return m_has_bom      ; }
    uint8_t stride()     const { return m_stride       ; }

    cbl_char_t mapped_character(cbl_char_t ch) 
      {
      // The assumption is that anybody calling this routine is providing
      // a single-byte character in the DEFAULT_SOURCE_ENCODING encoding.  We
      // return the equivalent character in the m_encoding
      cbl_char_t retval;
      std::unordered_map<cbl_char_t, cbl_char_t>::const_iterator it =
                                                   m_map_of_encodings.find(ch);
      if( it != m_map_of_encodings.end() )
        {
        retval = it->second;
        }
      else
        {
        retval = 0;
        size_t outlength = 0;
        const char *mapped = __gg__iconverter(DEFAULT_SOURCE_ENCODING,
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
    cbl_char_t high_value_character()
      {
      cbl_char_t retval = 0;
      if( __gg__high_value_character == DEFAULT_HIGH_VALUE_8 )
        {
        switch(m_stride)
          {
          case 1:
            retval = DEFAULT_HIGH_VALUE_8;
            break;
          case 2:
            retval = DEFAULT_HIGH_VALUE_16;
            break;
          case 4:
            retval = DEFAULT_HIGH_VALUE_32 ;
            break;
          }
        }
      else
        {
        retval = mapped_character(__gg__high_value_character);
        }
      return retval;
      }

    cbl_char_t figconst_character(cbl_figconst_t figconst)
      {
      cbl_char_t const_char = 0;  // Head off a compiler warning
      switch(figconst)
        {
        case normal_value_e :
          abort();
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

  cbl_char_t
  set_digit_negative(cbl_char_t digit, bool is_negative)
    {
    // Returns a 0-9 digit with the internal sign bit altered for ascii or
    // ebcdic.
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

  bool
  is_like_utf8() const
    {
    return m_is_like_utf8;
    }

  void
  memset(void *dest_, cbl_char_t ch, size_t bytelength)
    {
    uint8_t *dest = static_cast<uint8_t *>(dest_);
    switch(m_stride)
      {
      case 1:
        {
        if( (ch & 0xFFFFFF00) == 0x00000000 )
          {
          // This is the normal case of filling a buffer with a single byte
          ::memset(dest, ch & 0xff, bytelength);
          }
        else
          {
          // We are being asked to fill a byte-wide buffer with a multi-byte
          // character.
          uint8_t byte3 = ch >> 24;
          uint8_t byte2 = ch >> 16;
          uint8_t byte1 = ch >>  8;
          uint8_t byte0 = ch;
          size_t fill;
          size_t i=0;
          if( byte3 )
            {
            fill = bytelength / 4;
            while( i<fill )
              {
              dest[i++] = byte0;
              dest[i++] = byte1;
              dest[i++] = byte2;
              dest[i++] = byte3;
              }
            }
          else if( byte2 )
            {
            fill = bytelength / 3;
            while( i<fill )
              {
              dest[i++] = byte0;
              dest[i++] = byte1;
              dest[i++] = byte2;
              }
            }
          else
            {
            fill = bytelength / 2;
            while( i<fill )
              {
              dest[i++] = byte0;
              dest[i++] = byte1;
              }
            }
          while( i < bytelength )
            {
            dest[i++] = mapped_character(ascii_space);
            }
          }
        break;
        }

      case 2:
        {
        assert( !(bytelength&1) );
        // We know the target has an even number of bytes available.  We also
        // know that each codepoint is usually one, but sometimes two, pairs
        // of bytes
        uint16_t top_half    = ch>>16;
        uint16_t bottom_half = ch;
        size_t fill = bytelength;
        size_t i = 0;
        uint16_t *p = PTRCAST(uint16_t, dest);
        while( i<fill )
          {
          p[i/2] = bottom_half;
          i += 2;
          if( i>= fill )
            {
            break;
            }
          if( top_half )
            {
            p[i/2] = bottom_half;
            i += 2;
            }
          }
        if( i < bytelength )
          {
          // We were trying to put two-pair values into the destination, but
          // there were an odd number of pairs available.
          p[i] = mapped_character(ascii_space);
          i += 2; // cppcheck-suppress unreadVariable
          }
        break;
        }

      case 4:
        {
        assert( !(bytelength&3) );
        // We know the target has multiple of four bytes available.
        uint32_t *p = PTRCAST(uint32_t, dest);
        size_t i = 0;
        while( i<bytelength )
          {
          p[i/4] = ch;
          i += 4;
          }
        break;
        }
      }
    }

  void putch(cbl_char_t ch, void *base_, size_t location)
    {
    // This routine puts a character at a byte location.  It's up to the
    // user to provide the correct byte location, and update it by the stride
    // when necessary.
    uint8_t *base = static_cast<uint8_t *>(base_);
    memcpy(base+location, &ch, m_stride);
    if( m_stride < 4 )
      {
      location += m_stride;
      ch >>= (8 * m_stride);
      while(ch)
        {
        memcpy(base+location, &ch, m_stride);
        location += m_stride;
        ch >>= (8 * m_stride);
        }
      }
    }

  void putch(cbl_char_t ch, void *base_, size_t *location)
    {
    // This routine puts a character at a location, and updates the location
    uint8_t *base = static_cast<uint8_t *>(base_);
    memcpy(base+*location, &ch, m_stride);
    *location += m_stride;
    if( m_stride < 4 )
      {
      ch >>= 8 * m_stride;
      while(ch)
        {
        memcpy(base+*location, &ch, m_stride);
        *location += m_stride;
        ch >>= 8 * m_stride;
        }
      }
    }

  cbl_char_t getch(const void *base_, size_t location) const
    {
    // This routine gets a character at a location, and updates the location
    cbl_char_t retval = 0;
    const uint8_t *base = static_cast<const uint8_t *>(base_);

    memcpy(&retval, base+location, m_stride);
////    location += m_stride;
////  We need to do something about UTF-8 snd UTF-16
////    while(ch)
////      {
////      memcpy(base+*location, &ch, m_stride);
////      *location += m_stride;
////      ch >>= 8 * m_stride;
////      }
    return retval;
    }

  cbl_char_t getch(const void *base_, size_t *location) const
    {
    // This routine gets a character at a location, and updates the location
    cbl_char_t retval = 0;
    const uint8_t *base = static_cast<const uint8_t *>(base_);

    memcpy(&retval, base+*location, m_stride);
    *location += m_stride;
////  We need to do something about UTF-8 snd UTF-16
////    while(ch)
////      {
////      memcpy(base+*location, &ch, m_stride);
////      *location += m_stride;
////      ch >>= 8 * m_stride;
////      }
    return retval;
    }

  unsigned long long strtoull(char *in, char **end, int /*base*/)
    {
    // This is like strtoull(3), but the base is restricted to 10.
    size_t index = 0;
    unsigned long long retval = 0;
    cbl_char_t mapped_0 = mapped_character(ascii_0);
    cbl_char_t mapped_9 = mapped_character(ascii_9);
    for(;;)
      {
      cbl_char_t ch = getch(in, &index);
      if( ch < mapped_0 || ch > mapped_9 )
        {
        break;
        }
      retval *= 10;
      retval += ch & 0x0F;
      }
    *end = in + index-m_stride ;
    return retval;
    }

    template <typename T>
    size_t
    Strlen( T *input, ssize_t limit = SSIZE_MAX ) {
      size_t i;
      for( i = 0; i < (limit / sizeof(T)) && input[i] != 0; i++ )
        ;
      return i;
    }
    size_t strlen2( const void *converted, ssize_t limit = SSIZE_MAX ) {
      switch(m_stride) {
      case 1:
        return Strlen( reinterpret_cast<const char*>(converted), limit );
      case 2:
        return Strlen( reinterpret_cast<const uint16_t*>(converted), limit );
      case 4:
        return Strlen( reinterpret_cast<const uint16_t*>(converted), limit );
      }
      //// gcc_unreachable();
      return -1; // Mollify cppcheck.
    }
    
  size_t
  strlen( const void *converted,
          ssize_t limit = SSIZE_MAX)
    {
    size_t retval;

    union
      {
      const uint8_t  *p8 ;
      const uint16_t *p16;
      const uint32_t *p32;
      } ;
    const uint8_t *p_start = reinterpret_cast<const uint8_t *>(converted);
    p8 = p_start;
    switch(m_stride)
      {
      case 1:
        {
        // Loop until the pointer is past the limit, or until we hit
        // a character that is all zeroes
        while(*p8)
          {
          if( p8 - p_start > limit )
            {
            break;
            }
          p8 += 1;
          }
        break;
        }
      case 2:
        {
        // Loop until the pointer is past the limit, or until we hit
        // a character that is all zeroes
        while(*p16)
          {
          if( p8 - p_start > limit )
            {
            break;
            }
          p8 += 2;
          }
        break;
        }
      case 4:
        {
        // Loop until the pointer is past the limit, or until we hit
        // a character that is all zeroes
        while(*p32)
          {
          if( p8 - p_start > limit )
            {
            break;
            }
          p8 += 4;
          }
        break;
        }
      }
    retval = p8 - p_start;
    return retval;
    }
  };

#endif
