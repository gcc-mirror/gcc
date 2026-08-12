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

#ifndef CHARMAPS_H
#define CHARMAPS_H

#include <cassert>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <map>
#include <string>
#include <type_traits>
#include <unordered_map>
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
 *  being manipulated by the generated code of the cobol executable.  The
 *  actual
 *  codeset of "internal" is either EBCDIC (in the form of Code Page 1140 or
 *  ASCII (Code Page 1252)
 *
 *  Third is the C++ source code of the GCOBOL compiler; this comment is
 *  in that environment.  We neither know, nor care, if this code is encoded in
 *  in UTF-8 (as is probable, in these enlighted days of 2022) or
 *  something like
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
 *  forcing it to be one or the other.  Codepoints in that domain are
 *  referenced
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

    This means that we are not going to try to compile EBCDIC COBOL
    source code;
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
#define ascii_lbrace           ((uint8_t)('{'))
#define ascii_rbrace           ((uint8_t)('}'))
#define ascii_at               ((uint8_t)('@'))
#define ascii_ff               ((uint8_t)('\f'))
#define ascii_return           ((uint8_t)('\r'))
#define ascii_newline          ((uint8_t)('\n'))
#define ebcdic_return          ((uint8_t)(0x0D))
#define ebcdic_zero            ((uint8_t)(0xF0))
#define ebcdic_plus            ((uint8_t)(0x4E))
#define ebcdic_minus           ((uint8_t)(0x60))
#define ebcdic_newline         ((uint8_t)(0x25))

extern unsigned char __gg__data_spaces[1]      ;
extern unsigned char __gg__data_low_values[1]  ;
extern unsigned char __gg__data_zeros[1]       ;
extern unsigned char __gg__data_high_values[1] ;
extern unsigned char __gg__data_quotes[1]      ;
extern unsigned char __gg__data_upsi_0[2]      ;

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

extern iconv_t helpful_iconv_open(const char *tocode, const char *fromcode);

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

#define HOST_32_ENCODING (cobol_target_big_endian() ? iconv_UTF_32BE_e : iconv_UTF_32LE_e)

#ifndef IN_TARGET_LIBS
void error_msg_direct( const char gmsgid[], ... );
#endif


static inline unsigned char *
charmap_as_unsigned_chars(char *p)
  {
  return reinterpret_cast<unsigned char *>(p);
  }

static inline const unsigned char *
charmap_as_unsigned_chars(const char *p)
  {
  return reinterpret_cast<const unsigned char *>(p);
  }

class charmap_t;

/*
 * cbl_iconv_t calls iconv_open(3) using either names or cbl_encoding_t pairs.
 * If used in the compiler, failure results in a compiler error message.  If
 * used in libgcobol, failure raises EC-IMP-ICONV-OPEN.
 *
 * The destructor closes all handles successfully opened.
 */
class cbl_iconv_t {
  struct iconv_key_t {
    cbl_encoding_t to, from;
    const char *tocode, *fromcode; // these are the names used by iconv_open(3)
    iconv_key_t() : to(no_encoding_e),
                    from(no_encoding_e),
                    tocode(NULL),
                    fromcode(NULL) {}
    iconv_key_t( cbl_encoding_t to, cbl_encoding_t from )
      : to(to), from(from)
      , tocode(__gg__encoding_iconv_name(to))
      , fromcode(__gg__encoding_iconv_name(from))

    {}
    iconv_key_t( const char *tocode, const char *fromcode )
      : to(__gg__encoding_iconv_type(tocode))
      , from(__gg__encoding_iconv_type(fromcode))
      , tocode(tocode)
      , fromcode(fromcode)
    {}
    bool operator<( const iconv_key_t& that ) const {
      if( from == that.from ) {
        return to < that.to;
      }
      return from < that.from;
    }
  };
  std::map<iconv_key_t, iconv_t> cds;
 protected:
  void close_all() {
    for( auto elem : cds ) {
      iconv_t cd = elem.second;
      if( valid(cd) ) {
        iconv_close(cd);
      }
    }
  }

  template <typename T> // T may be const char* or cbl_encoding_t
  iconv_t open_impl( T tocode, T fromcode ) {
    iconv_key_t key(tocode, fromcode);
    auto p = cds.find(key);
    if( p != cds.end() ) return p->second;

    iconv_t cd = helpful_iconv_open(key.tocode, key.fromcode);
    cds[key] = cd; // whether or not failed

    if( ! valid(cd) ) {
#ifdef IN_TARGET_LIBS
      exception_raise(ec_imp_iconv_open_e);
#else
      error_msg_direct( "%s: cannot convert to %qs from %qs",
                        "iconv_open", key.tocode, key.fromcode );
#endif
    }
    return cd;
  }
 public:
  ~cbl_iconv_t() { close_all(); }
  static bool valid( iconv_t cd ) { return cd != iconv_t(-1); }
  iconv_t open( const char *tocode, const char *fromcode ) {
    return open_impl(tocode, fromcode);
  }
  iconv_t open( cbl_encoding_t to, cbl_encoding_t from ) {
    return open_impl(to, from);
  }
};

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

    // In numeric display with sign internal, this bit gets turned on in either
    // the leading or trailing digit to indicate the value is negative.  It
    // is the single bit turned on for the `@` character.
    uint8_t m_ascii_sign_bit[4];

    // This map retains the ASCII-to-encoded value in m_encoding, so that
    // iconv need be called but once for each ASCII value.
    std::unordered_map<cbl_char_t, cbl_char_t> m_map_of_encodings;

    cbl_char_t
    get_encoded_char(const void *base_, size_t location) const
      {
      // The idea here is that we look into a stream of encoded characters.
      // Starting at base_+location, we pick up m_stride characters and put
      // them into the cbl_char_t (which is 32-bit unsigned integer) so that
      // retval is not dependent on endianness of either the host machine or
      // the target machine.

      const unsigned char *base = static_cast<const unsigned char *>(base_);
      const unsigned char *p = base + location;
      cbl_char_t retval;

      switch(m_stride)
        {
        case 1:
          {
          retval = p[0];
          break;
          }

        case 2:
          {
          if(m_is_big_endian)
            {
            // The first byte is the high-order byte
            retval = (p[0]<<8) + p[1];
            }
          else
            {
            // The first byte is the low-order byte
            retval = (p[1]<<8) + p[0];
            }
          break;
          }

        default:
          {
          if(m_is_big_endian)
            {
            // The first byte is the high-order byte
            retval = (p[0]<<24) + (p[1]<<16) + (p[2]<<8) + p[3];
            }
          else
            {
            // The first byte is the low-order byte
            retval = (p[3]<<24) + (p[2]<<16) + (p[1]<<8) + p[0];
            }
          break;
          }
        }

      return retval;
      }

    void
    put_encoded_char(cbl_char_t ch, void *base_, size_t location) const
      {
      // This is the reverse of get encoded character.  The value in ch is
      // placed in memory

      unsigned char *base = static_cast<unsigned char *>(base_);
      unsigned char *p = base + location;

      switch(m_stride)
        {
        case 1:
          p[0] = static_cast<unsigned char>(ch);
          break;

        case 2:
          {
          if(m_is_big_endian)
            {
            // The first byte is the high-order byte
            p[0] = ch>>8;
            p[1] = ch;
            }
          else
            {
            // The first byte is the low-order byte
            p[1] = ch>>8;
            p[0] = ch;
            }
          break;
          }

        default:
          if(m_is_big_endian)
            {
            // The first byte is the high-order byte
            p[0] = ch>>24;
            p[1] = ch>>16;
            p[2] = ch>>8;
            p[3] = ch;
            }
          else
            {
            // The first byte is the low-order byte
            p[3] = ch>>24;
            p[2] = ch>>16;
            p[1] = ch>>8;
            p[0] = ch;
            }
          break;
        }
      }

  public:
    explicit charmap_t(cbl_encoding_t e)
      : m_encoding(e)
      , m_is_valid(false)
      , m_is_big_endian(false)
      , m_has_bom (false)
      , m_is_like_utf8(false)
      , m_stride(1)
      {
      // We are constructing a new charmap_t from an arbitrary encoding.  We
      // need to figure out how wide it is, its endianness, whether or not
      // it is EBCDIC-based, and so on.

      // We do that by converting "0" to the target encoding, and we analyze
      // what we get back.

      size_t outlength = 0;
      char challenge[] = "0";
      char response_[8];
      cbl_iconv_t cbl_iconv;

      iconv_t cd = cbl_iconv.open(m_encoding, DEFAULT_SOURCE_ENCODING);
      if( ! cbl_iconv.valid(cd) ) {
        return;  // All hope abandon, ye who enter here.
      }
      char *inbuf  = challenge;
      char *outbuf = response_;
      size_t inbytesleft = 1;
      size_t outbytesleft = sizeof(response_);
      /*size_t nret = */ iconv( cd,
                            &inbuf,  &inbytesleft,
                            &outbuf, &outbytesleft);
      outlength = sizeof(response_) - outbytesleft;

      const unsigned char *response = charmap_as_unsigned_chars(response_);

      unsigned char char_0 = 0x00;

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
          m_has_bom = true;
          char_0 = response[4];
          }
        else if( response[0] == 0xFE && response[1] == 0xFF )
          {
          m_has_bom = true;
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
      cd = cbl_iconv.open(iconv_CP1252_e, m_encoding);
      if( ! cbl_iconv.valid(cd) ) {
        return;  // All hope abandon, ye who enter here.
      }
      challenge[0] = static_cast<char>(0x80);// This is the CP1252 Euro symbol.
      inbuf  = challenge;
      outbuf = response_;
      inbytesleft = 1;
      outbytesleft = sizeof(response_);
      iconv(cd,
            &inbuf,  &inbytesleft,
            &outbuf, &outbytesleft);
      outlength = sizeof(response_) - outbytesleft;
      m_is_like_utf8 = (outlength == 3);

      if( !is_like_ebcdic() )
        {
        memset(m_ascii_sign_bit, 0x00, 4);
        if( m_is_big_endian )
          {
          m_ascii_sign_bit[m_stride-1] = 0x40;
          }
        else
          {
          m_ascii_sign_bit[0] = 0x40;
          }
        }
      }

    bool is_valid()             const { return m_is_valid       ; }
    bool is_big_endian()        const { return m_is_big_endian  ; }
    bool has_bom()              const { return m_has_bom        ; }
    uint8_t stride()            const { return m_stride         ; }

    cbl_char_t
    mapped_character(unsigned char ch)
      {
      // The assumption is that anybody calling this routine is providing
      // a single-byte character in the DEFAULT_SOURCE_ENCODING encoding.  We
      // return the equivalent character in the m_encoding.
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
                                              &ch,
                                              1,
                                              &outlength);
        retval = get_encoded_char(mapped, 0);
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
      if( false && __gg__high_value_character == DEFAULT_HIGH_VALUE_8 )
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

    uint8_t figconst_character(cbl_figconst_t figconst)
      {
      uint8_t const_char = 0;  // Head off a compiler warning
      switch(figconst)
        {
        case normal_value_e :
          // Just leave it at zero
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
        retval = !!(digit & m_ascii_sign_bit[m_stride-1]);
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
      // We need to do this in a loop because of the headaches caused by
      // dealing with, for instance, little-endian characters on a big-endian
      // architecture.
      case sign_type_ascii:
        {
        if( is_negative )
          {
          digit |= m_ascii_sign_bit[m_stride-1];
          }
        else
          {
          digit &= ~m_ascii_sign_bit[m_stride-1];
          }
        break;
        }

      case sign_type_ebcdic:
        {
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
      }
    return digit;
    }

  void
  set_streamed_digit_negative(uint8_t *digit, bool is_negative)
    {
    // Enter with digit pointing to a digit that needs to be adjusted for
    // numeric-display internal signededness.

    // The loop might look odd, but it's how I decided to handle issues of
    // big-endian characters on little-endian architectures, and
    // little-endian characters on big-endian architectures, and so on.
    switch(m_numeric_sign_type)
      {
      case sign_type_ascii:
        {
        if( is_negative )
          {
          for(int i=0; i<m_stride; i++ )
            {
            digit[i] |= m_ascii_sign_bit[i];
            }
          }
        else
          {
          for(int i=0; i<m_stride; i++ )
            {
            digit[i] &= ~m_ascii_sign_bit[i];
            }
          }
        break;
        }

      case sign_type_ebcdic:
        {
        if( is_negative )
          {
          *digit &= ~NUMERIC_DISPLAY_SIGN_BIT_EBCDIC;
          }
        else
          {
          *digit |= NUMERIC_DISPLAY_SIGN_BIT_EBCDIC;
          }
        break;
        }
      }
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
    uint8_t byte3 = ch >> 24;
    uint8_t byte2 = ch >> 16;
    uint8_t byte1 = ch >>  8;
    uint8_t byte0 = ch      ;
    unsigned char *dest = static_cast<unsigned char *>(dest_);
    switch(m_stride)
      {
      case 1:
        {
        if( (ch & 0xFFFFFF00) == 0x00000000 )
          {
          // This is the normal case of filling a buffer with a single byte.
          std::memset(dest, ch & 0xff, bytelength);
          }
        else
          {
          // We are being asked to fill a byte-wide buffer with a multi-byte
          // character.
          size_t i = 0;
          if( byte3 )
            {
            while( i + 4 <= bytelength )
              {
              dest[i++] = byte0;
              dest[i++] = byte1;
              dest[i++] = byte2;
              dest[i++] = byte3;
              }
            }
          else if( byte2 )
            {
            while( i + 3 <= bytelength )
              {
              dest[i++] = byte0;
              dest[i++] = byte1;
              dest[i++] = byte2;
              }
            }
          else
            {
            while( i + 2 <= bytelength )
              {
              dest[i++] = byte0;
              dest[i++] = byte1;
              }
            }
          while( i < bytelength )
            {
            dest[i++] = static_cast<unsigned char>(
                                          mapped_character(ascii_space));
            }
          }
        break;
        }

      case 2:
        {
        assert( !(bytelength & 1) );
        // We know the target has an even number of bytes available.  We also
        // know that each codepoint is usually one, but sometimes two, pairs
        // of bytes.
        size_t i = 0;
        while( i < bytelength )
          {
          if( byte3 | byte2 )
            {
            if( i + 4 <= bytelength )
              {
              if( m_is_big_endian )
                {
                dest[i+0] = byte3;
                dest[i+1] = byte2;
                dest[i+2] = byte1;
                dest[i+3] = byte0;
                }
              else
                {
                dest[i+3] = byte3;
                dest[i+2] = byte2;
                dest[i+1] = byte1;
                dest[i+0] = byte0;
                }
              i += 4;
              }
            else
              {
              if( m_is_big_endian)
                {
                dest[i+1] = ascii_space;
                dest[i+0] = 0;
                }
              else
                {
                dest[i+1] = 0;
                dest[i+0] = ascii_space;
                }
              i += 2;
              }
            }
          else
            {
            if( m_is_big_endian )
              {
              dest[i+0] = byte1;
              dest[i+1] = byte0;
              }
            else
              {
              dest[i+1] = byte1;
              dest[i+0] = byte0;
              }
            i += 2;
            }
          }
        break;
        }

      case 4:
        {
        assert( !(bytelength & 3) );
        // We know the target has a multiple of four bytes available.
        for( size_t i = 0; i < bytelength; i += 4 )
          {
          if( m_is_big_endian )
            {
            dest[i+0] = byte3;
            dest[i+1] = byte2;
            dest[i+2] = byte1;
            dest[i+3] = byte0;
            }
          else
            {
            dest[i+3] = byte3;
            dest[i+2] = byte2;
            dest[i+1] = byte1;
            dest[i+0] = byte0;
            }
          }
        break;
        }
      }
    }

  void
  putch(cbl_char_t ch, void *base_, size_t location)
    {
    // This routine puts a character at a byte location.
    put_encoded_char(ch, base_, location);
    }

  void
  putch(cbl_char_t ch, void *base_, size_t *location)
    {
    // This routine puts a character at a location, and updates the location.
    this->putch(ch, base_, *location);
    *location += m_stride;
    }

  cbl_char_t
  getch(const void *base_, size_t location) const
    {
    // This routine gets the encoded character at a byte location.
    return get_encoded_char(base_, location);
    }

  cbl_char_t
  getch(const void *base_, size_t *location) const
    {
    // This routine gets a character at a location, and updates the location.
    cbl_char_t retval = this->getch(base_, *location);
    *location += m_stride;
    return retval;
    }

  cbl_char_t
  getch_native(const void *base_, size_t location) const
    {
    // This routine handles the situation where, for example, a character is
    // picked up, and the program needs to know if is in the range of
    // '0' through '9'.  So, if the charset is big-endian, but this is a
    // little-endian machine, then the value needs to be byte-flipped.
    cbl_char_t retval =  get_encoded_char(base_, location);

    // retval is the encoded value.
    bool target_big_endian = cobol_target_big_endian() ; // cppcheck-suppress knownConditionTrueFalse
    if(    ( m_is_big_endian && !target_big_endian)      // cppcheck-suppress knownConditionTrueFalse
        || (!m_is_big_endian &&  target_big_endian) )    // cppcheck-suppress knownConditionTrueFalse
      {
      // Flip the encoded value to match the machine's endianness
      if( m_stride == 4 )
        {
        retval = __builtin_bswap32(retval);
        }
      else
        {
        uint16_t v = retval;
        v = __builtin_bswap16(v);
        retval = v;
        }
      }
    return retval;
    }

  cbl_char_t
  getch_native(const void *base_, size_t *location) const
    {
    // This routine gets a character at a location, and updates the location.
    cbl_char_t retval = this->getch_native(base_, *location);
    *location += m_stride;
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
    *end = in + index - m_stride ;
    return retval;
    }

    size_t
    strlen2(const void *converted, ssize_t limit = SSIZE_MAX)
      {
      return strlen(converted, limit) / m_stride;
      }

  size_t
  strlen(const void *converted, ssize_t limit = SSIZE_MAX) const
    {
    const unsigned char *p_start =
      static_cast<const unsigned char *>(converted);
    size_t limit_bytes;

    if( limit < 0 )
      {
      limit_bytes = SIZE_MAX;
      }
    else
      {
      limit_bytes = static_cast<size_t>(limit);
      }

    size_t offset = 0;
    while( offset + m_stride <= limit_bytes )
      {
      if( getch(p_start, offset) == 0 )
        {
        break;
        }
      offset += m_stride;
      }

    return offset;
    }

  void
  get_byte_string(char *ach, char ch)
    {
    /* This routine takes a single ASCII character and converts it to the
       m_stride bytes of the m_encoding, and copies those bytes to the ach
       array, which had best be defined as ach[4] for generality.  The idea
       here is to create a byte stream that can be, for example, copied to
       a file without going crazy about the endianness of the target machine
       and the endianness of the encoding. */
    size_t nbytes;
    const char *converted = __gg__iconverter(DEFAULT_SOURCE_ENCODING,
                                             m_encoding,
                                             &ch,
                                             1,
                                             &nbytes);
    memcpy(ach, converted, nbytes);
    }
  };

char char_from_figconst(cbl_figconst_t figconst);

#endif
