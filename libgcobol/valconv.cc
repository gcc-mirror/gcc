// This file is included in both the libgcobol and gcc/cobol compilations
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

#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <algorithm>
#include <unordered_map>

#include "ec.h"
#include "common-defs.h"
#include "charmaps.h"

#include "valconv.h"
#include "exceptl.h"

int __gg__decimal_point        = '.'  ;
int __gg__decimal_separator    = ','  ;
int __gg__quote_character      = '"'  ;
int __gg__low_value_character  = 0x00 ;
int __gg__high_value_character = 0xFF ;
char **__gg__currency_signs           ;

int __gg__default_currency_sign;

char *__gg__ct_currency_signs[256];  // Compile-time currency signs

std::unordered_map<size_t, alphabet_state> __gg__alphabet_states;

extern "C"
void
__gg__realloc_if_necessary(char **dest, size_t *dest_size, size_t new_size)
  {
  if( new_size > *dest_size )
    {
    // Find the next power of two bigger than us:
    new_size |= new_size>>1;
    new_size |= new_size>>2;
    new_size |= new_size>>4;
    new_size |= new_size>>8;
    new_size |= new_size>>16;
    new_size |= new_size>>32;
    *dest_size = new_size + 1;
    *dest = (char *)realloc(*dest, *dest_size);
    }
  }

extern "C"
void
__gg__alphabet_create(  cbl_encoding_t encoding,
                        size_t alphabet_index,
                        unsigned char *alphabet,
                        int low_char,
                        int high_char )
  {
  assert( encoding == custom_encoding_e );

  std::unordered_map<size_t, alphabet_state>::const_iterator it =
    __gg__alphabet_states.find(alphabet_index);

  if( it == __gg__alphabet_states.end() )
    {
    alphabet_state new_state;
    new_state.low_char  = low_char;
    new_state.high_char = high_char;

    for(int i=0; i<256; i++)
      {
      if( alphabet[i] != 0xFF )
        {
        new_state.collation[i] = alphabet[i] ;
        }
      else
        {
        // Use a value bigger than HIGH, but which will sort according to the
        // original character-based order.
        new_state.collation[i] = 256 + i;
        }
      }
    __gg__alphabet_states[alphabet_index] = new_state;
    }

  return;
  }


static int
expand_picture(char *dest, const char *picture)
  {
  // 'dest' must be of adequate length to hold the expanded picture string,
  // including any extra characters due to a CURRENCY SIGN expansion.

  int ch;
  int prior_ch = NULLCH;
  char *d = dest;
  const char *p = picture;

  long repeat;

  int currency_symbol = NULLCH;

  while( (ch = (*p++ & 0xFF) ) )
    {
    if( ch == ascii_oparen )
      {
      // Pick up the number after the left parenthesis
      char *endchar;
      repeat = strtol(p, &endchar, 10);

      // We subtract one because we know that the character just before
      // the parenthesis was already placed in dest
      repeat -= 1;

      // Update p to the character after the right parenthesis
      p = endchar + 1;
      while(repeat--)
        {
        *d++ = prior_ch;
        }
      }
    else
      {
      prior_ch = ch;
      *d++ = ch;
      }

    if( __gg__currency_signs[ch] )
      {
      // We are going to be mapping ch to a string in the final result:
      prior_ch = ch;
      currency_symbol = ch;
      }
    }

  size_t dest_length = d-dest;

  // We have to take into account the possibility that the currency symbol
  // mapping might be to a string of more than one character:

  if( currency_symbol )
    {
    size_t sign_length = strlen(__gg__currency_signs[currency_symbol]) - 1;
    if( sign_length )
      {
      char *pcurrency = strchr(dest, currency_symbol);
      assert(pcurrency);
      memmove(    pcurrency + sign_length,
                  pcurrency,
                  dest_length - (pcurrency-dest));
      for(size_t i=0; i<sign_length; i++)
        {
        pcurrency[i] = ascii_B;
        }
      dest_length += sign_length;
      }
    }

  return (int)(dest_length);
  }

static int
Lindex(const char *dest, int length, char ch)
  {
  int retval = -1;
  for(int i=0; i<length; i++)
    {
    if( dest[i] == ch )
      {
      // Finds the leftmost
      retval = i;
      break;
      }
    }
  return retval;
  }

static int
Rindex(const char *dest, int length, char ch)
  {
  int retval = -1;
  for(int i=0; i<length; i++)
    {
    if( dest[i] == ch )
      {
      // Finds the rightmost
      retval = i;
      }
    }
  return retval;
  }

extern "C"
bool
__gg__string_to_numeric_edited( char * const dest,
                                char *source,       // In source characters
                                int rdigits,
                                int is_negative,
                                const char *picture)
  {
  // We need to expand the picture string.  We assume that the caller left
  // enough room in dest to take the expanded picture string.

  int dlength = expand_picture(dest, picture);

  // At the present time, I am taking a liberty. In principle, a 'V'
  // character is supposed to be logical decimal place rather than a physical
  // one.  In practice, I am not sure what that would mean in a numeric edited
  // value.  So, I am treating V as a decimal point.

  for(int i=0; i<dlength; i++)
    {
    if( dest[i] == ascii_v || dest[i] == ascii_V )
      {
      dest[i] = __gg__decimal_point;
      }
    }

  // This is the length of the source string, which is all digits, and has
  // an implied decimal point at the rdigits place.  We assume that any
  // source_period or ascii_V in the picture is in the right place
  const int slength = (int)strlen(source);

  // As a setting up exercise, let's deal with the possibility of a CR/DB:
  if( dlength >= 2 )
    {
    // It's a positive number, so we might have to get rid of a CR or DB:
    char ch1 = toupper((unsigned char)dest[dlength-2]);
    char ch2 = toupper((unsigned char)dest[dlength-1]);
    if(     (ch1 == ascii_D && ch2 == ascii_B)
            ||  (ch1 == ascii_C && ch2 == ascii_R) )
      {
      if( !is_negative )
        {
        // Per the spec, because the number is positive, those two
        // characters become blank:
        dest[dlength-2] = ascii_space;
        dest[dlength-1] = ascii_space;
        }
      // Trim the dlength by two to reflect those two positions at the
      // right edge, and from here on out we can ignore them.
      dlength -= 2;
      }
    }

  // We need to know if we have a currency picture symbol in this string:
  // This is the currency character in the PICTURE
  unsigned char currency_picture = NULLCH;
  const char *currency_sign = NULL;   // This is the text we output when
  //                                  // encountering the currency_picture
  //                                  // character
  // Note that the currency_picture can be upper- or lower-case, which is why
  // we can't treat dest[] to toupper.  That makes me sad, because I have
  // to do some tests for upper- and lower-case Z, and so on.
  for(int i=0; i<dlength; i++)
    {
    int ch = (unsigned int)dest[i] & 0xFF;
    if( __gg__currency_signs[ch] )
      {
      currency_picture = ch;
      currency_sign = __gg__currency_signs[ch];
      break;
      }
    }

  // Calculate the position of the decimal point:
  int decimal_point_index = slength - rdigits;

  // Find the source position of the leftmost non-zero digit in source
  int nonzero_index;
  for(nonzero_index=0; nonzero_index<slength; nonzero_index++)
    {
    if( source[nonzero_index] != ascii_zero )
      {
      break;
      }
    }

  bool is_zero = (nonzero_index == slength);

  // Push nonzero_index to the left to account for explicit ascii_nine
  // characters:
  int non_native_zeros = slength - nonzero_index;

  // Count up the number of nines
  int nines = 0;
  for(int i=0; i<dlength; i++)
    {
    if( dest[i] == ascii_nine )
      {
      nines += 1;
      }
    }
  if( nines > non_native_zeros )
    {
    non_native_zeros = nines;
    nonzero_index = slength - non_native_zeros;
    if( nonzero_index < 0 )
      {
      nonzero_index = 0;
      }
    }
  // nonzero_index is now the location of the leftmost digit that we will
  // output as a digit.  Everything to its left is a leading zero, and might
  // get replaced with a floating replacement.

  // We are now in a position to address specific situations:

  // This is the case of leading zero suppression
  if( (strchr(picture, ascii_Z)) || (strchr(picture, ascii_z)) )
    {
    int leftmost_indexA = Lindex(dest, dlength, ascii_Z);
    int leftmost_indexB = Lindex(dest, dlength, ascii_z);
    if( leftmost_indexA == -1 )
      {
      leftmost_indexA = leftmost_indexB;
      }
    if( leftmost_indexB == -1 )
      {
      leftmost_indexB = leftmost_indexA;
      }

    int rightmost_indexA = Lindex(dest, dlength, ascii_Z);
    int rightmost_indexB = Lindex(dest, dlength, ascii_z);
    if( rightmost_indexA == -1 )
      {
      rightmost_indexA = leftmost_indexB;
      }
    if( rightmost_indexB == -1 )
      {
      rightmost_indexB = leftmost_indexA;
      }

    int leftmost_index  = std::min(leftmost_indexA,  leftmost_indexB);
    int rightmost_index = std::max(rightmost_indexA, rightmost_indexB);

    // We are doing replacement editing: leading zeroes get replaced with
    // spaces.
    if( is_zero && nines == 0 )
      {
      // Corner case:  The value is zero, and all numeric positions
      // are suppressed.  The result is all spaces:
      memset(dest, ascii_space, dlength);
      }
    else
      {
      int index_s = slength-1;    // Index into source string of digits
      int index_d = dlength-1;    // Index into the destination
      bool reworked_string = false;

      while(index_d >=0)
        {
        // Pick up the destination character that we will replace:
        char ch_d = dest[index_d];

        if( ch_d == currency_picture )
          {
          // We are going to lay down the currency string.  Keep
          // in mind that our caller nicely left room for it
          size_t sign_len = strlen(currency_sign);
          while(sign_len > 0)
            {
            dest[index_d--] = currency_sign[--sign_len];
            }
          continue;
          }

        char ch_s;
        if( index_s < 0 )
          {
          // I don't think this can happen, but just in case:
          ch_s = ascii_zero;
          }
        else
          {
          ch_s = source[index_s];
          }

        if( index_s <= nonzero_index && !reworked_string)
          {
          reworked_string = true;
          // index_s is the location of the leftmost non-zero
          // digit.

          // So, we are about to enter the world of leading
          // zeroes.

          // The specification says, at this point, that
          // all B 0 / , and . inside the floating string
          // are to be considered part of the floating string:

          // So, we edit dest[] to make that true:
          int rlim = rightmost_index > index_d ? index_d : rightmost_index;

          for(int i=leftmost_index; i<rlim; i++)
            {
            if(     dest[i] == ascii_b
                    ||  dest[i] == ascii_B
                    ||  dest[i] == ascii_slash
                    ||  dest[i] == ascii_zero
                    ||  dest[i] == __gg__decimal_separator )
              {
              dest[i] = ascii_space;
              }
            }
          // Any B 0 / , immediately to the right are
          // also part of the floating_character string

          for(int i=rlim+1; i<index_d; i++)
            {
            if( !(      dest[i] == ascii_b
                        ||  dest[i] == ascii_B
                        ||  dest[i] == ascii_slash
                        ||  dest[i] == ascii_zero
                        ||  dest[i] == __gg__decimal_separator))
              {
              break;
              }
            dest[i] = ascii_space;
            }
          }


        if( index_s >= decimal_point_index )
          {
          // We are to the right of the decimal point, and so we
          // don't do any replacement.  We either insert a character,
          // or we replace with a digit:
          switch(ch_d)
            {
            // We are to the right of the decimal point, so Z is
            // a character position
            case ascii_z:
            case ascii_Z:
            case ascii_nine:
              index_s -= 1;
              break;
            case ascii_b:
            case ascii_B:
              ch_s = ascii_space;
              break;
            case ascii_plus:
              if( !is_negative )
                {
                ch_s = ascii_plus;
                }
              else
                {
                ch_s = ascii_minus;
                }
              break;
            case ascii_minus:
              if( !is_negative )
                {
                ch_s = ascii_space;
                }
              else
                {
                ch_s = ascii_minus;
                }
              break;
            case ascii_P:
            case ascii_p:
              // P-scaling has been handled by changing the value
              // and the number of rdigits, so these characters
              // are ignored here:
              break;
            default:
              // Valid possibilities are  0  /  ,
              // Just leave them be
              ch_s = ch_d;
              break;
            }
          dest[index_d] = ch_s;
          }
        else
          {
          // We are to the left of the decimal point:
          if( ch_d == __gg__decimal_point )
            {
            // Do this assignment to handle the situation where
            // period and comma have been swapped.  It's necessary
            // because the case statement can't take a variable
            ch_s = __gg__decimal_point;
            }
          else
            {
            switch(ch_d)
              {
              case ascii_nine:
                index_s -= 1;
                break;
              case ascii_v:
              case ascii_V:
                ch_s = __gg__decimal_point;
                break;
              case ascii_plus:
                if( !is_negative )
                  {
                  ch_s = ascii_plus;
                  }
                else
                  {
                  ch_s = ascii_minus;
                  }
                break;
              case ascii_minus:
                if( !is_negative )
                  {
                  ch_s = ascii_space;
                  }
                else
                  {
                  ch_s = ascii_minus;
                  }
                break;

              case ascii_z:
              case ascii_Z:
                if( index_s < nonzero_index)
                  {
                  // We are in the leading zeroes, so they are
                  // replaced with a space
                  ch_s = ascii_space;
                  }
                index_s -= 1;
                break;

              case ascii_b:
              case ascii_B:
                ch_s = ascii_space;
                break;

              default:
                // Valid possibilities are  0 / , which
                // at this point all get replaced with spaces:
                if( index_s < nonzero_index)
                  {
                  // We are in the leading zeroes, so they are
                  // replaced with a space
                  ch_s = ascii_space;
                  }
                else
                  {
                  // We still have digits to send out, so the output
                  // is a copy of the PICTURE string
                  ch_s = ch_d;
                  }
              }
            }
          dest[index_d] = ch_s;
          }

        index_d -= 1;
        }
      }
    }

  // This is the case of leading zero replacement:
  else if( strchr(picture, ascii_asterisk) )
    {
    int leftmost_index  = Lindex(dest, dlength, ascii_asterisk);
    int rightmost_index = Rindex(dest, dlength, ascii_asterisk);
    // We are doing replacement editing: leading zeroes get replaced with
    // asterisks, except that any decimal point is put into place:
    if( is_zero && nines == 0 )
      {
      // We need to re-initialize dest, because of the possibility
      // of a CR/DB at the end of the line
      dlength = expand_picture(dest, picture);

      for(int i=0; i<dlength; i++)
        {
        if(     dest[i] == ascii_v
                ||  dest[i] == ascii_V
                ||  dest[i] == __gg__decimal_point )
          {
          dest[i] = __gg__decimal_point;
          }
        else
          {
          dest[i] = ascii_asterisk;
          }
        }
      }
    else
      {
      int index_s = slength-1;    // Index into source string of digits
      int index_d = dlength-1;    // Index into the destination
      bool reworked_string = false;

      while(index_d >=0)
        {
        // Pick up the destination character that we will replace:
        char ch_d = dest[index_d];

        if( ch_d == currency_picture )
          {
          // We are going to lay down the currency string.  Keep
          // in mind that our caller nicely left room for it
          size_t sign_len = strlen(currency_sign);
          while(sign_len > 0)
            {
            dest[index_d--] = currency_sign[--sign_len];
            }
          continue;
          }

        char ch_s;
        if( index_s < 0 )
          {
          // I don't think this can happen, but just in case:
          ch_s = ascii_zero;
          }
        else
          {
          ch_s = source[index_s];
          }

        if( index_s <= nonzero_index && !reworked_string)
          {
          reworked_string = true;
          // index_s is the location of the leftmost non-zero
          // digit.

          // So, we are about to enter the world of leading
          // zeroes.

          // The specification says, at this point, that
          // all B 0 / , and . inside the floating string
          // are to be considered part of the floating string:

          // So, we edit dest[] to make that true:
          int rlim = rightmost_index > index_d ? index_d : rightmost_index;

          for(int i=leftmost_index; i<rlim; i++)
            {
            if(     dest[i] == ascii_b
                    ||  dest[i] == ascii_B
                    ||  dest[i] == ascii_slash
                    ||  dest[i] == ascii_zero
                    ||  dest[i] == __gg__decimal_separator )
              {
              dest[i] = ascii_asterisk;
              }
            }
          // Any B 0 / , immediately to the right are
          // also part of the floating_character string

          for(int i=rlim+1; i<index_d; i++)
            {
            if( !(      dest[i] == ascii_b
                        ||  dest[i] == ascii_B
                        ||  dest[i] == ascii_slash
                        ||  dest[i] == ascii_zero
                        ||  dest[i] == __gg__decimal_separator))
              {
              break;
              }
            dest[i] = ascii_asterisk;
            }
          }

        if( index_s >= decimal_point_index )
          {
          // We are to the right of the decimal point, and so we
          // don't do any replacement.  We either insert a character,
          // or we replace with a digit:
          switch(ch_d)
            {
            // We are to the right of the decimal point, so asterisk
            // is a a character position
            case ascii_asterisk:
            case ascii_nine:
              index_s -= 1;
              break;
            case ascii_b:
            case ascii_B:
              ch_s = ascii_space;
              break;
            case ascii_plus:
              if( !is_negative )
                {
                ch_s = ascii_plus;
                }
              else
                {
                ch_s = ascii_minus;
                }
              break;
            case ascii_minus:
              if( !is_negative )
                {
                ch_s = ascii_space;
                }
              else
                {
                ch_s = ascii_minus;
                }
              break;
            default:
              // Valid possibilities are  0  /  ,
              // Just leave them be
              ch_s = ch_d;
              break;
            }
          dest[index_d] = ch_s;
          }
        else
          {
          // We are to the left of the decimal point:
          if( ch_d == __gg__decimal_point )
            {
            ch_s = __gg__decimal_point;
            }
          else
            {
            switch(ch_d)
              {
              case ascii_nine:
                index_s -= 1;
                break;
              case ascii_v:
              case ascii_V:
                ch_s = __gg__decimal_point;
                break;
              case ascii_plus:
                if( !is_negative )
                  {
                  ch_s = ascii_plus;
                  }
                else
                  {
                  ch_s = ascii_minus;
                  }
                break;
              case ascii_minus:
                if( !is_negative )
                  {
                  ch_s = ascii_space;
                  }
                else
                  {
                  ch_s = ascii_minus;
                  }
                break;

              case ascii_asterisk:
                if( index_s < nonzero_index)
                  {
                  // We are in the leading zeroes, so they are
                  // replaced with an asterisk
                  ch_s = ascii_asterisk;
                  }
                index_s -= 1;
                break;

              case ascii_b:
              case ascii_B:
                ch_s = ascii_space;
                break;

              default:
                // Valid possibilities are  0 / , which
                // at this point all get replaced with spaces:
                if( index_s < nonzero_index)
                  {
                  // We are in the leading zeroes, so they are
                  // replaced with our suppression character
                  ch_s = ascii_asterisk;
                  }
                else
                  {
                  // We still have digits to send out, so the output
                  // is a copy of the PICTURE string
                  ch_s = ch_d;
                  }
              }
            }
          dest[index_d] = ch_s;
          }

        index_d -= 1;
        }
      }
    }
  else
    {
    // At this point, we check for a floating $$, ++, or --
    unsigned char floating_character = 0;

    int leftmost_index;
    int rightmost_index;

    leftmost_index  = Lindex(dest, dlength, ascii_plus);
    rightmost_index = Rindex(dest, dlength, ascii_plus);
    if( rightmost_index > leftmost_index)
      {
      floating_character = ascii_plus;
      goto got_float;
      }

    leftmost_index  = Lindex(dest, dlength, ascii_minus);
    rightmost_index = Rindex(dest, dlength, ascii_minus);
    if( rightmost_index > leftmost_index)
      {
      floating_character = ascii_minus;
      goto got_float;
      }

    leftmost_index  = Lindex(dest, dlength, currency_picture);
    rightmost_index = Rindex(dest, dlength, currency_picture);
    if( rightmost_index > leftmost_index)
      {
      floating_character = currency_picture;
      goto got_float;
      }
got_float:

    if( floating_character )
      {
      if( is_zero && nines == 0 )
        {
        // Special case:
        memset(dest, ascii_space, dlength);
        }
      else
        {
        const char *decimal_location = strchr(dest, __gg__decimal_point);
        if( !decimal_location )
          {
          decimal_location = strchr(dest, ascii_v);
          }
        if( !decimal_location )
          {
          decimal_location = strchr(dest, ascii_V);
          }
        if( !decimal_location )
          {
          decimal_location = dest + dlength;
          }
        int decimal_index = (int)(decimal_location - dest);

        if( rightmost_index > decimal_index )
          {
          rightmost_index = decimal_index -1;
          }

        int index_s = slength-1;    // Index into source string of digits
        int index_d = dlength-1;    // Index into the destination
        bool in_float_string = false;
        bool reworked_string = false;

        while(index_d >=0)
          {
          // Pick up the destination character that we will replace:
          unsigned char ch_d = dest[index_d];
          char ch_s = ascii_caret;   // Flag this as being replaced

          if( index_d == leftmost_index )
            {
            // At this point ch_d is the leftmost floating_character,
            // which means it *must* go into the output stream,
            // and that means we are truncating any remaining input.

            // Setting nonzero_index to be one character to the right
            // means that the following logic will think that any
            // source characters from here on out are zeroes
            nonzero_index = index_s+1;
            }

          if( ch_d != floating_character && ch_d == currency_picture )
            {
            // This is a non-floating currency_picture characger
            // We are going to lay down the currency string.  Keep
            // in mind that our caller nicely left room for it
            size_t sign_len = strlen(currency_sign);
            while(sign_len > 0)
              {
              dest[index_d--] = currency_sign[--sign_len];
              }
            continue;
            }
          if( ch_d != floating_character && ch_d == ascii_plus )
            {
            // This is a non-floating plus sign
            if( !is_negative )
              {
              ch_s = ascii_plus;
              }
            else
              {
              ch_s = ascii_minus;
              }
            dest[index_d--] = ch_s;
            continue;
            }
          if( ch_d != floating_character && ch_d == ascii_minus )
            {
            // This is a non-floating minus sign
            if( !is_negative )
              {
              ch_s = ascii_space;
              }
            else
              {
              ch_s = ascii_minus;
              }
            dest[index_d--] = ch_s;
            continue;
            }

          if( index_s < 0 )
            {
            // I don't think this can happen, but just in case:
            ch_s = ascii_zero;
            }
          else
            {
            ch_s = source[index_s];
            if( index_s <= nonzero_index && !reworked_string)
              {
              reworked_string = true;
              // index_s is the location of the leftmost non-zero
              // digit.

              // So, we are about to enter the world of leading
              // zeroes.

              // The specification says, at this point, that
              // all B 0 / , and . inside the floating string
              // are to be considered part of the floating string:

              // So, we edit dest[] to make that true:
              int rlim = rightmost_index > index_d ? index_d : rightmost_index;

              for(int i=leftmost_index; i<rlim; i++)
                {
                if(     dest[i] == ascii_b
                        ||  dest[i] == ascii_B
                        ||  dest[i] == ascii_slash
                        ||  dest[i] == ascii_zero
                        ||  dest[i] == __gg__decimal_separator
                        ||  dest[i] == __gg__decimal_point )
                  {
                  dest[i] = floating_character;
                  }
                }
              // Any B 0 / , immediately to the right are
              // also part of the floating_character string

              for(int i=rlim+1; i<index_d; i++)
                {
                if( !(      dest[i] == ascii_b
                            ||  dest[i] == ascii_B
                            ||  dest[i] == ascii_slash
                            ||  dest[i] == ascii_zero
                            ||  dest[i] == __gg__decimal_separator))
                  {
                  break;
                  }
                dest[i] = floating_character;
                }
              }
            }
          if( index_s >= decimal_point_index )
            {
            // We are to the right of the decimal point, and so we
            // don't do any replacement.  We either insert a character,
            // or we replace with a digit:
            switch(ch_d)
              {
              case ascii_nine:
                index_s -= 1;
                break;
              case ascii_b:
              case ascii_B:
                ch_s = ascii_space;
                break;
              default:
                if( ch_d == floating_character )
                  {
                  // We are laying down a digit
                  index_s -= 1;
                  }
                else
                  {
                  // Valid possibilities are  0  /  ,
                  // Just leave them be
                  ch_s = ch_d;
                  }
                break;
              }
            dest[index_d] = ch_s;
            }
          else
            {
            // We are to the left of the decimal point:

            if( ch_d == __gg__decimal_point )
              {
              ch_s = __gg__decimal_point;
              }
            else if (ch_d == floating_character)
              {
              if( index_s < nonzero_index )
                {
                // We are in the leading zeroes.
                if( !in_float_string )
                  {
                  in_float_string = true;
                  // We have arrived at the rightmost floating
                  // character in the leading zeroes

                  if( floating_character == currency_picture )
                    {
                    size_t sign_len = strlen(currency_sign);
                    while(sign_len > 0)
                      {
                      dest[index_d--] = currency_sign[--sign_len];
                      }
                    continue;
                    }
                  if( floating_character  == ascii_plus )
                    {
                    if( !is_negative )
                      {
                      ch_s = ascii_plus;
                      }
                    else
                      {
                      ch_s = ascii_minus;
                      }
                    dest[index_d--] = ch_s;
                    continue;
                    }
                  if( floating_character == ascii_minus )
                    {
                    if( !is_negative )
                      {
                      ch_s = ascii_space;
                      }
                    else
                      {
                      ch_s = ascii_minus;
                      }
                    dest[index_d--] = ch_s;
                    continue;
                    }
                  }
                else
                  {
                  // We are in the leading zeros and the
                  // floating character location is to our
                  // right.  So, we put down a space:
                  dest[index_d--] = ascii_space;
                  continue;
                  }
                }
              else
                {
                // We hit a floating character, but we aren't
                // yet in the leading zeroes
                index_s -= 1;
                dest[index_d--] = ch_s;
                continue;
                }
              }

            if( ch_d == __gg__decimal_point)
              {
              ch_s = __gg__decimal_point;
              }
            else
              {
              switch(ch_d)
                {
                case ascii_nine:
                  index_s -= 1;
                  break;
                case ascii_v:
                case ascii_V:
                  ch_s = __gg__decimal_point;
                  break;
                case ascii_b:
                case ascii_B:
                  ch_s = ascii_space;
                  break;

                default:
                  // Valid possibilities are  0 / , which
                  // at this point all get replaced with spaces:
                  if( index_s < nonzero_index)
                    {
                    // We are in the leading zeroes, so they are
                    // replaced with our suppression character
                    ch_s = ascii_space;
                    }
                  else
                    {
                    // We still have digits to send out, so the output
                    // is a copy of the PICTURE string
                    ch_s = ch_d;
                    }
                }
              }
            dest[index_d] = ch_s;
            }

          index_d -= 1;
          }
        }
      }
    else
      {
      // Simple replacement editing
      int index_s = slength-1;    // Index into source string of digits
      int index_d = dlength-1;    // Index into the destination

      while(index_d >=0)
        {
        // Pick up the destination character that we will replace:
        char ch_d = dest[index_d];

        if( ch_d == currency_picture )
          {
          // We are going to lay down the currency string.  Keep
          // in mind that our caller nicely left room for it
          size_t sign_len = strlen(currency_sign);
          while(sign_len > 0)
            {
            dest[index_d--] = currency_sign[--sign_len];
            }
          continue;
          }

        char ch_s;
        if( index_s < 0 )
          {
          // I don't think this can happen, but just in case:
          ch_s = ascii_zero;
          }
        else
          {
          ch_s = source[index_s];
          }
        switch(ch_d)
          {
          // We are to the right of the decimal point, so Z is
          // a character position
          case ascii_nine:
            index_s -= 1;
            break;
          case ascii_b:
          case ascii_B:
            ch_s = ascii_space;
            break;
          case ascii_plus:
            if( !is_negative )
              {
              ch_s = ascii_plus;
              }
            else
              {
              ch_s = ascii_minus;
              }
            break;
          case ascii_minus:
            if( !is_negative )
              {
              ch_s = ascii_space;
              }
            else
              {
              ch_s = ascii_minus;
              }
            break;
          default:
            // Valid possibilities are  0  /  ,
            // Just leave whatever is here alone
            ch_s = ch_d;
            break;
          }
        dest[index_d--] = ch_s;
        }
      }
    }
  bool retval = false;

  return retval;
  }

extern "C"
void
__gg__string_to_alpha_edited(   char *dest,
                                char *source,
                                int slength,
                                char *picture)
  {
  // Put the PICTURE into the data area.  If the caller didn't leave enough
  // room, well, poo on them.  Said another way; if they specify disaster,
  // disaster is what they will get.

  // This routine expands picture into dest using ascii characters, but
  // replaces them with internal characters

  int destlength = expand_picture(dest, picture);

  int dindex = 0;
  int sindex = 0;

  while( dindex < destlength )
    {
    char dch = dest[dindex];
    char sch;
    switch(dch)
      {
      case ascii_b:   // Replaced with space
      case ascii_B:
        dest[dindex] = internal_space;
        break;

      case ascii_zero:   // These are left alone:
        dest[dindex] = ascii_to_internal(ascii_zero);
        break;

      case ascii_slash:
        dest[dindex] = ascii_to_internal(ascii_slash);
        break;

      default:
        // We assume that the parser isn't giving us a bad PICTURE
        // string, which means this character should be X, A, or 9
        // We don't check; we just replace it:
        if(sindex < slength)
          {
          sch = source[sindex++];
          }
        else
          {
          sch = internal_space;
          }
        dest[dindex] = sch;
      }
    dindex += 1;
    }
  }

extern "C"
void
__gg__currency_sign_init()
  {
  for(int symbol=0; symbol<256; symbol++)
    {
    if( __gg__currency_signs[symbol] )
      {
      free(__gg__currency_signs[symbol]);
      __gg__currency_signs[symbol] = NULL;
      }
    }
  }

extern "C"
void
__gg__currency_sign(int symbol, const char *sign)
  {
  __gg__currency_signs[symbol] = strdup(sign);
  __gg__default_currency_sign = *sign;
  }

extern "C"
void
__gg__remove_trailing_zeroes(char *p)
  {
  // *p is a floating point number created by strfromN.  There will be no
  // leading spaces nor unnecessary leading zeroes, but there could be trailing
  // zeroes, e.g.  123.456000 or 1.23456000E2
  // Remove the trailing zeroes:
  if( *p == '-' )
    {
    p += 1;
    }
  char *left = p;
  char *right;
  char *pE = strchr(p, 'E');
  if( pE )
    {
    right = pE - 1;
    }
  else
    {
    right = p + strlen(p)-1;
    }

  if( strchr(left, '.') )
    {
    while(*right == '0' || *right == internal_space)
      {
      right -= 1;
      }
    if( *right == '.' )
      {
      right -= 1;
      }
    }

  right += 1;
  memmove(p, left, right-left);
  if( pE )
    {
    memmove(p + (right-left), pE, strlen(pE)+1);
    }
  else
    {
    p[right-left] = '\0';
    }
  }

// This is a convenient place to put this table, since it is used in both the
// run-time and compile-time code, and this source code module is one of the
// ones that are copied from ./libgcobol to gcc/cobol as part of the build.

ec_descr_t __gg__exception_table[] = {
  { ec_all_e,                    ec_category_none_e,
   "EC-ALL", "Any exception" },

  { ec_argument_e,               ec_category_none_e,
   "EC-ARGUMENT", "Argument error" },
  { ec_argument_function_e,      ec_category_fatal_e,
   "EC-ARGUMENT-FUNCTION", "Function argument error" },
  { ec_argument_imp_e,           uc_category_implementor_e,
   "EC-ARGUMENT-IMP", "Implementor-defined argument error" },

  { ec_argument_imp_command_e,   uc_category_implementor_e,
   "EC-ARGUMENT-IMP-COMMAND", "COMMAND-LINE Subscript out of bounds" },
  { ec_argument_imp_environment_e, uc_category_implementor_e,
   "EC-ARGUMENT-IMP-ENVIRONMENT", "Envrionment Variable is not defined" },

  { ec_bound_e,                  ec_category_none_e,
   "EC-BOUND", "Boundary violation" },
  { ec_bound_func_ret_value_e,   uc_category_nonfatal_e,
  "EC-BOUND-FUNC-RET-VALUE",
  "Intrinsic function output does not fit in returned value item" },
  { ec_bound_imp_e,              uc_category_implementor_e,
   "EC-BOUND-IMP", "Implementor-defined boundary violation" },
  { ec_bound_odo_e,              ec_category_fatal_e,
   "EC-BOUND-ODO", "OCCURS ... DEPENDING ON data item out of bounds" },
  { ec_bound_overflow_e,         uc_category_nonfatal_e,
   "EC-BOUND-OVERFLOW",
    "Current capacity of dynamic-capacity table greater than expected value" },
  { ec_bound_ptr_e,              uc_category_fatal_e,
   "EC-BOUND-PTR", "Data-pointer contains an address that is out of bounds" },
  { ec_bound_ref_mod_e,          ec_category_fatal_e,
   "EC-BOUND-REF-MOD", "Reference modifier out of bounds" },
  { ec_bound_set_e,              uc_category_nonfatal_e,
   "EC-BOUND-SET", "Invalid use of SET to set capacity of "
                      "dynamic-capacity table above specified maximum" },
  { ec_bound_subscript_e,        ec_category_fatal_e,
   "EC-BOUND-SUBSCRIPT", "Subscript out of bounds" },
  { ec_bound_table_limit_e,      uc_category_fatal_e,
   "EC-BOUND-TABLE-LIMIT",
    "Capacity of dynamic-capacity table would exceed implementor's maximum" },

  { ec_data_e,                   ec_category_none_e,
   "EC-DATA", "Data exception" },
  { ec_data_conversion_e,        uc_category_nonfatal_e,
   "EC-DATA-CONVERSION",
    "Conversion failed because of incomplete character correspondence" },
  { ec_data_imp_e,               uc_category_implementor_e,
   "EC-DATA-IMP", "Implementor-defined data exception" },
  { ec_data_incompatible_e,      uc_category_fatal_e,
   "EC-DATA-INCOMPATIBLE", "Incompatible data exception" },
  { ec_data_not_finite_e,        uc_category_fatal_e,
   "EC-DATA-NOT-FINITE",
    "Attempt to use a data item described with a standard floating-point usage "
    "when its contents are either a NaN or a representation of infinity" },
  { ec_data_overflow_e,          uc_category_fatal_e,
   "EC-DATA-OVERFLOW",
    "Exponent overflow during MOVE to a receiving data item described with a "
    "standard floating-point usage" },
  { ec_data_ptr_null_e,          uc_category_fatal_e,
   "EC-DATA-PTR-NULL",
    "Based item data-pointer is set to NULL when referenced" },

  { ec_external_data_mismatch_e,   uc_category_fatal_e,
   "EC-EXTERNAL-DATA-MISMATCH",
   "File referencing control item conflict because the linage, "
   "file status or relative key references are not to the same item " },
   { ec_external_file_mismatch_e,  uc_category_fatal_e,
     "EC-EXTERNAL-FILE-MISMATCH",
     "File control SELECT statements are not compatible" },
   { ec_external_format_conflict_e,  uc_category_fatal_e,
     "EC-EXTERNAL-FORMAT-CONFLICT",
     "Data definitions definitions do not conform" },

  { ec_flow_e,                   ec_category_none_e,
   "EC-FLOW", "Execution control flow violation" },
  { ec_flow_global_exit_e,       uc_category_fatal_e,
   "EC-FLOW-GLOBAL-EXIT", "EXIT PROGRAM in a global Declarative" },
  { ec_flow_global_goback_e,     uc_category_fatal_e,
   "EC-FLOW-GLOBAL-GOBACK", "GOBACK in a global declarative" },
  { ec_flow_imp_e,               uc_category_implementor_e,
   "EC-FLOW-IMP", "Implementor-defined control flow violation" },
  { ec_flow_release_e,           uc_category_fatal_e,
   "EC-FLOW-RELEASE", "RELEASE not in range of SORT" },
  { ec_flow_report_e,            uc_category_fatal_e,
   "EC-FLOW-REPORT",
    "GENERATE, INITIATE, or TERMINATE during USE BEFORE REPORTING declarative" },
  { ec_flow_return_e,            uc_category_fatal_e,
   "EC-FLOW-RETURN", "RETURN not in range of MERGE or SORT" },
  { ec_flow_search_e,            uc_category_fatal_e,
   "EC-FLOW-SEARCH",
    "Invalid use of SET to change capacity of dynamic- capacity table during "
    "SEARCH of same table" },
  { ec_flow_use_e,               uc_category_fatal_e,
   "EC-FLOW-USE", "A USE statement caused another to be executed" },

  { ec_function_e,               ec_category_none_e,
   "EC-FUNCTION", "Function exception" },
  { ec_function_not_found_e,     uc_category_fatal_e,
   "EC-FUNCTION-NOT-FOUND",
    "Function not found or function pointer does not point to a function" },
  { ec_function_ptr_invalid_e,   uc_category_fatal_e,
   "EC-FUNCTION-PTR-INVALID", "Signature mismatch" },
  { ec_function_ptr_null_e,      uc_category_fatal_e,
   "EC-FUNCTION-PTR-NULL",
    "Function pointer used in calling a function is NULL" },

  { ec_io_e,                     ec_category_none_e,
   "EC-IO", "Input-output exception" },
  { ec_io_at_end_e,              uc_category_nonfatal_e,
   "EC-I-O-AT-END", "I-O status 1x" },
  { ec_io_eop_e,                 uc_category_nonfatal_e,
   "EC-I-O-EOP", "An end of page condition occurred" },
  { ec_io_eop_overflow_e,        uc_category_nonfatal_e,
   "EC-I-O-EOP-OVERFLOW", "A page overflow condition occurred" },
  { ec_io_file_sharing_e,        uc_category_nonfatal_e,
   "EC-I-O-FILE-SHARING", "I-O status 6x" },
  { ec_io_imp_e,                 uc_category_implementor_e,
   "EC-I-O-IMP", "I-O status 9x" },
  { ec_io_invalid_key_e,         uc_category_nonfatal_e,
   "EC-I-O-INVALID-KEY", "I-O status 2x" },
  { ec_io_linage_e,              uc_category_fatal_e,
   "EC-I-O-LINAGE",
    "The value of a data item referenced in the LINAGE clause is not within "
    "the required range" },
  { ec_io_logic_error_e,         uc_category_fatal_e,
   "EC-I-O-LOGIC-ERROR", "I-O status 4x" },
  { ec_io_permanent_error_e,     uc_category_fatal_e,
   "EC-I-O-PERMANENT-ERROR", "I-O status 3x" },
  { ec_io_record_operation_e,    uc_category_nonfatal_e,
   "EC-I-O-RECORD-OPERATION", "I-O status 5x" },

  { ec_imp_e,                    ec_category_none_e,
   "EC-IMP", "Implementor-defined exception condition" },

  { ec_imp_suffix_e,             ec_category_none_e,
   "EC-IMP-SUFFIX", "Imp" },

  { ec_locale_e,                 ec_category_none_e,
   "EC-LOCALE", "Any locale related exception" },
  { ec_locale_imp_e,             uc_category_implementor_e,
   "EC-LOCALE-IMP", "Implementor-defined locale related exception" },
  { ec_locale_incompatible_e,    uc_category_fatal_e,
   "EC-LOCALE-INCOMPATIBLE",
    "The referenced locale does not specify the expected characters in "
    "LC_COLLATE" },
  { ec_locale_invalid_e,         uc_category_fatal_e,
   "EC-LOCALE-INVALID", "Locale content is invalid or incomplete" },
  { ec_locale_invalid_ptr_e,     uc_category_fatal_e,
   "EC-LOCALE-INVALID-PTR", "Pointer does not reference a saved locale" },
  { ec_locale_missing_e,         uc_category_fatal_e,
   "EC-LOCALE-MISSING", "The specified locale is not available" },
  { ec_locale_size_e,            uc_category_fatal_e,
   "EC-LOCALE-SIZE", "Digits were truncated in locale editing" },

  { ec_oo_e,                     ec_category_none_e,
   "EC-OO", "Any predefined OO related exception" },
  { ec_oo_arg_omitted_e,         uc_category_fatal_e,
   "EC-OO-ARG-OMITTED", "Reference to an omitted argument" },
  { ec_oo_conformance_e,         uc_category_fatal_e,
   "EC-OO-CONFORMANCE", "Failure for an object-view" },
  { ec_oo_exception_e,           uc_category_fatal_e,
   "EC-OO-EXCEPTION", "An exception object was not handled" },
  { ec_oo_imp_e,                 uc_category_implementor_e,
   "EC-OO-IMP", "Implementor-defined OO exception" },
  { ec_oo_method_e,              uc_category_fatal_e,
   "EC-OO-METHOD", "Requested method is not available" },
  { ec_oo_null_e,                uc_category_fatal_e,
   "EC-OO-NULL",
    "Method invocation was attempted with a null object reference" },
  { ec_oo_resource_e,            uc_category_fatal_e,
   "EC-OO-RESOURCE", "Insufficient system resources to create the object" },
  { ec_oo_universal_e,           uc_category_fatal_e,
   "EC-OO-UNIVERSAL", "A runtime type check failed" },

  { ec_order_e,                  ec_category_none_e,
   "EC-ORDER", "Ordering exception" },
  { ec_order_imp_e,              uc_category_implementor_e,
   "EC-ORDER-IMP", "Implementor-defined ordering exception" },
  { ec_order_not_supported_e,    uc_category_fatal_e,
   "EC-ORDER-NOT-SUPPORTED",
    "Cultural ordering table or ordering level specified for "
    "STANDARD-COMPARE function not supported" },

  { ec_overflow_e,               ec_category_none_e,
   "EC-OVERFLOW", "Overflow condition" },
  { ec_overflow_imp_e,           uc_category_implementor_e,
   "EC-OVERFLOW-IMP", "Implementor-defined overflow condition" },
  { ec_overflow_string_e,        uc_category_nonfatal_e,
   "EC-OVERFLOW-STRING", "STRING overflow condition" },
  { ec_overflow_unstring_e,      uc_category_nonfatal_e,
   "EC-OVERFLOW-UNSTRING", "UNSTRING overflow condition" },

  { ec_program_e,                ec_category_none_e,
   "EC-PROGRAM", "Inter-program communication exception" },
  { ec_program_arg_mismatch_e,   uc_category_fatal_e,
   "EC-PROGRAM-ARG-MISMATCH", "Parameter mismatch" },
  { ec_program_arg_omitted_e,    uc_category_fatal_e,
   "EC-PROGRAM-ARG-OMITTED", "A reference to an omitted argument" },
  { ec_program_cancel_active_e,  uc_category_fatal_e,
   "EC-PROGRAM-CANCEL-ACTIVE", "Canceled program active" },
  { ec_program_imp_e,            uc_category_implementor_e,
   "EC-PROGRAM-IMP",
    "Implementor-defined inter-program communication exception" },
  { ec_program_not_found_e,      uc_category_fatal_e,
   "EC-PROGRAM-NOT-FOUND", "Called program not found" },
  { ec_program_ptr_null_e,       uc_category_fatal_e,
   "EC-PROGRAM-PTR-NULL", "Program-pointer used in CALL is set to NULL" },
  { ec_program_recursive_call_e, uc_category_fatal_e,
   "EC-PROGRAM-RECURSIVE-CALL", "Called program active" },
  { ec_program_resources_e,      uc_category_fatal_e,
   "EC-PROGRAM-RESOURCES", "Resources not available for called program" },

  { ec_raising_e,                ec_category_none_e,
   "EC-RAISING", "EXIT ... RAISING or GOBACK RAISING exception" },
  { ec_raising_imp_e,            uc_category_implementor_e,
   "EC-RAISING-IMP",
    "Implementor-defined EXIT ... RAISING or GOBACK RAISING exception" },
  { ec_raising_not_specified_e,  uc_category_fatal_e,
   "EC-RAISING-NOT-SPECIFIED",
    "EXIT ... RAISING or GOBACK RAISING an EC-USER exception condition not "
    "specified in RAISING phrase of procedure division header" },

  { ec_range_e,                  ec_category_none_e,
   "EC-RANGE", "Range exception" },
  { ec_range_imp_e,              uc_category_implementor_e,
   "EC-RANGE-IMP", "Implementor-defined range exception" },
  { ec_range_index_e,            uc_category_fatal_e,
   "EC-RANGE-INDEX",
    "Index set outside the range of values allowed by the implementor" },
  { ec_range_inspect_size_e,     uc_category_fatal_e,
   "EC-RANGE-INSPECT-SIZE", "Size of replace items in INSPECT differs" },
  { ec_range_invalid_e,          uc_category_nonfatal_e,
   "EC-RANGE-INVALID",
    "Starting value of THROUGH range greater than ending value" },
  { ec_range_perform_varying_e,  uc_category_fatal_e,
   "EC-RANGE-PERFORM-VARYING",
    "Setting of varied item in PERFORM is negative" },
  { ec_range_ptr_e,              uc_category_fatal_e,
   "EC-RANGE-PTR", "Pointer SET UP or DOWN is outside range" },
  { ec_range_search_index_e,     uc_category_nonfatal_e,
   "EC-RANGE-SEARCH-INDEX",
    "No table element found in SEARCH because initial index out of range" },
  { ec_range_search_no_match_e,  uc_category_nonfatal_e,
   "EC-RANGE-SEARCH-NO-MATCH",
    "No table element found in SEARCH because no element matched criteria" },

  { ec_report_e,                 ec_category_none_e,
   "EC-REPORT", "Report writer exception" },
  { ec_report_active_e,          uc_category_fatal_e,
   "EC-REPORT-ACTIVE", "INITIATE on an active report" },
  { ec_report_column_overlap_e,  uc_category_nonfatal_e,
   "EC-REPORT-COLUMN-OVERLAP", "Overlapping report items" },
  { ec_report_file_mode_e,       uc_category_fatal_e,
   "EC-REPORT-FILE-MODE",
    "An INITIATE statement was executed for a file connector that was not "
    "open in the extend or output mode" },
  { ec_report_imp_e,             uc_category_implementor_e,
   "EC-REPORT-IMP", "Implementor-defined report writer exception" },
  { ec_report_inactive_e,        uc_category_fatal_e,
   "EC-REPORT-INACTIVE", "GENERATE or TERMINATE on an inactive report" },
  { ec_report_line_overlap_e,    uc_category_nonfatal_e,
   "EC-REPORT-LINE-OVERLAP", "Overlapping report lines" },
  { ec_report_not_terminated_e,  uc_category_nonfatal_e,
   "EC-REPORT-NOT-TERMINATED", "Report file closed with active report" },
  { ec_report_page_limit_e,      uc_category_nonfatal_e,
   "EC-REPORT-PAGE-LIMIT", "Vertical page limit exceeded" },
  { ec_report_page_width_e,      uc_category_nonfatal_e,
   "EC-REPORT-PAGE-WIDTH", "Page width exceeded" },
  { ec_report_sum_size_e,        uc_category_fatal_e,
   "EC-REPORT-SUM-SIZE", "Overflow of sum counter" },
  { ec_report_varying_e,         uc_category_fatal_e,
   "EC-REPORT-VARYING", "VARYING clause expression noninteger" },

  { ec_screen_e,                 ec_category_none_e,
   "EC-SCREEN", "Screen handling exception" },
  { ec_screen_field_overlap_e,   uc_category_nonfatal_e,
   "EC-SCREEN-FIELD-OVERLAP", "Screen fields overlap" },
  { ec_screen_imp_e,             uc_category_implementor_e,
   "EC-SCREEN-IMP", "Implementor-defined screen handling exception" },
  { ec_screen_item_truncated_e,  uc_category_nonfatal_e,
   "EC-SCREEN-ITEM-TRUNCATED", "Screen field too long for line" },
  { ec_screen_line_number_e,     uc_category_nonfatal_e,
   "EC-SCREEN-LINE-NUMBER",
    "Screen item line number exceeds terminal size" },
  { ec_screen_starting_column_e, uc_category_nonfatal_e,
   "EC-SCREEN-STARTING-COLUMN",
    "Screen item starting column exceeds line size" },

  { ec_size_e,                   ec_category_none_e,
   "EC-SIZE", "Size error exception" },
  { ec_size_address_e,           uc_category_fatal_e,
   "EC-SIZE-ADDRESS", "Invalid pointer arithmetic" },
  { ec_size_exponentiation_e,    ec_category_fatal_e,
   "EC-SIZE-EXPONENTIATION", "Exponentiation rules violated" },
  { ec_size_imp_e,               uc_category_implementor_e,
   "EC-SIZE-IMP", "Implementor-defined size error exception" },
  { ec_size_overflow_e,          ec_category_fatal_e,
   "EC-SIZE-OVERFLOW", "Arithmetic overflow in calculation" },
  { ec_size_truncation_e,        ec_category_fatal_e,
   "EC-SIZE-TRUNCATION", "Significant digits truncated in store" },
  { ec_size_underflow_e,         ec_category_fatal_e,
   "EC-SIZE-UNDERFLOW", "Floating-point underflow" },
  { ec_size_zero_divide_e,       ec_category_fatal_e,
   "EC-SIZE-ZERO-DIVIDE", "Division by zero" },

  { ec_sort_merge_e,             ec_category_none_e,
   "EC-SORT-MERGE", "SORT or MERGE exception" },
  { ec_sort_merge_active_e,      uc_category_fatal_e,
   "EC-SORT-MERGE-ACTIVE",
    "File SORT or MERGE executed when one is already active" },
  { ec_sort_merge_file_open_e,   ec_category_fatal_e,
   "EC-SORT-MERGE-FILE-OPEN",
    "A USING or GIVING file is open upon execution of a SORT or MERGE" },
  { ec_sort_merge_imp_e,         uc_category_implementor_e,
   "EC-SORT-MERGE-IMP",
    "Implementor-defined SORT or MERGE exception" },
  { ec_sort_merge_release_e,     uc_category_fatal_e,
   "EC-SORT-MERGE-RELEASE", "RELEASE record too long or too short" },
  { ec_sort_merge_return_e,      uc_category_fatal_e,
   "EC-SORT-MERGE-RETURN", "RETURN executed when at end condition exists" },
  { ec_sort_merge_sequence_e,    uc_category_fatal_e,
   "EC-SORT-MERGE-SEQUENCE", "Sequence error on MERGE USING file" },

  { ec_storage_e,                ec_category_none_e,
   "EC-STORAGE", "Storage allocation exception" },
  { ec_storage_imp_e,            uc_category_implementor_e,
   "EC-STORAGE-IMP", "Implementor-defined storage allocation exception" },
  { ec_storage_not_alloc_e,      uc_category_nonfatal_e,
   "EC-STORAGE-NOT-ALLOC",
    "The data-pointer specified in a FREE statement does not identify "
    "currently allocated storage" },
  { ec_storage_not_avail_e,      uc_category_nonfatal_e,
   "EC-STORAGE-NOT-AVAIL",
    "The amount of storage requested by an ALLOCATE statement "
    "is not available"},
  { ec_user_e,                   ec_category_none_e,
   "EC-USER", "User-defined exception condition" },
  { ec_user_suffix_e,            uc_category_nonfatal_e,
   "EC-USER-SUFFIX", "Level-3 user-defined exception condition" },

  { ec_validate_e,               ec_category_none_e,
   "EC-VALIDATE", "VALIDATE exception" },
  { ec_validate_content_e,       uc_category_nonfatal_e,
   "EC-VALIDATE-CONTENT", "VALIDATE content error" },
  { ec_validate_format_e,        uc_category_nonfatal_e,
   "EC-VALIDATE-FORMAT", "VALIDATE format error" },
  { ec_validate_imp_e,           uc_category_implementor_e,
   "EC-VALIDATE-IMP", "Implementor-defined VALIDATE exception" },
  { ec_validate_relation_e,      uc_category_nonfatal_e,
   "EC-VALIDATE-RELATION", "VALIDATE relation error" },
  { ec_validate_varying_e,       uc_category_fatal_e,
   "EC-VALIDATE-VARYING", "VARYING clause expression noninteger" },
} ;

ec_descr_t *__gg__exception_table_end = __gg__exception_table + COUNT_OF(__gg__exception_table);

