/* URLDecoder.java -- Class to decode URL's from encoded form.
   Copyright (C) 1998, 1999, 2000, 2001 Free Software Foundation, Inc.

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.
 
GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */

package java.net;

import java.io.UnsupportedEncodingException;

/**
  * This utility class contains one static method that converts a 
  * string encoded in the x-www-form-urlencoded format to the original
  * text.  The x-www-form-urlencoded format 
  * replaces certain disallowed characters with
  * encoded equivalents.  All upper case and lower case letters in the
  * US alphabet remain as is, the space character (' ') is replaced with
  * '+' sign, and all other characters are converted to a "%XX" format
  * where XX is the hexadecimal representation of that character.  Note
  * that since unicode characters are 16 bits, and this method encodes only
  * 8 bits of information, the lower 8 bits of the character are used.
  * <p>
  * This method is very useful for decoding strings sent to CGI scripts
  *
  * Written using on-line Java Platform 1.2 API Specification.
  * Status:  Believed complete and correct.
  *
  * @since 1.2
  *
  * @author Warren Levy <warrenl@cygnus.com>
  * @author Aaron M. Renn (arenn@urbanophile.com) (documentation comments)
  * @date April 22, 1999.
  */
public class URLDecoder
{
/**
  * This method translates the passed in string from x-www-form-urlencoded
  * format and returns it.
  *
  * @param source The String to convert
  *
  * @return The converted String
  */
  public static String decode(String s)
  {
    String str = s.replace('+', ' ');
    String result = "";
    int i;
    int start = 0;
    while ((i = str.indexOf('%', start)) >= 0)
      {
	result = result + str.substring(start, i) +
		 (char) Integer.parseInt(str.substring(i + 1, i + 3), 16);
	start = i + 3;
      }

    if (start < str.length())
      result = result + str.substring(start);

    return result;
  }
} // class URLDecoder

