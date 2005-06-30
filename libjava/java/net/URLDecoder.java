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
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */

package java.net;

import java.io.UnsupportedEncodingException;


/**
 * This utility class contains static methods that converts a
 * string encoded in the x-www-form-urlencoded format to the original
 * text.  The x-www-form-urlencoded format replaces certain disallowed
 * characters with encoded equivalents.  All upper case and lower case
 * letters in the US alphabet remain as is, the space character (' ')
 * is replaced with '+' sign, and all other characters are converted to a
 * "%XX" format where XX is the hexadecimal representation of that character
 * in a given character encoding (default is "UTF-8").
 * <p>
 * This method is very useful for decoding strings sent to CGI scripts
 *
 * Written using on-line Java Platform 1.2/1.4 API Specification.
 * Status:  Believed complete and correct.
 *
 * @since 1.2
 *
 * @author Warren Levy (warrenl@cygnus.com)
 * @author Aaron M. Renn (arenn@urbanophile.com) (documentation comments)
 * @author Mark Wielaard (mark@klomp.org)
 */
public class URLDecoder
{
  /**
   * Public contructor. Note that this class has only static methods.
   */
  public URLDecoder()
  {
  }

  /**
   * This method translates the passed in string from x-www-form-urlencoded
   * format using the default encoding "UTF-8" to decode the hex encoded
   * unsafe characters.
   *
   * @param s the String to convert
   *
   * @return the converted String
   *
   * @deprecated
   */
  public static String decode(String s)
  {
    try
      {
	return decode(s, "UTF-8");
      }
    catch (UnsupportedEncodingException uee)
      {
	// Should never happen since UTF-8 encoding should always be supported
	return s;
      }
  }

  /**
   * This method translates the passed in string from x-www-form-urlencoded
   * format using the given character encoding to decode the hex encoded
   * unsafe characters.
   *
   * This implementation will decode the string even if it contains
   * unsafe characters (characters that should have been encoded) or if the
   * two characters following a % do not represent a hex encoded byte.
   * In those cases the unsafe character or the % character will be added
   * verbatim to the decoded result.
   *
   * @param s the String to convert
   * @param encoding the character encoding to use the decode the hex encoded
   *        unsafe characters
   *
   * @return the converted String
   *
   * @exception UnsupportedEncodingException If the named encoding is not
   * supported
   *
   * @since 1.4
   */
  public static String decode(String s, String encoding)
    throws UnsupportedEncodingException
  {
    // First convert all '+' characters to spaces.
    String str = s.replace('+', ' ');

    // Then go through the whole string looking for byte encoded characters
    int i;
    int start = 0;
    byte[] bytes = null;
    int length = str.length();
    StringBuffer result = new StringBuffer(length);
    while ((i = str.indexOf('%', start)) >= 0)
      {
	// Add all non-encoded characters to the result buffer
	result.append(str.substring(start, i));
	start = i;

	// Get all consecutive encoded bytes
	while ((i + 2 < length) && (str.charAt(i) == '%'))
	  i += 3;

	// Decode all these bytes
	if ((bytes == null) || (bytes.length < ((i - start) / 3)))
	  bytes = new byte[((i - start) / 3)];

	int index = 0;
	try
	  {
	    while (start < i)
	      {
		String sub = str.substring(start + 1, start + 3);
		bytes[index] = (byte) Integer.parseInt(sub, 16);
		index++;
		start += 3;
	      }
	  }
	catch (NumberFormatException nfe)
	  {
	    // One of the hex encoded strings was bad
	  }

	// Add the bytes as characters according to the given encoding
	result.append(new String(bytes, 0, index, encoding));

	// Make sure we skip to just after a % sign
	// There might not have been enough encoded characters after the %
	// or the hex chars were not actually hex chars (NumberFormatException)
	if (start < length && s.charAt(start) == '%')
	  {
	    result.append('%');
	    start++;
	  }
      }

    // Add any characters left
    if (start < str.length())
      result.append(str.substring(start));

    return result.toString();
  }
} // class URLDecoder
