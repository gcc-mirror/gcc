/* URLEncoder.java -- Class to convert strings to a properly encoded URL
   Copyright (C) 1998, 1999, 2001, 2002, 2003 Free Software Foundation, Inc.

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

/*
 * Written using on-line Java Platform 1.2/1.4 API Specification, as well
 * as "The Java Class Libraries", 2nd edition (Addison-Wesley, 1998).
 * Status:  Believed complete and correct.
 */

 /**
  * This utility class contains static methods that converts a 
  * string into a fully encoded URL string in x-www-form-urlencoded
  * format.  This format replaces certain disallowed characters with
  * encoded equivalents.  All upper case and lower case letters in the
  * US alphabet remain as is, the space character (' ') is replaced with
  * '+' sign, and all other characters are converted to a "%XX" format
  * where XX is the hexadecimal representation of that character in a
  * certain encoding (by default, the platform encoding, though the
  * standard is "UTF-8").
  * <p>
  * This method is very useful for encoding strings to be sent to CGI scripts
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  * @author Warren Levy <warrenl@cygnus.com>
  * @author Mark Wielaard (mark@klomp.org)
  */
public class URLEncoder
{
  /**
   * This method translates the passed in string into x-www-form-urlencoded
   * format using the default encoding.  The standard encoding is
   * "UTF-8", and the two-argument form of this method should be used
   * instead.
   *
   * @param s The String to convert
   *
   * @return The converted String
   *
   * @deprecated
   */
  public static String encode(String s)
  {
    try
      {
	// We default to 8859_1 for compatibility with the same
	// default elsewhere in the library.
        return encode(s, System.getProperty("file.encoding", "8859_1"));
      }
    catch (UnsupportedEncodingException uee)
      {
        // Should never happen since default should always be supported
	return s;
      }
  }

  /**
   * This method translates the passed in string into x-www-form-urlencoded
   * format using the character encoding to hex-encode the unsafe characters.
   *
   * @param s The String to convert
   * @param encoding The encoding to use for unsafe characters
   *
   * @return The converted String
   *
   * @exception UnsupportedEncodingException If the named encoding is not
   * supported
   *
   * @since 1.4
   */
  public static String encode(String s, String encoding)
    throws UnsupportedEncodingException
  {
    int length = s.length();
    int start = 0;
    int i = 0;

    StringBuffer result = new StringBuffer(length);
    while (true)
    {
      while ( i < length && isSafe(s.charAt(i)) )
	i++;

      // Safe character can just be added
      result.append(s.substring(start, i));

      // Are we done?
      if (i >= length)
	return result.toString();
      else if (s.charAt(i) == ' ')
        {
	  result.append('+');  // Replace space char with plus symbol.
	  i++;
	}
      else
	{
	  // Get all unsafe characters
	  start = i;
	  char c;
	  while ( i < length && (c = s.charAt(i)) != ' ' && !isSafe(c) )
	    i++;

	  // Convert them to %XY encoded strings
	  String unsafe = s.substring(start,i);
	  byte bytes[] = unsafe.getBytes(encoding);
	  for (int j = 0; j < bytes.length; j++)
	    {
	      result.append('%');
	      int val = bytes[j];
	      result.append(hex.charAt((val & 0xf0) >> 4));
	      result.append(hex.charAt(val & 0x0f));
	    }
	}
      start = i;
    }
  }

  /**
   * Private static method that returns true if the given char is either
   * a uppercase or lowercase letter from 'a' till 'z', or a digit froim
   * '0' till '9', or one of the characters '-', '_', '.' or '*'. Such
   * 'safe' character don't have to be url encoded.
   */
  private static boolean isSafe(char c)
  {
    return  ((c >= 'a' && c <= 'z') ||
	     (c >= 'A' && c <= 'Z') ||
	     (c >= '0' && c <= '9') ||
	     c == '-' || c == '_' || c == '.' || c == '*');
  }

  /**
   * Private constructor that does nothing. Included to avoid a default
   * public constructor being created by the compiler.
   */
  private URLEncoder() { }

  /**
   * Used to convert to hex.  We don't use Integer.toHexString, since
   * it converts to lower case (and the Sun docs pretty clearly
   * specify upper case here), and because it doesn't provide a
   * leading 0.
   */
  private static final String hex = "0123456789ABCDEF";
} // class URLEncoder
