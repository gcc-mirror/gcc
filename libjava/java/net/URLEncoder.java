// URLEncoder.java - Provides a method for encoding strings according to
//		     application/x-www-form-urlencoded MIME type.

/* Copyright (C) 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.net;
import java.io.UnsupportedEncodingException;

/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date April 22, 1999.
 */

/**
 * Written using on-line Java Platform 1.2 API Specification, as well
 * as "The Java Class Libraries", 2nd edition (Addison-Wesley, 1998).
 * Status:  Believed complete and correct.
 */

public class URLEncoder
{
  // This method, per the JCL, is conservative in that it encodes
  // some "allowable" characters as % triplets.
  public static String encode(String s)
  {
    // Get the bytes in ISO-Latin-1 (i.e. 8859_1) per the JCL.
    // Even though it is the default in most cases, it's specified here
    // just in case System.getProperty("file.encoding") is not "8859_1".
    String result = "";
    try
      {
	byte[] buf = s.getBytes("8859_1");
	int start = 0;
	for (int i = 0; i < buf.length; i++)
	  // For efficiency, check the byte in order of most likely
	  // possibility so as to minimize the number of comparisons.
	  // Hence, exclude all the alphanumeric & allowed special chars first.
	  if ((buf[i] >= 'a' && buf[i] <= 'z') ||
	      (buf[i] >= 'A' && buf[i] <= 'Z') ||
	      (buf[i] >= '0' && buf[i] <= '9') ||
	      buf[i] == '-' || buf[i] == '_' || buf[i] == '.' || buf[i] == '*')
	    ; // This is the most likely case so exclude first for efficiency.
	  else if (buf[i] == ' ')
	    buf[i] = (byte) '+';  // Replace space char with plus symbol.
	  else
	    {
	      result = result + new String(buf, start, i - start, "8859_1") +
			"%" + Integer.toHexString(((int) buf[i]) & 0xFF);
	      start = i + 1;
	    }

	// Append remainder of allowable chars from the string, if any.
	if (start < buf.length)
	  result = result +
		   new String(buf, start, buf.length - start, "8859_1");
      }
    catch (UnsupportedEncodingException ex)
      {
	// This should never happen as "8859_1" is the default encoding.
	return s;
      }

    return result;
  }
}
