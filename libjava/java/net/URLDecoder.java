// URLDecoder.java - Provides a method for decoding strings according to
//		     application/x-www-form-urlencoded MIME type.

/* Copyright (C) 1999  Red Hat, Inc.

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
 * Written using on-line Java Platform 1.2 API Specification.
 * Status:  Believed complete and correct.
 */

// JDK1.2
public class URLDecoder
{
  // This method, per the JCL, is conservative in that it encodes
  // some "allowable" characters as % triplets.
  public static String decode(String s) throws Exception
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
}
