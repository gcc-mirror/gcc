/* Copyright (C) 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.convert; 
 
/**
 * Convert Ascii with \ u XXXX-escapes to Unicode.
 * @author Per Bothner <bothner@cygnus.com>
 * @date April 1999.
 */

public class Input_JavaSrc extends BytesToUnicode
{
  public String getName() { return "JavaSrc"; }

  // 0: normal
  // 1: seen '\\'
  // 2: seen '\\' and 'u'
  // 3: seen '\\' and need to emit value.
  // 4, 5, 6, 7:  seen '\\u', 'u' and (state-3) hex digits.
  int state = 0;

  int value;

  public int read (char[] outbuffer, int outpos, int count)
  {
    int origpos = outpos;
    for (;;)
      {
	if (inpos >= inlength)
	  break;
	if (outpos - origpos >= count)
	  break;
	char b = (char) (inbuffer[inpos++] & 0xFF);
	switch (state)
	  {
	  case 0:
	    if (b == '\\')
	      {
		state = 1;
		continue;
	      }
	    break;
	  case 1:
	    if (b == 'u')
	      {
		state = 2;
		continue;
	      }
	    if (b != '\\')
	      {
		value = b;
		b = '\\';
		state = 3;
	      }
	    break;
	  case 3:
	    b = (char) value;
	    break;
	  default:  //  case 4:  case 5:  case 6:  case 7:
	    int digit = Character.digit(b, 16);
	    if (digit < 0)
	      {
		b = '\uFFFD';
		state = 0;
	      }
	    else
	      {
		value = value * 16 + digit;
		if (state < 7)
		  {
		    state++;
		    continue;
		  }
		b = (char) value;
	      }
	    state = 0;
	  }
	outbuffer[outpos++] = b;
      }
    return outpos - origpos;
  }
}

