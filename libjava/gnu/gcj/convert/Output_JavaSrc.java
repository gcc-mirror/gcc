/* Copyright (C) 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.convert; 
 
/**
 * Convert Unicode to Ascii with \ u XXXX-escapes.
 * @author Per Bothner <bothner@cygnus.com>
 * @date April 1999.
 */

public class Output_JavaSrc extends UnicodeToBytes
{
  public String getName() { return "JavaSrc"; }

  // Number of bytes remaining before pending_char has been written.
  int todo;
 int pending_char;

  public int write (char[] inbuffer, int inpos, int inlength)
  {
    int start_pos = inpos;
    int avail = buf.length - count;
    for (;;)
      {
	if (avail == 0)
	  break;
	switch (todo)
	  {
	  case 1:
	    if (pending_char == '\\')
	      {
		buf[count++] = (byte) '\\';
		avail--;
		todo = 0;
		continue;
	      }
	    /* ... else fall through ... */
	  case 2:
	  case 3:
	  case 4:
	    todo--;
	    int digit = ((int) pending_char >> (todo * 4)) & 0xF;
	    buf[count++] = (byte) Character.forDigit(digit, 16);
	    avail--;
	    continue;
	  case 5:
	    buf[count++] = (byte) 'u';
	    avail--;
	    todo = 4;
	    continue;
	  default:
	    ;
	  }
	if (inlength == 0)
	  break;
	char ch = inbuffer[inpos++];
	inlength--;
	if (ch == '\\')
	  {
	    buf[count++] = (byte) '\\';
	    pending_char = ch;
	    todo = 1;
	    avail--;
	  }
	else if (ch < 127)
	  {
	    buf[count++] = (byte) ch;
	    avail--;
	  }
	else
	  {
	    buf[count++] = (byte) '\\';
	    pending_char = ch;
	    todo = 5;
	    avail--;
	  }
      }
    return inpos - start_pos;
  }
}
