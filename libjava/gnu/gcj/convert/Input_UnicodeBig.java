/* Copyright (C) 2004  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.convert;

public class Input_UnicodeBig extends BytesToUnicode
{
  /** 0, 8, or 16 bits of a partially constructed character. */
  char partial;
  /** How many bytes of partial are valid. */
  int partial_count;

  public String getName() { return "UnicodeBig"; }

  public int read (char[] outbuffer, int outpos, int count)
  {
    int origcount = count;
    for (;;)
      {
	if (partial_count == 2)
	  {
	    if (count == 0)
	      break;
	    if (partial == 0xFEFF)
	      ; // drop byte order mark
	    // else if (partial >= 0xFFFe)  ERROR;
	    else
	      outbuffer[outpos++] = partial;
	    count--;
	    partial_count = 0;
	    partial = 0;
	  }
	else if (inpos >= inlength)
	  break;
	else
	  {
	    int b = inbuffer[inpos++] & 0xFF;
	    partial = (char) (((int) partial << 8) + b);
	    partial_count++;
	  }
      }
    return origcount - count;
  }
}
