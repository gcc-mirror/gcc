/* Copyright (C) 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.convert;

/**
 * Convert ISO-Latin-1 (8851-1) text to Unicode.
 * @author Per Bothner <bothner@cygnus.com>
 * @date March 1999.
 */

public class Input_8859_1 extends BytesToUnicode
{
  public String getName() { return "8859_1"; }

  public int read (char[] outbuffer, int outpos, int count)
  {
    int origpos = outpos;
    // Make sure fields of this are in registers.
    int inpos = this.inpos;
    byte[] inbuffer = this.inbuffer;
    int inavail = this.inlength - inpos;
    int outavail = count;
    if (outavail > inavail)
      outavail = inavail;
    while (--outavail >= 0)
      {
	outbuffer[outpos++] = (char) (inbuffer[inpos++] & 0xFF);
      }
    this.inpos = inpos;
    return outpos - origpos;
  }
}
