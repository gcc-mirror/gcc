/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.convert;

/**
 * Convert ASCII text to Unicode.
 * @date October 2000
 */

public class Input_ASCII extends BytesToUnicode
{
  public String getName() { return "ASCII"; }

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
	outbuffer[outpos++] = (char) (inbuffer[inpos++] & 0x7f);
      }
    this.inpos = inpos;
    return outpos - origpos;
  }
}
