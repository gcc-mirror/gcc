/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.convert; 
 
/**
 * Convert Unicode ASCII
 * Unrecognized characters are printed as `?'.
 * @date October 2000
 */

public class Output_ASCII extends UnicodeToBytes
{
  public String getName() { return "ASCII"; }

  /**
   * @return number of chars converted. */
  public int write (char[] inbuffer, int inpos, int inlength)
  {
    int count = this.count;
    byte[] buf = this.buf;
    int avail = buf.length - count;
    if (inlength > avail)
      inlength = avail;
    for (int i = inlength;  --i >= 0;  )
      {
	char c = inbuffer[inpos++];
	buf[count++] = (byte) ((c > 0x7f) ? '?' : c);
      }
    this.count = count;
    return inlength;
  }

  public int write (String str, int inpos, int inlength, char[] work)
  {
    int count = this.count;
    byte[] buf = this.buf;
    int avail = buf.length - count;
    if (inlength > avail)
      inlength = avail;
    for (int i = inlength;  --i >= 0;  )
      {
	char c = str.charAt(inpos++);
	buf[count++] = (byte) ((c > 0x7f) ? '?' : c);
      }
    this.count = count;
    return inlength;
  }
}
