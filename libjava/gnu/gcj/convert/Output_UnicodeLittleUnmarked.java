/* Copyright (C) 2004  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.convert;

/**
 * Convert to Unicode Little Endian, no marker
 */
public class Output_UnicodeLittleUnmarked extends UnicodeToBytes
{
  public String getName() { return "UnicodeLittleUnmarked"; }

  /** Convert chars to bytes.
    * Converted bytes are written to buf, starting at count.
    * @param inbuffer source of characters to convert
    * @param inpos index of initial character in inbuffer to convert
    * @param inlength number of characters to convert
    * @return number of chars converted
    * Also, this.count is increment by the number of bytes converted.
    */
  public int write (char[] inbuffer, int inpos, int inlength)
  {
    int avail = buf.length - count;
    if (inlength * 2 > avail)
      inlength = avail / 2;
    for (int i = inlength; i > 0; i--)
      {
        char c = inbuffer[inpos++];
        buf[count] = (byte)c;
        buf[count+1] = (byte)(c >> 8);
	count += 2;
      }
    return inlength;
  }
}

