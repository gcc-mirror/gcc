/* Adler.java - Computes Adler32 data checksum of a data stream
   Copyright (C) 1999, 2000 Free Software Foundation, Inc.

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

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */

package java.util.zip;

/*
 * Written using on-line Java Platform 1.2 API Specification, as well
 * as "The Java Class Libraries", 2nd edition (Addison-Wesley, 1998).
 * The actual Adler32 algorithm is taken from RFC 1950.
 * Status:  Believed complete and correct.
 */

/**
 * Computes Adler32 data checksum of a data stream.
 * The actual Adler32 algorithm is described in RFC 1950
 * (ZLIB Compressed Data Format Specification version 3.3).
 * Can be used to get the CRC32 over a stream if used with checked input/output
 * streams.
 *
 * @see InflaterInputStream
 * @see InflaterOutputStream
 *
 * @author Per Bothner
 * @date April 6, 1999.
 */
public class Adler32 implements Checksum
{

  /** largest prime smaller than 65536 */
  private static int BASE = 65521;
 
  private int s1;
  private int s2;

  /**
   * Creates an Adler32 data checksum.
   */
  public Adler32 ()
  {
    reset();
  }

  /**
   * Resets the Adler32 data checksum as if no update was ever called.
   */
  public void reset () { s1 = 1;  s2 = 0; }

  /**
   * Adds one byte to the data checksum.
   *
   * @param bval the data value to add. The high byte of the int is ignored.
   */
  public void update (int bval)
  {
    s1 = (s1 + (bval & 0xFF)) % BASE;
    s2 = (s1 + s2) % BASE;
  }

  /**
   * Adds the complete byte array to the data checksum.
   */
  public void update (byte[] buffer)
  {
    update(buffer, 0, buffer.length);
  }

  /**
   * Adds the byte array to the data checksum.
   *
   * @param buf the buffer which contains the data
   * @param off the offset in the buffer where the data starts
   * @param len the length of the data
   */
  public void update (byte[] buf, int off, int len)
  {
    int s1 = this.s1;
    int s2 = this.s2;
    while (len > 0)
      {
	// We can defer the modulo operation.
	int n = 4000;
	if (n > len)
	  n = len;
	len -= n;
	while (--n >= 0)
	  {
	    s1 = s1 + (buf[off++] & 0xFF);
	    s2 = s2 + s1;
	  }
	s1 %= BASE;
	s2 %= BASE;
      }
    this.s1 = s1;
    this.s2 = s2;
  }

  /**
   * Returns the Adler32 data checksum computed so far.
   */
  public long getValue()
  {
    return ((long) s2 << 16) + s1;
  }
}
