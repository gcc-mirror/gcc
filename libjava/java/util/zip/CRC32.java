/* CRC32.java - Computes CRC32 data checksum of a data stream
   Copyright (C) 1999. 2000, 2001 Free Software Foundation, Inc.

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
 * The actual CRC32 algorithm is taken from RFC 1952.
 * Status:  Believed complete and correct.
 */

/**
 * Computes CRC32 data checksum of a data stream.
 * The actual CRC32 algorithm is described in RFC 1952
 * (GZIP file format specification version 4.3).
 * Can be used to get the CRC32 over a stream if used with checked input/output
 * streams.
 *
 * @see InflaterInputStream
 * @see DeflaterOutputStream
 *
 * @author Per Bothner
 * @date April 1, 1999.
 */
public class CRC32 implements Checksum
{
  /** The crc data checksum so far. */
  private int crc = 0;

  /** The fast CRC table. Computed once when the CRC32 class is loaded. */
  private static int[] crc_table = make_crc_table();

  /** Make the table for a fast CRC. */
  private static int[] make_crc_table ()
  {
    int[] crc_table = new int[256];
    for (int n = 0; n < 256; n++)
      {
	int c = n;
	for (int k = 8;  --k >= 0; )
	  {
	    if ((c & 1) != 0)
	      c = 0xedb88320 ^ (c >>> 1);
	    else
	      c = c >>> 1;
	  }
	crc_table[n] = c;
      }
    return crc_table;
  }

  /**
   * Returns the CRC32 data checksum computed so far.
   */
  public long getValue ()
  {
    return (long) crc & 0xffffffffL;
  }

  /**
   * Resets the CRC32 data checksum as if no update was ever called.
   */
  public void reset () { crc = 0; }

  /**
   * Updates the checksum with the int bval. 
   *
   * @param bval (the byte is taken as the lower 8 bits of bval)
   */

  public void update (int bval)
  {
    int c = ~crc;
    c = crc_table[(c ^ bval) & 0xff] ^ (c >>> 8);
    crc = ~c;
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
    int c = ~crc;
    while (--len >= 0)
      c = crc_table[(c ^ buf[off++]) & 0xff] ^ (c >>> 8);
    crc = ~c;
  }

  /**
   * Adds the complete byte array to the data checksum.
   */
  public void update (byte[] buf) { update(buf, 0, buf.length); }
}
