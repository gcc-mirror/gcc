/* GZIPInputStream.java - Input filter for reading gzip file
   Copyright (C) 1999, 2000, 2001, 2002, 2004 Free Software Foundation, Inc.

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

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */

package java.util.zip;

import java.io.InputStream;
import java.io.IOException;

/**
 * This filter stream is used to decompress a "GZIP" format stream. 
 * The "GZIP" format is described in RFC 1952.
 *
 * @author John Leuner
 * @author Tom Tromey
 * @since JDK 1.1
 */
public class GZIPInputStream
  extends InflaterInputStream
{
  /**
   * The magic number found at the start of a GZIP stream.
   */
  public static final int GZIP_MAGIC = 0x8b1f;

  static final int Z_DEFLATED = 8;

  /**
   * The mask for bit 1 of the flag byte.
   */
  static final int HEAD_CRC = 0x02;

  /**
   * The mask for bit 2 of the flag byte.
   */
  static final int EXTRA_FIELD = 0x04;

  /**
   * The mask for bit 3 of the flag byte.
   */
  static final int ORIG_NAME = 0x08;

  /**
   * The mask for bit 4 of the flag byte.
   */
  static final int COMMENT = 0x10;

  /**
   * The mask for all reserved bits of the flag byte.
   */
  static final int RESERVED = 0xe0;
  
  /**
   * The CRC-32 checksum value for uncompressed data.
   */
  protected CRC32 crc; 

  /**
   * Indicates whether or not the end of the stream has been reached.
   */  
  protected boolean eos;

  /**
   * Creates a GZIPInputStream with the default buffer size.
   *
   * @param in The stream to read compressed data from 
   *           (in GZIP format).
   *
   * @throws IOException if an error occurs during an I/O operation.
   */
  public GZIPInputStream(InputStream in)
    throws IOException
  {
    this(in, 4096);
  }

  /**
   * Creates a GZIPInputStream with the specified buffer size.
   *
   * @param in The stream to read compressed data from 
   *           (in GZIP format).
   * @param size The size of the buffer to use.
   *
   * @throws IOException if an error occurs during an I/O operation.
   * @throws IllegalArgumentException if <code>size</code>
   * is less than or equal to 0.
   */
  public GZIPInputStream(InputStream in, int size)
    throws IOException
  {
    super(in, new Inflater(true), size);

    // NOTE: header reading code taken from zlib's gzio.c.

    // Read the magic number.
    int magic = eof_read() | (eof_read() << 8);
    if (magic != GZIP_MAGIC)
      throw new ZipException("gzip header corrupted");

    int method = eof_read();
    int flags = eof_read();
    // Test from zlib.
    if (method != Z_DEFLATED || (flags & RESERVED) != 0)
      throw new ZipException("gzip header corrupted");

    // Discard time, xflags, OS code.
    for (int i = 0; i < 6; ++i)
      eof_read();

    // Skip the extra field.
    if ((flags & EXTRA_FIELD) != 0)
      {
	int len = eof_read() | (eof_read() << 8);
	while (len-- != 0)
	  eof_read();
      }

    if ((flags & ORIG_NAME) != 0)
      {
	while (true)
	  {
	    int c = eof_read();
	    if (c == 0)
	      break;
	  }
      }

    if ((flags & COMMENT) != 0)
      {
	while (true)
	  {
	    int c = eof_read();
	    if (c == 0)
	      break;
	  }
      }

    if ((flags & HEAD_CRC) != 0)
      {
	// FIXME: consider checking CRC of the header.
	eof_read();
	eof_read();
      }

    crc = new CRC32();
  }

  /**
   * Closes the input stream.
   *
   * @throws IOException if an error occurs during an I/O operation.
   */
  public void close()
    throws IOException
  {
    // Nothing to do here.
    super.close();
  }

  private final int eof_read() throws IOException
  {
    int r = in.read();
    if (r == -1)
      throw new ZipException("gzip header corrupted");
    return r & 0xff;
  }

  /**
   * Reads in GZIP-compressed data and stores it in uncompressed form
   * into an array of bytes.  The method will block until either
   * enough input data becomes available or the compressed stream
   * reaches its end.
   *
   * @param buf the buffer into which the uncompressed data will
   *            be stored.
   * @param offset the offset indicating where in <code>buf</code>
   *               the uncompressed data should be placed.
   * @param len the number of uncompressed bytes to be read.
   */
  public int read(byte[] buf, int offset, int len) throws IOException
  {
    if (eos)
      return -1;
    int r = super.read(buf, offset, len);
    if (r == -1)
      {
	eos = true;

	byte[] tmp = new byte[8];
	// First copy remaining bytes from inflater input buffer.
	int avail = inf.getRemaining();
	System.arraycopy(this.buf, this.len - avail, tmp, 0, avail);

	// Now read remaining bytes from wrapped input stream.
	for (int i = avail; i < 8; ++i)
	  {
	    tmp[i] = (byte) eof_read();
	  }

	// Be careful to avoid sign extension here; CRC32.getValue()
	// returns a long.
	long header_crc = read4(tmp, 0) & 0xffffffffL;
	if (crc.getValue() != header_crc)
	  throw new ZipException("corrupted gzip file - crc mismatch");
	int isize = read4(tmp, 4);
	if (inf.getTotalOut() != isize)
	  throw new ZipException("corrupted gzip file - size mismatch");
	return -1;
      }
    crc.update(buf, offset, r);
    return r;
  }

  private final int read4(byte[] buf, int offset) throws IOException
  {
    return (((buf[offset + 3] & 0xFF) << 24) + ((buf[offset + 2] & 0xFF) << 16)
	    + ((buf[offset + 1] & 0xFF) << 8) + (buf[offset] & 0xFF));
  }
}
