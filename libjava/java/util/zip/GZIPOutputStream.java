/* GZIPOutputStream.java - Create a file in gzip format
   Copyright (C) 1999, 2000, 2001 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

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

import java.io.IOException;
import java.io.OutputStream;

/**
 * This filter stream is used to compress a stream into a "GZIP" stream. 
 * The "GZIP" format is described in RFC 1952.
 *
 * @author John Leuner
 * @author Tom Tromey
 * @since JDK 1.1
 */

/* Written using on-line Java Platform 1.2 API Specification
 * and JCL book.
 * Believed complete and correct.
 */

public class GZIPOutputStream extends DeflaterOutputStream
{
  /**
   * CRC-32 value for uncompressed data
   */
  protected CRC32 crc;

  /**
   * Creates a GZIPOutputStream with the default buffer size
   *
   * @param out The stream to read data (to be compressed) from 
   * 
   */
  public GZIPOutputStream(OutputStream out) throws IOException
  {
    this(out, 4096);
  }

  /**
   * Creates a GZIPOutputStream with the specified buffer size
   *
   * @param out The stream to read compressed data from 
   * @param size Size of the buffer to use 
   */
  public GZIPOutputStream(OutputStream out, int size) throws IOException
  {
    super(out, new Deflater(Deflater.DEFAULT_COMPRESSION, true), size);
    crc = new CRC32();
    put2(GZIPInputStream.GZIP_MAGIC);
    out.write(GZIPInputStream.Z_DEFLATED);
    // No flags for now.
    out.write(0);
    // No time either.
    put2(0);
    put2(0);
    // No xflags either.
    out.write(0);
    // FIXME: unknown OS.
    out.write(255);
  }

  public synchronized void write(int bval) throws IOException
  {
    super.write(bval);
    crc.update(bval);
  }

  public synchronized void write(byte[] buf) throws IOException
  {
    write(buf, 0, buf.length);
  }

  public synchronized void write(byte[] buf, int off, int len)
    throws IOException
  {
    super.write(buf, off, len);
    crc.update(buf, off, len);
  }

  /**
   * Writes remaining compressed output data to the output stream
   * and closes it.
   */
  public void close() throws IOException
  {
    finish();
    out.close();
  }

  public void finish() throws IOException
  {
    super.finish();
    put4((int) crc.getValue());
    put4(def.getTotalIn());
  }

  private final void put2(int i) throws IOException
  {
    out.write(i);
    out.write(i >> 8);
  }

  private final void put4 (int i) throws IOException
  {
    out.write(i);
    out.write(i >> 8);
    out.write(i >> 16);
    out.write(i >> 24);
  }
}
