// GZIPOutputStream.java - Create a file in gzip format.

/* Copyright (C) 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.util.zip;

import java.io.IOException;
import java.io.OutputStream;

/**
 * @author Tom Tromey
 * @date May 17, 1999
 */

/* Written using on-line Java Platform 1.2 API Specification
 * and JCL book.
 * Believed complete and correct.
 */

public class GZIPOutputStream extends DeflaterOutputStream
{
  public void close () throws IOException
  {
    finish ();
    out.close ();
  }

  public void finish () throws IOException
  {
    super.finish();
    put4 ((int) crc.getValue());
    put4 (def.getTotalIn());
  }

  public GZIPOutputStream (OutputStream out) throws IOException
  {
    this (out, 512);
  }

  public GZIPOutputStream (OutputStream out, int readsize) throws IOException
  {
    super (out, new Deflater (Deflater.DEFAULT_COMPRESSION, true), readsize);

    put2 (GZIPInputStream.GZIP_MAGIC);
    out.write (GZIPInputStream.Z_DEFLATED);
    // No flags for now.
    out.write (0);
    // No time either.
    put2 (0);
    put2 (0);
    // No xflags either.
    out.write (0);
    // FIXME: unknown OS.
    out.write (255);

    crc = new CRC32 ();
  }

  public synchronized void write (byte[] buf) throws IOException
  {
    write (buf, 0, buf.length);
  }

  public synchronized void write (byte[] buf, int off, int len)
    throws IOException
  {
    super.write(buf, off, len);
    crc.update(buf, off, len);
  }

  private final void put2 (int i) throws IOException
  {
    out.write (i);
    out.write (i >> 8);
  }

  private final void put4 (int i) throws IOException
  {
    out.write (i);
    out.write (i >> 8);
    out.write (i >> 16);
    out.write (i >> 24);
  }

  // Checksum used by this stream.
  protected CRC32 crc;
}
