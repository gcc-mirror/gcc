// DeflaterOutputStream.java - Output filter for compressing.

/* Copyright (C) 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.util.zip;

import java.io.FilterOutputStream;
import java.io.OutputStream;
import java.io.IOException;

/**
 * @author Tom Tromey
 * @date May 17, 1999
 */

/* Written using on-line Java Platform 1.2 API Specification
 * and JCL book.
 * Believed complete and correct.
 */

public class DeflaterOutputStream extends FilterOutputStream
{
  public void close () throws IOException
  {
    finish ();
    out.close();
  }

  protected void deflate () throws IOException
  {
    while (true)
      {
	int len = def.deflate(buf, 0, buf.length);
	if (len == 0 || len == -1)
	  break;
	out.write(buf, 0, len);
      }
  }

  public DeflaterOutputStream (OutputStream out)
  {
    this (out, new Deflater (), 512);
  }

  public DeflaterOutputStream (OutputStream out, Deflater defl)
  {
    this (out, defl, 512);
  }

  public DeflaterOutputStream(OutputStream out, Deflater defl, int bufsize)
  {
    super (out);
    buf = new byte[bufsize];
    def = defl;
  }

  public void finish () throws IOException
  {
    def.finish();
    deflate ();
  }

  public void write (int bval) throws IOException
  {
    byte[] b = new byte[1];
    b[0] = (byte) bval;
    write (b, 0, 1);
  }

  public void write (byte[] buf, int off, int len) throws IOException
  {
    def.setInput (buf, off, len);
    deflate ();
  }

  // The retrieval buffer.
  protected byte[] buf;

  // Deflater used to compress data.
  protected Deflater def;
}
