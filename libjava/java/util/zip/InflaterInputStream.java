// InflaterInputStream.java - Input stream filter for decompressing.

/* Copyright (C) 1999, 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.util.zip;

import java.io.FilterInputStream;
import java.io.InputStream;
import java.io.IOException;

/**
 * @author Tom Tromey
 * @date May 17, 1999
 */

/* Written using on-line Java Platform 1.2 API Specification
 * and JCL book.
 * Believed complete and correct.
 */

public class InflaterInputStream extends FilterInputStream
{
  protected void fill () throws IOException
  {
    if (inf == null)
      throw new IOException ("stream closed");
    len = in.read(buf, 0, buf.length);
    if (len != -1)
      inf.setInput(buf, 0, len);
  }

  public InflaterInputStream (InputStream in)
  {
    this (in, new Inflater (), 512);
  }

  public InflaterInputStream (InputStream in, Inflater infl)
  {
    this (in, infl, 512);
  }

  public InflaterInputStream (InputStream in, Inflater infl, int bufsize)
  {
    super (in);
    this.inf = infl;
    this.buf = new byte[bufsize];
  }

  public int read () throws IOException
  {
    byte[] buf = new byte[1];
    int r = read (buf, 0, 1);
    if (r != -1)
      r = buf[0] & 0xff;
    return r;
  }

  public int read (byte[] buf, int off, int len) throws IOException
  {
    if (inf == null)
      throw new IOException ("stream closed");
    if (inf.finished())
      return -1;
    if (inf.needsInput())
      fill ();
    if (this.len == -1)
      return -1; // Couldn't get any more data to feed to the Inflater
    if (inf.needsDictionary())
      return -1;
    try
      {
	return inf.inflate(buf, off, len);
      }
    catch (DataFormatException dfe)
      {
	throw new ZipException (dfe.getMessage());
      }
  }

  public void close () throws IOException
  {
    inf = null;
    super.close ();
  }

  public int available () throws IOException
  {
    // According to the JDK 1.2 docs, this should only ever return 0
    // or 1 and should not be relied upon by Java programs.
    if (inf == null)
      throw new IOException ("stream closed");
    return inf.finished () ? 0 : 1;
  }

  public long skip (long n) throws IOException
  {
    if (inf == null)
      throw new IOException ("stream closed");

    if (n == 0)
      return 0;

    int min = (int) Math.min(n, 1024);
    byte[] buf = new byte[min];

    long s = 0;
    while (n > 0)
      {
	int r = read (buf, 0, min);
	if (r == -1)
	  break;
	n -= r;
	s += r;
	min = (int) Math.min(n, 1024);
      }

    return s;
  }

  // Buffer for delivering uncompressed data to inflater.
  protected byte[] buf;

  // Inflater used to decompress data.
  protected Inflater inf;

  // Number of read bytes in buf.
  protected int len;
}
