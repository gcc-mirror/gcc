// CheckedInputStream.java - Compute checksum of data being read.

/* Copyright (C) 1999  Red Hat, Inc.

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

public class CheckedInputStream extends FilterInputStream
{
  public CheckedInputStream (InputStream in, Checksum sum)
  {
    super (in);
    this.sum = sum;
  }

  public Checksum getChecksum ()
  {
    return sum;
  }

  public int read () throws IOException
  {
    int x = in.read();
    if (x != -1)
      sum.update(x);
    return x;
  }

  public int read (byte[] buf, int off, int len) throws IOException
  {
    int r = in.read(buf, off, len);
    if (r != -1)
      sum.update(buf, off, r);
    return r;
  }

  public long skip (long n) throws IOException
  {
    if (n == 0)
      return 0;

    int min = (int) Math.min(n, 1024);
    byte[] buf = new byte[min];

    long s = 0;
    while (n > 0)
      {
	int r = in.read(buf, 0, min);
	if (r == -1)
	  break;
	n -= r;
	s += r;
	sum.update(buf, 0, r);
      }

    return s;
  }

  // The checksum object.
  private Checksum sum;
}
