// CheckedOutputStream.java - Compute checksum of data being written.

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

public class CheckedOutputStream extends FilterOutputStream
{
  public CheckedOutputStream (OutputStream out, Checksum cksum)
  {
    super (out);
    this.sum = cksum;
  }

  public Checksum getChecksum ()
  {
    return sum;
  }

  public void write (int bval) throws IOException
  {
    out.write(bval);
    sum.update(bval);
  }

  public void write (byte[] buf, int off, int len) throws IOException
  {
    out.write(buf, off, len);
    sum.update(buf, off, len);
  }

  // The checksum object.
  private Checksum sum;
}
