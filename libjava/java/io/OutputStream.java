// OutputStream.java - Send output bytes to output sink.

/* Copyright (C) 1998, 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.io;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date September 24, 1998 
 */

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * Status:  Complete to version 1.1.
 */

public abstract class OutputStream
{
  public abstract void write (int b) throws IOException;

  public void write (byte[] b) throws IOException, NullPointerException
  {
    write (b, 0, b.length);
  }

  public void write (byte[] b, int off, int len)
    throws IOException, NullPointerException, IndexOutOfBoundsException
  {
    if (off < 0 || len < 0 || off + len > b.length)
      throw new ArrayIndexOutOfBoundsException ();
    for (int i = 0; i < len; ++i)
      write (b[off + i]);
  }

  public void flush () throws IOException
  {
  }

  public void close () throws IOException
  {
  }
}
