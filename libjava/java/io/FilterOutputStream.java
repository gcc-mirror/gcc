// FilterOutputStream.java - A filtered stream

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

public class FilterOutputStream extends OutputStream
{
  public void close () throws IOException
  {
    flush ();
    out.close();
  }

  public FilterOutputStream (OutputStream ox)
  {
    out = ox;
  }

  public void flush () throws IOException
  {
    out.flush();
  }

  public void write (int b) throws IOException
  {
    out.write(b);
  }

  public void write (byte[] b) throws IOException, NullPointerException
  {
    // Don't do checking here, per Java Lang Spec.
    write (b, 0, b.length);
  }

  public void write (byte[] b, int off, int len)
    throws IOException, NullPointerException, IndexOutOfBoundsException
  {
    // Don't do checking here, per Java Lang Spec.
    for (int i=0; i < len; i++) 
      write (b[off + i]);
  }

  // The output stream.
  protected OutputStream out;
}
