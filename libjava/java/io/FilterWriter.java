// FilterWriter.java - Filtered character output stream.

/* Copyright (C) 1998, 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.io;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date September 25, 1998 
 */

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * Status:  Complete to version 1.1.
 */

public abstract class FilterWriter extends Writer
{
  public void close () throws IOException
  {
    out.close();
  }

  protected FilterWriter (Writer ox)
  {
    super (ox);
    out = ox;
  }

  public void flush () throws IOException
  {
    out.flush();
  }

  public void write (int oneChar) throws IOException
  {
    out.write(oneChar);
  }

  public void write (char[] buffer, int offset, int count) throws IOException
  {
    out.write(buffer, offset, count);
  }

  public void write (String str, int offset, int count) throws IOException
  {
    out.write(str, offset, count);
  }

  // Where our writes should go.
  protected Writer out;
}
