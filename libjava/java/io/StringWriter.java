// StringWriter.java - StringBuffer output stream

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
 * Status:  Complete to 1.1.
 */

public class StringWriter extends Writer
{
  public void close ()
  {
    // JCL says this does nothing.  This seems to violate the Writer
    // contract, in that other methods should still throw and
    // IOException after a close.  Still, we just follow JCL.
  }

  public void flush ()
  {
  }

  public StringBuffer getBuffer ()
  {
    return buffer;
  }

  public StringWriter ()
  {
    this (16);
  }

  protected StringWriter (int size)
  {
    super ();
    buffer = new StringBuffer (size);
    lock = buffer;
  }

  public String toString ()
  {
    return buffer.toString();
  }

  public void write (int oneChar)
  {
    buffer.append((char) oneChar);
  }

  public void write (char[] chars, int offset, int len)
  {
    buffer.append(chars, offset, len);
  }

  public void write (String str)
  {
    buffer.append(str);
  }

  public void write (String str, int offset, int len)
  {
    buffer.append(str.substring(offset, offset + len));
  }

  // The string buffer.
  private StringBuffer buffer;
}
