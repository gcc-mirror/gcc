/* Copyright (C) 1998, 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package java.io;

import java.util.Enumeration;

/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date November 3, 1998.  
 */
/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct.
 */
 
public class SequenceInputStream extends InputStream
{
  /* The handle for the current input stream. */
  private InputStream in;

  /* Secondary input stream; not used if constructed w/ enumeration. */
  private InputStream in2;

  /* The enum handle; not used if constructed w/ 2 explicit input streams. */
  private Enumeration enum;

  public SequenceInputStream(Enumeration e)
  {
    enum = e;
    in = (InputStream) enum.nextElement();
    in2 = null;
  }

  public SequenceInputStream(InputStream s1, InputStream s2)
  {
    in = s1;
    in2 = s2;
  }

  public int available() throws IOException
  {
    if (in == null)
      return 0;

    return in.available();
  }

  public void close() throws IOException
  {
    while (in != null)
      {
	in.close();
	in = getNextStream ();
      }
  }

  public int read() throws IOException
  {
    int ch = -1;

    while (in != null && (ch = in.read()) < 0)
      {
	in.close();
        in = getNextStream();
      }

    return ch;
  }

  public int read(byte[] b, int off, int len) throws IOException
  {
    int ch = -1;

    // The validity of the parameters will be checked by in.read so
    // don't bother doing it here.
    while (in != null && (ch = in.read(b, off, len)) < 0)
      {
	in.close();
        in = getNextStream();
      }

    return ch;
  }

  private InputStream getNextStream()
  {
    InputStream nextIn = null;

    if (enum != null)
      {
        if (enum.hasMoreElements())
	  nextIn = (InputStream) enum.nextElement();
      }
    else
      if (in2 != null)
	{
	  nextIn = in2;
	  in2 = null;
	}

    return nextIn;
  }
}
