// BufferedWriter.java - Filtered character output stream.

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

// Why not extend FilterWriter?
public class BufferedWriter extends Writer
{
  public BufferedWriter (Writer out)
  {
    this (out, 8192);
  }

  public BufferedWriter (Writer ox, int size)
  {
    super (ox);
    out = ox;
    buffer = new char[size];
    count = 0;
  }

  public void close () throws IOException
  {
    localFlush ();
    out.close();
  }

  public void flush () throws IOException
  {
    localFlush ();
    out.flush();
  }

  public void newLine () throws IOException
  {
    write (System.getProperty("line.separator"));
  }

  public void write (int oneChar) throws IOException
  {
    synchronized (lock)
      {
	buffer[count++] = (char) oneChar;
	if (count == buffer.length)
	  localFlush ();
      }
  }

  public void write (char[] buf, int offset, int len) throws IOException
  {
    if (offset < 0 || len < 0 || offset + len > buf.length)
      throw new ArrayIndexOutOfBoundsException ();

    synchronized (lock)
      {
	// Bypass buffering if there is too much incoming data.
	if (count + len > buffer.length)
	  {
	    localFlush ();
	    out.write(buf, offset, len);
	  }
	else
	  {
	    System.arraycopy(buf, offset, buffer, count, len);
	    count += len;
	    if (count == buffer.length)
	      localFlush ();
	  }
      }
  }

  public void write (String str, int offset, int len) throws IOException
  {
    if (offset < 0 || len < 0 || offset + len < str.length())
      throw new ArrayIndexOutOfBoundsException ();

    synchronized (lock)
      {
	if (count + len > buffer.length)
	  {
	    localFlush ();
	    out.write(str, offset, len);
	  }
	else
	  {
	    str.getChars(offset, offset + len, buffer, count);
	    count += len;
	    if (count == buffer.length)
	      localFlush ();
	  }
      }
  }

  private final void localFlush () throws IOException
  {
    if (count > 0)
      {
	synchronized (lock)
	  {
	    out.write(buffer, 0, count);
	    count = 0;
	  }
      }
  }

  // The downstream writer.
  private Writer out;
  // The character buffer.
  char[] buffer;
  // Number of valid chars in buffer.
  int count;
}
