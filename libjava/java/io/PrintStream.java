// PrintStream.java - Print string representations

/* Copyright (C) 1998, 1999  Cygnus Solutions

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
 * Status:  Not finished.
 */

public class PrintStream extends FilterOutputStream
{
  public boolean checkError ()
  {
    return error;
  }

  public void close ()
  {
    try
      {
	out.close();
      }
    catch (IOException e)
      {
	setError ();
      }
  }

  public void flush ()
  {
    try
      {
	out.flush();
      }
    catch (IOException e)
      {
	setError ();
      }
  }

  private final void print (String str, boolean check_term)
  {
    try
      {
	write(str.getBytes());
	if (check_term
	    && auto_flush
	    && str.indexOf(line_separator) != -1)
	  flush ();
      }
    catch (IOException e)
      {
	setError ();
      }
  }

  public void print (boolean bool)
  {
    print (String.valueOf(bool), false);
  }

  public void print (int inum)
  {
    print (String.valueOf(inum), false);
  }

  public void print (long lnum)
  {
    print (String.valueOf(lnum), false);
  }

  public void print (float fnum)
  {
    print (String.valueOf(fnum), false);
  }

  public void print (double dnum)
  {
    print (String.valueOf(dnum), false);
  }

  public void print (Object obj)
  {
    print (String.valueOf(obj), false);
  }

  public void print (String str)
  {
    print (str == null ? "null" : str, true);
  }

  public void print (char ch)
  {
    print (String.valueOf(ch), true);
  }

  public void print (char[] charArray)
  {
    print (String.valueOf(charArray), true);
  }

  public void println ()
  {
    print (line_separator, false);
    if (auto_flush)
      flush ();
  }

  public void println (boolean bool)
  {
    print (String.valueOf(bool), false);
    println ();
  }

  public void println (int inum)
  {
    print (String.valueOf(inum), false);
    println ();
  }

  public void println (long lnum)
  {
    print (String.valueOf(lnum), false);
    println ();
  }

  public void println (float fnum)
  {
    print (String.valueOf(fnum), false);
    println ();
  }

  public void println (double dnum)
  {
    print (String.valueOf(dnum), false);
    println ();
  }

  public void println (Object obj)
  {
    print (String.valueOf(obj), false);
    println ();
  }

  public void println (String str)
  {
    print (str == null ? "null" : str, false);
    println ();
  }

  public void println (char ch)
  {
    print (String.valueOf(ch), false);
    println ();
  }

  public void println (char[] charArray)
  {
    print (String.valueOf(charArray), false);
    println ();
  }

  public PrintStream (OutputStream out)
  {
    super (out);
    error = false;
    auto_flush = false;
  }

  public PrintStream (OutputStream out, boolean af)
  {
    super (out);
    error = false;
    auto_flush = af;
  }

  protected void setError ()
  {
    error = true;
  }

  public void write (int oneByte)
  {
    try
      {
	out.write(oneByte);
	// JCL says to do this.  I think it is wrong.  FIXME.
	if (auto_flush && oneByte == '\n')
	  out.flush();
      }
    catch (IOException e)
      {
	setError ();
      }
  }

  public void write (byte[] buffer, int offset, int count)
  {
    try
      {
	out.write(buffer, offset, count);
	// FIXME: JCL says to flush.  But elsewhere the JCL says to
	// use write to write the stringified form of an object, and
	// only to flush if that string contains the line separator.
	// How to resolve the contradiction?
	if (auto_flush)
	  out.flush();
      }
    catch (IOException e)
      {
	setError ();
      }
  }

  // True if error occurred.
  private boolean error;
  // True if auto-flush.
  private boolean auto_flush;

  // Line separator string.
  private static final String line_separator
    = System.getProperty("line.separator");
}
