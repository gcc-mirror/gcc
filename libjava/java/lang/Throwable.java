// Throwable.java - Superclass for all exceptions.

/* Copyright (C) 1998, 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.lang;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.Serializable;
import java.io.OutputStreamWriter;
import java.io.OutputStream;
import java.io.FilterOutputStream;
import java.io.IOException;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date October 30, 1998 
 */

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * Status: Sufficient for compiled code, but methods applicable to
 * bytecode not implemented.  JDK 1.1.
 */

/* A CPlusPlusDemangler sits on top of a PrintWriter.  All input is
 * passed through the "c++filt" program (part of GNU binutils) which
 * demangles internal symbols to their C++ source form.
 *
 * Closing a CPlusPlusDemangler doesn't close the underlying
 * PrintWriter; it does, however close underlying process and flush
 * all its buffers, so it's possible to guarantee that after a
 * CPlusPlusDemangler has been closed no more will ever be written to
 * the underlying PrintWriter.
 *
 * FIXME: This implictly converts data from the input stream, which is
 * a stream of characters, to a stream of bytes.  We need a way of
 * handling Unicode characters in demangled identifiers.  */

class CPlusPlusDemangler extends OutputStream
{
  java.io.OutputStream procOut;
  java.io.InputStream procIn;
  java.lang.Process proc;
  PrintWriter p;

  /* The number of bytes written to the underlying PrintWriter.  This
     provides a crude but fairly portable way to determine whether or
     not the attempt to exec c++filt worked. */  
  public int written = 0;

  CPlusPlusDemangler (PrintWriter writer) throws IOException
  {
    p = writer;
    proc = Runtime.getRuntime ().exec ("c++filt -s java");
    procOut = proc.getOutputStream ();
    procIn = proc.getInputStream ();
  }

  public void write (int b) throws IOException
  {
    procOut.write (b);
    while (procIn.available () != 0)
      {
	int c = procIn.read ();
	if (c == -1)
	  break;
	else
	  {
	    p.write (c);
	    written++;
	  }
      }
  }
  
  public void close () throws IOException
  {
    procOut.close ();
    int c;
    while ((c = procIn.read ()) != -1)
      {
	p.write (c);
	written++;
      }
    p.flush ();
    try
      {
	proc.waitFor ();
      }
    catch (InterruptedException _)
      {
      }
  }    
}

public class Throwable implements Serializable
{
  public native Throwable fillInStackTrace ();

  public String getLocalizedMessage ()
  {
    return getMessage ();
  }

  public String getMessage ()
  {
    return detailMessage;
  }

  public void printStackTrace ()
  {
    printStackTrace (System.err);
  }

  public void printStackTrace (PrintStream ps)
  {
    PrintWriter writer = new PrintWriter (ps);
    printStackTrace (writer);
  }

  public void printStackTrace (PrintWriter wr)
  {
    try
      {
	CPlusPlusDemangler cPlusPlusFilter = new CPlusPlusDemangler (wr);
	PrintWriter writer = new PrintWriter (cPlusPlusFilter);
	printRawStackTrace (writer);	
	writer.close ();
	if (cPlusPlusFilter.written == 0) // The demangler has failed...
	  printRawStackTrace (wr);
      }
    catch (Exception e1)
      {
	printRawStackTrace (wr);
      }
  }

  public Throwable ()
  {
    detailMessage = null;
    fillInStackTrace ();
  }

  public Throwable (String message)
  {
    detailMessage = message;
    fillInStackTrace ();
  }

  public String toString ()
  {
    return ((detailMessage == null)
	    ? getClass().getName()
	    : getClass().getName() + ": " + getMessage ());
  }

  private native final void printRawStackTrace (PrintWriter wr);
  
  // Name of this field comes from serialization spec.
  private String detailMessage;

  // Setting this flag to false prevents fillInStackTrace() from running.
  static boolean trace_enabled = true;
  private transient byte stackTrace[];
  private static final long serialVersionUID = -3042686055658047285L;
}
