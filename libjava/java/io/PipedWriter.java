// PipedWriter.java - Piped character stream.

/* Copyright (C) 1998, 1999  Red Hat, Inc.

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

public class PipedWriter extends Writer
{
  public void close () throws IOException
  {
    closed = true;
  }

  public void connect (PipedReader sink) throws IOException
  {
    if (closed)
      throw new IOException ("already closed");
    if (reader != null)
      {
	if (reader == sink)
	  return;
	throw new IOException ("already connected");
      }
    try
      {
	reader = sink;
	reader.connect(this);
      }
    catch (IOException e)
      {
	reader = null;
	throw e;
      }
  }

  public void flush () throws IOException
  {
    // We'll throw an exception if we're closed, but there's nothing
    // else to do here.
    if (closed)
      throw new IOException ("closed");
  }

  public PipedWriter ()
  {
    super ();
    closed = false;
  }

  public PipedWriter (PipedReader sink) throws IOException
  {
    super ();
    closed = false;
    connect (sink);
  }

  public void write (char buffer[], int offset, int count) throws IOException
  {
    if (closed)
      throw new IOException ("closed");
    reader.receive(buffer, offset, count);
  }

  boolean isClosed ()
  {
    return closed;
  }

  // The associated reader.
  private PipedReader reader;
  private boolean closed;
}
