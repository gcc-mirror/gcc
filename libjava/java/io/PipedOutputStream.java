// PipedOutputStream.java - Write bytes to a pipe.

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
 * Status:  Believed complete and correct.
 */

public class PipedOutputStream extends OutputStream
{
  public void close () throws IOException
  {
    closed = true;

    // Notify PipedInputStream that there is no more data to be had.
    destination.receive(-1);
  }

  public void connect (PipedInputStream dest) throws IOException
  {
    if (closed)
      throw new IOException("pipe closed");

    if (destination != null)
      if (destination == dest)
	return;
      else
        throw new IOException("pipe already connected");

    destination = dest;
    try
    {
      dest.connect(this);
    }
    catch (IOException ex)
    {
      destination = null;
      throw ex;
    }
  }

  public synchronized void flush () throws IOException
  {
    // There doesn't seem to be anything to do here.

    // TBD: Should this maybe do a notifyAll as a way for the user
    // to wake up the input stream to check for bytes to read?  Shouldn't
    // be necessary but if there aren't any bytes, other threads will just
    // go blocak again anyway so it wouldn't hurt.
  }

  public PipedOutputStream ()
  {
    closed = false;
  }

  public PipedOutputStream (PipedInputStream dest) throws IOException
  {
    closed = false;
    connect (dest);
  }

  public void write (int oneByte) throws IOException
  {
    if (closed)
      throw new IOException ();
    destination.receive(oneByte);
  }

  // This is mentioned in the JCL book, but we don't really need it.
  // If there were a corresponding receive() method on
  // PipedInputStream then we could get better performance using
  // this.
  // public void write (byte[] buffer, int offset, int count)
  // throws IOException;

  // Instance variables.
  private PipedInputStream destination;
  private boolean closed;
}
