// PipedReader.java - Piped character stream.

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

public class PipedReader extends Reader
{
  public void close () throws IOException
  {
    closed = true;
  }

  public void connect (PipedWriter src) throws IOException
  {
    if (closed)
      throw new IOException ("already closed");
    if (writer != null)
      {
	if (writer == src)
	  return;
	throw new IOException ("already connected");
      }
    try
      {
	writer = src;
	writer.connect(this);
      }
    catch (IOException e)
      {
	writer = null;
	throw e;
      }
  }

  public PipedReader ()
  {
    super ();
    writer = null;
    closed = false;
    in = -1;
    out = 0;
    pipeBuffer = new char[1024];
  }

  public PipedReader (PipedWriter src) throws IOException
  {
    super ();
    closed = false;
    in = -1;
    out = 0;
    pipeBuffer = new char[1024];
    connect (src);
  }

  public int read (char buf[], int offset, int count) throws IOException
  {
    if (closed)
      throw new IOException ("closed");
    if (count < 0)
      throw new ArrayIndexOutOfBoundsException ();
    int toCopy = count;
    synchronized (lock)
      {
	while (toCopy > 0)
	  {
	    // Wait for data in the pipe.  If the writer is closed and
	    // no data has been copied into the output buffer, return
	    // the magic EOF number.
	    while (in == -1)
	      {
		if (writer.isClosed())
		  {
		    if (toCopy < count)
		      return count - toCopy;
		    return -1;
		  }

		// Note that JCL doesn't say this is the right thing
		// to do.  Still, it feels right, and we must deal
		// with an interrupt somehow.
		try
		  {
		    lock.wait();
		  }
		catch (InterruptedException e)
		  {
		    InterruptedIOException io
		      = new InterruptedIOException (e.getMessage());
		    io.bytesTransferred = count - toCopy;
		    throw io;
		  }
	      }
	    // Now copy some data from pipe into user buffer.
	    int len;
	    if (in < out)
	      len = pipeBuffer.length - out;
	    else
	      len = in - out;
	    len = len > toCopy ? toCopy : len;
	    System.arraycopy(pipeBuffer, out, buf, offset, len);
	    out += len;
	    if (out == pipeBuffer.length)
	      out = 0;
	    toCopy -= len;
	    offset += len;
	    // If we've read all the data, then reset so that we know
	    // there is nothing left to be read.
	    if (in == out)
	      in = -1;
	    // Tell anybody waiting for space in the buffer.
	    lock.notifyAll();
	  }
      }
    return count;
  }

  void receive (char buf[], int offset, int count) throws IOException
  {
    if (count < 0)
      throw new ArrayIndexOutOfBoundsException ();
    int original = count;
    synchronized (lock)
      {
	while (count > 0)
	  {
	    // Wait until there is some space in the buffer.
	    while (in == out)
	      {
		try
		  {
		    lock.wait();
		  }
		catch (InterruptedException e)
		  {
		    // Turn interrupts into IO interrupts.
		    InterruptedIOException io
		      = new InterruptedIOException (e.getMessage());
		    io.bytesTransferred = original - count;
		    throw io;
		  }
	      }

	    // Compute destination in the pipe.
	    int base, len;
	    if (in == -1)
	      {
		base = 0;
		len = pipeBuffer.length;
	      }
	    else if (in < out)
	      {
		base = in;
		len = out - in;
	      }
	    else
	      {
		base = in;
		len = pipeBuffer.length - in;
	      }
	    int copyLen = len > count ? count : len;
	    // Copy data and update local state.
	    System.arraycopy(buf, offset, pipeBuffer, base, copyLen);
	    in = base + copyLen;
	    if (in == pipeBuffer.length)
	      in = 0;
	    count -= copyLen;
	    offset += copyLen;
	    // Tell anybody waiting for data.
	    lock.notifyAll();
	  }
      }
  }


  boolean isClosed ()
  {
    return closed;
  }

  // The associated writer.
  private PipedWriter writer;
  // True if this reader has been closed.
  boolean closed;

  // Index of next character to overwrite when receive() is called.
  // If -1, then that means the buffer is empty.
  private int in;
  // Index of next character to return from read().
  private int out;

  // The pipe buffer itself.
  private char[] pipeBuffer;
}
