/* Copyright (C) 1998, 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package java.io;

/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date October 29, 1998.  
 */
/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct.
 */
 
public class PipedInputStream extends InputStream
{
  /* The size of the pipe's circular input buffer. */
  protected static final int PIPE_SIZE = 1024;

  /* The circular buffer into which incoming data is placed. */
  protected byte[] buffer;

  /* The index in the buffer at which the next byte of data will be stored. */
  protected int in = -1;

  /* The index in the buffer at which the next byte of data will be read. */
  protected int out = 0;

  /* The output stream this is connected to; used to check for errors. */
  private PipedOutputStream po = null;

  /* Flag to indicate that the output stream was closed. */
  private boolean outClosed = false;

  public PipedInputStream(PipedOutputStream src) throws IOException
  {
    buffer = new byte[PIPE_SIZE];
    connect(src);
  }

  public PipedInputStream()
  {
    buffer = new byte[PIPE_SIZE];
  }

  public synchronized int available() throws IOException
  {
    if (in < 0)
      return 0;

    if (in > out)
      return in - out;

    // Buffer has wrapped around.
    return buffer.length - out + in;
  }

  public void close() throws IOException
  {
    buffer = null;
    po = null;

    // Mark as empty for available method.
    in = -1;
  }

  public void connect(PipedOutputStream src) throws IOException
  {
    if (buffer == null)
      throw new IOException("pipe closed");

    if (po != null)
      if (po == src)
	return;
      else
        throw new IOException("pipe already connected");

    po = src;
    try
    {
      src.connect(this);
    }
    catch (IOException ex)
    {
      po = null;
      throw ex;
    }
  }

  public synchronized int read() throws IOException
  {
    // TBD: Spec says to throw IOException if thread writing to output stream
    // died.  What does this really mean?  Theoretically, multiple threads
    // could be writing to this object.  Do you track the first, last, or
    // all of them?
    if (po == null)
      if (buffer == null)
        throw new IOException("pipe closed");
      else
        throw new IOException("pipe unconnected");

    // Block until there's something to read or output stream was closed.
    while (in < 0)
      try
	{
	  if (outClosed)
	    return -1;
	  wait();
	}
      catch (InterruptedException ex)
       {
	 throw new InterruptedIOException();
       }

    // Let other threads know there's room to write now.
    notifyAll();

    int retval = buffer[out++] & 0xFF;

    // Wrap back around if at end of the array.
    if (out >= buffer.length)
      out = 0;

    // When the last byte available is read, mark the buffer as empty.
    if (out == in)
      {
        in = -1;
	out = 0;
      }
      
    return retval;
  }

  public synchronized int read(byte[] b, int off, int len) throws IOException
  {
    if (off < 0 || len < 0 || off + len > b.length)
      throw new ArrayIndexOutOfBoundsException();

    // TBD: Spec says to throw IOException if thread writing to output stream
    // died.  What does this really mean?  Theoretically, multiple threads
    // could be writing to this object.  Do you track the first, last, or
    // all of them?
    if (po == null)
      if (buffer == null)
        throw new IOException("pipe closed");
      else
        throw new IOException("pipe unconnected");

    // Block until there's something to read or output stream was closed.
    while (in < 0)
      try
	{
	  if (outClosed)
	    return -1;
	  wait();
	}
      catch (InterruptedException ex)
       {
	 throw new InterruptedIOException();
       }

    // Let other threads know there's room to write now.
    notifyAll();

    int numRead;
    len = Math.min(len, available());
    if (in <= out && len >= (numRead = buffer.length - out))
      {
	// Buffer has wrapped around; need to copy in 2 steps.
	// Copy to the end of the buffer first; second copy may be of zero
	// bytes but that is ok.  Doing it that way saves having to check
	// later if 'out' has grown to buffer.length.
        System.arraycopy(buffer, out, b, off, numRead);
	len -= numRead;
	off += numRead;
	out = 0;
      }
    else
      numRead = 0;

    System.arraycopy(buffer, out, b, off, len);
    numRead += len;
    out += len;

    // When the last byte available is read, mark the buffer as empty.
    if (out == in)
      {
        in = -1;
	out = 0;
      }

    return numRead;
  }

  protected synchronized void receive(int b) throws IOException
  {
    if (buffer == null)
      throw new IOException("pipe closed");

    // TBD: Spec says to throw IOException if thread reading from input stream
    // died.  What does this really mean?  Theoretically, multiple threads
    // could be reading to this object (why else would 'read' be synchronized?).
    // Do you track the first, last, or all of them?

    if (b < 0)
      {
        outClosed = true;
	notifyAll();	// In case someone was blocked in a read.
	return;
      }

    // Block until there's room in the pipe.
    while (in == out)
      try
	{
	  wait();
	}
      catch (InterruptedException ex)
       {
	 throw new InterruptedIOException();
       }

    // Check if buffer is empty.
    if (in < 0)
      in = 0;

    buffer[in++] = (byte) b;

    // Wrap back around if at end of the array.
    if (in >= buffer.length)
      in = 0;

    // Let other threads know there's something to read when this returns.
    notifyAll();
  }
}
