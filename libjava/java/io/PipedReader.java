/* PipedReader.java -- Read portion of piped character streams.
   Copyright (C) 1998, 1999, 2000, 2001 Free Software Foundation, Inc.

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.
 
GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */

package java.io;

// NOTE: This implementation is very similar to that of PipedInputStream. 
// If you fix a bug in here, chances are you should make a similar change to 
// the PipedInputStream code.

/**
  * An input stream that reads characters from a piped writer to which it is 
  * connected. 
  * <p>
  * Data is read and written to an internal buffer.  It is highly recommended
  * that the <code>PipedReader</code> and connected <code>PipedWriter</code>
  * be part of different threads.  If they are not, there is a possibility
  * that the read and write operations could deadlock their thread.
  *
  * @specnote The JDK implementation appears to have some undocumented 
  *           functionality where it keeps track of what thread is writing
  *           to pipe and throws an IOException if that thread susequently
  *           dies. This behaviour seems dubious and unreliable - we don't
  *           implement it.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public class PipedReader extends Reader
{
  /** PipedWriter to which this is connected. Null only if this 
    * Reader hasn't been connected yet. */
  PipedWriter source;

  /** Set to true if close() has been called on this Reader. */
  boolean closed;

  /**
    * The size of the internal buffer used for input/output.
    */
  static final int PIPE_SIZE = 2048;

  /**
    * This is the internal circular buffer used for storing chars written
    * to the pipe and from which chars are read by this stream
    */
  char[] buffer = new char[PIPE_SIZE];

  /**
    * The index into buffer where the next char from the connected
    * <code>PipedWriter</code> will be written. If this variable is 
    * equal to <code>out</code>, then the buffer is full. If set to < 0,
    * the buffer is empty.
    */
  int in = -1;

  /**
    * This index into the buffer where chars will be read from.
    */
  int out = 0;

  /** Buffer used to implement single-argument read/receive */
  char[] read_buf = new char[1];

  /**
    * Creates a new <code>PipedReader</code> that is not connected to a 
    * <code>PipedWriter</code>.  It must be connected before chars can 
    * be read from this stream.
    */
  public PipedReader()
  {
  }

  /**
    * This constructor creates a new <code>PipedReader</code> and connects
    * it to the passed in <code>PipedWriter</code>. The stream is then 
    * ready for reading.
    *
    * @param source The <code>PipedWriter</code> to connect this stream to
    *
    * @exception IOException If <code>source</code> is already connected.
    */
  public PipedReader(PipedWriter source) throws IOException
  {
    connect(source);
  }

  /**
    * This method connects this stream to the passed in 
    * <code>PipedWriter</code>.
    * This stream is then ready for reading.  If this stream is already
    * connected or has been previously closed, then an exception is thrown
    *
    * @param source The <code>PipedWriter</code> to connect this stream to
    *
    * @exception IOException If this PipedReader or <code>source</code> 
    *                        has been connected already.
    */
  public void connect(PipedWriter source) throws IOException
  {
    // The JDK (1.3) does not appear to check for a previously closed 
    // connection here.
    
    if (this.source != null || source.sink != null)
      throw new IOException ("Already connected");
    
    source.sink = this;
    this.source = source;
  }
  
  /**
    * This method is used by the connected <code>PipedWriter</code> to
    * write chars into the buffer.
    *
    * @param buf The array containing chars to write to this stream
    * @param offset The offset into the array to start writing from
    * @param len The number of chars to write.
    *
    * @exception IOException If an error occurs
    * @specnote This code should be in PipedWriter.write, but we
    *           put it here in order to support that bizarre recieve(int)
    *           method.
    */  
  void receive(char[] buf, int offset, int len)
    throws IOException
  {
    synchronized (lock)
    {
      if (closed)
	throw new IOException ("Pipe closed");

      int bufpos = offset;
      int copylen;

      while (len > 0)
	{
          try
	    {
	      while (in == out)
		{
		  // The pipe is full. Wake up any readers and wait for them.
		  lock.notifyAll();
		  lock.wait();
		  // The pipe could have been closed while we were waiting.
	          if (closed)
		    throw new IOException ("Pipe closed");
		}
	    }
	  catch (InterruptedException ix)
	    {
              throw new InterruptedIOException ();
	    }

	  if (in < 0) // The pipe is empty.
	    in = 0;

	  // Figure out how many chars from buf can be copied without 
	  // overrunning out or going past the length of the buffer.
	  if (in < out)
	    copylen = Math.min (len, out - in);
	  else
	    copylen = Math.min (len, buffer.length - in);

	  // Copy chars until the pipe is filled, wrapping if necessary.
	  System.arraycopy(buf, bufpos, buffer, in, copylen);
	  len -= copylen;
	  bufpos += copylen;
	  in += copylen;
	  if (in == buffer.length)
	    in = 0;
	}
      // Notify readers that new data is in the pipe.
      lock.notifyAll();
    }
  }
  
  /**
    * This method reads chars from the stream into a caller supplied buffer.
    * It starts storing chars at position <code>offset</code> into the 
    * buffer and
    * reads a maximum of <code>len</code> chars.  Note that this method 
    * can actually
    * read fewer than <code>len</code> chars.  The actual number of chars 
    * read is
    * returned.  A -1 is returned to indicated that no chars can be read
    * because the end of the stream was reached.  If the stream is already
    * closed, a -1 will again be returned to indicate the end of the stream.
    * <p>
    * This method will block if no char is available to be read.
    */
  public int read() throws IOException
  {
    // Method operates by calling the multichar overloaded read method
    // Note that read_buf is an internal instance variable.  I allocate it
    // there to avoid constant reallocation overhead for applications that
    // call this method in a loop at the cost of some unneeded overhead
    // if this method is never called.

    int r = read(read_buf, 0, 1);
    return r != -1 ? read_buf[0] : -1;
  }
  
  /**
    * This method reads characters from the stream into a caller supplied 
    * buffer. It starts storing chars at position <code>offset</code> into 
    * the buffer and reads a maximum of <code>len</code> chars.  Note that 
    * this method can actually read fewer than <code>len</code> chars.  
    * The actual number of chars read is
    * returned.  A -1 is returned to indicated that no chars can be read
    * because the end of the stream was reached - ie close() was called on the
    * connected PipedWriter.
    * <p>
    * This method will block if no chars are available to be read.
    *
    * @param buf The buffer into which chars will be stored
    * @param offset The index into the buffer at which to start writing.
    * @param len The maximum number of chars to read.
    *
    * @exception IOException If <code>close()</code> was called on this Piped
    *                        Reader.
    */  
  public int read(char[] buf, int offset, int len)
    throws IOException
  {
    synchronized (lock)
    {
      if (source == null)
	throw new IOException ("Not connected");
      if (closed)
	throw new IOException ("Pipe closed");

      // If the buffer is empty, wait until there is something in the pipe 
      // to read.
      try
	{
	  while (in < 0)
	    {
	      if (source.closed)
		return -1;
	      lock.wait();
	    }
	}
      catch (InterruptedException ix)
	{
          throw new InterruptedIOException();
	}

      int total = 0;
      int copylen;

      while (true)
	{
	  // Figure out how many chars from the pipe can be copied without 
	  // overrunning in or going past the length of buf.
	  if (out < in)
	    copylen = Math.min (len, in - out);
	  else
	    copylen = Math.min (len, buffer.length - out);

          System.arraycopy (buffer, out, buf, offset, copylen);
	  offset += copylen;
	  len -= copylen;
	  out += copylen;
	  total += copylen;

	  if (out == buffer.length)
	    out = 0;

	  if (out == in)
	    {
	      // Pipe is now empty.
	      in = -1;
	      out = 0;
	    }

          // If output buffer is filled or the pipe is empty, we're done.
	  if (len == 0 || in == -1)
	    {
	      // Notify any waiting Writer that there is now space
	      // to write.
	      lock.notifyAll();
	      return total;
	    }
	}
    }
  }
  
  public boolean ready() throws IOException
  {
    // The JDK 1.3 implementation does not appear to check for the closed or 
    // unconnected stream conditions here.  However, checking for a
    // closed stream is explicitly required by the JDK 1.2 and 1.3
    // documentation (for Reader.close()), so we do it.
    
    synchronized (lock)
    {
      if (closed)
	throw new IOException("Pipe closed");

      if (in < 0)
	return false;

      int count;
      if (out < in)
	count = in - out;
      else
	count = (buffer.length - out) - in;

      return (count > 0);
    }
  }
  
  /**
  * This methods closes the stream so that no more data can be read
  * from it.
  *
  * @exception IOException If an error occurs
  */
  public void close() throws IOException
  {
    synchronized (lock)
    {
      closed = true;
      // Wake any thread which may be in receive() waiting to write data.
      lock.notifyAll();
    }
  }
}

