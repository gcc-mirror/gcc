/* PipedOutputStream.java -- Write portion of piped streams.
   Copyright (C) 1998, 2000, 2001, 2003 Free Software Foundation, Inc.

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

// NOTE: This implementation is very similar to that of PipedWriter.  If you 
// fix a bug in here, chances are you should make a similar change to the 
// PipedWriter code.

/**
  * This class writes its bytes to a <code>PipedInputStream</code> to 
  * which it is connected.
  * <p>
  * It is highly recommended that a <code>PipedOutputStream</code> and its
  * connected <code>PipedInputStream</code> be in different threads.  If 
  * they are in the same thread, read and write operations could deadlock
  * the thread.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public class PipedOutputStream extends OutputStream
{
  /** Target PipedInputStream to which this is connected. Null only if this 
    * OutputStream hasn't been connected yet. */
  PipedInputStream sink;
  
  /** Set to true if close() has been called on this OutputStream. */
  boolean closed;
  
  /** 
    * Create an unconnected PipedOutputStream.  It must be connected
    * to a <code>PipedInputStream</code> using the <code>connect</code>
    * method prior to writing any data or an exception will be thrown.
    */
  public PipedOutputStream()
  {
  }

  /**
     * Create a new <code>PipedOutputStream</code> instance
    * to write to the specified <code>PipedInputStream</code>.  This stream
    * is then ready for writing.
    *
    * @param sink The <code>PipedInputStream</code> to connect this stream to.
    *
    * @exception IOException If <code>sink</code> has already been connected 
    *                        to a different PipedOutputStream.
    */
  public PipedOutputStream(PipedInputStream sink) throws IOException
  {
    sink.connect(this);
  }

  /**
    * Connects this object to the specified <code>PipedInputStream</code> 
    * object.  This stream will then be ready for writing.
    *
    * @param sink The <code>PipedInputStream</code> to connect this stream to
    *
    * @exception IOException If the stream has not been connected or has
    *                        been closed.
    */
  public void connect(PipedInputStream sink) throws IOException
  {
    if (this.sink != null || sink.source != null)
      throw new IOException ("Already connected");
    sink.connect(this);
  }

  /**
    * Write a single byte of date to the stream.  Note that this method will 
    * block if the <code>PipedInputStream</code> to which this object is 
    * connected has a full buffer.
    *
    * @param b The byte of data to be written, passed as an <code>int</code>.
    *
    * @exception IOException If the stream has not been connected or has
    *                        been closed.
    */  
  public void write(int b) throws IOException
  {
    if (sink == null)
      throw new IOException ("Not connected");
    if (closed)
      throw new IOException ("Pipe closed");
      
    sink.receive (b);
  }
  
  /**
    * This method writes <code>len</code> bytes of data from the byte array
    * <code>buf</code> starting at index <code>offset</code> in the array
    * to the stream.  Note that this method will block if the  
    * <code>PipedInputStream</code> to which this object is connected has
    * a buffer that cannot hold all of the bytes to be written.
    *
    * @param buffer The array containing bytes to write to the stream.
    * @param offset The index into the array to start writing bytes from.
    * @param len The number of bytes to write.
    *
    * @exception IOException If the stream has not been connected or has
    *                        been closed.
    */
  public void write(byte[] buffer, int offset, int len) throws IOException
  {
    if (sink == null)
      throw new IOException ("Not connected");
    if (closed)
      throw new IOException ("Pipe closed");
      
    sink.receive(buffer, offset, len);
  }

  /**
    * This method does nothing.
    *
    * @exception IOException If the stream is closed.
    * @specnote You'd think that this method would block until the sink
    *           had read all available data. Thats not the case - this method
    *           appears to be a no-op?
    */
  public void flush() throws IOException
  {
  }
  
  /**
    * This method closes this stream so that no more data can be written
    * to it. Any further attempts to write to this stream may throw an
    * exception
    *
    * @exception IOException If an error occurs
    */
  public void close() throws IOException
  {
    // A close call on an unconnected PipedOutputStream has no effect.
    if (sink != null)
      {
	closed = true;
	// Notify any waiting readers that the stream is now closed.
	synchronized (sink)
	{	  
	  sink.notifyAll();
	}
      }
  }
}
