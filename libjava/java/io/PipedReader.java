/* PipedReader.java -- Input stream that reads from an output stream
   Copyright (C) 1998, 1999, 2000 Free Software Foundation, Inc.

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

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */


package java.io;

/**
  * This class is an input stream that reads its chars from an output stream
  * to which it is connected. 
  * <p>
  * Data is read and written to an internal buffer.  It is highly recommended
  * that the <code>PipedReader</code> and connected <code>PipedWriter</code>
  * be part of different threads.  If they are not, there is a possibility
  * that the read and write operations could deadlock their thread.
  *
  * @version 0.0
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public class PipedReader extends Reader
{

/*************************************************************************/

/*
 * Class Variables
 */

/**
  * The size of the internal buffer used for input/output.  Note that this
  * can be overriden by setting the system property 
  * <code>gnu.java.io.PipedReader.pipe_size</code> to the desired size shown 
  * in chars.  This is not a standard part of the class library.  Note that 
  * since this variable is <code>final</code>, it cannot be changed to refect 
  * the size specified in the property.
  * <p>
  * The value for this variable is 2048.
  */
private static final int PIPE_SIZE = 2048;

/**
  * This is the real pipe size.  It defaults to PIPE_SIZE, unless overridden
  * by use of the system property <code>gnu.java.io.PipedReader.pipe_size</code>.
  */
private static int pipe_size;

static
{
  pipe_size =  Integer.getInteger("gnu.java.io.PipedReader.pipe_size",
                                  PIPE_SIZE).intValue();
}

/*************************************************************************/

/*
 * Instance Variables
 */

/**
  * This is the internal circular buffer used for storing chars written
  * to the pipe and from which chars are read by this stream
  */
private char[] buffer = new char[pipe_size];

/**
  * The index into buffer where the chars written char the connected
  * <code>PipedWriter</code> will be written.  If this variables is less
  * than 0, then the buffer is empty.  If this variable is equal to 
  * <code>out</code>, then the buffer is full
  */
private int in = -1;

/**
  * This index into the buffer where chars will be read from.
  */
private int out = 0;

/**
  * This variable is <code>true</code> if this object has ever been connected
  * to a <code>PipedWriter</code>, and <code>false</code> otherwise.  It is used
  * to detect an attempt to connect an already connected stream or to
  * otherwise use the stream before it is connected.
  */
private boolean ever_connected = false;

/**
  * This variable is set to <code>true</code> if the <code>close()</code> method is
  * called.  This value is checked prevents a caller from re-opening the
  * stream.
  */
private boolean closed = false;

/**
  * This variable is the PipedWriter to which this stream is connected.
  */
PipedWriter src;

/*************************************************************************/

/*
 * Constructors
 */

/**
  * This constructor creates a new <code>PipedReader</code> that is not 
  * connected to a <code>PipedWriter</code>.  It must be connected before
  * chars can be read from this stream.
  */
public
PipedReader()
{
  return;
}

/*************************************************************************/

/**
  * This constructor creates a new <code>PipedReader</code> and connects
  * it to the passed in <code>PipedWriter</code>. The stream is then read
  * for reading.
  *
  * @param src The <code>PipedWriter</code> to connect this stream to
  *
  * @exception IOException If an error occurs
  */
public
PipedReader(PipedWriter src) throws IOException
{
  connect(src);
}

/*************************************************************************/

/*
 * Instance Variables
 */

/**
  * This method connects this stream to the passed in <code>PipedWriter</code>.
  * This stream is then ready for reading.  If this stream is already
  * connected or has been previously closed, then an exception is thrown
  *
  * @param src The <code>PipedWriter</code> to connect this stream to
  *
  * @exception IOException If an error occurs
  */
public void
connect(PipedWriter src) throws IOException
{
  if (src == this.src)
    return;

  if (ever_connected)
    throw new IOException("Already connected");

  if (closed)
    throw new IOException("Stream is closed and cannot be reopened");

  synchronized (lock)
    {
      src.connect(this);

      ever_connected = true;
    } // synchronized
}

/*************************************************************************/

/**
  * This methods closes the stream so that no more data can be read
  * from it.
  *
  * @exception IOException If an error occurs
  */
public void
close() throws IOException
{
  synchronized (lock)
    {
      closed = true;
      notifyAll();
    } // synchronized
}

/*************************************************************************/

/**
  * This method determines whether or not this stream is ready to be read.
  * If this metho returns <code>false</code> an attempt to read may (but is
  * not guaranteed to) block.
  *
  * @return <code>true</code> if this stream is ready to be read, <code>false</code> otherwise
  *
  * @exception IOException If an error occurs
  */
public boolean
ready() throws IOException
{
  if (in == -1)
    return(false);

  if (out == (in - 1))
    return(false);

  if ((out == pipe_size) && (in == 0))
    return(false);

  return(true);
}

/*************************************************************************/

/**
  * This method reads a single char from the pipe and returns it as an
  * <code>int</code>.
  * <p>
  * This method will block if no chars are available to be read.
  *
  * @return An char read from the pipe, or -1 if the end of stream is 
  * reached.
  *
  * @exception IOException If an error occurs.
  */
public int
read() throws IOException
{
  char[] buf = new char[1];

  return(read(buf, 0, buf.length));
}

/*************************************************************************/

/**
  * This method reads chars from the stream into a caller supplied buffer.
  * It starts storing chars at position <code>offset</code> into the buffer and
  * reads a maximum of <cod>>len</code> chars.  Note that this method can actually
  * read fewer than <code>len</code> chars.  The actual number of chars read is
  * returned.  A -1 is returned to indicated that no chars can be read
  * because the end of the stream was reached.  If the stream is already
  * closed, a -1 will again be returned to indicate the end of the stream.
  * <p>
  * This method will block if no chars are available to be read.
  *
  * @param buf The buffer into which chars will be stored
  * @param offset The index into the buffer at which to start writing.
  * @param len The maximum number of chars to read.
  */
public int
read(char[] buf, int offset, int len) throws IOException
{
  if (!ever_connected)
    throw new IOException("Not connected"); 

  synchronized (lock)
    {
      int chars_read = 0;
      for (;;)
	{
	  // If there are chars, take them
	  if (in != -1)
	    {
	      int desired_chars = len - chars_read;

	      // We are in a "wrap" condition
	      if (out > in)
		{
		  if (desired_chars > (pipe_size - out))
		    {
		      if (in == 0)
			desired_chars = (pipe_size - out) - 1;
		      else
			desired_chars = pipe_size - out;

		      System.arraycopy(buffer, out, buf, offset + chars_read,
				       desired_chars);

		      chars_read += desired_chars;
		      out += desired_chars;
		      desired_chars = len - chars_read;

		      if (out == pipe_size)
			out = 0;

		      notifyAll();
		    }
		  else
		    {
		      if ((out + desired_chars) == in)
			--desired_chars;

		      if (((out + desired_chars) == pipe_size) && (in == 0)) 
			desired_chars = (pipe_size - out) - 1;

		      System.arraycopy(buffer, out, buf, offset + chars_read,
				       desired_chars); 

		      chars_read += desired_chars;
		      out += desired_chars;
		      desired_chars = len - chars_read;

		      if (out == pipe_size)
			out = 0;

		      notifyAll();
		    }
		}
 
	      // We are in a "no wrap" or condition (can also be fall through
	      // from above
	      if (in > out)
		{
		  if (desired_chars >= ((in - out) - 1))
		    desired_chars = (in - out) - 1;

		  System.arraycopy(buffer, out, buf, offset + chars_read, 
				   desired_chars);

		  chars_read += desired_chars;
		  out += desired_chars;
		  desired_chars = len - chars_read;

		  if (out == pipe_size)
		    out = 0;

		  notifyAll();
		}
	    }

	  // If we are done, return
	  if (chars_read == len)
	    return(chars_read);

	  // Return a short count if necessary
	  if (chars_read > 0 && chars_read < len)
	    return(chars_read);

	  // Handle the case where the end of stream was encountered.
	  if (closed)
	    {
	      // We never let in == out so there might be one last char
	      // available that we have not copied yet.
	      if (in != -1)
		{
		  buf[offset + chars_read] = buffer[out];
		  in = -1;
		  ++out;
		  ++chars_read;
		}

	      if (chars_read != 0)
		return(chars_read);
	      else
		return(-1);
	    }

	  // Wait for a char to be read
	  try
	    {
	      wait();
	    }
	  catch(InterruptedException e) { ; }
	} 
    } // synchronized
}

/*************************************************************************/

/**
  * This method is used by the connected <code>PipedWriter</code> to
  * write chars into the buffer.  It uses this method instead of directly
  * writing the chars in order to obtain ownership of the object's monitor
  * for the purposes of calling <code>notify</code>.
  *
  * @param buf The array containing chars to write to this stream
  * @param offset The offset into the array to start writing from
  * @param len The number of chars to write.
  *
  * @exception IOException If an error occurs
  */
void
write(char[] buf, int offset, int len) throws IOException
{
  if (len <= 0)
    return;

  synchronized (lock)
    {
      int total_written = 0;
      while (total_written < len)
	{
	  // If we are not at the end of the buffer with out = 0
	  if (!((in == (buffer.length - 1)) && (out == 0)))
	    {
	      // This is the "no wrap" situation
	      if ((in - 1) >= out)
		{
		  int chars_written = 0;
		  if ((buffer.length - in) > (len - total_written))
		    chars_written = (len - total_written);
		  else if (out == 0)
		    chars_written = (buffer.length - in) - 1;
		  else 
		    chars_written = (buffer.length - in);

		  if (chars_written > 0) 
		    System.arraycopy(buf, offset + total_written, buffer, in, 
				     chars_written);
		  total_written += chars_written;
		  in += chars_written;

		  if (in == buffer.length)
		    in = 0;

		  notifyAll();
		}
	      // This is the "wrap" situtation
	      if ((out > in) && (total_written != len))
		{
		  int chars_written = 0;

		  // Do special processing if we are at the beginning
		  if (in == -1)
		    {
		      in = 0;

		      if (buffer.length > len)
			chars_written = len;
		      else
			chars_written = buffer.length - 1;
		    }
		  else if (((out - in) - 1) < (len - total_written))
		    {
		      chars_written = (out - in) - 1;
		    }
		  else
		    {
		      chars_written = len - total_written;
		    }

		  // If the buffer is full, wait for it to empty out
		  if ((out - 1) == in)
		    {
		      try
			{         
			  wait(); 
			}
		      catch (InterruptedException e) 
			{ 
			  continue; 
			}
		    }

		  System.arraycopy(buf, offset + total_written, buffer, in,
				   chars_written);
		  total_written += chars_written;
		  in += chars_written;

		  if (in == buffer.length)
		    in = 0;

		  notifyAll();
		}
	    }
	  // Wait for some reads to occur before we write anything.
	  else
	    {
	      try
		{
		  wait();
		}
	      catch (InterruptedException e) { ; }
	    }
	}
    } // synchronized
}

} // class PipedReader

