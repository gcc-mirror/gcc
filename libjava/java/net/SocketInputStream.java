/* SocketInputStream.java -- An InputStream for Sockets
   Copyright (C) 1998, 2000, 2002 Free Software Foundation, Inc.

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


package java.net;

import java.io.InputStream;
import java.io.IOException;

/**
  * This class contains an implementation of <code>InputStream</code> for 
  * sockets.  It in an internal only class used by <code>PlainSocketImpl</code>.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
class SocketInputStream extends InputStream
{

/*************************************************************************/

/*
 * Instance Variables
 */

/**
  * The PlainSocketImpl object this stream is associated with
  */
private PlainSocketImpl impl;

/*************************************************************************/

/*
 * Constructors
 */

/**
  * Builds an instance of this class from a PlainSocketImpl object
  */
protected
SocketInputStream(PlainSocketImpl impl)
{
  this.impl = impl;
}

/*************************************************************************/

/*
 * Instance Methods
 */

/**
  * Returns the number of bytes available to be read before blocking
  */
public int
available() throws IOException
{
  return(impl.available());
}

/*************************************************************************/

/**
  * Determines if "mark" functionality is supported on this stream.  For
  * sockets, this is always false.  Note that the superclass default is
  * false, but it is overridden out of safety concerns and/or paranoia.
  */
public boolean
markSupported()
{
  return(false);
}

/*************************************************************************/

/**
  * Do nothing mark method since we don't support this functionality.  Again,
  * overriding out of paranoia.
  *
  * @param readlimit In theory, the number of bytes we can read before the mark becomes invalid
  */
public void
mark(int readlimit)
{
}

/*************************************************************************/

/**
  * Since we don't support mark, this method always throws an exception
  *
  * @exception IOException Everytime since we don't support this functionality
  */
public void
reset() throws IOException
{
  throw new IOException("Socket InputStreams do not support mark/reset");
}

/*************************************************************************/

/**
  * This method not only closes the stream, it closes the underlying socket
  * (and thus any connection) and invalidates any other Input/Output streams
  * for the underlying impl object
  */
public void
close() throws IOException
{
  impl.close();
} 

/*************************************************************************/

/**
  * Reads the next byte of data and returns it as an int.  
  *
  * @return The byte read (as an int) or -1 if end of stream);
  *
  * @exception IOException If an error occurs.
  */
public int
read() throws IOException
{
  byte buf[] = new byte[1];

  int bytes_read = read(buf, 0, buf.length);
 
  if (bytes_read != -1)
    return(buf[0] & 0xFF);
  else
    return(-1);
}

/*************************************************************************/

/**
  * Reads up to buf.length bytes of data into the caller supplied buffer.
  *
  * @return The actual number of bytes read or -1 if end of stream
  *
  * @exception IOException If an error occurs.
  */
public int
read(byte[] buf) throws IOException
{
  return(read(buf, 0, buf.length));
}

/*************************************************************************/

/**
  * Reads up to len bytes of data into the caller supplied buffer starting
  * at offset bytes from the start of the buffer
  *
  * @return The number of bytes actually read or -1 if end of stream
  *
  * @exception IOException If an error occurs.
  */
public int
read(byte[] buf, int offset, int len) throws IOException
{
  int bytes_read = impl.read(buf, offset, len);
  if (bytes_read == 0)
    return(-1);

  return(bytes_read);
}

} // class SocketInputStream

