/* SocketOutputStream.java -- OutputStream for PlainSocketImpl
   Copyright (C) 1998,2000 Free Software Foundation, Inc.

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

import java.io.OutputStream;
import java.io.IOException;

/**
  * This class is used internally by <code>PlainSocketImpl</code> to be the 
  * <code>OutputStream</code> subclass returned by its 
  * <code>getOutputStream method</code>.  It expects only to  be used in that 
  * context.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
class SocketOutputStream extends OutputStream
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
  * Build an instance of this class from a PlainSocketImpl object
  */
protected
SocketOutputStream(PlainSocketImpl impl)
{
  this.impl = impl;
}

/*************************************************************************/

/*
 * Instance Methods
 */

/**
  * This method closes the stream and the underlying socket connection.  This
  * action also effectively closes any other InputStream or OutputStream
  * object associated with the connection.
  *
  * @exception IOException If an error occurs
  */
public void
close() throws IOException
{
  impl.close();
}

/*************************************************************************/

/**
  * Hmmm, we don't seem to have a flush() method in Socket impl, so just
  * return for now, but this might need to be looked at later.
  *
  * @exception IOException Can't happen
  */
public void
flush() throws IOException
{
  return;
}

/*************************************************************************/

/**
  * Writes a byte (passed in as an int) to the given output stream
  * 
  * @param b The byte to write
  *
  * @exception IOException If an error occurs
  */
public void
write(int b) throws IOException
{
  byte buf[] = new byte[1];

  Integer i = new Integer(b);
  buf[0] = i.byteValue();

  write(buf, 0, buf.length);
}

/*************************************************************************/

/**
  * Write an array of bytes to the output stream
  *
  * @param buf The array of bytes to write
  *
  * @exception IOException If an error occurs
  */
public void
write(byte[] buf) throws IOException
{
  write(buf, 0, buf.length);
}

/*************************************************************************/

/**
  * Writes len number of bytes from the array buf to the stream starting
  * at offset bytes into the buffer.
  *
  * @param buf The buffer
  * @param offset Offset into the buffer to start writing from
  * @param len The number of bytes to write
  */
public void
write(byte[] buf, int offset, int len) throws IOException
{
  impl.write(buf, offset, len);
}

} // class SocketOutputStream

