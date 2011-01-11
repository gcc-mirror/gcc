/* ByteBufferOutputStream.java -- output stream with a growable underlying
                                  byte buffer.
   Copyright (C) 2006  Free Software Foundation, Inc.

This file is a part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
USA

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


package gnu.java.security.util;

import java.io.IOException;
import java.io.OutputStream;

import java.nio.ByteBuffer;

/**
 * An output stream that writes bytes to a ByteBuffer, which will be resized
 * if more space is needed.
 *
 * @author Casey Marshall (csm@gnu.org)
 */
public class ByteBufferOutputStream extends OutputStream
{
  private ByteBuffer buffer;

  public ByteBufferOutputStream()
  {
    this(256);
  }

  public ByteBufferOutputStream(int initialCapacity)
  {
    buffer = ByteBuffer.allocate(initialCapacity);
  }

  /* (non-Javadoc)
   * @see java.io.OutputStream#write(int)
   */
  public @Override synchronized void write(int b) throws IOException
  {
    if (!buffer.hasRemaining())
      growBuffer();
    buffer.put((byte) b);
  }

  public @Override synchronized void write(byte[] b, int offset, int length)
  {
    if (buffer.remaining() < length)
      growBuffer();
    buffer.put(b, offset, length);
  }

  public @Override void write(byte[] b)
  {
    write(b, 0, b.length);
  }

  /**
   * Get the current state of the buffer. The returned buffer will have
   * its position set to zero, its capacity set to the current limit,
   * and its limit set to its capacity.
   *
   * @return The buffer.
   */
  public ByteBuffer buffer()
  {
    return ((ByteBuffer) buffer.duplicate().flip()).slice();
  }

  public String toString()
  {
    return super.toString() + " [ buffer: " + buffer + " ]";
  }

  private void growBuffer()
  {
    int newCapacity = buffer.capacity();
    if (newCapacity < 16384) // If the buffer isn't huge yet, double its size
      newCapacity = newCapacity << 1;
    else // Otherwize, increment by a bit.
      newCapacity += 4096;
    ByteBuffer newBuffer = ByteBuffer.allocate(newCapacity);
    buffer.flip();
    newBuffer.put(buffer);
    buffer = newBuffer;
  }
}
