/* ByteArrayRequestBodyWriter.java --
   Copyright (C) 2004 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

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


package gnu.java.net.protocol.http;

/**
 * A simple request body writer using a byte array.
 *
 * @author Chris Burdess (dog@gnu.org)
 */
public class ByteArrayRequestBodyWriter
  implements RequestBodyWriter
{

  /**
   * The content.
   */
  protected byte[] content;

  /**
   * The position within the content at which the next read will occur.
   */
  protected int pos;

  /**
   * Constructs a new byte array request body writer with the specified
   * content.
   * @param content the content buffer
   */
  public ByteArrayRequestBodyWriter(byte[] content)
  {
    this.content = content;
    pos = 0;
  }

  /**
   * Returns the total number of bytes that will be written in a single pass
   * by this writer.
   */
  public int getContentLength()
  {
    return content.length;
  }

  /**
   * Initialises the writer.
   * This will be called before each pass.
   */
  public void reset()
  {
    pos = 0;
  }

  /**
   * Writes body content to the supplied buffer.
   * @param buffer the content buffer
   * @return the number of bytes written
   */
  public int write(byte[] buffer)
  {
    int len = content.length - pos;
    len = (buffer.length < len) ? buffer.length : len;
    if (len > -1)
      {
        System.arraycopy(content, pos, buffer, 0, len);
        pos += len;
      }
    return len;
  }

}
