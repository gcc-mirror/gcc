/* Authenticator.java --ByteArrayResponseBodyReader.java --
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
 * Simple response body reader that stores content in a byte array.
 *
 * @author Chris Burdess (dog@gnu.org)
 */
public class ByteArrayResponseBodyReader
  implements ResponseBodyReader
{

  /**
   * The content.
   */
  protected byte[] content;

  /**
   * The position in the content at which the next write will occur.
   */
  protected int pos;

  /**
   * The length of the buffer.
   */
  protected int len;

  /**
   * Constructs a new byte array response body reader.
   */
  public ByteArrayResponseBodyReader()
  {
    this(4096);
  }
  
  /**
   * Constructs a new byte array response body reader with the specified
   * initial buffer size.
   * @param size the initial buffer size
   */
  public ByteArrayResponseBodyReader(int size)
  {
    content = new byte[size];
    pos = len = 0;
  }

  /**
   * This reader accepts all responses.
   */ 
  public boolean accept(Request request, Response response)
  {
    return true;
  }

  public void read(byte[] buffer, int offset, int length)
  {
    int l = length - offset;
    if (pos + l > content.length)
      {
        byte[] tmp = new byte[content.length * 2];
        System.arraycopy(content, 0, tmp, 0, pos);
        content = tmp;
      }
    System.arraycopy(buffer, offset, content, pos, l);
    pos += l;
    len = pos;
  }

  public void close()
  {
    pos = 0;
  }

  /**
   * Retrieves the content of this reader as a byte array.
   * The size of the returned array is the number of bytes read.
   */
  public byte[] toByteArray()
  {
    byte[] ret = new byte[len];
    System.arraycopy(content, 0, ret, 0, len);
    return ret;
  }
  
}

