/* NamedInputStream.java - 
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

package gnu.xml.libxmlj.util;

import java.io.FilterInputStream;
import java.io.InputStream;
import java.io.IOException;
import java.io.PushbackInputStream;

/**
 * An input stream associated with an XML system ID.
 * It can report the system ID and the first few bytes of the stream
 * in order to detect the character encoding of the stream.
 *
 * @author <a href='dog@gnu.org'>Chris Burdess</a>
 */
public class NamedInputStream
extends FilterInputStream
{

  private static int DETECT_BUFFER_SIZE = 50;
  
  private String name;

  NamedInputStream (String name, InputStream in, int size)
  {
    super (new PushbackInputStream (in, size));
    this.name = name;
  }

  /**
   * Returns the name of the stream (the XML system ID).
   */
  public String getName ()
  {
    return name;
  }

  /**
   * Returns the first few bytes of the stream for character encoding
   * purposes. The entire stream can thereafter be read normally from the
   * beginning. This method is only valid if no bytes have yet been read
   * from the stream.
   */
  public byte[] getDetectBuffer ()
    throws IOException
  {
    PushbackInputStream p = (PushbackInputStream) in;
    byte[] buffer = new byte[DETECT_BUFFER_SIZE];
    int len = p.read (buffer);
    if (len < 0)
      {
        return null;
      }
    else
      {
        p.unread (buffer, 0, len);
        byte[] ret = new byte[len];
        System.arraycopy (buffer, 0, ret, 0, len);
        return ret;
      }
  }
  
}
