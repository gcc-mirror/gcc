/* SSLRecordHandler.java -- a class that handles SSL record layer messages.
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
exception statement from your version.  */


package gnu.javax.net.ssl;

import java.nio.ByteBuffer;
import javax.net.ssl.SSLException;

public abstract class SSLRecordHandler
{
  private final byte contentType;

  /**
   * Create a new record handler for the given content type.
   */
  protected SSLRecordHandler (final byte contentType)
  {
    this.contentType = contentType;
  }

  /**
   * Handle an SSL record layer message, encapsulated in the supplied
   * input buffer, and writing any output bytes to the output
   * buffer. The input buffer is always only limited to the bytes that
   * encapsulate the <em>fragment</em> of the record layer message
   * &mdash; that is, the content-type, version, and length fields are
   * not present in the input buffer, and the limit of the input
   * buffer is always only as large as the fragment. If the message
   * being read is not contained entirely within the given buffer,
   * then the implementation should cache the bytes read as input, and
   * wait until subsequent calls finish the object being read.
   *
   * <p>Technically, we expect only APPLICATION messages to ever
   * produce output, but do suppose that extensions to the SSL
   * protocol could allow other channels that produce output.
   *
   * @param input The input buffer.
   * @param output The output buffer.
   */
  public abstract void handle (final ByteBuffer input,
                               final ByteBuffer output)
    throws SSLException;

  /**
   * Returns the record layer content type that this handler is for.
   *
   * @return The content type value.
   */
  public final byte contentType ()
  {
    return contentType;
  }

  public boolean equals (final Object o)
  {
    if (!(o instanceof SSLRecordHandler))
      return false;
    return ((SSLRecordHandler) o).contentType == contentType;
  }

  public int hashCode ()
  {
    return contentType & 0xFF;
  }
}