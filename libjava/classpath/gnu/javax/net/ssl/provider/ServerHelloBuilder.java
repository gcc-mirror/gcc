/* ServerHelloBuilder.java --
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


package gnu.javax.net.ssl.provider;

import java.nio.ByteBuffer;

/**
 * @author csm
 *
 */
public class ServerHelloBuilder extends ServerHello implements Builder
{
  public ServerHelloBuilder()
  {
    // Allocate a large enough buffer to hold a hello with the maximum
    // size session ID, and no extensions.
    super(ByteBuffer.allocate(SESSID_OFFSET2 + 35));
  }

  public ByteBuffer buffer()
  {
    return ((ByteBuffer) buffer.duplicate().position(0).limit(length())).slice();
  }

  // We don't reallocate the buffer in any of the following methods,
  // because we always allocate a large enough buffer for the base
  // object in the constructor.

  public void setVersion (final ProtocolVersion version)
  {
    buffer.putShort (0, (short) version.rawValue ());
  }

  public void setSessionId (final byte[] sessionId)
  {
    setSessionId (sessionId, 0, sessionId.length);
  }

  public void setSessionId (final byte[] sessionId, final int offset,
                            final int length)
  {
    if (length < 0 || length > 32)
      throw new IllegalArgumentException("length must be between 0 and 32");
    buffer.put(SESSID_OFFSET, (byte) length);
    ((ByteBuffer) buffer.duplicate().position(SESSID_OFFSET2))
      .put(sessionId, offset, length);
  }

  public void setCipherSuite (final CipherSuite suite)
  {
    int offset = SESSID_OFFSET + (buffer.get(SESSID_OFFSET) & 0xFF) + 1;
    ((ByteBuffer) buffer.duplicate().position(offset)).put(suite.id());
  }

  public void setCompressionMethod (final CompressionMethod comp)
  {
    int offset = SESSID_OFFSET + (buffer.get(SESSID_OFFSET) & 0xFF) + 3;
    buffer.put (offset, (byte) comp.getValue ());
  }

  // For extensions, we do reallocate the buffer.

  public void setDisableExtensions(boolean disable)
  {
    disableExtensions = disable;
  }

  public void setExtensionsLength (final int length)
  {
    if (length < 0 || length > 16384)
      throw new IllegalArgumentException("length must be nonnegative and not exceed 16384");
    int needed = SESSID_OFFSET2 + (buffer.get(SESSID_OFFSET) & 0xFF) + 5 + length;
    if (buffer.capacity() < needed)
      ensureCapacity(needed);
    buffer.putShort (SESSID_OFFSET2 + (buffer.get (SESSID_OFFSET) & 0xFF) + 3,
                     (short) length);
  }

  public void setExtensions(ByteBuffer extensions)
  {
    extensions = (ByteBuffer)
      extensions.duplicate().limit(extensions.position() + extensionsLength());
    ((ByteBuffer) buffer.duplicate().position(SESSID_OFFSET2
                                              + (buffer.get(SESSID_OFFSET) & 0xFF)
                                              )).put(extensions);
  }

  public void ensureCapacity(int newCapacity)
  {
    ByteBuffer newBuffer = ByteBuffer.allocate(newCapacity);
    newBuffer.put(buffer);
    newBuffer.position(0);
    buffer = newBuffer;
  }
}
