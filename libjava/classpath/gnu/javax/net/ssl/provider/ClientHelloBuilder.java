/* ClientHelloBuilder.java -- 
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
import java.util.List;

/**
 * Builder for {@link ClientHello} objects.
 * 
 * @author Casey Marshall (csm@gnu.org)
 */
public class ClientHelloBuilder extends ClientHello implements Builder
{
  public ClientHelloBuilder()
  {
    super(ByteBuffer.allocate(256));
  }

  /* (non-Javadoc)
   * @see gnu.javax.net.ssl.provider.Builder#buffer()
   */
  public ByteBuffer buffer()
  {
    return (ByteBuffer) buffer.duplicate().position(0).limit(length());
  }

  public void setVersion(final ProtocolVersion version)
  {
    ensureCapacity(2);
    buffer.putShort(0, (short) version.rawValue ());
  }

  public void setSessionId (final byte[] buffer)
  {
    setSessionId(buffer, 0, buffer.length);
  }

  public void setSessionId (final byte[] buffer, final int offset, final int length)
  {
    ensureCapacity(SESSID_OFFSET2 + length);
    int len = Math.min (32, length);
    this.buffer.put (SESSID_OFFSET, (byte) len);
    this.buffer.position (SESSID_OFFSET2);
    this.buffer.put (buffer, offset, len);
  }
  
  public void setCipherSuites(List<CipherSuite> suites)
  {
    int off = getCipherSuitesOffset();
    ensureCapacity(off + (2 * suites.size()) + 2);
    buffer.putShort(off, (short) (suites.size() * 2));
    int i = 2;
    for (CipherSuite suite : suites)
      {
        ((ByteBuffer) buffer.duplicate().position(off+i)).put(suite.id());
        i += 2;
      }
  }
  
  public void setCompressionMethods(List<CompressionMethod> methods)
  {
    int off = getCompressionMethodsOffset();
    ensureCapacity(off + methods.size() + 1);
    buffer.put(off, (byte) methods.size());
    for (CompressionMethod method : methods)
      buffer.put(++off, (byte) method.getValue());
  }

  public void setExtensionsLength (final int length)
  {
    if (length < 0 || length > 16384)
      throw new IllegalArgumentException("length must be nonnegative and not exceed 16384");
    int needed = getExtensionsOffset() + 2 + length;
    if (buffer.capacity() < needed)
      ensureCapacity(needed);
    buffer.putShort(getExtensionsOffset(), (short) length);
  }
  
  public void setExtensions(ByteBuffer extensions)
  {
    int elen = extensions.getShort(0) & 0xFFFF;
    setExtensionsLength(elen);
    ((ByteBuffer) buffer.duplicate().position(getExtensionsOffset())).put(extensions);
  }
  
  public void setDisableExtensions(boolean disableExtensions)
  {
    this.disableExtensions = disableExtensions;
  }
  
  public void ensureCapacity(final int length)
  {
    if (buffer.capacity() >= length)
      return;
    ByteBuffer newBuf = ByteBuffer.allocate(length);
    newBuf.put((ByteBuffer) buffer.position(0));
    newBuf.position(0);
    this.buffer = newBuf;
  }
}
