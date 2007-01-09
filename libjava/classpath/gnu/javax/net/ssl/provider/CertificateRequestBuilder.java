/* CertificateRequestBuilder.java -- 
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

import static gnu.javax.net.ssl.provider.CertificateRequest.ClientCertificateType;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.List;

import javax.security.auth.x500.X500Principal;

/**
 * Builder for {@link CertificateRequest} objects.
 * 
 * @author Casey Marshall (csm@gnu.org)
 */
public class CertificateRequestBuilder extends CertificateRequest
  implements Builder
{
  public CertificateRequestBuilder()
  {
    super(ByteBuffer.allocate(1024));
  }

  /* (non-Javadoc)
   * @see gnu.javax.net.ssl.provider.Builder#buffer()
   */
  public ByteBuffer buffer()
  {
    return ((ByteBuffer) buffer.duplicate().limit(length())).slice();
  }

  public void setTypes(List<ClientCertificateType> types)
  {
    ensureCapacity(types.size() + 3);
    buffer.put(0, (byte) types.size());
    ByteBuffer b = (ByteBuffer) buffer.duplicate().position(1);
    for (ClientCertificateType type : types)
      b.put((byte) type.getValue());
  }
  
  public void setAuthorities(List<X500Principal> authorities)
  {
    ByteArrayOutputStream out = new ByteArrayOutputStream(1024);
    for (X500Principal auth : authorities)
      {
        byte[] encoded = auth.getEncoded();
        out.write((encoded.length >>> 8) & 0xFF);
        out.write( encoded.length        & 0xFF);
        try
          {
            out.write(encoded);
          }
        catch (IOException ignored)
          {
            // Ignored; we use a ByteArrayOutputStream.
          }
      }
    byte[] auths = out.toByteArray();
    int typesLen = 1 + (buffer.get(0) & 0xFF);
    int len = typesLen + auths.length + 2;
    ensureCapacity(len);
    buffer.putShort(typesLen, (short) auths.length);
    ((ByteBuffer) buffer.duplicate().position(typesLen + 2)).put(auths);
  }
  
  public void ensureCapacity(final int capacity)
  {
    if (buffer.capacity() >= capacity)
      return;
    ByteBuffer newBuffer = ByteBuffer.allocate(capacity);
    newBuffer.duplicate().put(buffer);
    buffer = newBuffer;
  }
}
