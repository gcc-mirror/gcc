/* CertificateBuilder.java --
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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.List;
import java.security.cert.CertificateException;

/**
 * Builder for {@link Certificate} objects.
 *
 * @author Casey Marshall (csm@gnu.org)
 */
public class CertificateBuilder extends Certificate implements Builder
{
  public CertificateBuilder(final CertificateType certType)
  {
    super(ByteBuffer.allocate(1024), certType);
  }

  /* (non-Javadoc)
   * @see gnu.javax.net.ssl.provider.Builder#buffer()
   */
  public ByteBuffer buffer()
  {
    return ((ByteBuffer) buffer.duplicate().position(0).limit(length())).slice();
  }

  public void setCertificates (final List<? extends java.security.cert.Certificate> certificates)
    throws CertificateException
  {
    ByteArrayOutputStream out = new ByteArrayOutputStream(1024);
    for (java.security.cert.Certificate cert : certificates)
      {
        byte[] encoded = cert.getEncoded();
        out.write((encoded.length >>> 16) & 0xFF);
        out.write((encoded.length >>>  8) & 0xFF);
        out.write( encoded.length         & 0xFF);
        try
          {
            out.write(encoded);
          }
        catch (IOException shouldNotHappen)
          {
            // ignore; this is a ByteArrayOutputStream.
          }
      }
    byte[] certs = out.toByteArray();
    // There is only one field in Certificate; so it is easy to reallocate.
    if (buffer.capacity() < certs.length + 3)
      buffer = ByteBuffer.allocate(certs.length + 3);
    buffer.put(0, (byte) (certs.length >>> 16));
    buffer.putShort(1, (short) certs.length);
    ((ByteBuffer) buffer.duplicate().position(3)).put(certs);
  }
}
