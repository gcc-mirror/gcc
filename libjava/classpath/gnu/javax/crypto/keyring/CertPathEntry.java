/* CertPathEntry.java --
   Copyright (C) 2003, 2006 Free Software Foundation, Inc.

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


package gnu.javax.crypto.keyring;

import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.IOException;
import java.security.cert.Certificate;
import java.security.cert.CertificateEncodingException;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.util.Date;

/**
 * A primitive entry that contains a path of X.509 certificates.
 */
public final class CertPathEntry
    extends PrimitiveEntry
{
  public static final int TYPE = 8;
  private Certificate[] path;

  public CertPathEntry(Certificate[] path, Date creationDate,
                       Properties properties)
  {
    super(TYPE, creationDate, properties);
    if (path == null || path.length == 0)
      throw new IllegalArgumentException("no certificate path");
    this.path = (Certificate[]) path.clone();
  }

  private CertPathEntry()
  {
    super(TYPE);
  }

  public static CertPathEntry decode(DataInputStream in) throws IOException
  {
    CertPathEntry entry = new CertPathEntry();
    entry.properties.decode(in);
    entry.makeCreationDate();
    int len = in.readInt();
    MeteredInputStream in2 = new MeteredInputStream(in, len);
    try
      {
        CertificateFactory fact = CertificateFactory.getInstance("X.509");
        entry.path = (Certificate[]) fact.generateCertificates(in2).toArray(new Certificate[0]);
      }
    catch (CertificateException ce)
      {
        throw new MalformedKeyringException(ce.toString());
      }
    return entry;
  }

  public Certificate[] getCertPath()
  {
    return path;
  }

  protected void encodePayload() throws IOException
  {
    ByteArrayOutputStream bout = new ByteArrayOutputStream(1024);
    byte[] enc = null;
    try
      {
        for (int i = 0; i < path.length; i++)
          bout.write(path[i].getEncoded());
      }
    catch (CertificateEncodingException cee)
      {
        throw new IOException(cee.toString());
      }
    payload = bout.toByteArray();
  }
}
