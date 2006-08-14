/* CertificateEntry.java -- 
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

import java.io.DataInputStream;
import java.io.IOException;
import java.security.cert.Certificate;
import java.security.cert.CertificateEncodingException;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.util.Date;

/**
 * An immutable class representing a trusted certificate entry.
 */
public final class CertificateEntry
    extends PrimitiveEntry
{
  public static final int TYPE = 5;
  /** The certificate. */
  private Certificate certificate;

  /**
   * Creates a new certificate entry.
   * 
   * @param certificate The certificate.
   * @param creationDate The creation date.
   * @param properties The alias.
   * @throws IllegalArgumentException If any argument is null, or if the alias
   *           is empty.
   */
  public CertificateEntry(Certificate certificate, Date creationDate,
                          Properties properties)
  {
    super(TYPE, creationDate, properties);
    if (certificate == null)
      throw new IllegalArgumentException("no certificate");
    this.certificate = certificate;
    this.properties.put("type", certificate.getType());
  }

  private CertificateEntry()
  {
    super(TYPE);
  }

  public static CertificateEntry decode(DataInputStream in) throws IOException
  {
    CertificateEntry entry = new CertificateEntry();
    entry.properties.decode(in);
    entry.makeCreationDate();
    String type = entry.properties.get("type");
    if (type == null)
      throw new MalformedKeyringException("no certificate type");
    int len = in.readInt();
    MeteredInputStream in2 = new MeteredInputStream(in, len);
    try
      {
        CertificateFactory fact = CertificateFactory.getInstance(type);
        entry.certificate = fact.generateCertificate(in2);
      }
    catch (CertificateException ce)
      {
        throw new MalformedKeyringException(ce.toString());
      }
    if (! in2.limitReached())
      throw new MalformedKeyringException("extra data at end of payload");
    return entry;
  }

  /**
   * Returns this entry's certificate.
   * 
   * @return The certificate.
   */
  public Certificate getCertificate()
  {
    return certificate;
  }

  protected void encodePayload() throws IOException
  {
    try
      {
        payload = certificate.getEncoded();
      }
    catch (CertificateEncodingException cee)
      {
        throw new IOException(cee.toString());
      }
  }
}
