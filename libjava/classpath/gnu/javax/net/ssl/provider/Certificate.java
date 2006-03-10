/* Certificate.java -- SSL Certificate message.
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


package gnu.javax.net.ssl.provider;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.EOFException;
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.StringReader;
import java.io.StringWriter;

import java.security.cert.CertificateEncodingException;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;

import java.util.LinkedList;

import javax.net.ssl.SSLProtocolException;

final class Certificate implements Handshake.Body
{

  // Fields.
  // -------------------------------------------------------------------------

  private final X509Certificate[] certs;

  // Constructors.
  // -------------------------------------------------------------------------

  Certificate(X509Certificate[] certs)
  {
    if (certs == null)
      {
        throw new NullPointerException();
      }
    this.certs = certs;
  }

  // Class methods.
  // -------------------------------------------------------------------------

  static Certificate read(InputStream in, CertificateType type)
    throws IOException
  {
    if (type == CertificateType.X509)
      {
        int len = (in.read() & 0xFF) << 16 | (in.read() & 0xFF) << 8
                | (in.read() & 0xFF);
        byte[] buf = new byte[len];
        int count = 0;
        while (count < len)
          {
            int l = in.read(buf, count, len - count);
            if (l == -1)
              {
                throw new EOFException("unexpected end of stream");
              }
            count += l;
          }
        try
          {
            LinkedList certs = new LinkedList();
            CertificateFactory fact = CertificateFactory.getInstance("X.509");
            ByteArrayInputStream bin = new ByteArrayInputStream(buf);
            count = 0;
            while (count < len)
              {
                int len2 = (bin.read() & 0xFF) << 16 | (bin.read() & 0xFF) << 8
                         | (bin.read() & 0xFF);
                certs.add(fact.generateCertificate(bin));
                count += len2 + 3;
              }
            return new Certificate((X509Certificate[])
              certs.toArray(new X509Certificate[certs.size()]));
          }
        catch (CertificateException ce)
          {
            SSLProtocolException sslpe = new SSLProtocolException(ce.getMessage());
            sslpe.initCause (ce);
            throw sslpe;
          }
      }
    else if (type == CertificateType.OPEN_PGP)
      {
        throw new UnsupportedOperationException("not yet implemented");
      }
    else
      throw new Error("unsupported certificate type "+type);
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  public void write(OutputStream out) throws IOException
  {
    ByteArrayOutputStream bout = new ByteArrayOutputStream();
    try
      {
        for (int i = 0; i < certs.length; i++)
          {
            byte[] enc = certs[i].getEncoded();
            bout.write((enc.length >>> 16) & 0xFF);
            bout.write((enc.length >>>  8) & 0xFF);
            bout.write( enc.length & 0xFF);
            bout.write(enc);
          }
      }
    catch (CertificateEncodingException cee)
      {
        throw new Error("cannot encode certificates");
      }
    catch (IOException ignored)
      {
      }
    out.write(bout.size() >>> 16 & 0xFF);
    out.write(bout.size() >>>  8 & 0xFF);
    out.write(bout.size() & 0xFF);
    bout.writeTo(out);
  }

  X509Certificate[] getCertificates()
  {
    return certs;
  }

  public String toString()
  {
    StringWriter str = new StringWriter();
    PrintWriter out = new PrintWriter(str);
    out.println("struct {");
    out.println("  certificateList =");
    for (int i = 0; i < certs.length; i++)
      {
        BufferedReader r =
          new BufferedReader(new StringReader(certs[i].toString()));
        String s;
        try
          {
            while ((s = r.readLine()) != null)
              {
                out.print("    ");
                out.println(s);
              }
          }
        catch (IOException ignored)
          {
          }
      }
    out.println("} Certificate;");
    return str.toString();
  }
}
