/* Certificate.java -- SSL certificate message.
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

import java.io.ByteArrayInputStream;
import java.io.PrintWriter;
import java.io.StringWriter;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;

import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

/**
 * The certificate object. This is used by both the client and the server
 * to send their certificates (if any) to one another.
 * 
 * <pre>opaque ASN.1Cert&lt;1..2^24-1&gt;;

struct {
  ASN.1Cert certificate_list&lt;0..2^24-1&gt;;
} Certificate;</pre>
 *
 * @author Casey Marshall (csm@gnu.org)
 */
public class Certificate implements Handshake.Body
{

  // Fields.
  // -------------------------------------------------------------------------

  protected ByteBuffer buffer;
  protected final CertificateType type;

  // Constructors.
  // -------------------------------------------------------------------------

  public Certificate (final ByteBuffer buffer, final CertificateType type)
  {
    buffer.getClass ();
    type.getClass ();
    this.buffer = buffer.duplicate().order(ByteOrder.BIG_ENDIAN);
    this.type = type;
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  public int length ()
  {
    return (((buffer.get (0) & 0xFF) << 24)
            | buffer.getShort (1)) + 3;
  }

  public List<java.security.cert.Certificate> certificates ()
    throws CertificateException, NoSuchAlgorithmException
  {
    LinkedList<java.security.cert.Certificate> list
      = new LinkedList<java.security.cert.Certificate>();
    CertificateFactory factory = CertificateFactory.getInstance(type.toString());
    int length = (((buffer.get(0) & 0xFF) << 16)
                  | (buffer.getShort(1) & 0xFFFF));
    ByteBuffer b = (ByteBuffer) buffer.duplicate().position(3);
    for (int i = 3; i < length; )
      {
        int length2 = (((b.get () & 0xFF) << 16)
                       | (b.getShort () & 0xFFFF));
        byte[] buf = new byte[length2];
        b.position(i+3);
        b.get (buf);
        list.add(factory.generateCertificate (new ByteArrayInputStream (buf)));
        i += length2 + 3;
        b.position(i);
      }
    return list;
  }

  public String toString ()
  {
    return toString (null);
  }

  public String toString (final String prefix)
  {
    StringWriter str = new StringWriter();
    PrintWriter out = new PrintWriter(str);
    if (prefix != null)
      out.print (prefix);
    out.println ("struct {");
    try
      {
        List certs = certificates ();
        if (prefix != null)
          out.print (prefix);
        out.print ("  certificateList: [");
        out.print (certs.size ());
        out.println ("] {");
        for (Iterator it = certs.iterator (); it.hasNext (); )
          {
            java.security.cert.Certificate cert =
              (java.security.cert.Certificate) it.next ();
            if (prefix != null)
              out.print (prefix);
            out.print ("    ");
            if (cert instanceof X509Certificate)
              out.print (((X509Certificate) cert).getSubjectDN ());
            else
              out.print (cert);
            out.println (";");
          }
        if (prefix != null)
          out.print (prefix);
        out.println ("  };");
      }
    catch (CertificateException ce)
      {
        if (prefix != null)
          out.print (prefix);
        out.print ("  ");
        out.print (ce);
        out.println (";");
      }
    catch (NoSuchAlgorithmException nsae)
      {
        if (prefix != null)
          out.print (prefix);
        out.print ("  ");
        out.print (nsae);
        out.println (";");
      }
    out.print ("} Certificate;");
    return str.toString();
  }
}
