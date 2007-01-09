/* CertificateRequest.java -- SSL CertificateRequest message.
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

import java.io.PrintWriter;
import java.io.StringWriter;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;

/**
 * A request by the server for a client certificate.
 *
 * <pre>
struct
{
  ClientCertificateType certificate_types&lt;1..2^8-1&gt;;
  DistinguishedName certificate_authorities&lt;3..2^16-1&gt;;
} CertificateRequest;
</pre>
 */
public class CertificateRequest implements Handshake.Body
{

  // Fields.
  // -------------------------------------------------------------------------

  protected ByteBuffer buffer;
  
  // Constructor.
  // -------------------------------------------------------------------------

  public CertificateRequest(final ByteBuffer buffer)
  {
    this.buffer = buffer.duplicate().order(ByteOrder.BIG_ENDIAN);
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  public int length ()
  {
    int o1 = (buffer.get (0) & 0xFF) + 1;
    return o1 + (buffer.getShort (o1) & 0xFFFF) + 2;
  }

  public ClientCertificateTypeList types ()
  {
    return new ClientCertificateTypeList(buffer.duplicate());
  }

  public X500PrincipalList authorities ()
  {
    int offset = (buffer.get (0) & 0xFF) + 1;
    return new X500PrincipalList (((ByteBuffer) buffer.position(offset)).slice());
  }

  public String toString()
  {
    return toString (null);
  }

  public String toString (final String prefix)
  {
    StringWriter str = new StringWriter();
    PrintWriter out = new PrintWriter(str);
    String subprefix = "  ";
    if (prefix != null) subprefix = prefix + "  ";
    if (prefix != null) out.print (prefix);
    out.println("struct {");
    if (prefix != null) out.print (prefix);
    out.println ("  types =");
    out.println (types ().toString (subprefix));
    if (prefix != null) out.print (prefix);
    out.println("  authorities =");
    out.println (authorities ().toString (subprefix));
    if (prefix != null) out.print (prefix);
    out.print ("} CertificateRequest;");
    return str.toString();
  }

  public static enum ClientCertificateType
  {
    RSA_SIGN     (1),
    DSS_SIGN     (2),
    RSA_FIXED_DH (3),
    DSS_FIXED_DH (4);

    private final int value;

    // Constructor.
    // -----------------------------------------------------------------------

    private ClientCertificateType (final int value)
    {
      this.value = value;
    }

    // Class method.
    // -----------------------------------------------------------------------

    static ClientCertificateType forValue (final int value)
    {
      switch (value)
        {
        case 1: return RSA_SIGN;
        case 2: return DSS_SIGN;
        case 3: return RSA_FIXED_DH;
        case 4: return DSS_FIXED_DH;
        default: throw new IllegalArgumentException("unknown client certificate type: " + value);
        }
    }

    public int getValue()
    {
      return value;
    }
  }
}
