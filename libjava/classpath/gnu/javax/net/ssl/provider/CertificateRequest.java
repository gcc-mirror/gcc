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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.EOFException;
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.StringWriter;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;

import java.util.LinkedList;
import java.security.Principal;

final class CertificateRequest implements Handshake.Body
{

  // Fields.
  // -------------------------------------------------------------------------

  private final ClientType[] types;
  private final Principal[] authorities;

  // Constructor.
  // -------------------------------------------------------------------------

  CertificateRequest(ClientType[] types, Principal[] authorities)
  {
    if (types == null)
      {
        throw new NullPointerException();
      }
    this.types = types;
    if (authorities == null)
      {
        throw new NullPointerException();
      }
    this.authorities = authorities;
  }

  // Class methods.
  // -------------------------------------------------------------------------

  static CertificateRequest read(InputStream in) throws IOException
  {
    DataInputStream din = new DataInputStream(in);
    ClientType[] types = new ClientType[din.readUnsignedByte()];
    for (int i = 0; i < types.length; i++)
      {
        types[i] = ClientType.read(din);
      }

    LinkedList authorities = new LinkedList();
    byte[] buf = new byte[din.readUnsignedShort()];
    din.readFully(buf);
    ByteArrayInputStream bin = new ByteArrayInputStream(buf);
    try
      {
        String x500name = Util.getSecurityProperty("jessie.x500.class");
        if (x500name == null)
          {
            x500name = "org.metastatic.jessie.pki.X500Name";
          }
        Class x500class = null;
        ClassLoader cl = ClassLoader.getSystemClassLoader();
        if (cl != null)
          {
            x500class = cl.loadClass(x500name);
          }
        else
          {
            x500class = Class.forName(x500name);
          }
        Constructor c = x500class.getConstructor(new Class[] { new byte[0].getClass() });
        while (bin.available() > 0)
          {
            buf = new byte[(bin.read() & 0xFF) << 8 | (bin.read() & 0xFF)];
            bin.read(buf);
            authorities.add(c.newInstance(new Object[] { buf }));
          }
      }
    catch (IOException ioe)
      {
        throw ioe;
      }
    catch (Exception ex)
      {
        throw new Error(ex.toString());
      }
    return new CertificateRequest(types,
      (Principal[]) authorities.toArray(new Principal[authorities.size()]));
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  public void write(OutputStream out) throws IOException
  {
    ByteArrayOutputStream bout = new ByteArrayOutputStream();
    out.write(types.length);
    for (int i = 0; i < types.length; i++)
      {
        out.write(types[i].getValue());
      }

    try
      {
        Class x500class = authorities[0].getClass();
        Method m = x500class.getMethod("getEncoded", null);
        for (int i = 0; i < authorities.length; i++)
          {
            byte[] buf = (byte[]) m.invoke(authorities[i], null);
            bout.write(buf.length >>> 8 & 0xFF);
            bout.write(buf.length & 0xFF);
            bout.write(buf, 0, buf.length);
          }
      }
    catch (Exception ex)
      {
        throw new Error(ex.toString());
      }
    out.write(bout.size() >>> 8 & 0xFF);
    out.write(bout.size() & 0xFF);
    bout.writeTo(out);
  }

  ClientType[] getTypes()
  {
    return types;
  }

  String[] getTypeStrings()
  {
    try
      {
        return (String[]) Util.transform(types, String.class, "toString", null);
      }
    catch (Exception x)
      {
        return null;
      }
  }

  Principal[] getAuthorities()
  {
    return authorities;
  }

  public String toString()
  {
    StringWriter str = new StringWriter();
    PrintWriter out = new PrintWriter(str);
    out.println("struct {");
    out.print("  types = ");
    for (int i = 0; i < types.length; i++)
      {
        out.print(types[i]);
        if (i != types.length - 1)
          out.print(", ");
      }
    out.println(";");
    out.println("  authorities =");
    for (int i = 0; i < authorities.length; i++)
      {
        out.print("    ");
        out.print(authorities[i].getName());
        if (i != types.length - 1)
          out.println(",");
      }
    out.println(";");
    out.println("} CertificateRequest;");
    return str.toString();
  }

  // Inner class.
  // -------------------------------------------------------------------------

  static final class ClientType implements Enumerated
  {

    // Constants and fields.
    // -----------------------------------------------------------------------

    static final ClientType
      RSA_SIGN     = new ClientType(1), DSS_SIGN     = new ClientType(2),
      RSA_FIXED_DH = new ClientType(3), DSS_FIXED_DH = new ClientType(4);

    private final int value;

    // Constructor.
    // -----------------------------------------------------------------------

    private ClientType(int value)
    {
      this.value = value;
    }

    // Class method.
    // -----------------------------------------------------------------------

    static ClientType read(InputStream in) throws IOException
    {
      int i = in.read();
      if (i == -1)
        {
          throw new EOFException("unexpected end of input stream");
        }
      switch (i & 0xFF)
        {
        case 1: return RSA_SIGN;
        case 2: return DSS_SIGN;
        case 3: return RSA_FIXED_DH;
        case 4: return DSS_FIXED_DH;
        default: return new ClientType(i);
        }
    }

    // Instance methods.
    // -----------------------------------------------------------------------

    public byte[] getEncoded()
    {
      return new byte[] { (byte) value };
    }

    public int getValue()
    {
      return value;
    }

    public String toString()
    {
      switch (value)
        {
        case 1: return "rsa_sign";
        case 2: return "dss_sign";
        case 3: return "rsa_fixed_dh";
        case 4: return "dss_fixed_dh";
        default: return "unknown(" + value + ")";
        }
    }
  }
}
