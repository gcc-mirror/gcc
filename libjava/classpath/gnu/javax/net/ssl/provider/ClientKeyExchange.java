/* ClientKeyExchange.java -- SSL ClientKeyExchange message.
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
import java.io.DataInputStream;
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.StringReader;
import java.io.StringWriter;

import java.math.BigInteger;

import java.security.PublicKey;
import java.security.interfaces.RSAKey;
import javax.crypto.interfaces.DHPublicKey;

final class ClientKeyExchange implements Handshake.Body
{

  // Fields.
  // -------------------------------------------------------------------------

  private final Object exObject;

  // Constructors.
  // -------------------------------------------------------------------------

  ClientKeyExchange(byte[] encryptedSecret)
  {
    exObject = encryptedSecret;
  }

  ClientKeyExchange(BigInteger bigint)
  {
    exObject = bigint;
  }

  // Class method.
  // -------------------------------------------------------------------------

  static ClientKeyExchange read(InputStream in, CipherSuite suite,
                                PublicKey key)
    throws IOException
  {
    DataInputStream din = new DataInputStream(in);
    if (suite.getKeyExchange().equals("RSA"))
      {
        int len = 0;
        if (suite.getVersion() == ProtocolVersion.SSL_3)
          {
            len = (((RSAKey) key).getModulus().bitLength()+7) / 8;
          }
        else
          {
            len = din.readUnsignedShort();
          }
        byte[] buf = new byte[len];
        din.readFully(buf);
        return new ClientKeyExchange(buf);
      }
    else if (suite.getKeyExchange().equals("SRP"))
      {
        byte[] buf = new byte[din.readUnsignedShort()];
        din.readFully(buf);
        return new ClientKeyExchange(new BigInteger(1, buf));
      }
    else if (key == null || !(key instanceof DHPublicKey))  // explicit.
      {
        byte[] buf = new byte[din.readUnsignedShort()];
        din.readFully(buf);
        return new ClientKeyExchange(new BigInteger(1, buf));
      }
    else
      {
        return new ClientKeyExchange(new byte[0]);
      }
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  public void write(OutputStream out) throws IOException
  {
    throw new UnsupportedOperationException("use write(java.io.OutputStream,ProtocolVersion) instead");
  }

  public void write(OutputStream out, ProtocolVersion version) throws IOException
  {
    if (exObject instanceof byte[])
      {
        byte[] b = (byte[]) exObject;
        if (b.length > 0)
          {
            if (version != ProtocolVersion.SSL_3)
              {
                out.write(b.length >>> 8 & 0xFF);
                out.write(b.length & 0xFF);
              }
            out.write(b);
          }
      }
    else
      {
        byte[] bigint = ((BigInteger) exObject).toByteArray();
        if (bigint[0] == 0x00)
          {
            out.write(bigint.length - 1 >>> 8 & 0xFF);
            out.write(bigint.length - 1 & 0xFF);
            out.write(bigint, 1, bigint.length - 1);
          }
        else
          {
            out.write(bigint.length >>> 8 & 0xFF);
            out.write(bigint.length & 0xFF);
            out.write(bigint);
          }
      }
  }

  Object getExchangeObject()
  {
    return exObject;
  }

  public String toString()
  {
    StringWriter str = new StringWriter();
    PrintWriter out = new PrintWriter(str);
    out.println("struct {");
    if (exObject instanceof byte[] && ((byte[]) exObject).length > 0)
      {
        out.println("  encryptedPreMasterSecret =");
        out.print(Util.hexDump((byte[]) exObject, "    "));
      }
    else if (exObject instanceof BigInteger)
      {
        out.println("  clientPublic = " + ((BigInteger) exObject).toString(16) + ";");
      }
    out.println("} ClientKeyExchange;");
    return str.toString();
  }
}
