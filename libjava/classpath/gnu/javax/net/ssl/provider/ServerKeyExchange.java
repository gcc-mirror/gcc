/* ServerKeyExchange.java -- SSL ServerKeyExchange message.
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
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.StringReader;
import java.io.StringWriter;

import java.math.BigInteger;

import java.security.PublicKey;
import java.security.interfaces.RSAPublicKey;

import javax.crypto.interfaces.DHPublicKey;
import javax.crypto.spec.DHParameterSpec;

import javax.net.ssl.SSLProtocolException;

import gnu.javax.crypto.key.dh.GnuDHPublicKey;
import gnu.javax.crypto.key.srp6.SRPPublicKey;

class ServerKeyExchange implements Handshake.Body
{

  // Fields.
  // -------------------------------------------------------------------------

  private PublicKey publicKey;
  private Signature signature;
  private byte[] srpSalt;

  // Constructor.
  // -------------------------------------------------------------------------

  ServerKeyExchange(PublicKey publicKey, Signature signature)
  {
    this(publicKey, signature, null);
  }

  ServerKeyExchange(PublicKey publicKey, Signature signature, byte[] srpSalt)
  {
    this.publicKey = publicKey;
    this.signature = signature;
    this.srpSalt = srpSalt;
  }

  // Class methods.
  // -------------------------------------------------------------------------

  static ServerKeyExchange read(InputStream in, CipherSuite suite,
                                PublicKey serverKey)
    throws IOException
  {
    DataInputStream din = new DataInputStream(in);
    PublicKey key = null;
    byte[] salt = null;
    String kex = suite.getKeyExchange();
    if (kex.equals("DHE"))
      {
        BigInteger p, g, y;
        byte[] buf = new byte[din.readUnsignedShort()];
        din.readFully(buf);
        p = new BigInteger(1, buf);
        buf = new byte[din.readUnsignedShort()];
        din.readFully(buf);
        g = new BigInteger(1, buf);
        buf = new byte[din.readUnsignedShort()];
        din.readFully(buf);
        y = new BigInteger(1, buf);
        key = new GnuDHPublicKey(null, p, g, y);
      }
    else if (kex.equals("RSA"))
      {
        BigInteger n, e;
        byte[] buf = new byte[din.readUnsignedShort()];
        din.readFully(buf);
        n = new BigInteger(1, buf);
        buf = new byte[din.readUnsignedShort()];
        din.readFully(buf);
        e = new BigInteger(1, buf);
        key = new JessieRSAPublicKey(n, e);
      }
    else if (kex.equals("SRP"))
      {
        BigInteger N, g, B;
        byte[] buf = new byte[din.readUnsignedShort()];
        din.readFully(buf);
        N = new BigInteger(1, buf);
        buf = new byte[din.readUnsignedShort()];
        din.readFully(buf);
        g = new BigInteger(1, buf);
        salt = new byte[din.readUnsignedByte()];
        din.readFully(salt);
        buf = new byte[din.readUnsignedShort()];
        din.readFully(buf);
        B = new BigInteger(1, buf);
        try
          {
            key = new SRPPublicKey(N, g, B);
          }
        catch (IllegalArgumentException iae)
          {
            throw new SSLProtocolException(iae.getMessage());
          }
      }
    else
      {
        throw new SSLProtocolException("invalid kex algorithm");
      }

    Signature sig = null;
    if (!suite.getSignature().equals("anon"))
      {
        sig = Signature.read(in, suite, serverKey);
      }
    return new ServerKeyExchange(key, sig, salt);
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  public void write(OutputStream out) throws IOException
  {
    write(out, ProtocolVersion.TLS_1);
  }

  public void write(OutputStream out, ProtocolVersion version)
    throws IOException
  {
    if (publicKey instanceof DHPublicKey)
      {
        writeBigint(out, ((DHPublicKey) publicKey).getParams().getP());
        writeBigint(out, ((DHPublicKey) publicKey).getParams().getG());
        writeBigint(out, ((DHPublicKey) publicKey).getY());
      }
    else if (publicKey instanceof RSAPublicKey)
      {
        writeBigint(out, ((RSAPublicKey) publicKey).getModulus());
        writeBigint(out, ((RSAPublicKey) publicKey).getPublicExponent());
      }
    else if (publicKey instanceof SRPPublicKey)
      {
        writeBigint(out, ((SRPPublicKey) publicKey).getN());
        writeBigint(out, ((SRPPublicKey) publicKey).getG());
        out.write(srpSalt.length);
        out.write(srpSalt);
        writeBigint(out, ((SRPPublicKey) publicKey).getY());
      }
    if (signature != null)
      {
        signature.write(out, version);
      }
  }

  PublicKey getPublicKey()
  {
    return publicKey;
  }

  Signature getSignature()
  {
    return signature;
  }

  byte[] getSRPSalt()
  {
    return srpSalt;
  }

  public String toString()
  {
    StringWriter str = new StringWriter();
    PrintWriter out = new PrintWriter(str);
    out.println("struct {");
    out.println("  publicKey = struct {");
    if (publicKey instanceof DHPublicKey)
      {
        out.println("    p = " +
                   ((DHPublicKey) publicKey).getParams().getP().toString(16) +
                   ";");
        out.println("    g = " +
                   ((DHPublicKey) publicKey).getParams().getG().toString(16) +
                   ";");
        out.println("    y = " + ((DHPublicKey) publicKey).getY().toString(16) +
                   ";");
        out.println("  } DHPublicKey;");
      }
    else if (publicKey instanceof RSAPublicKey)
      {
        out.println("    modulus = " +
                   ((RSAPublicKey) publicKey).getModulus().toString(16) +
                   ";");
        out.println("    exponent = " +
                   ((RSAPublicKey) publicKey).getPublicExponent().toString(16) +
                   ";");
        out.println("  } RSAPublicKey;");
      }
    else if (publicKey instanceof SRPPublicKey)
      {
        out.println("    N = "+((SRPPublicKey) publicKey).getN().toString(16)+";");
        out.println("    g = "+((SRPPublicKey) publicKey).getG().toString(16)+";");
        out.println("    salt = " + Util.toHexString(srpSalt, ':') + ";");
        out.println("    B = "+((SRPPublicKey) publicKey).getY().toString(16)+";");
        out.println("  } SRPPublicKey;");
      }
    if (signature != null)
      {
        out.println("  signature =");
        BufferedReader r = new BufferedReader(new StringReader(signature.toString()));
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
    out.println("} ServerKeyExchange;");
    return str.toString();
  }

  private void writeBigint(OutputStream out, BigInteger bigint)
    throws IOException
  {
    byte[] b = bigint.toByteArray();
    if (b[0] == 0x00)
      {
        out.write((b.length - 1) >>> 8 & 0xFF);
        out.write((b.length - 1) & 0xFF);
        out.write(b, 1, b.length - 1);
      }
    else
      {
        out.write(b.length >>> 8 & 0xFF);
        out.write(b.length & 0xFF);
        out.write(b);
      }
  }
}
