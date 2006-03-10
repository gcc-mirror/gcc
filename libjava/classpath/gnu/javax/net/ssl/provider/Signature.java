/* Signature.java -- SSL signature message.
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
import java.io.DataInputStream;
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.StringWriter;

import java.math.BigInteger;

import java.security.PublicKey;
import java.security.interfaces.RSAKey;

import java.util.Arrays;

import gnu.java.security.der.*;

class Signature implements Constructed
{

  // Fields.
  // -------------------------------------------------------------------------

  private final Object sigValue;
  private final String sigAlg;

  // Constructor.
  // -------------------------------------------------------------------------

  Signature(Object sigValue, String sigAlg)
  {
    this.sigValue = sigValue;
    this.sigAlg = sigAlg;
  }

  // Class method.
  // -------------------------------------------------------------------------

  static Signature read(InputStream in, CipherSuite suite, PublicKey key)
    throws IOException
  {
    Object sigValue = null;
    DataInputStream din = new DataInputStream(in);
    int len = din.readUnsignedShort();
    sigValue = new byte[len];
    din.readFully((byte[]) sigValue);
    if (suite.getSignature() == "DSS")
      {
        DERReader der = new DERReader(new ByteArrayInputStream((byte[]) sigValue));
        if (der.read().getTag() != DER.SEQUENCE)
          {
            throw new IOException("expecting DER SEQUENCE");
          }
        BigInteger r = (BigInteger) der.read().getValue();
        BigInteger s = (BigInteger) der.read().getValue();
        sigValue = new BigInteger[] { r, s };
      }
    return new Signature(sigValue, suite.getSignature());
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
    byte[] result = null;
    if (sigValue instanceof byte[])
      {
        result = (byte[]) sigValue;
      }
    else
      {
        DERValue r = new DERValue(DER.INTEGER, ((BigInteger[]) sigValue)[0]);
        DERValue s = new DERValue(DER.INTEGER, ((BigInteger[]) sigValue)[1]);
        DERValue sig = new DERValue(DER.SEQUENCE|DER.CONSTRUCTED,
                                    Arrays.asList(new Object[] { r, s }));
        result = sig.getEncoded();
      }
    out.write(result.length >>> 8 & 0xFF);
    out.write(result.length & 0xFF);
    out.write(result);
  }

  Object getSigValue()
  {
    return sigValue;
  }

  String getSigAlg()
  {
    return sigAlg;
  }

  public String toString()
  {
    StringWriter str = new StringWriter();
    PrintWriter out = new PrintWriter(str);
    out.println("struct {");
    if (sigAlg.equals("RSA"))
      {
        out.print(Util.hexDump((byte[]) sigValue, "  "));
      }
    else
      {
        out.println("  r = " + ((BigInteger[]) sigValue)[0].toString(16) + ";");
        out.println("  s = " + ((BigInteger[]) sigValue)[1].toString(16) + ";");
      }
    out.println("} Signature;");
    return str.toString();
  }
}
