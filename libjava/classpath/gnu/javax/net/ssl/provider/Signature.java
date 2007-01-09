/* Signature.java -- SSL Signature structure.
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

import java.nio.ByteBuffer;

import java.security.PublicKey;
import java.security.interfaces.RSAKey;

import java.util.Arrays;

import gnu.java.security.der.*;

/**
 * The signature structure.
 *
 * <pre>
select (SignatureAlgorithm)
{
case anonymous:
  struct { };
case rsa:
  digitally-signed struct
  {
    opaque md5_hash[16];
    opaque sha_hash[20];
  };
case dsa:
  digitally-signed struct
  {
    opaque sha_hash[20];
  };
} Signature;</pre>
 */
public class Signature implements Builder, Constructed
{

  // Fields.
  // -------------------------------------------------------------------------

  private final ByteBuffer buffer;
  private final SignatureAlgorithm alg;

  // Constructor.
  // -------------------------------------------------------------------------

  public Signature (final ByteBuffer buffer, final SignatureAlgorithm alg)
  {
    this.buffer = buffer;
    this.alg = alg;
  }
  
  public Signature (final byte[] sigValue, final SignatureAlgorithm alg)
  {
    buffer = ByteBuffer.allocate(sigValue.length + 2);
    buffer.putShort((short) sigValue.length);
    buffer.put(sigValue);
    buffer.position(0);
    this.alg = alg;
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  public int length ()
  {
    if (alg.equals (SignatureAlgorithm.ANONYMOUS))
      return 0;
    return (buffer.getShort (0) & 0xFFFF) + 2;
  }
  
  public ByteBuffer buffer()
  {
    return (ByteBuffer) buffer.duplicate().limit(length());
  }

  public byte[] signature ()
  {
    if (alg.equals (SignatureAlgorithm.ANONYMOUS))
      return new byte[0];
    int length = buffer.getShort (0) & 0xFFFF;
    byte[] buf = new byte[length];
    ((ByteBuffer) buffer.duplicate().position(2)).get(buf);
    return buf;
  }

  public void setSignature (final byte[] signature)
  {
    setSignature (signature, 0, signature.length);
  }

  public void setSignature (final byte[] signature, final int offset, final int length)
  {
    if (alg.equals (SignatureAlgorithm.ANONYMOUS))
      return;
    buffer.putShort (0, (short) length);
    buffer.position (2);
    buffer.put (signature, offset, length);
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
    out.println("struct {");
    if (!alg.equals (SignatureAlgorithm.ANONYMOUS))
      {
        String subprefix = "  ";
        if (prefix != null)
          subprefix = prefix + subprefix;
        out.print (Util.hexDump (signature (), subprefix));
      }
    if (prefix != null)
      out.print (prefix);
    out.print ("} Signature;");
    return str.toString();
  }
}
