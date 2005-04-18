/* GnuDSAPublicKey.java --- Gnu DSA Public Key
   Copyright (C) 1999,2003,2004 Free Software Foundation, Inc.

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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
exception statement from your version. */


package gnu.java.security.provider;

import gnu.java.security.OID;
import gnu.java.security.der.BitString;
import gnu.java.security.der.DER;
import gnu.java.security.der.DERValue;
import gnu.java.security.der.DERWriter;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.math.BigInteger;
import java.security.interfaces.DSAParams;
import java.security.interfaces.DSAPublicKey;
import java.security.spec.DSAParameterSpec;
import java.util.ArrayList;

public class GnuDSAPublicKey implements DSAPublicKey
{
  private byte[] encodedKey;
  BigInteger y;
  BigInteger p;
  BigInteger q;
  BigInteger g;

  public GnuDSAPublicKey(BigInteger y, BigInteger p, BigInteger q, BigInteger g )
  {
    this.y = y;
    this.p = p;
    this.q = q;
    this.g = g;
  }

  public String getAlgorithm()
  {
    return "DSA";
  }

  public String getFormat()
  {
    return "X.509";
  }

  /**
   * The encoded form of DSA public keys is:
   *
   * <blockquote><pre>
   * SubjectPublicKeyInfo ::= SEQUENCE {
   *   algorithm AlgorithmIdentifier,
   *   subjectPublicKey BIT STRING }
   * </pre></blockquote>
   */
  public byte[] getEncoded()
  {
    if (encodedKey != null)
      return (byte[]) encodedKey.clone();
    try
      {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        ArrayList spki = new ArrayList(2);
        ArrayList alg = new ArrayList(2);
        alg.add(new DERValue(DER.OBJECT_IDENTIFIER,
                new OID("1.2.840.113549.1.1.1")));
        ArrayList params = new ArrayList(3);
        params.add(new DERValue(DER.INTEGER, p));
        params.add(new DERValue(DER.INTEGER, q));
        params.add(new DERValue(DER.INTEGER, g));
        alg.add(new DERValue(DER.CONSTRUCTED|DER.SEQUENCE, params));
        spki.add(new DERValue(DER.CONSTRUCTED|DER.SEQUENCE, alg));
        spki.add(new DERValue(DER.BIT_STRING, new BitString(y.toByteArray())));
        DERWriter.write(out, new DERValue(DER.CONSTRUCTED|DER.SEQUENCE, spki));
        return (byte[]) (encodedKey = out.toByteArray()).clone();
      }
    catch (IOException ioe)
      {
        return null;
      }
  }

  public DSAParams getParams()
  {
    if (p == null || q == null || g == null)
      return null;
    return (DSAParams)(new DSAParameterSpec(p,q,g));
  }

  public BigInteger getY()
  {
    return y;
  }

  public String toString()
  {
    return
      "GnuDSAPublicKey: y=" + (y != null ? y.toString(16) : "(null)") +
      " p=" + (p != null ? p.toString(16) : "(null)") +
      " q=" + (q != null ? q.toString(16) : "(null)") +
      " g=" + (g != null ? g.toString(16) : "(null)");
  }
}
