/* GnuDHPublicKey.java -- A Diffie-Hellman public key.
   Copyright (C) 2004  Free Software Foundation, Inc.

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

import java.math.BigInteger;
import java.util.ArrayList;

import javax.crypto.interfaces.DHPublicKey;
import javax.crypto.spec.DHParameterSpec;

public class GnuDHPublicKey implements DHPublicKey
{

  // Fields.
  // -------------------------------------------------------------------------

  private byte[] encoded;
  private final DHParameterSpec params;
  private final BigInteger Y;
  private final BigInteger q;

  // Constructor.
  // -------------------------------------------------------------------------

  public GnuDHPublicKey(DHParameterSpec params, BigInteger Y, BigInteger q)
  {
    this.params = params;
    this.Y = Y;
    this.q = q;
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  public BigInteger getY()
  {
    return Y;
  }

  public DHParameterSpec getParams()
  {
    return params;
  }

  public String getAlgorithm()
  {
    return "DH";
  }

  public String getFormat()
  {
    return "X.509";
  }

  public byte[] getEncoded()
  {
    if (encoded != null)
      return (byte[]) encoded.clone();
    ArrayList spki = new ArrayList(2);
    ArrayList alg = new ArrayList(2);
    alg.add(new DERValue(DER.OBJECT_IDENTIFIER, new OID("1.2.840.10046.2.1")));
    ArrayList param = new ArrayList(3);
    param.add(new DERValue(DER.INTEGER, params.getP()));
    param.add(new DERValue(DER.INTEGER, params.getG()));
    param.add(new DERValue(DER.INTEGER, q));
    alg.add(new DERValue(DER.CONSTRUCTED|DER.SEQUENCE, param));
    spki.add(new DERValue(DER.CONSTRUCTED|DER.SEQUENCE, alg));
    spki.add(new DERValue(DER.BIT_STRING, new BitString(Y.toByteArray())));
    encoded = new DERValue(DER.CONSTRUCTED|DER.SEQUENCE, spki).getEncoded();
    if (encoded != null)
      return (byte[]) encoded.clone();
    return null;
  }
}
