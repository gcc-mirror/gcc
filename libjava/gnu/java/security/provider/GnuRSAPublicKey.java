/* GnuRSAPublicKey.java -- GNU RSA public key.
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
import java.security.interfaces.RSAPublicKey;
import java.security.spec.RSAPublicKeySpec;
import java.util.ArrayList;

class GnuRSAPublicKey implements RSAPublicKey
{

  // Fields.
  // -------------------------------------------------------------------------

  private final RSAPublicKeySpec spec;
  private byte[] encodedKey;

  // Constructor.
  // -------------------------------------------------------------------------

  public GnuRSAPublicKey(RSAPublicKeySpec spec)
  {
    this.spec = spec;
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  public BigInteger getModulus()
  {
    return spec.getModulus();
  }

  public BigInteger getPublicExponent()
  {
    return spec.getPublicExponent();
  }

  public String getAlgorithm()
  {
    return "RSA";
  }

  public String getFormat()
  {
    return "X.509";
  }

  public byte[] getEncoded()
  {
    if (encodedKey != null)
      return (byte[]) encodedKey.clone();
    ArrayList key = new ArrayList(2);
    key.add(new DERValue(DER.INTEGER, getModulus()));
    key.add(new DERValue(DER.INTEGER, getPublicExponent()));
    DERValue rsapk = new DERValue(DER.SEQUENCE|DER.CONSTRUCTED, key);
    ArrayList alg = new ArrayList(2);
    alg.add(new DERValue(DER.OBJECT_IDENTIFIER,
                         new OID("1.2.840.113549.1.1.1")));
    alg.add(new DERValue(DER.NULL, null));
    ArrayList spki = new ArrayList(2);
    spki.add(new DERValue(DER.SEQUENCE|DER.CONSTRUCTED, alg));
    spki.add(new DERValue(DER.BIT_STRING, new BitString(rsapk.getEncoded())));
    encodedKey = new DERValue(DER.SEQUENCE|DER.CONSTRUCTED, spki).getEncoded();
    return (byte[]) encodedKey.clone();
  }
}
