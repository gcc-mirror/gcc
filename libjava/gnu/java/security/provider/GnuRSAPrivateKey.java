/* GnuRSAPrivateKey.java -- GNU RSA private key.
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

import java.math.BigInteger;

import java.security.interfaces.RSAPrivateCrtKey;
import java.security.spec.RSAPrivateCrtKeySpec;

import java.util.ArrayList;

import gnu.java.security.OID;
import gnu.java.security.der.DER;
import gnu.java.security.der.DERValue;

class GnuRSAPrivateKey implements RSAPrivateCrtKey
{

  // Fields.
  // -------------------------------------------------------------------------

  private final RSAPrivateCrtKeySpec spec;
  private byte[] encodedKey;

  // Constructor.
  // -------------------------------------------------------------------------

  public GnuRSAPrivateKey(RSAPrivateCrtKeySpec spec)
  {
    this.spec = spec;
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  public BigInteger getModulus()
  {
    return spec.getModulus();
  }

  public BigInteger getPrivateExponent()
  {
    return spec.getPrivateExponent();
  }

  public BigInteger getCrtCoefficient()
  {
    return spec.getCrtCoefficient();
  }

  public BigInteger getPrimeExponentP()
  {
    return spec.getPrimeExponentP();
  }

  public BigInteger getPrimeExponentQ()
  {
    return spec.getPrimeExponentQ();
  }

  public BigInteger getPrimeP()
  {
    return spec.getPrimeP();
  }

  public BigInteger getPrimeQ()
  {
    return spec.getPrimeQ();
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
    return "PKCS#8";
  }

  /**
   * The encoded form is:
   *
   * <pre>
   * RSAPrivateKey ::= SEQUENCE {
   *   version Version,
   *   modulus INTEGER, -- n
   *   publicExponent INTEGER, -- e
   *   privateExponent INTEGER, -- d
   *   prime1 INTEGER, -- p
   *   prime2 INTEGER, -- q
   *   exponent1 INTEGER, -- d mod (p-1)
   *   exponent2 INTEGER, -- d mod (q-1)
   *   coefficient INTEGER -- (inverse of q) mod p }
   * </pre>
   *
   * <p>Which is in turn encoded in a PrivateKeyInfo structure from PKCS#8.
   */
  public byte[] getEncoded()
  {
    if (encodedKey != null)
      return (byte[]) encodedKey.clone();
    ArrayList key = new ArrayList(9);
    key.add(new DERValue(DER.INTEGER, BigInteger.ZERO));
    key.add(new DERValue(DER.INTEGER, getModulus()));
    key.add(new DERValue(DER.INTEGER, getPublicExponent()));
    key.add(new DERValue(DER.INTEGER, getPrivateExponent()));
    key.add(new DERValue(DER.INTEGER, getPrimeP()));
    key.add(new DERValue(DER.INTEGER, getPrimeQ()));
    key.add(new DERValue(DER.INTEGER, getPrimeExponentP()));
    key.add(new DERValue(DER.INTEGER, getPrimeExponentQ()));
    key.add(new DERValue(DER.INTEGER, getCrtCoefficient()));
    DERValue pk = new DERValue(DER.SEQUENCE|DER.CONSTRUCTED, key);
    ArrayList pki = new ArrayList(3);
    pki.add(new DERValue(DER.INTEGER, BigInteger.ZERO));
    ArrayList alg = new ArrayList(2);
    alg.add(new DERValue(DER.OBJECT_IDENTIFIER,
                         new OID("1.2.840.113549.1.1.1")));
    alg.add(new DERValue(DER.NULL, null));
    pki.add(new DERValue(DER.CONSTRUCTED|DER.SEQUENCE, alg));
    pki.add(new DERValue(DER.OCTET_STRING, pk.getEncoded()));
    encodedKey = new DERValue(DER.SEQUENCE|DER.CONSTRUCTED, pki).getEncoded();
    return (byte[]) encodedKey.clone();
  }
}
