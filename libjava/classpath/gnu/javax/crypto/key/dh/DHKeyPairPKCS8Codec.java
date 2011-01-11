/* DHKeyPairPKCS8Codec.java -- PKCS#8 encoder/decoder for DH keys
   Copyright (C) 2006 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

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


package gnu.javax.crypto.key.dh;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.math.BigInteger;
import java.security.InvalidParameterException;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.util.ArrayList;

import gnu.java.security.OID;
import gnu.java.security.Registry;
import gnu.java.security.der.DER;
import gnu.java.security.der.DERReader;
import gnu.java.security.der.DERValue;
import gnu.java.security.der.DERWriter;
import gnu.java.security.key.IKeyPairCodec;
import gnu.java.security.util.DerUtil;
import gnu.java.security.util.Util;

public class DHKeyPairPKCS8Codec
    implements IKeyPairCodec
{
  private static final OID DH_ALG_OID = new OID(Registry.DH_OID_STRING);

  // implicit 0-arguments constructor

  public int getFormatID()
  {
    return PKCS8_FORMAT;
  }

  /**
   * @throws InvalidParameterException ALWAYS.
   */
  public byte[] encodePublicKey(PublicKey key)
  {
    throw new InvalidParameterException("Wrong format for public keys");
  }

  /**
   * Returns the DER-encoded form of the PKCS#8 ASN.1 <i>PrivateKeyInfo</i>
   * representation of a DH private key. The ASN.1 specification is as follows:
   *
   * <pre>
   *   PrivateKeyInfo ::= SEQUENCE {
   *     version              INTEGER, -- MUST be 0
   *     privateKeyAlgorithm  AlgorithmIdentifier,
   *     privateKey           OCTET STRING
   *   }
   *
   *   AlgorithmIdentifier ::= SEQUENCE {
   *     algorithm   OBJECT IDENTIFIER,
   *     parameters  ANY DEFINED BY algorithm OPTIONAL
   *   }
   *
   *   DhParams ::= SEQUENCE {
   *     p  INTEGER, -- odd prime, p=jq +1
   *     g  INTEGER, -- generator, g
   *     q  INTEGER  -- factor of p-1
   *   }
   * </pre>
   * <p>
   * <b>IMPORTANT</b>: with RI's {@link javax.crypto.spec.DHGenParameterSpec}
   * and {@link javax.crypto.spec.DHParameterSpec} classes, we may end up with
   * Diffie-Hellman keys that have a <code>null</code> for the <code>q</code>
   * parameter. RFC-2631 DOES NOT allow for an <i>optional</i> value for that
   * parameter, hence we replace such null values with <code>0</code>, and do
   * the reverse in the corresponding decode method.
   *
   * @return the DER encoded form of the ASN.1 representation of the
   *         <i>PrivateKeyInfo</i> field in an X.509 certificate.
   * @throw InvalidParameterException if an error occurs during the marshalling
   *        process.
   */
  public byte[] encodePrivateKey(PrivateKey key)
  {
    if (! (key instanceof GnuDHPrivateKey))
      throw new InvalidParameterException("Wrong key type");

    DERValue derVersion = new DERValue(DER.INTEGER, BigInteger.ZERO);

    DERValue derOID = new DERValue(DER.OBJECT_IDENTIFIER, DH_ALG_OID);

    GnuDHPrivateKey pk = (GnuDHPrivateKey) key;
    BigInteger p = pk.getParams().getP();
    BigInteger g = pk.getParams().getG();
    BigInteger q = pk.getQ();
    if (q == null)
      q = BigInteger.ZERO;
    BigInteger x = pk.getX();

    ArrayList params = new ArrayList(3);
    params.add(new DERValue(DER.INTEGER, p));
    params.add(new DERValue(DER.INTEGER, g));
    params.add(new DERValue(DER.INTEGER, q));
    DERValue derParams = new DERValue(DER.CONSTRUCTED | DER.SEQUENCE, params);

    ArrayList algorithmID = new ArrayList(2);
    algorithmID.add(derOID);
    algorithmID.add(derParams);
    DERValue derAlgorithmID = new DERValue(DER.CONSTRUCTED | DER.SEQUENCE,
                                           algorithmID);

    DERValue derPrivateKey = new DERValue(DER.OCTET_STRING, Util.trim(x));

    ArrayList pki = new ArrayList(3);
    pki.add(derVersion);
    pki.add(derAlgorithmID);
    pki.add(derPrivateKey);
    DERValue derPKI = new DERValue(DER.CONSTRUCTED | DER.SEQUENCE, pki);

    byte[] result;
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    try
      {
        DERWriter.write(baos, derPKI);
        result = baos.toByteArray();
      }
    catch (IOException e)
      {
        InvalidParameterException y = new InvalidParameterException();
        y.initCause(e);
        throw y;
      }

    return result;
  }

  /**
   * @throws InvalidParameterException ALWAYS.
   */
  public PublicKey decodePublicKey(byte[] input)
  {
    throw new InvalidParameterException("Wrong format for public keys");
  }

  /**
   * @param input the byte array to unmarshall into a valid DH
   *          {@link PrivateKey} instance. MUST NOT be null.
   * @return a new instance of a {@link GnuDHPrivateKey} decoded from the
   *         <i>PrivateKeyInfo</i> material fed as <code>input</code>.
   * @throw InvalidParameterException if an exception occurs during the
   *        unmarshalling process.
   */
  public PrivateKey decodePrivateKey(byte[] input)
  {
    if (input == null)
      throw new InvalidParameterException("Input bytes MUST NOT be null");

    BigInteger version, p, q, g, x;
    DERReader der = new DERReader(input);
    try
      {
        DERValue derPKI = der.read();
        DerUtil.checkIsConstructed(derPKI, "Wrong PrivateKeyInfo field");

        DERValue derVersion = der.read();
        if (! (derVersion.getValue() instanceof BigInteger))
          throw new InvalidParameterException("Wrong Version field");

        version = (BigInteger) derVersion.getValue();
        if (version.compareTo(BigInteger.ZERO) != 0)
          throw new InvalidParameterException("Unexpected Version: " + version);

        DERValue derAlgoritmID = der.read();
        DerUtil.checkIsConstructed(derAlgoritmID, "Wrong AlgorithmIdentifier field");

        DERValue derOID = der.read();
        OID algOID = (OID) derOID.getValue();
        if (! algOID.equals(DH_ALG_OID))
          throw new InvalidParameterException("Unexpected OID: " + algOID);

        DERValue derParams = der.read();
        DerUtil.checkIsConstructed(derParams, "Wrong DSS Parameters field");

        DERValue val = der.read();
        DerUtil.checkIsBigInteger(val, "Wrong P field");
        p = (BigInteger) val.getValue();
        val = der.read();
        DerUtil.checkIsBigInteger(val, "Wrong G field");
        g = (BigInteger) val.getValue();
        val = der.read();
        DerUtil.checkIsBigInteger(val, "Wrong Q field");
        q = (BigInteger) val.getValue();
        if (q.compareTo(BigInteger.ZERO) == 0)
          q = null;

        val = der.read();
        byte[] xBytes = (byte[]) val.getValue();
        x = new BigInteger(1, xBytes);
      }
    catch (IOException e)
      {
        InvalidParameterException y = new InvalidParameterException();
        y.initCause(e);
        throw y;
      }

    return new GnuDHPrivateKey(Registry.PKCS8_ENCODING_ID, q, p, g, x);
  }
}
