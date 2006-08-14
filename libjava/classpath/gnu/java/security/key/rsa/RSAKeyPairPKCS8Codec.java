/* RSAKeyPairPKCS8Codec.java -- PKCS#8 Encoding/Decoding handler
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


package gnu.java.security.key.rsa;

import gnu.java.security.Configuration;
import gnu.java.security.OID;
import gnu.java.security.Registry;
import gnu.java.security.der.DER;
import gnu.java.security.der.DERReader;
import gnu.java.security.der.DERValue;
import gnu.java.security.der.DERWriter;
import gnu.java.security.key.IKeyPairCodec;
import gnu.java.security.util.DerUtil;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.math.BigInteger;
import java.security.InvalidParameterException;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.util.ArrayList;
import java.util.logging.Logger;

/**
 * An implementation of an {@link IKeyPairCodec} that knows how to encode /
 * decode PKCS#8 ASN.1 external representation of RSA private keys.
 */
public class RSAKeyPairPKCS8Codec
    implements IKeyPairCodec
{
  private static final Logger log = Logger.getLogger(RSAKeyPairPKCS8Codec.class.getName());
  private static final OID RSA_ALG_OID = new OID(Registry.RSA_OID_STRING);

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
   * Returns the PKCS#8 ASN.1 <i>PrivateKeyInfo</i> representation of an RSA
   * private key. The ASN.1 specification is as follows:
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
   * </pre>
   * <p>
   * As indicated in RFC-2459: "The parameters field shall have ASN.1 type NULL
   * for this algorithm identifier.".
   * <p>
   * The <i>privateKey</i> field, which is an OCTET STRING, contains the
   * DER-encoded form of the RSA private key defined as:
   * <pre>
   *   RSAPrivateKey ::= SEQUENCE {
   *     version                 INTEGER, -- MUST be 0
   *     modulus                 INTEGER, -- n
   *     publicExponent          INTEGER, -- e
   *     privateExponent         INTEGER, -- d
   *     prime1                  INTEGER, -- p
   *     prime2                  INTEGER, -- q
   *     exponent1               INTEGER, -- d mod (p-1)
   *     exponent2               INTEGER, -- d mod (q-1)
   *     coefficient             INTEGER, -- (inverse of q) mod p
   *   }
   * </pre>
   * 
   * @return the DER encoded form of the ASN.1 representation of the
   *         <i>PrivateKeyInfo</i> field for an RSA {@link PrivateKey}..
   * @throw InvalidParameterException if an error occurs during the marshalling
   *        process.
   */
  public byte[] encodePrivateKey(PrivateKey key)
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "encodePrivateKey()", key);
    if (! (key instanceof GnuRSAPrivateKey))
      throw new InvalidParameterException("Wrong key type");

    GnuRSAPrivateKey pk = (GnuRSAPrivateKey) key;
    BigInteger n = pk.getN();
    BigInteger e = pk.getE();
    BigInteger d = pk.getPrivateExponent();
    BigInteger p = pk.getPrimeP();
    BigInteger q = pk.getPrimeQ();
    BigInteger dP = pk.getPrimeExponentP();
    BigInteger dQ = pk.getPrimeExponentQ();
    BigInteger qInv = pk.getCrtCoefficient();

    DERValue derVersion = new DERValue(DER.INTEGER, BigInteger.ZERO);

    DERValue derOID = new DERValue(DER.OBJECT_IDENTIFIER, RSA_ALG_OID);

    ArrayList algorithmID = new ArrayList(2);
    algorithmID.add(derOID);
    algorithmID.add(new DERValue(DER.NULL, null));
    DERValue derAlgorithmID = new DERValue(DER.CONSTRUCTED | DER.SEQUENCE,
                                           algorithmID);

    DERValue derRSAVersion = new DERValue(DER.INTEGER, BigInteger.ZERO);
    DERValue derN = new DERValue(DER.INTEGER, n);
    DERValue derE = new DERValue(DER.INTEGER, e);
    DERValue derD = new DERValue(DER.INTEGER, d);
    DERValue derP = new DERValue(DER.INTEGER, p);
    DERValue derQ = new DERValue(DER.INTEGER, q);
    DERValue derDP = new DERValue(DER.INTEGER, dP);
    DERValue derDQ = new DERValue(DER.INTEGER, dQ);
    DERValue derQInv = new DERValue(DER.INTEGER, qInv);

    ArrayList rsaPrivateKey = new ArrayList();
    rsaPrivateKey.add(derRSAVersion);
    rsaPrivateKey.add(derN);
    rsaPrivateKey.add(derE);
    rsaPrivateKey.add(derD);
    rsaPrivateKey.add(derP);
    rsaPrivateKey.add(derQ);
    rsaPrivateKey.add(derDP);
    rsaPrivateKey.add(derDQ);
    rsaPrivateKey.add(derQInv);
    DERValue derRSAPrivateKey = new DERValue(DER.CONSTRUCTED | DER.SEQUENCE,
                                             rsaPrivateKey);
    byte[] pkBytes = derRSAPrivateKey.getEncoded();
    DERValue derPrivateKey = new DERValue(DER.OCTET_STRING, pkBytes);

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
    catch (IOException x)
      {
        InvalidParameterException y = new InvalidParameterException();
        y.initCause(x);
        throw y;
      }
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "encodePrivateKey()", result);
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
   * @param input the byte array to unmarshall into a valid RSA
   *          {@link PrivateKey} instance. MUST NOT be null.
   * @return a new instance of a {@link GnuRSAPrivateKey} decoded from the
   *         <i>PrivateKeyInfo</i> material fed as <code>input</code>.
   * @throw InvalidParameterException if an exception occurs during the
   *        unmarshalling process.
   */
  public PrivateKey decodePrivateKey(byte[] input)
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "decodePrivateKey()", input);
    if (input == null)
      throw new InvalidParameterException("Input bytes MUST NOT be null");

    BigInteger version, n, e, d, p, q, dP, dQ, qInv;
    DERReader der = new DERReader(input);
    try
      {
        DERValue derPKI = der.read();
        DerUtil.checkIsConstructed(derPKI, "Wrong PrivateKeyInfo field");

        DERValue derVersion = der.read();
        DerUtil.checkIsBigInteger(derVersion, "Wrong Version field");
        version = (BigInteger) derVersion.getValue();
        if (version.compareTo(BigInteger.ZERO) != 0)
          throw new InvalidParameterException("Unexpected Version: " + version);

        DERValue derAlgoritmID = der.read();
        DerUtil.checkIsConstructed(derAlgoritmID, "Wrong AlgorithmIdentifier field");

        DERValue derOID = der.read();
        OID algOID = (OID) derOID.getValue();
        if (! algOID.equals(RSA_ALG_OID))
          throw new InvalidParameterException("Unexpected OID: " + algOID);

        // rfc-2459 states that this field is OPTIONAL but NULL if/when present
        DERValue val = der.read();
        if (val.getTag() == DER.NULL)
          val = der.read();

        byte[] pkBytes = (byte[]) val.getValue();
        der = new DERReader(pkBytes);
        DERValue derRSAPrivateKey = der.read();
        DerUtil.checkIsConstructed(derRSAPrivateKey, "Wrong RSAPrivateKey field");
        
        val = der.read();
        DerUtil.checkIsBigInteger(val, "Wrong RSAPrivateKey Version field");
        version = (BigInteger) val.getValue();
        if (version.compareTo(BigInteger.ZERO) != 0)
          throw new InvalidParameterException("Unexpected RSAPrivateKey Version: "
                                              + version);

        val = der.read();
        DerUtil.checkIsBigInteger(val, "Wrong modulus field");
        n = (BigInteger) val.getValue();
        val = der.read();
        DerUtil.checkIsBigInteger(val, "Wrong publicExponent field");
        e = (BigInteger) val.getValue();
        val = der.read();
        DerUtil.checkIsBigInteger(val, "Wrong privateExponent field");
        d = (BigInteger) val.getValue();
        val = der.read();
        DerUtil.checkIsBigInteger(val, "Wrong prime1 field");
        p = (BigInteger) val.getValue();
        val = der.read();
        DerUtil.checkIsBigInteger(val, "Wrong prime2 field");
        q = (BigInteger) val.getValue();
        val = der.read();
        DerUtil.checkIsBigInteger(val, "Wrong exponent1 field");
        dP = (BigInteger) val.getValue();
        val = der.read();
        DerUtil.checkIsBigInteger(val, "Wrong exponent2 field");
        dQ = (BigInteger) val.getValue();
        val = der.read();
        DerUtil.checkIsBigInteger(val, "Wrong coefficient field");
        qInv = (BigInteger) val.getValue();
      }
    catch (IOException x)
      {
        InvalidParameterException y = new InvalidParameterException();
        y.initCause(x);
        throw y;
      }
    PrivateKey result = new GnuRSAPrivateKey(Registry.PKCS8_ENCODING_ID,
                                             n, e, d, p, q, dP, dQ, qInv);
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "decodePrivateKey()", result);
    return result;
  }
}
