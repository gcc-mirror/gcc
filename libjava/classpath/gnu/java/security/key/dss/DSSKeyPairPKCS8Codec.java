/* DSSKeyPairPKCS8Codec.java -- PKCS#8 Encoding/Decoding handler
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


package gnu.java.security.key.dss;

import gnu.java.security.Configuration;
import gnu.java.security.OID;
import gnu.java.security.Registry;
import gnu.java.security.der.DER;
import gnu.java.security.der.DERReader;
import gnu.java.security.der.DERValue;
import gnu.java.security.der.DERWriter;
import gnu.java.security.key.IKeyPairCodec;
import gnu.java.security.util.DerUtil;
import gnu.java.security.util.Util;

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
 * decode PKCS#8 ASN.1 external representation of DSS private keys.
 *
 * @author Casey Marshall (rsdio@metastatic.org)
 */
public class DSSKeyPairPKCS8Codec
    implements IKeyPairCodec
{
  private static final Logger log = Logger.getLogger(DSSKeyPairPKCS8Codec.class.getName());
  private static final OID DSA_ALG_OID = new OID(Registry.DSA_OID_STRING);

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
   * Returns the PKCS#8 ASN.1 <i>PrivateKeyInfo</i> representation of a DSA
   * private key. The ASN.1 specification is as follows:
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
   *   DssParams ::= SEQUENCE {
   *     p   INTEGER,
   *     q   INTEGER,
   *     g   INTEGER
   *   }
   * </pre>
   * 
   * @return the DER encoded form of the ASN.1 representation of the
   *         <i>PrivateKeyInfo</i> field in an X.509 certificate.
   * @throw InvalidParameterException if an error occurs during the marshalling
   *        process.
   */
  public byte[] encodePrivateKey(PrivateKey key)
  {
    if (! (key instanceof DSSPrivateKey))
      throw new InvalidParameterException("Wrong key type");

    DERValue derVersion = new DERValue(DER.INTEGER, BigInteger.ZERO);

    DERValue derOID = new DERValue(DER.OBJECT_IDENTIFIER, DSA_ALG_OID);

    DSSPrivateKey pk = (DSSPrivateKey) key;
    BigInteger p = pk.getParams().getP();
    BigInteger q = pk.getParams().getQ();
    BigInteger g = pk.getParams().getG();
    BigInteger x = pk.getX();

    ArrayList params = new ArrayList(3);
    params.add(new DERValue(DER.INTEGER, p));
    params.add(new DERValue(DER.INTEGER, q));
    params.add(new DERValue(DER.INTEGER, g));
    DERValue derParams = new DERValue(DER.CONSTRUCTED | DER.SEQUENCE, params);

    ArrayList algorithmID = new ArrayList(2);
    algorithmID.add(derOID);
    algorithmID.add(derParams);
    DERValue derAlgorithmID = new DERValue(DER.CONSTRUCTED | DER.SEQUENCE,
                                           algorithmID);

    // The OCTET STRING is the DER encoding of an INTEGER.
    DERValue derX = new DERValue(DER.INTEGER, x);
    DERValue derPrivateKey = new DERValue(DER.OCTET_STRING, derX.getEncoded());

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
        InvalidParameterException y = new InvalidParameterException(e.getMessage());
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
   * @param input the byte array to unmarshall into a valid DSS
   *          {@link PrivateKey} instance. MUST NOT be null.
   * @return a new instance of a {@link DSSPrivateKey} decoded from the
   *         <i>PrivateKeyInfo</i> material fed as <code>input</code>.
   * @throw InvalidParameterException if an exception occurs during the
   *        unmarshalling process.
   */
  public PrivateKey decodePrivateKey(byte[] input)
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "decodePrivateKey");
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
        if (! algOID.equals(DSA_ALG_OID))
          throw new InvalidParameterException("Unexpected OID: " + algOID);

        DERValue derParams = der.read();
        DerUtil.checkIsConstructed(derParams, "Wrong DSS Parameters field");

        DERValue val = der.read();
        DerUtil.checkIsBigInteger(val, "Wrong P field");
        p = (BigInteger) val.getValue();
        val = der.read();
        DerUtil.checkIsBigInteger(val, "Wrong Q field");
        q = (BigInteger) val.getValue();
        val = der.read();
        DerUtil.checkIsBigInteger(val, "Wrong G field");
        g = (BigInteger) val.getValue();

        val = der.read();
        if (Configuration.DEBUG)
          log.fine("val = " + val);
        byte[] xBytes = (byte[]) val.getValue();
        if (Configuration.DEBUG)
          log.fine(Util.dumpString(xBytes, "xBytes: "));
        DERReader der2 = new DERReader(xBytes);
        val = der2.read();
        DerUtil.checkIsBigInteger(val, "Wrong X field");
        x = (BigInteger) val.getValue();
      }
    catch (IOException e)
      {
        InvalidParameterException y = new InvalidParameterException(e.getMessage());
        y.initCause(e);
        throw y;
      }
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "decodePrivateKey");
    return new DSSPrivateKey(Registry.PKCS8_ENCODING_ID, p, q, g, x);
  }
}
