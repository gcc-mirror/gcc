/* EncryptedPrivateKeyInfo.java -- As in PKCS #8.
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


package javax.crypto;

import gnu.java.security.OID;
import gnu.java.security.der.DER;
import gnu.java.security.der.DERReader;
import gnu.java.security.der.DERValue;

import java.io.IOException;
import java.security.AlgorithmParameters;
import java.security.NoSuchAlgorithmException;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.PKCS8EncodedKeySpec;
import java.util.ArrayList;
import java.util.List;

/**
 * An implementation of the <code>EncryptedPrivateKeyInfo</code> ASN.1
 * type as specified in <a
 * href="http://www.rsasecurity.com/rsalabs/pkcs/pkcs-8/">PKCS #8 -
 * Private-Key Information Syntax Standard</a>.
 *
 * <p>The ASN.1 type <code>EncryptedPrivateKeyInfo</code> is:
 *
 * <blockquote>
 * <pre>EncryptedPrivateKeyInfo ::= SEQUENCE {
 *   encryptionAlgorithm EncryptionAlgorithmIdentifier,
 *   encryptedData EncryptedData }
 *
 * EncryptionAlgorithmIdentifier ::= AlgorithmIdentifier
 *
 * EncrytpedData ::= OCTET STRING
 *
 * AlgorithmIdentifier ::= SEQUENCE {
 *   algorithm  OBJECT IDENTIFIER,
 *   parameters ANY DEFINED BY algorithm OPTIONAL }</pre>
 * </blockquote>
 *
 * @author Casey Marshall (csm@gnu.org)
 * @since 1.4
 * @see java.security.spec.PKCS8EncodedKeySpec
 */
public class EncryptedPrivateKeyInfo
{

  // Fields.
  // ------------------------------------------------------------------------

  /** The encrypted data. */
  private byte[] encryptedData;

  /** The encoded, encrypted key. */
  private byte[] encoded;

  /** The OID of the encryption algorithm. */
  private OID algOid;

  /** The encryption algorithm's parameters. */
  private AlgorithmParameters params;

  /** The encoded ASN.1 algorithm parameters. */
  private byte[] encodedParams;

  // Constructors.
  // ------------------------------------------------------------------------

  /**
   * Create a new <code>EncryptedPrivateKeyInfo</code> object from raw
   * encrypted data and the parameters used for encryption.
   *
   * <p>The <code>encryptedData</code> array is cloned.
   *
   * @param params        The encryption algorithm parameters.
   * @param encryptedData The encrypted key data.
   * @throws java.lang.IllegalArgumentException If the
   *         <code>encryptedData</code> array is empty (zero-length).
   * @throws java.security.NoSuchAlgorithmException If the algorithm
   *         specified in the parameters is not supported.
   * @throws java.lang.NullPointerException If <code>encryptedData</code>
   *         is null.
   */
  public EncryptedPrivateKeyInfo(AlgorithmParameters params,
                                 byte[] encryptedData)
    throws IllegalArgumentException, NoSuchAlgorithmException
  {
    if (encryptedData.length == 0)
      {
        throw new IllegalArgumentException("0-length encryptedData");
      }
    this.params = params;
    algOid = new OID(params.getAlgorithm());
    this.encryptedData = (byte[]) encryptedData.clone();
  }

  /**
   * Create a new <code>EncryptedPrivateKeyInfo</code> from an encoded
   * representation, parsing the ASN.1 sequence.
   *
   * @param encoded The encoded info.
   * @throws java.io.IOException If parsing the encoded data fails.
   * @throws java.lang.NullPointerException If <code>encoded</code> is
   *         null.
   */
  public EncryptedPrivateKeyInfo(byte[] encoded)
    throws IOException
  {
    this.encoded = (byte[]) encoded.clone();
    decode();
  }

  /**
   * Create a new <code>EncryptedPrivateKeyInfo</code> from the cipher
   * name and the encrytpedData.
   *
   * <p>The <code>encryptedData</code> array is cloned.
   *
   * @param algName       The name of the algorithm (as an object identifier).
   * @param encryptedData The encrypted key data.
   * @throws java.lang.IllegalArgumentException If the
   *         <code>encryptedData</code> array is empty (zero-length).
   * @throws java.security.NoSuchAlgorithmException If algName is not
   *         the name of a supported algorithm.
   * @throws java.lang.NullPointerException If <code>encryptedData</code>
   *         is null.
   */
  public EncryptedPrivateKeyInfo(String algName, byte[] encryptedData)
    throws IllegalArgumentException, NoSuchAlgorithmException,
           NullPointerException
  {
    if (encryptedData.length == 0)
      {
        throw new IllegalArgumentException("0-length encryptedData");
      }
    this.algOid = new OID(algName);
    this.encryptedData = (byte[]) encryptedData.clone();
  }

  // Instance methods.
  // ------------------------------------------------------------------------

  /**
   * Return the name of the cipher used to encrypt this key.
   *
   * @return The algorithm name.
   */
  public String getAlgName()
  {
    return algOid.toString();
  }

  public AlgorithmParameters getAlgParameters()
  {
    if (params == null && encodedParams != null)
      {
        try
          {
            params = AlgorithmParameters.getInstance(getAlgName());
            params.init(encodedParams);
          }
        catch (NoSuchAlgorithmException ignore)
          {
          }
        catch (IOException ignore)
          {
          }
      }
    return params;
  }

  public synchronized byte[] getEncoded() throws IOException
  {
    if (encoded == null) encode();
    return (byte[]) encoded.clone();
  }

  public byte[] getEncryptedData()
  {
    return encryptedData;
  }

  public PKCS8EncodedKeySpec getKeySpec(Cipher cipher)
    throws InvalidKeySpecException
  {
    try
      {
        return new PKCS8EncodedKeySpec(cipher.doFinal(encryptedData));
      }
    catch (Exception x)
      {
        throw new InvalidKeySpecException(x.toString());
      }
  }

  // Own methods.
  // -------------------------------------------------------------------------

  private void decode() throws IOException
  {
    DERReader der = new DERReader(encoded);
    DERValue val = der.read();
    if (val.getTag() != DER.SEQUENCE)
      throw new IOException("malformed EncryptedPrivateKeyInfo");
    val = der.read();
    if (val.getTag() != DER.SEQUENCE)
      throw new IOException("malformed AlgorithmIdentifier");
    int algpLen = val.getLength();
    DERValue oid = der.read();
    if (oid.getTag() != DER.OBJECT_IDENTIFIER)
      throw new IOException("malformed AlgorithmIdentifier");
    algOid = (OID) oid.getValue();
    if (algpLen == 0)
      {
        val = der.read();
        if (val.getTag() != 0)
          {
            encodedParams = val.getEncoded();
            der.read();
          }
      }
    else if (oid.getEncodedLength() < val.getLength())
      {
        val = der.read();
        encodedParams = val.getEncoded();
      }
    val = der.read();
    if (val.getTag() != DER.OCTET_STRING)
      throw new IOException("malformed AlgorithmIdentifier");
    encryptedData = (byte[]) val.getValue();
  }

  private void encode() throws IOException
  {
    List algId = new ArrayList(2);
    algId.add(new DERValue(DER.OBJECT_IDENTIFIER, algOid));
    getAlgParameters();
    if (params != null)
      {
        algId.add(DERReader.read(params.getEncoded()));
      }
    List epki = new ArrayList(2);
    epki.add(new DERValue(DER.CONSTRUCTED|DER.SEQUENCE, algId));
    epki.add(new DERValue(DER.OCTET_STRING, encryptedData));
    encoded = new DERValue(DER.CONSTRUCTED|DER.SEQUENCE, epki).getEncoded();
  }
}
