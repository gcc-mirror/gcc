/* SignerInfo.java -- a SignerInfo object, from PKCS #7
   Copyright (C) 2004, 2005  Free Software Foundation, Inc.

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

package gnu.java.security.pkcs;

import gnu.java.security.OID;
import gnu.java.security.ber.BER;
import gnu.java.security.ber.BEREncodingException;
import gnu.java.security.ber.BERReader;
import gnu.java.security.ber.BERValue;
import gnu.java.security.der.DERValue;

import java.io.IOException;

import java.math.BigInteger;

import javax.security.auth.x500.X500Principal;

public class SignerInfo
{
  private final BigInteger version;
  private final BigInteger serialNumber;
  private final X500Principal issuer;
  private final OID digestAlgorithmId;
  private final byte[] digestAlgorithmParams;
  private final byte[] authenticatedAttributes;
  private final OID digestEncryptionAlgorithmId;
  private final byte[] digestEncryptionAlgorithmParams;
  private final byte[] encryptedDigest;
  private final byte[] unauthenticatedAttributes;

  private static final boolean DEBUG = false;
  private static void debug(String msg)
  {
    System.err.print("SignerInfo >> ");
    System.err.println(msg);
  }

  /**
   * Parse a SignerInfo object.
   */
  public SignerInfo(BERReader ber) throws IOException
  {
    DERValue val = ber.read();
    if (DEBUG)
      debug("SignerInfo: " + val);
    if (!val.isConstructed())
      throw new BEREncodingException("malformed SignerInfo");

    val = ber.read();
    if (val.getTag() != BER.INTEGER)
      throw new BEREncodingException("malformed Version");
    version = (BigInteger) val.getValue();

    if (DEBUG)
      debug("  Version: " + version);

    val = ber.read();
    if (!val.isConstructed())
      throw new BEREncodingException("malformed IssuerAndSerialNumber");

    if (DEBUG)
      debug("  IssuerAndSerialNumber: " + val);

    val = ber.read();
    if (!val.isConstructed())
      throw new BEREncodingException("malformed Issuer");
    issuer = new X500Principal(val.getEncoded());
    ber.skip(val.getLength());
    if (DEBUG)
      debug("    Issuer: " + issuer);

    val = ber.read();
    if (val.getTag() != BER.INTEGER)
      throw new BEREncodingException("malformed SerialNumber");
    serialNumber = (BigInteger) val.getValue();
    if (DEBUG)
      debug("    SerialNumber: " + serialNumber);

    val = ber.read();
    if (!val.isConstructed())
      throw new BEREncodingException("malformed DigestAlgorithmIdentifier");
    if (DEBUG)
      debug("  DigestAlgorithmIdentifier: " + val);

    int count = 0;
    DERValue val2 = ber.read();
    if (val2.getTag() != BER.OBJECT_IDENTIFIER)
      throw new BEREncodingException("malformed AlgorithmIdentifier");
    digestAlgorithmId = (OID) val2.getValue();
    if (DEBUG)
      debug("    OID: " + digestAlgorithmId);

    if (BERValue.isIndefinite(val))
      {
        val2 = ber.read();
        if (val2 != BER.END_OF_SEQUENCE)
          {
            digestAlgorithmParams = val2.getEncoded();
            val2 = ber.read();
            if (val2 != BER.END_OF_SEQUENCE)
              throw new BEREncodingException("expecting BER end-of-sequence");
          }
        else
          digestAlgorithmParams = null;
      }
    else if (val2.getEncodedLength() < val.getLength())
      {
        val2 = ber.read();
        digestAlgorithmParams = val2.getEncoded();
        if (val2.isConstructed())
          ber.skip(val2.getLength());
      }
    else
      digestAlgorithmParams = null;
    if(DEBUG)
      debug("    params: " + (digestAlgorithmParams == null ? null
                              : new BigInteger(digestAlgorithmParams).toString(16)));

    val = ber.read();
    if (val.getTag() == 0)
      {
        authenticatedAttributes = val.getEncoded();
        val = ber.read();
        if (val.isConstructed())
          ber.skip(val.getLength());
        if (DEBUG)
          debug("  AuthenticatedAttributes: " + val);
        val = ber.read();
      }
    else
      authenticatedAttributes = null;

    if (!val.isConstructed())
      throw new BEREncodingException("malformed DigestEncryptionAlgorithmIdentifier");
    if (DEBUG)
      debug("  DigestEncryptionAlgorithmIdentifier: " + val);
    count = 0;
    val2 = ber.read();
    if (val2.getTag() != BER.OBJECT_IDENTIFIER)
      throw new BEREncodingException("malformed AlgorithmIdentifier");
    digestEncryptionAlgorithmId = (OID) val2.getValue();
    if (DEBUG)
      debug("    OID: " + digestEncryptionAlgorithmId);

    if (BERValue.isIndefinite(val))
      {
        val2 = ber.read();
        if (val2 != BER.END_OF_SEQUENCE)
          {
            digestEncryptionAlgorithmParams = val2.getEncoded();
            val2 = ber.read();
            if (val2 != BER.END_OF_SEQUENCE)
              throw new BEREncodingException("expecting BER end-of-sequence");
          }
        else
          digestEncryptionAlgorithmParams = null;
      }
    else if (val2.getEncodedLength() < val.getLength())
      {
        val2 = ber.read();
        digestEncryptionAlgorithmParams = val2.getEncoded();
        if (val2.isConstructed())
          ber.skip(val2.getLength());
      }
    else
      digestEncryptionAlgorithmParams = null;
    if(DEBUG)
      debug("    params: " + (digestEncryptionAlgorithmParams == null ? null
                              : new BigInteger(digestEncryptionAlgorithmParams).toString(16)));

    val = ber.read();
    if (val.getTag() != BER.OCTET_STRING)
      throw new BEREncodingException("malformed EncryptedDigest");
    encryptedDigest = (byte[]) val.getValue();
    if (DEBUG)
      debug("  EncryptedDigest: " + new BigInteger(1, encryptedDigest).toString(16));

    if (ber.peek() == 1)
      unauthenticatedAttributes = ber.read().getEncoded();
    else
      unauthenticatedAttributes = null;

    if (ber.peek() == 0)
      ber.read();
  }

  public BigInteger getVersion()
  {
    return version;
  }

  public BigInteger getSerialNumber()
  {
    return serialNumber;
  }

  public X500Principal getIssuer()
  {
    return issuer;
  }

  public OID getDigestAlgorithmId()
  {
    return digestAlgorithmId;
  }

  public byte[] getDigestAlgorithmParams()
  {
    return (digestAlgorithmParams != null
            ? (byte[]) digestAlgorithmParams.clone()
            : null);
  }

  public byte[] getAuthenticatedAttributes()
  {
    return (authenticatedAttributes != null
            ? (byte[]) authenticatedAttributes.clone()
            : null);
  }

  public OID getDigestEncryptionAlgorithmId()
  {
    return digestEncryptionAlgorithmId;
  }

  public byte[] getDigestEncryptionAlgorithmParams()
  {
    return (digestEncryptionAlgorithmParams != null
            ? (byte[]) digestEncryptionAlgorithmParams.clone()
            : null);
  }

  public byte[] getEncryptedDigest()
  {
    return (encryptedDigest != null ? (byte[]) encryptedDigest.clone() : null);
  }

  public byte[] getUnauthenticatedAttributes()
  {
    return (unauthenticatedAttributes != null
            ? (byte[]) unauthenticatedAttributes.clone()
            : null);
  }
}
