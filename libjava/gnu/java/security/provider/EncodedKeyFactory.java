/* EncodedKeyFactory.java -- encoded key factory.
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


package gnu.java.security.provider;

import gnu.java.security.OID;
import gnu.java.security.der.BitString;
import gnu.java.security.der.DERReader;
import gnu.java.security.der.DERValue;

import java.io.IOException;
import java.math.BigInteger;
import java.security.AlgorithmParameters;
import java.security.InvalidKeyException;
import java.security.Key;
import java.security.KeyFactorySpi;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.spec.DSAParameterSpec;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.InvalidParameterSpecException;
import java.security.spec.KeySpec;
import java.security.spec.PKCS8EncodedKeySpec;
import java.security.spec.RSAPrivateCrtKeySpec;
import java.security.spec.RSAPublicKeySpec;
import java.security.spec.X509EncodedKeySpec;

import javax.crypto.spec.DHParameterSpec;

/**
 * A factory for keys encoded in either the X.509 format (for public
 * keys) or the PKCS#8 format (for private keys).
 *
 * @author Casey Marshall (rsdio@metastatic.org)
 */
public class EncodedKeyFactory extends KeyFactorySpi
{

  // Constants.
  // ------------------------------------------------------------------------

  private static final OID ID_DSA = new OID("1.2.840.10040.4.1");
  private static final OID ID_RSA = new OID("1.2.840.113549.1.1.1");
  private static final OID ID_DH  = new OID("1.2.840.10046.2.1");

  // Instance methods.
  // ------------------------------------------------------------------------

  public PublicKey engineGeneratePublic(KeySpec spec)
    throws InvalidKeySpecException
  {
    if (!(spec instanceof X509EncodedKeySpec))
      throw new InvalidKeySpecException("only supports X.509 key specs");
    DERReader der = new DERReader(((X509EncodedKeySpec) spec).getEncoded());
    try
      {
        DERValue spki = der.read();
        if (!spki.isConstructed())
          {
            throw new InvalidKeySpecException("malformed encoded key");
          }
        DERValue alg = der.read();
        if (!alg.isConstructed())
          {
            throw new InvalidKeySpecException("malformed encoded key");
          }
        DERValue val = der.read();
        if (!(val.getValue() instanceof OID))
          {
            throw new InvalidKeySpecException("malformed encoded key");
          }
        OID algId = (OID) val.getValue();
        byte[] algParams = null;
        if (alg.getLength() > val.getEncodedLength())
          {
            val = der.read();
            algParams = val.getEncoded();
            if (val.isConstructed())
              der.skip(val.getLength());
          }
        val = der.read();
        if (!(val.getValue() instanceof BitString))
          {
            throw new InvalidKeySpecException("malformed encoded key");
          }
        byte[] publicKey = ((BitString) val.getValue()).toByteArray();
        if (algId.equals(ID_DSA))
          {
            BigInteger p = null, g = null, q = null, Y;
            if (algParams != null)
              {
                DERReader dsaParams = new DERReader(algParams);
                val = dsaParams.read();
                if (!val.isConstructed())
                  throw new InvalidKeySpecException("malformed DSA parameters");
                val = dsaParams.read();
                if (!(val.getValue() instanceof BigInteger))
                  throw new InvalidKeySpecException("malformed DSA parameters");
                p = (BigInteger) val.getValue();
                val = dsaParams.read();
                if (!(val.getValue() instanceof BigInteger))
                  throw new InvalidKeySpecException("malformed DSA parameters");
                q = (BigInteger) val.getValue();
                val = dsaParams.read();
                if (!(val.getValue() instanceof BigInteger))
                  throw new InvalidKeySpecException("malformed DSA parameters");
                g = (BigInteger) val.getValue();
              }
            DERReader dsaPub = new DERReader(publicKey);
            val = dsaPub.read();
            if (!(val.getValue() instanceof BigInteger))
              throw new InvalidKeySpecException("malformed DSA parameters");
            Y = (BigInteger) val.getValue();
            return new GnuDSAPublicKey(Y, p, q, g);
          }
        else if (algId.equals(ID_RSA))
          {
            DERReader rsaParams = new DERReader(publicKey);
            if (!rsaParams.read().isConstructed())
              {
                throw new InvalidKeySpecException("malformed encoded key");
              }
            return new GnuRSAPublicKey(new RSAPublicKeySpec(
              (BigInteger) rsaParams.read().getValue(),
              (BigInteger) rsaParams.read().getValue()));
          }
        else if (algId.equals(ID_DH))
          {
            if (algParams == null)
              throw new InvalidKeySpecException("missing DH parameters");
            DERReader dhParams = new DERReader(algParams);
            val = dhParams.read();
            BigInteger p, g, q, Y;
            if (!val.isConstructed())
              throw new InvalidKeySpecException("malformed DH parameters");
            val = dhParams.read();
            if (!(val.getValue() instanceof BigInteger))
              throw new InvalidKeySpecException("malformed DH parameters");
            p = (BigInteger) val.getValue();
            val = dhParams.read();
            if (!(val.getValue() instanceof BigInteger))
              throw new InvalidKeySpecException("malformed DH parameters");
            g = (BigInteger) val.getValue();
            val = dhParams.read();
            if (!(val.getValue() instanceof BigInteger))
              throw new InvalidKeySpecException("malformed DH parameters");
            q = (BigInteger) val.getValue();
            DERReader dhPub = new DERReader(publicKey);
            val = dhPub.read();
            if (!(val.getValue() instanceof BigInteger))
              throw new InvalidKeySpecException("malformed DH parameters");
            Y = (BigInteger) val.getValue();
            return (PublicKey) new GnuDHPublicKey(new DHParameterSpec(p, g), Y, q);
          }
        else
          throw new InvalidKeySpecException("unknown algorithm: " + algId);
      }
    catch (IOException ioe)
      {
        throw new InvalidKeySpecException(ioe.getMessage());
      }
  }

  public PrivateKey engineGeneratePrivate(KeySpec spec)
    throws InvalidKeySpecException
  {
    if (!(spec instanceof PKCS8EncodedKeySpec))
      {
        throw new InvalidKeySpecException("only supports PKCS8 key specs");
      }
    DERReader der = new DERReader(((PKCS8EncodedKeySpec) spec).getEncoded());
    try
      {
        DERValue pki = der.read();
        if (!pki.isConstructed())
          {
            throw new InvalidKeySpecException("malformed encoded key");
          }
        DERValue val = der.read();
        if (!(val.getValue() instanceof BigInteger))
          {
            throw new InvalidKeySpecException("malformed encoded key");
          }
        DERValue alg = der.read();
        if (!alg.isConstructed())
          {
            throw new InvalidKeySpecException("malformed encoded key");
          }
        val = der.read();
        if (!(val.getValue() instanceof OID))
          {
            throw new InvalidKeySpecException("malformed encoded key");
          }
        OID algId = (OID) val.getValue();
        byte[] algParams = null;
        if (alg.getLength() > val.getEncodedLength())
          {
            val = der.read();
            algParams = val.getEncoded();
            if (val.isConstructed())
              der.skip(val.getLength());
          }
        byte[] privateKey = (byte[]) der.read().getValue();
        if (algId.equals(ID_DSA))
          {
            if (algParams == null)
              {
                throw new InvalidKeySpecException("missing DSA parameters");
              }
            AlgorithmParameters params = AlgorithmParameters.getInstance("DSA");
            params.init(algParams);
            DSAParameterSpec dsaSpec = (DSAParameterSpec)
              params.getParameterSpec(DSAParameterSpec.class);
            DERReader dsaPriv = new DERReader(privateKey);
            return new GnuDSAPrivateKey((BigInteger) dsaPriv.read().getValue(),
              dsaSpec.getP(), dsaSpec.getQ(), dsaSpec.getG());
          }
        else if (algId.equals(ID_RSA))
          {
            DERReader rsaParams = new DERReader(privateKey);
            if (!rsaParams.read().isConstructed())
              throw new InvalidKeySpecException("malformed encoded key");
            return new GnuRSAPrivateKey(new RSAPrivateCrtKeySpec(
              (BigInteger) rsaParams.read().getValue(), // n
              (BigInteger) rsaParams.read().getValue(), // e
              (BigInteger) rsaParams.read().getValue(), // d
              (BigInteger) rsaParams.read().getValue(), // p
              (BigInteger) rsaParams.read().getValue(), // q
              (BigInteger) rsaParams.read().getValue(), // d mod (p - 1)
              (BigInteger) rsaParams.read().getValue(), // d mod (q - 1)
              (BigInteger) rsaParams.read().getValue())); // (inv q) mod p
          }
        else
          throw new InvalidKeySpecException("unknown algorithm: " + algId);
      }
    catch (InvalidParameterSpecException iapse)
      {
        throw new InvalidKeySpecException(iapse.getMessage());
      }
    catch (NoSuchAlgorithmException nsae)
      {
        throw new InvalidKeySpecException(nsae.getMessage());
      }
    catch (IOException ioe)
      {
        throw new InvalidKeySpecException(ioe.getMessage());
      }
  }

  public KeySpec engineGetKeySpec(Key key, Class speClass)
    throws InvalidKeySpecException
  {
    if ((key instanceof PrivateKey) && key.getFormat().equals("PKCS#8")
        && speClass.isAssignableFrom(PKCS8EncodedKeySpec.class))
      return new PKCS8EncodedKeySpec(key.getEncoded());
    else if ((key instanceof PublicKey) && key.getFormat().equals("X.509")
        && speClass.isAssignableFrom(X509EncodedKeySpec.class))
      return new X509EncodedKeySpec(key.getEncoded());
    else
      throw new InvalidKeySpecException();
  }

  public Key engineTranslateKey(Key key) throws InvalidKeyException
  {
    throw new InvalidKeyException("translating keys not supported");
  }
}
