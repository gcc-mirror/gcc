/* RSAKeyFactory.java -- RSA key-factory JCE Adapter
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


package gnu.java.security.jce.sig;

import gnu.java.security.Registry;
import gnu.java.security.key.rsa.GnuRSAPrivateKey;
import gnu.java.security.key.rsa.GnuRSAPublicKey;
import gnu.java.security.key.rsa.RSAKeyPairPKCS8Codec;
import gnu.java.security.key.rsa.RSAKeyPairX509Codec;

import java.math.BigInteger;
import java.security.InvalidKeyException;
import java.security.Key;
import java.security.KeyFactorySpi;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.interfaces.RSAPrivateCrtKey;
import java.security.interfaces.RSAPrivateKey;
import java.security.interfaces.RSAPublicKey;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.KeySpec;
import java.security.spec.PKCS8EncodedKeySpec;
import java.security.spec.RSAPrivateCrtKeySpec;
import java.security.spec.RSAPrivateKeySpec;
import java.security.spec.RSAPublicKeySpec;
import java.security.spec.X509EncodedKeySpec;

public class RSAKeyFactory
    extends KeyFactorySpi
{
  // implicit 0-arguments constructor

  protected PublicKey engineGeneratePublic(KeySpec keySpec)
      throws InvalidKeySpecException
  {
    if (keySpec instanceof RSAPublicKeySpec)
      {
        RSAPublicKeySpec spec = (RSAPublicKeySpec) keySpec;
        BigInteger n = spec.getModulus();
        BigInteger e = spec.getPublicExponent();
        return new GnuRSAPublicKey(Registry.X509_ENCODING_ID, n, e);
      }
    if (keySpec instanceof X509EncodedKeySpec)
      {
        X509EncodedKeySpec spec = (X509EncodedKeySpec) keySpec;
        byte[] encoded = spec.getEncoded();
        PublicKey result;
        try
          {
            return new RSAKeyPairX509Codec().decodePublicKey(encoded);
          }
        catch (RuntimeException x)
          {
            throw new InvalidKeySpecException(x.getMessage(), x);
          }
      }
    throw new InvalidKeySpecException("Unsupported (public) key specification");
  }

  protected PrivateKey engineGeneratePrivate(KeySpec keySpec)
      throws InvalidKeySpecException
  {
    if (keySpec instanceof RSAPrivateCrtKeySpec)
      {
        RSAPrivateCrtKeySpec spec = (RSAPrivateCrtKeySpec) keySpec;
        BigInteger n = spec.getModulus();
        BigInteger e = spec.getPublicExponent();
        BigInteger d = spec.getPrivateExponent();
        BigInteger p = spec.getPrimeP();
        BigInteger q = spec.getPrimeQ();
        BigInteger dP = spec.getPrimeExponentP();
        BigInteger dQ = spec.getPrimeExponentQ();
        BigInteger qInv = spec.getCrtCoefficient();
        return new GnuRSAPrivateKey(Registry.PKCS8_ENCODING_ID,
                                    n, e, d, p, q, dP, dQ, qInv);
      }
    if (keySpec instanceof PKCS8EncodedKeySpec)
      {
        PKCS8EncodedKeySpec spec = (PKCS8EncodedKeySpec) keySpec;
        byte[] encoded = spec.getEncoded();
        PrivateKey result;
        try
          {
            return new RSAKeyPairPKCS8Codec().decodePrivateKey(encoded);
          }
        catch (RuntimeException x)
          {
            throw new InvalidKeySpecException(x.getMessage(), x);
          }
      }
    throw new InvalidKeySpecException("Unsupported (private) key specification");
  }

  protected KeySpec engineGetKeySpec(Key key, Class keySpec)
      throws InvalidKeySpecException
  {
    if (key instanceof RSAPublicKey)
      {
        if (keySpec.isAssignableFrom(RSAPublicKeySpec.class))
        {
          RSAPublicKey rsaKey = (RSAPublicKey) key;
          BigInteger n = rsaKey.getModulus();
          BigInteger e = rsaKey.getPublicExponent();
          return new RSAPublicKeySpec(n, e);
        }
        if (keySpec.isAssignableFrom(X509EncodedKeySpec.class))
          {
            if (key instanceof GnuRSAPublicKey)
              {
                GnuRSAPublicKey rsaKey = (GnuRSAPublicKey) key;
                byte[] encoded = rsaKey.getEncoded(Registry.X509_ENCODING_ID);
                return new X509EncodedKeySpec(encoded);
              }

            if (Registry.X509_ENCODING_SORT_NAME.equalsIgnoreCase(key.getFormat()))
              {
                byte[] encoded = key.getEncoded();
                return new X509EncodedKeySpec(encoded);
              }
            throw new InvalidKeySpecException(
                "Wrong key type or unsupported (public) key specification");
          }
        throw new InvalidKeySpecException("Unsupported (public) key specification");
      }
    if ((key instanceof RSAPrivateCrtKey)
        && keySpec.isAssignableFrom(RSAPrivateCrtKeySpec.class))
      {
        RSAPrivateCrtKey rsaKey = (RSAPrivateCrtKey) key;
        BigInteger n = rsaKey.getModulus();
        BigInteger e = rsaKey.getPublicExponent();
        BigInteger d = rsaKey.getPrivateExponent();
        BigInteger p = rsaKey.getPrimeP();
        BigInteger q = rsaKey.getPrimeQ();
        BigInteger dP = rsaKey.getPrimeExponentP();
        BigInteger dQ = rsaKey.getPrimeExponentQ();
        BigInteger qInv = rsaKey.getCrtCoefficient();
        return new RSAPrivateCrtKeySpec(n, e, d, p, q, dP, dQ, qInv);
      }
    if ((key instanceof RSAPrivateKey)
        && keySpec.isAssignableFrom(RSAPrivateKeySpec.class))
      {
        RSAPrivateKey rsaKey = (RSAPrivateKey) key;
        BigInteger n = rsaKey.getModulus();
        BigInteger d = rsaKey.getPrivateExponent();
        return new RSAPrivateKeySpec(n, d);
      }
    if (keySpec.isAssignableFrom(PKCS8EncodedKeySpec.class))
      {
        if (key instanceof GnuRSAPrivateKey)
          {
            GnuRSAPrivateKey rsaKey = (GnuRSAPrivateKey) key;
            byte[] encoded = rsaKey.getEncoded(Registry.PKCS8_ENCODING_ID);
            return new PKCS8EncodedKeySpec(encoded);
          }
        if (Registry.PKCS8_ENCODING_SHORT_NAME.equalsIgnoreCase(key.getFormat()))
          {
            byte[] encoded = key.getEncoded();
            return new PKCS8EncodedKeySpec(encoded);
          }
        throw new InvalidKeySpecException(
            "Wrong key type or unsupported (private) key specification");
      }
    throw new InvalidKeySpecException(
        "Wrong key type or unsupported key specification");
  }

  protected Key engineTranslateKey(Key key) throws InvalidKeyException
  {
    if ((key instanceof GnuRSAPublicKey) || (key instanceof GnuRSAPrivateKey))
      return key;

    if (key instanceof RSAPublicKey)
      {
        RSAPublicKey rsaKey = (RSAPublicKey) key;
        BigInteger n = rsaKey.getModulus();
        BigInteger e = rsaKey.getPublicExponent();
        return new GnuRSAPublicKey(Registry.X509_ENCODING_ID, n, e);
      }
    if (key instanceof RSAPrivateCrtKey)
      {
        RSAPrivateCrtKey rsaKey = (RSAPrivateCrtKey) key;
        BigInteger n = rsaKey.getModulus();
        BigInteger e = rsaKey.getPublicExponent();
        BigInteger d = rsaKey.getPrivateExponent();
        BigInteger p = rsaKey.getPrimeP();
        BigInteger q = rsaKey.getPrimeQ();
        BigInteger dP = rsaKey.getPrimeExponentP();
        BigInteger dQ = rsaKey.getPrimeExponentQ();
        BigInteger qInv = rsaKey.getCrtCoefficient();
        return new GnuRSAPrivateKey(Registry.PKCS8_ENCODING_ID,
                                    n, e, d, p, q, dP, dQ, qInv);
      }
    throw new InvalidKeyException("Unsupported key type");
  }
}
