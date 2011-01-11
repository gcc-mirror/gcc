/* DSSKeyFactory.java -- JCE DSA key factory Adapter
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
import gnu.java.security.key.dss.DSSKeyPairPKCS8Codec;
import gnu.java.security.key.dss.DSSKeyPairX509Codec;
import gnu.java.security.key.dss.DSSPrivateKey;
import gnu.java.security.key.dss.DSSPublicKey;

import java.math.BigInteger;
import java.security.InvalidKeyException;
import java.security.Key;
import java.security.KeyFactorySpi;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.interfaces.DSAPrivateKey;
import java.security.interfaces.DSAPublicKey;
import java.security.spec.DSAPrivateKeySpec;
import java.security.spec.DSAPublicKeySpec;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.KeySpec;
import java.security.spec.PKCS8EncodedKeySpec;
import java.security.spec.X509EncodedKeySpec;

/**
 * DSA key factory.
 *
 * @author Casey Marshall (rsdio@metastatic.org)
 */
public class DSSKeyFactory
    extends KeyFactorySpi
{
  // implicit 0-arguments constructor

  protected PublicKey engineGeneratePublic(KeySpec keySpec)
      throws InvalidKeySpecException
  {
    if (keySpec instanceof DSAPublicKeySpec)
      {
        DSAPublicKeySpec spec = (DSAPublicKeySpec) keySpec;
        BigInteger p = spec.getP();
        BigInteger q = spec.getQ();
        BigInteger g = spec.getG();
        BigInteger y = spec.getY();
        return new DSSPublicKey(Registry.X509_ENCODING_ID, p, q, g, y);
      }
    if (keySpec instanceof X509EncodedKeySpec)
      {
        X509EncodedKeySpec spec = (X509EncodedKeySpec) keySpec;
        byte[] encoded = spec.getEncoded();
        PublicKey result;
        try
          {
            result = new DSSKeyPairX509Codec().decodePublicKey(encoded);
            return result;
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
    if (keySpec instanceof DSAPrivateKeySpec)
      {
        DSAPrivateKeySpec spec = (DSAPrivateKeySpec) keySpec;
        BigInteger p = spec.getP();
        BigInteger q = spec.getQ();
        BigInteger g = spec.getG();
        BigInteger x = spec.getX();
        return new DSSPrivateKey(Registry.PKCS8_ENCODING_ID, p, q, g, x);
      }
    if (keySpec instanceof PKCS8EncodedKeySpec)
      {
        PKCS8EncodedKeySpec spec = (PKCS8EncodedKeySpec) keySpec;
        byte[] encoded = spec.getEncoded();
        PrivateKey result;
        try
          {
            result = new DSSKeyPairPKCS8Codec().decodePrivateKey(encoded);
            return result;
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
    if (key instanceof DSAPublicKey)
      {
        if (keySpec.isAssignableFrom(DSAPublicKeySpec.class))
          {
            DSAPublicKey dsaKey = (DSAPublicKey) key;
            BigInteger p = dsaKey.getParams().getP();
            BigInteger q = dsaKey.getParams().getQ();
            BigInteger g = dsaKey.getParams().getG();
            BigInteger y = dsaKey.getY();
            return new DSAPublicKeySpec(y, p, q, g);
          }
        if (keySpec.isAssignableFrom(X509EncodedKeySpec.class))
          {
            if (key instanceof DSSPublicKey)
              {
                DSSPublicKey dssKey = (DSSPublicKey) key;
                byte[] encoded = dssKey.getEncoded(Registry.X509_ENCODING_ID);
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
    if (key instanceof DSAPrivateKey)
      {
        if (keySpec.isAssignableFrom(DSAPrivateKeySpec.class))
          {
            DSAPrivateKey dsaKey = (DSAPrivateKey) key;
            BigInteger p = dsaKey.getParams().getP();
            BigInteger q = dsaKey.getParams().getQ();
            BigInteger g = dsaKey.getParams().getG();
            BigInteger x = dsaKey.getX();
            return new DSAPrivateKeySpec(x, p, q, g);
          }
        if (keySpec.isAssignableFrom(PKCS8EncodedKeySpec.class))
          {
            if (key instanceof DSSPrivateKey)
              {
                DSSPrivateKey dssKey = (DSSPrivateKey) key;
                byte[] encoded = dssKey.getEncoded(Registry.PKCS8_ENCODING_ID);
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
        throw new InvalidKeySpecException("Unsupported (private) key specification");
      }
    throw new InvalidKeySpecException("Wrong key type or unsupported key specification");
  }

  protected Key engineTranslateKey(Key key) throws InvalidKeyException
  {
    if ((key instanceof DSSPublicKey) || (key instanceof DSSPrivateKey))
      return key;

    if (key instanceof DSAPublicKey)
      {
        DSAPublicKey dsaKey = (DSAPublicKey) key;
        BigInteger p = dsaKey.getParams().getP();
        BigInteger q = dsaKey.getParams().getQ();
        BigInteger g = dsaKey.getParams().getG();
        BigInteger y = dsaKey.getY();
        return new DSSPublicKey(Registry.X509_ENCODING_ID, p, q, g, y);
      }
    if (key instanceof DSAPrivateKey)
      {
        DSAPrivateKey dsaKey = (DSAPrivateKey) key;
        BigInteger p = dsaKey.getParams().getP();
        BigInteger q = dsaKey.getParams().getQ();
        BigInteger g = dsaKey.getParams().getG();
        BigInteger x = dsaKey.getX();
        return new DSSPrivateKey(Registry.PKCS8_ENCODING_ID, p, q, g, x);
      }
    throw new InvalidKeyException("Wrong key type");
  }
}
