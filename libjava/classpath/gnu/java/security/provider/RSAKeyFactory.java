/* RSAKeyFactory.java -- RSA key factory.
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

public class RSAKeyFactory extends KeyFactorySpi
{

  // Default constructor.
  // -------------------------------------------------------------------------

  // Instance methods.
  // -------------------------------------------------------------------------

  protected PrivateKey engineGeneratePrivate(KeySpec spec)
    throws InvalidKeySpecException
  {
    if (spec instanceof RSAPrivateCrtKeySpec)
      {
        return new GnuRSAPrivateKey((RSAPrivateCrtKeySpec) spec);
      }
    if (spec instanceof RSAPrivateKeySpec)
      {
        return new GnuRSAPrivateKey(new RSAPrivateCrtKeySpec(
          ((RSAPrivateKeySpec) spec).getModulus(), null,
          ((RSAPrivateKeySpec) spec).getPrivateExponent(), null,
          null, null, null, null));
      }
    if (spec instanceof PKCS8EncodedKeySpec)
      {
        EncodedKeyFactory ekf = new EncodedKeyFactory();
        PrivateKey pk = ekf.engineGeneratePrivate(spec);
        if (pk instanceof RSAPrivateKey)
          return pk;
      }
    throw new InvalidKeySpecException();
  }

  protected PublicKey engineGeneratePublic(KeySpec spec)
    throws InvalidKeySpecException
  {
    if (spec instanceof RSAPublicKeySpec)
      {
        return new GnuRSAPublicKey((RSAPublicKeySpec) spec);
      }
    if (spec instanceof X509EncodedKeySpec)
      {
        EncodedKeyFactory ekf = new EncodedKeyFactory();
        PublicKey pk = ekf.engineGeneratePublic(spec);
        if (pk instanceof RSAPublicKey)
          return pk;
      }
    throw new InvalidKeySpecException();
  }

  protected KeySpec engineGetKeySpec(Key key, Class keySpec)
    throws InvalidKeySpecException
  {
    if (keySpec.isAssignableFrom(RSAPrivateCrtKeySpec.class)
        && (key instanceof RSAPrivateCrtKey))
      {
        return new RSAPrivateCrtKeySpec(
          ((RSAPrivateCrtKey) key).getModulus(),
          ((RSAPrivateCrtKey) key).getPublicExponent(),
          ((RSAPrivateCrtKey) key).getPrivateExponent(),
          ((RSAPrivateCrtKey) key).getPrimeP(),
          ((RSAPrivateCrtKey) key).getPrimeQ(),
          ((RSAPrivateCrtKey) key).getPrimeExponentP(),
          ((RSAPrivateCrtKey) key).getPrimeExponentQ(),
          ((RSAPrivateCrtKey) key).getCrtCoefficient());
      }
    if (keySpec.isAssignableFrom(RSAPrivateKeySpec.class)
        && (key instanceof RSAPrivateKey))
      {
        return new RSAPrivateKeySpec(
          ((RSAPrivateCrtKey) key).getModulus(),
          ((RSAPrivateCrtKey) key).getPrivateExponent());
      }
    if (keySpec.isAssignableFrom(RSAPublicKeySpec.class)
        && (key instanceof RSAPublicKey))
      {
        return new RSAPublicKeySpec(
          ((RSAPrivateCrtKey) key).getModulus(),
          ((RSAPrivateCrtKey) key).getPublicExponent());
      }
    if (keySpec.isAssignableFrom(PKCS8EncodedKeySpec.class)
        && key.getFormat().equalsIgnoreCase("PKCS#8"))
      {
        return new PKCS8EncodedKeySpec(key.getEncoded());
      }
    if (keySpec.isAssignableFrom(X509EncodedKeySpec.class)
        && key.getFormat().equalsIgnoreCase("X.509"))
      {
        return new X509EncodedKeySpec(key.getEncoded());
      }
    throw new InvalidKeySpecException();
  }

  protected Key engineTranslateKey(Key key) throws InvalidKeyException
  {
    if (key instanceof RSAPrivateCrtKey)
      {
        return new GnuRSAPrivateKey(new RSAPrivateCrtKeySpec(
          ((RSAPrivateCrtKey) key).getModulus(),
          ((RSAPrivateCrtKey) key).getPublicExponent(),
          ((RSAPrivateCrtKey) key).getPrivateExponent(),
          ((RSAPrivateCrtKey) key).getPrimeP(),
          ((RSAPrivateCrtKey) key).getPrimeQ(),
          ((RSAPrivateCrtKey) key).getPrimeExponentP(),
          ((RSAPrivateCrtKey) key).getPrimeExponentQ(),
          ((RSAPrivateCrtKey) key).getCrtCoefficient()));
      }
    if (key instanceof RSAPrivateKey)
      {
        return new GnuRSAPrivateKey(new RSAPrivateCrtKeySpec(
          ((RSAPrivateKey) key).getModulus(), null,
          ((RSAPrivateKey) key).getPrivateExponent(), null,
          null, null, null, null));
      }
    if (key instanceof RSAPublicKey)
      {
        return new GnuRSAPublicKey(new RSAPublicKeySpec(
          ((RSAPrivateCrtKey) key).getModulus(),
          ((RSAPrivateCrtKey) key).getPublicExponent()));
      }
    throw new InvalidKeyException();
  }
}
