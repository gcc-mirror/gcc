/* DSAKeyFactory.java -- DSA key factory.
   Copyright (C) 2003 Free Software Foundation, Inc.

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
import java.security.interfaces.DSAPrivateKey;
import java.security.interfaces.DSAPublicKey;
import java.security.spec.DSAPrivateKeySpec;
import java.security.spec.DSAPublicKeySpec;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.KeySpec;

/**
 * DSA key factory.
 *
 * @author Casey Marshall (rsdio@metastatic.org)
 */
public class DSAKeyFactory extends KeyFactorySpi
{

  // Constructor.
  // ------------------------------------------------------------------------

  public DSAKeyFactory()
  {
    super();
  }

  // Instance methods.
  // ------------------------------------------------------------------------

  protected PrivateKey engineGeneratePrivate(KeySpec keySpec)
    throws InvalidKeySpecException
  {
    if (!(keySpec instanceof DSAPrivateKeySpec))
      throw new InvalidKeySpecException();
    return new GnuDSAPrivateKey(
      ((DSAPrivateKeySpec) keySpec).getX(),
      ((DSAPrivateKeySpec) keySpec).getP(),
      ((DSAPrivateKeySpec) keySpec).getQ(),
      ((DSAPrivateKeySpec) keySpec).getG());
  }

  protected PublicKey engineGeneratePublic(KeySpec keySpec)
    throws InvalidKeySpecException
  {
    if (!(keySpec instanceof DSAPublicKeySpec))
      throw new InvalidKeySpecException();
    return new GnuDSAPublicKey(
      ((DSAPublicKeySpec) keySpec).getY(),
      ((DSAPublicKeySpec) keySpec).getP(),
      ((DSAPublicKeySpec) keySpec).getQ(),
      ((DSAPublicKeySpec) keySpec).getG());
  }

  protected KeySpec engineGetKeySpec(Key key, Class keySpec)
    throws InvalidKeySpecException
  {
    if ((key instanceof DSAPublicKey) &&
         keySpec.isAssignableFrom(DSAPublicKeySpec.class))
      {
        return new DSAPublicKeySpec(((DSAPublicKey) key).getY(),
          ((DSAPublicKey) key).getParams().getP(),
          ((DSAPublicKey) key).getParams().getQ(),
          ((DSAPublicKey) key).getParams().getG());
      }
    if ((key instanceof DSAPrivateKey) &&
         keySpec.isAssignableFrom(DSAPrivateKeySpec.class))
      {
        return new DSAPrivateKeySpec(((DSAPrivateKey) key).getX(),
          ((DSAPrivateKey) key).getParams().getP(),
          ((DSAPrivateKey) key).getParams().getQ(),
          ((DSAPrivateKey) key).getParams().getG());
      }
    throw new InvalidKeySpecException();
  }

  protected Key engineTranslateKey(Key key) throws InvalidKeyException
  {
    if ((key instanceof GnuDSAPublicKey) || (key instanceof GnuDSAPrivateKey))
      return key;
    if (key instanceof DSAPublicKey)
      return new GnuDSAPublicKey(((DSAPublicKey) key).getY(),
        ((DSAPublicKey) key).getParams().getP(),
        ((DSAPublicKey) key).getParams().getQ(),
        ((DSAPublicKey) key).getParams().getG());
    if (key instanceof DSAPrivateKey)
      return new GnuDSAPrivateKey(((DSAPrivateKey) key).getX(),
        ((DSAPrivateKey) key).getParams().getP(),
        ((DSAPrivateKey) key).getParams().getQ(),
        ((DSAPrivateKey) key).getParams().getG());
    throw new InvalidKeyException();
  } 
}
