/* DiffieHellmanKeyFactoryImpl.java --
   Copyright (C) 2005 Free Software Foundation, Inc.

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

import gnu.javax.crypto.GnuDHPrivateKey;

import java.security.InvalidKeyException;
import java.security.Key;
import java.security.KeyFactorySpi;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.KeySpec;

import javax.crypto.spec.DHParameterSpec;
import javax.crypto.spec.DHPrivateKeySpec;
import javax.crypto.spec.DHPublicKeySpec;

import javax.crypto.interfaces.DHPrivateKey;
import javax.crypto.interfaces.DHPublicKey;

public class DiffieHellmanKeyFactoryImpl extends KeyFactorySpi
{
  protected PrivateKey engineGeneratePrivate (final KeySpec spec)
    throws InvalidKeySpecException
  {
    if (spec instanceof DHPrivateKeySpec)
      {
        DHPrivateKeySpec dh = (DHPrivateKeySpec) spec;
        return new GnuDHPrivateKey (dh.getX (),
                                    new DHParameterSpec (dh.getP (), dh.getG ()));
      }
    throw new InvalidKeySpecException ();
  }

  protected PublicKey engineGeneratePublic (final KeySpec spec)
    throws InvalidKeySpecException
  {
    if (spec instanceof DHPublicKeySpec)
      {
        DHPublicKeySpec dh = (DHPublicKeySpec) spec;
        return new GnuDHPublicKey (new DHParameterSpec (dh.getP (), dh.getG ()),
                                   dh.getY(), null);
      }
    throw new InvalidKeySpecException ();
  }

  protected KeySpec engineGetKeySpec (final Key key, final Class specClass)
    throws InvalidKeySpecException
  {
    if (key instanceof DHPrivateKey)
      {
        if (DHPrivateKeySpec.class.isAssignableFrom (specClass))
          {
            DHParameterSpec params = ((DHPrivateKey) key).getParams ();
            return new DHPrivateKeySpec (((DHPrivateKey) key).getX (),
                                         params.getP (), params.getG ());
          }
      }
    if (key instanceof DHPublicKey)
      {
        if (DHPublicKeySpec.class.isAssignableFrom (specClass))
          {
            DHParameterSpec params = ((DHPublicKey) key).getParams ();
            return new DHPublicKeySpec (((DHPublicKey) key).getY (),
                                        params.getP (), params.getG ());
          }
      }
    throw new InvalidKeySpecException ();
  }

  protected Key engineTranslateKey (final Key key)
    throws InvalidKeyException
  {
    if (key instanceof DHPrivateKey)
      {
        return new GnuDHPrivateKey (((DHPrivateKey) key).getX (),
                                    ((DHPrivateKey) key).getParams ());
      }
    if (key instanceof DHPublicKey)
      {
        return new GnuDHPublicKey (((DHPublicKey) key).getParams (),
                                   ((DHPublicKey) key).getY (), null);
      }
    throw new InvalidKeyException ();
  }
}
