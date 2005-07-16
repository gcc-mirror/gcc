/* SecretKeyFactorySpi.java -- Secret key factory service provider interface.
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

import java.security.InvalidKeyException;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.KeySpec;

/**
 * The <i>Service Provider Interface</i> (<b>SPI</b>) for the {@link
 * SecretKeyFactory} class.
 *
 * <p>Providers wishing to implement a secret key factory must
 * subclass this and provide an appropriate implementation for all the
 * abstract methods below, and provide an appropriate entry in the
 * master {@link java.security.Provider} class (the service name for
 * secret key factories is <code>"SecretKeyFactory"</code>).
 *
 * @author Casey Marshall (csm@gnu.org)
 * @since 1.4
 * @see SecretKeyFactory
 */
public abstract class SecretKeyFactorySpi
{

  // Constructor.
  // ------------------------------------------------------------------------

  /**
   * Create a new secret key factory SPI.
   */
  public SecretKeyFactorySpi()
  {
  }

  // Abstract instance methods.
  // ------------------------------------------------------------------------

  /**
   * Translate a {@link java.security.KeySpec} into a {@link SecretKey}.
   *
   * @param keySpec The key specification.
   * @return The secret key.
   * @throws java.security.spec.InvalidKeySpecException If the key specification
   *         cannot be translated into a secret key.
   */
  protected abstract SecretKey engineGenerateSecret(KeySpec keySpec)
    throws InvalidKeySpecException;

  /**
   * Translate a {@link SecretKey} into a {@link java.security.KeySpec}.
   *
   * @param key     The secret key.
   * @param keySpec The desired key specification class.
   * @return The key specification.
   * @throws java.security.spec.InvalidKeySpecException If the secret key cannot
   *         be translated into the desired key specification.
   */
  protected abstract KeySpec engineGetKeySpec(SecretKey key, Class keySpec)
    throws InvalidKeySpecException;

  /**
   * Translate a secret key into a different representation.
   *
   * @param key The secret key to translate.
   * @return The translated key.
   * @throws java.security.InvalidKeyException If the specified secret
   *         key cannot be translated.
   */
  protected abstract SecretKey engineTranslateKey(SecretKey key)
    throws InvalidKeyException;
}
