/* KeyGeneratorSpi.java -- The key generator service provider interface.
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


package javax.crypto;

import java.security.InvalidAlgorithmParameterException;
import java.security.SecureRandom;
import java.security.spec.AlgorithmParameterSpec;

/**
 * The <i>Service Provider Interface</i> (<b>SPI</b>) for the {@link
 * KeyGenerator} class.
 *
 * <p>Providers wishing to implement a key generator must subclass this
 * and provide an appropriate implementation for all the abstract
 * methods below, and provide an appropriate entry in the master {@link
 * java.security.Provider} class (the service name for key generators is
 * <code>"KeyGenerator"</code>).
 *
 * @author Casey Marshall (csm@gnu.org)
 * @since 1.4
 * @see KeyGenerator
 */
public abstract class KeyGeneratorSpi
{

  // Constructor.
  // ------------------------------------------------------------------------

  /** Create a new key generator SPI. */
  public KeyGeneratorSpi()
  {
  }

  // Abstract instance methods.
  // ------------------------------------------------------------------------

  /**
   * Generate a key, returning it as a {@link SecretKey}.
   *
   * @return The generated key.
   */
  protected abstract SecretKey engineGenerateKey();

  /**
   * Initialize this key generator with parameters and a source of
   * randomness.
   *
   * @param params The parameters.
   * @param random The source of randomness.
   * @throws java.security.InvalidAlgorithmParameterException If the
   *         parameters are inappropriate for this instance.
   */
  protected abstract void engineInit(AlgorithmParameterSpec params,
                                     SecureRandom random)
    throws InvalidAlgorithmParameterException;

  /**
   * Initialize this key generator with a key size (in bits) and a
   * source of randomness.
   *
   * @param keySize The target key size, in bits.
   * @param random  The source of randomness.
   * @throws java.security.InvalidParameterException If the
   *         key size is illogical or unsupported.
   */
  protected abstract void engineInit(int keySize, SecureRandom random);

  /**
   * Initialize this key generator with a source of randomness; the
   * implementation should use reasonable default parameters (such as
   * generated key size).
   *
   * @param random The source of randomness.
   */
  protected abstract void engineInit(SecureRandom random);
}
