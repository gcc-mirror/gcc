/* MacSpi.java -- The MAC service provider interface.
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
import java.security.InvalidKeyException;
import java.security.Key;
import java.security.spec.AlgorithmParameterSpec;

/**
 * This is the <i>Service Provider Interface</i> (<b>SPI</b>) for the
 * {@link Mac} class.
 *
 * <p>Providers wishing to implement a Mac must subclass this class and
 * provide appropriate implementations of all its abstract methods,
 * then provide an entry pointing to this implementation in the master
 * {@link java.security.Provider} class.
 *
 * <p>Implementations may optionally implement the {@link
 * java.lang.Cloneable} interface.
 *
 * @author Casey Marshall (csm@gnu.org)
 * @since 1.4
 */
public abstract class MacSpi
{

  // Constructor.
  // ------------------------------------------------------------------------

  /**
   * Create a new MacSpi instance.
   */
  public MacSpi()
  {
  }

  // Instance methods.
  // ------------------------------------------------------------------------

  /**
   * Returns a clone of this instance if cloning is supported.
   *
   * @return A clone of this instance.
   * @throws java.lang.CloneNotSupportedException If this instance does
   *         not support cloneing.
   */
  public Object clone() throws CloneNotSupportedException
  {
    return super.clone();
  }

  // Abstract instance methods.
  // ------------------------------------------------------------------------

  /**
   * Finalize the computation of this MAC and return the result as a
   * byte array.
   *
   * @return The MAC.
   */
  protected abstract byte[] engineDoFinal();

  /**
   * Return the total length, in bytes, of the computed MAC (the length
   * of the byte array returned by {@link #doFinal()}.
   *
   * @return The MAC length.
   */
  protected abstract int engineGetMacLength();

  /**
   * Initialize (or re-initialize) this instance.
   *
   * @param key    The key to use.
   * @param params The parameters to use.
   * @throws java.security.InvalidAlgorithmParameterException If this
   *         instance rejects the specified parameters.
   * @throws java.security.InvalidKeyException If this instance rejects
   *         the specified key.
   */
  protected abstract void engineInit(Key key, AlgorithmParameterSpec params)
    throws InvalidAlgorithmParameterException, InvalidKeyException;

  /**
   * Reset this instance. After this method succeeds, the state of this
   * instance should be the same as it was before any data was input
   * (possibly after a call to {@link
   * #init(java.security.Key,java.security.spec.AlgorithmParameterSpec)},
   * possibly not).
   */
  protected abstract void engineReset();

  /**
   * Update this MAC with a single byte.
   *
   * @param input The next byte.
   */
  protected abstract void engineUpdate(byte input);

  /**
   * Update this MAC with a portion of a byte array.
   *
   * @param input  The next bytes.
   * @param offset The index in <code>input</code> at which to start.
   * @param length The number of bytes to update.
   */
  protected abstract void engineUpdate(byte[] input, int offset, int length);
}
