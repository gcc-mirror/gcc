/* Mac.java -- The message authentication code interface.
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

import gnu.java.security.Engine;

import java.lang.reflect.InvocationTargetException;
import java.nio.ByteBuffer;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.Key;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.Provider;
import java.security.Security;
import java.security.spec.AlgorithmParameterSpec;

/**
 * This class implements a "message authentication code" (MAC), a method
 * to ensure the integrity of data transmitted between two parties who
 * share a common secret key.
 *
 * <p>The best way to describe a MAC is as a <i>keyed one-way hash
 * function</i>, which looks like:
 *
 * <blockquote><p><code>D = MAC(K, M)</code></blockquote>
 *
 * <p>where <code>K</code> is the key, <code>M</code> is the message,
 * and <code>D</code> is the resulting digest. One party will usually
 * send the concatenation <code>M || D</code> to the other party, who
 * will then verify <code>D</code> by computing <code>D'</code> in a
 * similar fashion. If <code>D == D'</code>, then the message is assumed
 * to be authentic.
 *
 * @author Casey Marshall (csm@gnu.org)
 */
public class Mac implements Cloneable
{

  // Fields.
  // ------------------------------------------------------------------------

  private static final String SERVICE = "Mac";

  /** The underlying MAC implementation. */
  private MacSpi macSpi;

  /** The provider we got our implementation from. */
  private Provider provider;

  /** The name of the algorithm. */
  private String algorithm;

  /** Whether or not we've been initialized. */
  private boolean virgin;

  // Constructor.
  // ------------------------------------------------------------------------

  /**
   * Creates a new Mac instance.
   *
   * @param macSpi    The underlying MAC implementation.
   * @param provider  The provider of this implementation.
   * @param algorithm The name of this MAC algorithm.
   */
  protected Mac(MacSpi macSpi, Provider provider, String algorithm)
  {
    this.macSpi = macSpi;
    this.provider = provider;
    this.algorithm = algorithm;
    virgin = true;
  }

  /**
   * Create an instance of the named algorithm from the first provider with an
   * appropriate implementation.
   *
   * @param algorithm The name of the algorithm.
   * @return An appropriate Mac instance, if the specified algorithm is
   *         implemented by a provider.
   * @throws NoSuchAlgorithmException If no implementation of the named
   *           algorithm is installed.
   * @throws IllegalArgumentException if <code>algorithm</code> is
   *           <code>null</code> or is an empty string.
   */
  public static final Mac getInstance(String algorithm)
      throws NoSuchAlgorithmException
  {
    Provider[] p = Security.getProviders();
    NoSuchAlgorithmException lastException = null;
    for (int i = 0; i < p.length; i++)
      try
        {
          return getInstance(algorithm, p[i]);
        }
      catch (NoSuchAlgorithmException x)
        {
          lastException = x;
        }
      if (lastException != null)
        throw lastException;
      throw new NoSuchAlgorithmException(algorithm);
  }

  /**
   * Create an instance of the named algorithm from the named provider.
   *
   * @param algorithm The name of the algorithm.
   * @param provider The name of the provider.
   * @return An appropriate Mac instance, if the specified algorithm is
   *         implemented by the named provider.
   * @throws NoSuchAlgorithmException If the named provider has no
   *           implementation of the algorithm.
   * @throws NoSuchProviderException If the named provider does not exist.
   * @throws IllegalArgumentException if either <code>algorithm</code> or
   *           <code>provider</code> is <code>null</code>, or if
   *           <code>algorithm</code> is an empty string.
   */
  public static final Mac getInstance(String algorithm, String provider)
      throws NoSuchAlgorithmException, NoSuchProviderException
  {
    if (provider == null)
      throw new IllegalArgumentException("provider MUST NOT be null");
    Provider p = Security.getProvider(provider);
    if (p == null)
      throw new NoSuchProviderException(provider);
    return getInstance(algorithm, p);
  }

  /**
   * Create an instance of the named algorithm from a provider.
   *
   * @param algorithm The name of the algorithm.
   * @param provider The provider.
   * @return An appropriate Mac instance, if the specified algorithm is
   *         implemented by the provider.
   * @throws NoSuchAlgorithmException If the provider has no implementation of
   *           the algorithm.
   * @throws IllegalArgumentException if either <code>algorithm</code> or
   *           <code>provider</code> is <code>null</code>, or if
   *           <code>algorithm</code> is an empty string.
   */
  public static final Mac getInstance(String algorithm, Provider provider)
      throws NoSuchAlgorithmException
  {
    StringBuilder sb = new StringBuilder("Mac algorithm [")
        .append(algorithm).append("] from provider[")
        .append(provider).append("] could not be created");
    Throwable cause;
    try
      {
        Object spi = Engine.getInstance(SERVICE, algorithm, provider);
        return new Mac((MacSpi) spi, provider, algorithm);
      }
    catch (InvocationTargetException x)
      {
        cause = x.getCause();
        if (cause instanceof NoSuchAlgorithmException)
          throw (NoSuchAlgorithmException) cause;
        if (cause == null)
          cause = x;
      }
    catch (ClassCastException x)
      {
        cause = x;
      }
    NoSuchAlgorithmException x = new NoSuchAlgorithmException(sb.toString());
    x.initCause(cause);
    throw x;
  }

  /**
   * Finishes the computation of a MAC and returns the digest.
   *
   * <p>After this method succeeds, it may be used again as just after a
   * call to <code>init</code>, and can compute another MAC using the
   * same key and parameters.
   *
   * @return The message authentication code.
   * @throws java.lang.IllegalStateException If this instnace has not
   *         been initialized.
   */
  public final byte[] doFinal() throws IllegalStateException
  {
    if (virgin)
      {
        throw new IllegalStateException("not initialized");
      }
    byte[] digest = macSpi.engineDoFinal();
    reset();
    return digest;
  }

  /**
   * Finishes the computation of a MAC with a final byte array (or
   * computes a MAC over those bytes only) and returns the digest.
   *
   * <p>After this method succeeds, it may be used again as just after a
   * call to <code>init</code>, and can compute another MAC using the
   * same key and parameters.
   *
   * @param input The bytes to add.
   * @return The message authentication code.
   * @throws java.lang.IllegalStateException If this instnace has not
   *         been initialized.
   */
  public final byte[] doFinal(byte[] input) throws IllegalStateException
  {
    update(input);
    byte[] digest = macSpi.engineDoFinal();
    reset();
    return digest;
  }

  /**
   * Finishes the computation of a MAC and places the result into the
   * given array.
   *
   * <p>After this method succeeds, it may be used again as just after a
   * call to <code>init</code>, and can compute another MAC using the
   * same key and parameters.
   *
   * @param output    The destination for the result.
   * @param outOffset The index in the output array to start.
   * @return The message authentication code.
   * @throws java.lang.IllegalStateException If this instnace has not
   *         been initialized.
   * @throws javax.crypto.ShortBufferException If <code>output</code> is
   *         not large enough to hold the result.
   */
  public final void doFinal(byte[] output, int outOffset)
  throws IllegalStateException, ShortBufferException
  {
    if (virgin)
      {
        throw new IllegalStateException("not initialized");
      }
    if (output.length - outOffset < getMacLength())
      {
        throw new ShortBufferException();
      }
    byte[] mac = macSpi.engineDoFinal();
    System.arraycopy(mac, 0, output, outOffset, getMacLength());
    reset();
  }

  /**
   * Returns the name of this MAC algorithm.
   *
   * @return The MAC name.
   */
  public final String getAlgorithm()
  {
    return algorithm;
  }

  /**
   * Get the size of the MAC. This is the size of the array returned by
   * {@link #doFinal()} and {@link #doFinal(byte[])}, and the minimum
   * number of bytes that must be available in the byte array passed to
   * {@link #doFinal(byte[],int)}.
   *
   * @return The MAC length.
   */
  public final int getMacLength()
  {
    return macSpi.engineGetMacLength();
  }

  /**
   * Get the provider of the underlying implementation.
   *
   * @return The provider.
   */
  public final Provider getProvider()
  {
    return provider;
  }

  /**
   * Initialize this MAC with a key and no parameters.
   *
   * @param key The key to initialize this instance with.
   * @throws java.security.InvalidKeyException If the key is
   *         unacceptable.
   */
  public final void init(Key key) throws InvalidKeyException
  {
    try
      {
        init(key, null);
      }
    catch (InvalidAlgorithmParameterException iape)
      {
        throw new IllegalArgumentException(algorithm + " needs parameters");
      }
  }

  /**
   * Initialize this MAC with a key and parameters.
   *
   * @param key    The key to initialize this instance with.
   * @param params The algorithm-specific parameters.
   * @throws java.security.InvalidAlgorithmParameterException If the
   *         algorithm parameters are unacceptable.
   * @throws java.security.InvalidKeyException If the key is
   *         unacceptable.
   */
  public final void init(Key key, AlgorithmParameterSpec params)
    throws InvalidAlgorithmParameterException, InvalidKeyException
  {
    macSpi.engineInit(key, params);
    virgin = false;                      // w00t!
  }

  /**
   * Reset this instance. A call to this method returns this instance
   * back to the state it was in just after it was initialized.
   */
  public final void reset()
  {
    macSpi.engineReset();
  }

  /**
   * Update the computation with a single byte.
   *
   * @param input The next byte.
   * @throws java.lang.IllegalStateException If this instance has not
   *         been initialized.
   */
  public final void update(byte input) throws IllegalStateException
  {
    if (virgin)
      {
        throw new IllegalStateException("not initialized");
      }
    macSpi.engineUpdate(input);
  }

  /**
   * Update the computation with a byte array.
   *
   * @param input The next bytes.
   * @throws java.lang.IllegalStateException If this instance has not
   *         been initialized.
   */
  public final void update(byte[] input) throws IllegalStateException
  {
    update(input, 0, input.length);
  }

  /**
   * Update the computation with a portion of a byte array.
   *
   * @param input  The next bytes.
   * @param offset The index in <code>input</code> to start.
   * @param length The number of bytes to update.
   * @throws java.lang.IllegalStateException If this instance has not
   *         been initialized.
   */
  public final void update(byte[] input, int offset, int length)
    throws IllegalStateException
  {
    if (virgin)
      {
        throw new IllegalStateException("not initialized");
      }
    macSpi.engineUpdate(input, offset, length);
  }

  /**
   * Update this MAC with the remaining bytes in the given buffer
   * @param buffer The input buffer.
   * @since 1.5
   */
  public final void update (final ByteBuffer buffer)
  {
    if (virgin)
      throw new IllegalStateException ("not initialized");
    macSpi.engineUpdate(buffer);
  }

  /**
   * Clone this instance, if the underlying implementation supports it.
   *
   * @return A clone of this instance.
   * @throws java.lang.CloneNotSupportedException If the underlying
   *         implementation is not cloneable.
   */
  public final Object clone() throws CloneNotSupportedException
  {
    Mac result = new Mac((MacSpi) macSpi.clone(), provider, algorithm);
    result.virgin = virgin;
    return result;
  }
}
