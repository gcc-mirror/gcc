/* MessageDigest.java --- The message digest interface.
   Copyright (C) 1999, 2002, 2003, 2006 Free Software Foundation, Inc.

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

package java.security;

import gnu.java.lang.CPStringBuilder;

import gnu.java.security.Engine;
import java.nio.ByteBuffer;

import java.lang.reflect.InvocationTargetException;

/**
 * Message digests are secure one-way hash functions that take arbitrary-sized
 * data and output a fixed-length hash value.
 *
 * @see MessageDigestSpi
 * @since JDK 1.1
 */
public abstract class MessageDigest extends MessageDigestSpi
{
  /** The service name for message digests. */
  private static final String MESSAGE_DIGEST = "MessageDigest";

  private String algorithm;
  Provider provider;
  private byte[] lastDigest;

  /**
   * Constructs a new instance of <code>MessageDigest</code> representing the
   * specified algorithm.
   * 
   * @param algorithm
   *          the name of the digest algorithm to use.
   */
  protected MessageDigest(String algorithm)
  {
    this.algorithm = algorithm;
    provider = null;
  }

  /**
   * Returns a new instance of <code>MessageDigest</code> representing the
   * specified algorithm.
   * 
   * @param algorithm the name of the digest algorithm to use.
   * @return a new instance representing the desired algorithm.
   * @throws NoSuchAlgorithmException if the algorithm is not implemented by any
   *           provider.
   * @throws IllegalArgumentException if <code>algorithm</code> is
   *           <code>null</code> or is an empty string.
   */
  public static MessageDigest getInstance(String algorithm)
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
   * Returns a new instance of <code>MessageDigest</code> representing the
   * specified algorithm from a named provider.
   * 
   * @param algorithm the name of the digest algorithm to use.
   * @param provider the name of the provider to use.
   * @return a new instance representing the desired algorithm.
   * @throws NoSuchAlgorithmException if the algorithm is not implemented by the
   *           named provider.
   * @throws NoSuchProviderException if the named provider was not found.
   * @throws IllegalArgumentException if either <code>algorithm</code> or
   *           <code>provider</code> is <code>null</code> or empty.
   */
  public static MessageDigest getInstance(String algorithm, String provider)
      throws NoSuchAlgorithmException, NoSuchProviderException
  {
    if (provider == null)
      throw new IllegalArgumentException("provider MUST NOT be null");
    provider = provider.trim();
    if (provider.length() == 0)
      throw new IllegalArgumentException("provider MUST NOT be empty");
    Provider p = Security.getProvider(provider);
    if (p == null)
      throw new NoSuchProviderException(provider);
    return getInstance(algorithm, p);
  }

  /**
   * Returns a new instance of <code>MessageDigest</code> representing the
   * specified algorithm from a designated {@link Provider}.
   * 
   * @param algorithm the name of the digest algorithm to use.
   * @param provider the {@link Provider} to use.
   * @return a new instance representing the desired algorithm.
   * @throws NoSuchAlgorithmException if the algorithm is not implemented by
   *           {@link Provider}.
   * @throws IllegalArgumentException if either <code>algorithm</code> or
   *           <code>provider</code> is <code>null</code>, or if
   *           <code>algorithm</code> is an empty string.
   * @since 1.4
   * @see Provider
   */
  public static MessageDigest getInstance(String algorithm, Provider provider)
    throws NoSuchAlgorithmException
  {
    CPStringBuilder sb = new CPStringBuilder("MessageDigest for algorithm [")
        .append(algorithm).append("] from provider[")
        .append(provider).append("] ");
    Object o;
    try
      {
        o = Engine.getInstance(MESSAGE_DIGEST, algorithm, provider);
      }
    catch (InvocationTargetException x)
      {
        Throwable cause = x.getCause();
        if (cause instanceof NoSuchAlgorithmException)
          throw (NoSuchAlgorithmException) cause;
        if (cause == null)
          cause = x;
        sb.append("could not be created");
        NoSuchAlgorithmException y = new NoSuchAlgorithmException(sb.toString());
        y.initCause(cause);
        throw y;
      }
    MessageDigest result;
    if (o instanceof MessageDigestSpi)
      result = new DummyMessageDigest((MessageDigestSpi) o, algorithm);
    else if (o instanceof MessageDigest)
      {
        result = (MessageDigest) o;
        result.algorithm = algorithm;
      }
    else
      {
        sb.append("is of an unexpected Type: ").append(o.getClass().getName());
        throw new NoSuchAlgorithmException(sb.toString());
      }
    result.provider = provider;
    return result;
  }

  /**
   * Returns the {@link Provider} of this instance.
   * 
   * @return the {@link Provider} of this instance.
   */
  public final Provider getProvider()
  {
    return provider;
  }

  /**
   * Updates the digest with the byte.
   * 
   * @param input byte to update the digest with.
   */
  public void update(byte input)
  {
    engineUpdate(input);
  }

  /**
   * Updates the digest with the bytes from the array starting from the
   * specified offset and using the specified length of bytes.
   * 
   * @param input
   *          bytes to update the digest with.
   * @param offset
   *          the offset to start at.
   * @param len
   *          length of the data to update with.
   */
  public void update(byte[] input, int offset, int len)
  {
    engineUpdate(input, offset, len);
  }

  /**
   * Updates the digest with the bytes of an array.
   * 
   * @param input bytes to update the digest with.
   */
  public void update(byte[] input)
  {
    engineUpdate(input, 0, input.length);
  }

  /**
   * Updates the digest with the remaining bytes of a buffer.
   * 
   * @param input The input byte buffer.
   * @since 1.5
   */
  public final void update (ByteBuffer input)
  {
    engineUpdate (input);
  }
  
  /**
   * Computes the final digest of the stored data.
   * 
   * @return a byte array representing the message digest.
   */
  public byte[] digest()
  {
    return lastDigest = engineDigest();
  }

  /**
   * Computes the final digest of the stored bytes and returns the result.
   * 
   * @param buf
   *          an array of bytes to store the result in.
   * @param offset
   *          an offset to start storing the result at.
   * @param len
   *          the length of the buffer.
   * @return Returns the length of the buffer.
   */
  public int digest(byte[] buf, int offset, int len) throws DigestException
  {
    return engineDigest(buf, offset, len);
  }

  /**
   * Computes a final update using the input array of bytes, then computes a
   * final digest and returns it. It calls {@link #update(byte[])} and then
   * {@link #digest(byte[])}.
   * 
   * @param input
   *          an array of bytes to perform final update with.
   * @return a byte array representing the message digest.
   */
  public byte[] digest(byte[] input)
  {
    update(input);
    return digest();
  }

  /**
   * Returns a string representation of this instance.
   * 
   * @return a string representation of this instance.
   */
  public String toString()
  {
    return (getClass()).getName() + " Message Digest <" + digestToString() + ">";
  }

  /**
   * Does a simple byte comparison of the two digests.
   * 
   * @param digesta
   *          first digest to compare.
   * @param digestb
   *          second digest to compare.
   * @return <code>true</code> if both are equal, <code>false</code>
   *         otherwise.
   */
  public static boolean isEqual(byte[] digesta, byte[] digestb)
  {
    if (digesta.length != digestb.length)
      return false;

    for (int i = digesta.length - 1; i >= 0; --i)
      if (digesta[i] != digestb[i])
        return false;

    return true;
  }

  /** Resets this instance. */
  public void reset()
  {
    engineReset();
  }

  /**
   * Returns the name of message digest algorithm.
   * 
   * @return the name of message digest algorithm.
   */
  public final String getAlgorithm()
  {
    return algorithm;
  }

  /**
   * Returns the length of the message digest. The default is zero which means
   * that the concrete implementation does not implement this method.
   * 
   * @return length of the message digest.
   * @since 1.2
   */
  public final int getDigestLength()
  {
    return engineGetDigestLength();
  }

  /**
   * Returns a clone of this instance if cloning is supported. If it does not
   * then a {@link CloneNotSupportedException} is thrown. Cloning depends on
   * whether the subclass {@link MessageDigestSpi} implements {@link Cloneable}
   * which contains the actual implementation of the appropriate algorithm.
   * 
   * @return a clone of this instance.
   * @throws CloneNotSupportedException
   *           the implementation does not support cloning.
   */
  public Object clone() throws CloneNotSupportedException
  {
    return super.clone();
  }

  private String digestToString()
  {
    byte[] digest = lastDigest;

    if (digest == null)
      return "incomplete";

    CPStringBuilder buf = new CPStringBuilder();
    int len = digest.length;
    for (int i = 0; i < len; ++i)
      {
        byte b = digest[i];
        byte high = (byte) ((b & 0xff) >>> 4);
        byte low = (byte) (b & 0xf);

        buf.append(high > 9 ? ('a' - 10) + high : '0' + high);
        buf.append(low > 9 ? ('a' - 10) + low : '0' + low);
      }

    return buf.toString();
  }
}
