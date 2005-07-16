/* MessageDigest.java --- The message digest interface.
   Copyright (C) 1999, 2002, 2003 Free Software Foundation, Inc.

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

import gnu.java.security.Engine;

/**
 * <p>This <code>MessageDigest</code> class provides applications the
 * functionality of a message digest algorithm, such as <i>MD5</i> or <i>SHA</i>.
 * Message digests are secure one-way hash functions that take arbitrary-sized
 * data and output a fixed-length hash value.</p>
 *
 * <p>A <code>MessageDigest</code> object starts out initialized. The data is
 * processed through it using the <code>update()</code> methods. At any point
 * <code>reset()</code> can be called to reset the digest. Once all the data to
 * be updated has been updated, one of the <code>digest()</code> methods should
 * be called to complete the hash computation.</p>
 *
 * <p>The <code>digest()</code> method can be called <b>once</b> for a given
 * number of updates. After <code>digest()</code> has been called, the
 * <code>MessageDigest</code> object is <b>reset</b> to its initialized state.
 * </p>
 *
 * <p>Implementations are free to implement the {@link Cloneable} interface.
 * Client applications can test cloneability by attempting cloning and catching
 * the {@link CloneNotSupportedException}:
 *
 * <pre>
 *    MessageDigest md = MessageDigest.getInstance("SHA");
 *    try
 *      {
 *        md.update(toChapter1);
 *        MessageDigest tc1 = md.clone();
 *        byte[] toChapter1Digest = tc1.digest();
 *        md.update(toChapter2);
 *        // ...
 *      }
 *    catch (CloneNotSupportedException x)
 *      {
 *        throw new DigestException("couldn't make digest of partial content");
 *      }
 * </pre>
 *
 * <p>Note that if a given implementation is not cloneable, it is still possible
 * to compute intermediate digests by instantiating several instances, if the
 * number of digests is known in advance.</p>
 *
 * <p>Note that this class is abstract and extends from {@link MessageDigestSpi}
 * for historical reasons. Application developers should only take notice of the
 * methods defined in this <code>MessageDigest</code> class; all the methods in
 * the superclass are intended for cryptographic service providers who wish to
 * supply their own implementations of message digest algorithms.</p>
 *
 * @see MessageDigestSpi
 * @see Provider
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
   * Creates a message digest with the specified algorithm name.
   *
   * @param algorithm the standard name of the digest algorithm.
   * See Appendix A in the Java Cryptography Architecture API
   * Specification &amp; Reference for information about standard
   * algorithm names.
   */
  protected MessageDigest(String algorithm)
  {
    this.algorithm = algorithm;
    provider = null;
  }

  /**
   * Generates a <code>MessageDigest</code> object that implements the specified
   * digest algorithm. If the default provider package provides an
   * implementation of the requested digest algorithm, an instance of
   * <code>MessageDigest</code> containing that implementation is returned. If
   * the algorithm is not available in the default package, other packages are
   * searched.
   *
   * @param algorithm the name of the algorithm requested. See Appendix A in the
   * Java Cryptography Architecture API Specification &amp; Reference for
   * information about standard algorithm names.
   * @return a Message Digest object implementing the specified algorithm.
   * @throws NoSuchAlgorithmException if the algorithm is not available in the
   * caller's environment.
   */
  public static MessageDigest getInstance(String algorithm)
    throws NoSuchAlgorithmException
  {
    Provider[] p = Security.getProviders();
    for (int i = 0; i < p.length; i++)
      {
        try
          {
            return getInstance(algorithm, p[i]);
          }
        catch (NoSuchAlgorithmException ignored)
	  {
	    // Ignore.
	  }
      }

    throw new NoSuchAlgorithmException(algorithm);
  }

  /**
   * Generates a <code>MessageDigest</code> object implementing the specified
   * algorithm, as supplied from the specified provider, if such an algorithm is
   * available from the provider.
   *
   * @param algorithm the name of the algorithm requested. See Appendix A in the
   * Java Cryptography Architecture API Specification &amp; Reference for
   * information about standard algorithm names.
   * @param provider the name of the provider.
   * @return a Message Digest object implementing the specified algorithm.
   * @throws NoSuchAlgorithmException if the algorithm is not available in the
   * package supplied by the requested provider.
   * @throws NoSuchProviderException if the provider is not available in the
   * environment.
   * @throws IllegalArgumentException if the provider name is null or empty.
   * @see Provider
   */
  public static MessageDigest getInstance(String algorithm, String provider)
    throws NoSuchAlgorithmException, NoSuchProviderException
  {
    if (provider == null || provider.length() == 0)
      throw new IllegalArgumentException("Illegal provider");

    Provider p = Security.getProvider(provider);
    if (p == null)
      throw new NoSuchProviderException(provider);

    return getInstance(algorithm, p);
  }

  /**
   * Generates a <code>MessageDigest</code> object implementing the specified
   * algorithm, as supplied from the specified provider, if such an algorithm
   * is available from the provider. Note: the provider doesn't have to be
   * registered.
   *
   * @param algorithm the name of the algorithm requested. See Appendix A in
   * the Java Cryptography Architecture API Specification &amp; Reference for
   * information about standard algorithm names.
   * @param provider the provider.
   * @return a Message Digest object implementing the specified algorithm.
   * @throws NoSuchAlgorithmException if the <code>algorithm</code> is not
   * available in the package supplied by the requested <code>provider</code>.
   * @throws IllegalArgumentException if the <code>provider</code> is
   * <code>null</code>.
   * @since 1.4
   * @see Provider
   */
  public static MessageDigest getInstance(String algorithm, Provider provider)
    throws NoSuchAlgorithmException
  {
    if (provider == null)
      throw new IllegalArgumentException("Illegal provider");

    MessageDigest result = null;
    Object o = null;
    try
      {
        o = Engine.getInstance(MESSAGE_DIGEST, algorithm, provider);
      }
    catch (java.lang.reflect.InvocationTargetException ite)
      {
        throw new NoSuchAlgorithmException(algorithm);
      }

    if (o instanceof MessageDigestSpi)
      {
        result = new DummyMessageDigest((MessageDigestSpi) o, algorithm);
      }
    else if (o instanceof MessageDigest)
      {
        result = (MessageDigest) o;
        result.algorithm = algorithm;
      }
    else
      {
        throw new NoSuchAlgorithmException(algorithm);
      }
    result.provider = provider;
    return result;
  }

  /**
   * Returns the provider of this message digest object.
   *
   * @return the provider of this message digest object.
   */
  public final Provider getProvider()
  {
    return provider;
  }

  /**
   * Updates the digest using the specified byte.
   *
   * @param input the byte with which to update the digest.
   */
  public void update(byte input)
  {
    engineUpdate(input);
  }

  /**
   * Updates the digest using the specified array of bytes, starting at the
   * specified offset.
   *
   * @param input the array of bytes.
   * @param offset the offset to start from in the array of bytes.
   * @param len the number of bytes to use, starting at offset.
   */
  public void update(byte[] input, int offset, int len)
  {
    engineUpdate(input, offset, len);
  }

  /**
   * Updates the digest using the specified array of bytes.
   *
   * @param input the array of bytes.
   */
  public void update(byte[] input)
  {
    engineUpdate(input, 0, input.length);
  }

  /**
   * Completes the hash computation by performing final operations such as
   * padding. The digest is reset after this call is made.
   *
   * @return the array of bytes for the resulting hash value.
   */
  public byte[] digest()
  {
    return lastDigest = engineDigest();
  }

  /**
   * Completes the hash computation by performing final operations such as
   * padding. The digest is reset after this call is made.
   *
   * @param buf An output buffer for the computed digest.
   * @param offset The offset into the output buffer to begin storing the digest.
   * @param len The number of bytes within buf allotted for the digest.
   * @return The number of bytes placed into buf.
   * @throws DigestException if an error occurs.
   */
  public int digest(byte[] buf, int offset, int len) throws DigestException
  {
    return engineDigest(buf, offset, len);
  }

  /**
   * Performs a final update on the digest using the specified array of bytes,
   * then completes the digest computation. That is, this method first calls
   * <code>update(input)</code>, passing the input array to the <code>update()
   * </code> method, then calls <code>digest()</code>.
   *
   * @param input the input to be updated before the digest is completed.
   * @return the array of bytes for the resulting hash value.
   */
  public byte[] digest(byte[] input)
  {
    update(input);
    return digest();
  }

  /**
   * Returns a string representation of this message digest object.
   *
   * @return a string representation of the object.
   */
  public String toString()
  {
    return (getClass()).getName() + " Message Digest <" + digestToString() + ">";
  }

  /**
   * Compares two digests for equality. Does a simple byte compare.
   *
   * @param digesta one of the digests to compare.
   * @param digestb the other digest to compare.
   * @return <code>true</code> if the digests are equal, <code>false</code>
   * otherwise.
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

  /** Resets the digest for further use. */
  public void reset()
  {
    engineReset();
  }

  /**
   * Returns a string that identifies the algorithm, independent of
   * implementation details. The name should be a standard Java Security name
   * (such as <code>"SHA"</code>, <code>"MD5"</code>, and so on). See Appendix
   * A in the Java Cryptography Architecture API Specification &amp; Reference
   * for information about standard algorithm names.
   *
   * @return the name of the algorithm.
   */
  public final String getAlgorithm()
  {
    return algorithm;
  }

  /**
   * Returns the length of the digest in bytes, or <code>0</code> if this
   * operation is not supported by the provider and the implementation is not
   * cloneable.
   *
   * @return the digest length in bytes, or <code>0</code> if this operation is
   * not supported by the provider and the implementation is not cloneable.
   * @since 1.2
   */
  public final int getDigestLength()
  {
    return engineGetDigestLength();
  }

  /**
   * Returns a clone if the implementation is cloneable.
   *
   * @return a clone if the implementation is cloneable.
   * @throws CloneNotSupportedException if this is called on an implementation
   * that does not support {@link Cloneable}.
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

    StringBuffer buf = new StringBuffer();
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
