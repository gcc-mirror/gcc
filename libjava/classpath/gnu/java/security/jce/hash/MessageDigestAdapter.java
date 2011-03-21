/* MessageDigestAdapter.java --
   Copyright (C) 2001, 2002, 2006 Free Software Foundation, Inc.

This file is a part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
USA

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
exception statement from your version.  */


package gnu.java.security.jce.hash;

import gnu.java.security.hash.IMessageDigest;
import gnu.java.security.hash.HashFactory;

import java.security.DigestException;
import java.security.MessageDigestSpi;

/**
 * The implementation of a generic {@link java.security.MessageDigest} adapter
 * class to wrap GNU hash instances.
 * <p>
 * This class defines the <i>Service Provider Interface</i> (<b>SPI</b>) for
 * the {@link java.security.MessageDigest} class, which provides the
 * functionality of a message digest algorithm, such as MD5 or SHA. Message
 * digests are secure one-way hash functions that take arbitrary-sized data and
 * output a fixed-length hash value.
 * <p>
 * All the abstract methods in the {@link MessageDigestSpi} class are
 * implemented by this class and all its sub-classes.
 * <p>
 * All the implementations which subclass this object, and which are serviced by
 * the GNU provider implement the {@link Cloneable} interface.
 */
class MessageDigestAdapter
    extends MessageDigestSpi
    implements Cloneable
{
  /** Our underlying hash instance. */
  private IMessageDigest adaptee;

  /**
   * Trivial protected constructor.
   *
   * @param mdName the canonical name of the hash algorithm.
   */
  protected MessageDigestAdapter(String mdName)
  {
    this(HashFactory.getInstance(mdName));
  }

  /**
   * Private constructor for cloning purposes.
   *
   * @param adaptee a clone of the underlying hash algorithm instance.
   */
  private MessageDigestAdapter(IMessageDigest adaptee)
  {
    super();

    this.adaptee = adaptee;
  }

  public Object clone()
  {
    return new MessageDigestAdapter((IMessageDigest) adaptee.clone());
  }

  public int engineGetDigestLength()
  {
    return adaptee.hashSize();
  }

  public void engineUpdate(byte input)
  {
    adaptee.update(input);
  }

  public void engineUpdate(byte[] input, int offset, int len)
  {
    adaptee.update(input, offset, len);
  }

  public byte[] engineDigest()
  {
    return adaptee.digest();
  }

  public int engineDigest(byte[] buf, int offset, int len)
      throws DigestException
  {
    int result = adaptee.hashSize();
    if (len < result)
      throw new DigestException();

    byte[] md = adaptee.digest();
    System.arraycopy(md, 0, buf, offset, result);
    return result;
  }

  public void engineReset()
  {
    adaptee.reset();
  }
}
