/* BaseMac.java -- 
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


package gnu.javax.crypto.mac;

import gnu.java.security.hash.IMessageDigest;

import java.security.InvalidKeyException;
import java.util.Map;

/**
 * A base abstract class to facilitate <i>MAC</i> (Message Authentication Code)
 * implementations.
 */
public abstract class BaseMac
    implements IMac
{
  /** The canonical name prefix of the <i>MAC</i>. */
  protected String name;
  /** Reference to the underlying hash algorithm instance. */
  protected IMessageDigest underlyingHash;
  /** The length of the truncated output in bytes. */
  protected int truncatedSize;

  /**
   * Trivial constructor for use by concrete subclasses.
   * 
   * @param name the canonical name of this instance.
   */
  protected BaseMac(String name)
  {
    super();

    this.name = name;
  }

  /**
   * Trivial constructor for use by concrete subclasses.
   * 
   * @param name the canonical name of this instance.
   * @param underlyingHash the underlying message digest algorithm instance.
   */
  protected BaseMac(String name, IMessageDigest underlyingHash)
  {
    this(name);

    if (underlyingHash != null)
      truncatedSize = underlyingHash.hashSize();
    this.underlyingHash = underlyingHash;
  }

  public String name()
  {
    return name;
  }

  public int macSize()
  {
    return truncatedSize;
  }

  public void update(byte b)
  {
    underlyingHash.update(b);
  }

  public void update(byte[] b, int offset, int len)
  {
    underlyingHash.update(b, offset, len);
  }

  public void reset()
  {
    underlyingHash.reset();
  }

  public Object clone() throws CloneNotSupportedException
  {
    BaseMac result = (BaseMac) super.clone();
    if (this.underlyingHash != null)
      result.underlyingHash = (IMessageDigest) this.underlyingHash.clone();

    return result;
  }

  public abstract void init(Map attributes) throws InvalidKeyException,
      IllegalStateException;

  public abstract byte[] digest();

  public abstract boolean selfTest();
}
