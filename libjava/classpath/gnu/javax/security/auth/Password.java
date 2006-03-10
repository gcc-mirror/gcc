/* Password.java -- opaque wrapper around a password.
   Copyright (C) 2004, 2006 Free Software Foundation, Inc.

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


package gnu.javax.security.auth;

import gnu.java.security.util.ExpirableObject;

import javax.security.auth.DestroyFailedException;

/**
 * Immutible, though destroyable, password class.
 *
 * <p>Extends {@link ExpirableObject}, implementing {@link doDestroy()}
 * in which encapsulated {@link char[]}, and {@link byte[]} password fields
 * are cleared (elements set to zero) in order to thwart memory heap
 * snooping.
 */
public final class Password extends ExpirableObject
{

  // Constants and variables
  // -------------------------------------------------------------------------

  /**
   * Password stored in {@link char[]} format.
   */
  private final char[] password;

  /**
   * Password stored in {@link byte[]} format.
   */
  private final byte[] bPassword;

  /**
   * Indicates whether this Password object's {@link doDestroy()} method has
   * been called.  See also, {@link ExpirableObject#Destroy()}.
   */
  private boolean mIsDestroyed = false;

  // Constructor(s)
  // -------------------------------------------------------------------------

  /**
   * Create a new expirable Password object that will expire after the
   * default timeout {@link ExpirableObject#DEFAULT_TIMEOUT}.
   *
   * @param password The character array password to associate with this
   * Password object.
   */
  public Password (char[] password)
  {
    this (password, 0, password.length, DEFAULT_TIMEOUT);
  }

  /**
   * Create a new expirable Password object that will expire after the
   * timeout denoted by constructor parameter, <i>delay</i>.
   *
   * @param password The character array password to associate with this
   * Password object.
   * @param delay The number of miliseconds before this Password object
   * will be automatically destroyed.
   */
  public Password (char[] password, long delay)
  {
    this (password, 0, password.length, delay);
  }

  /**
   * Create a new expirable Password object that will expire after the
   * default timeout {@link ExpirableObject#DEFAULT_TIMEOUT}.
   *
   * @param password The character array password to associate with this
   * Password object.
   * @param offset The <i>password</i> character array parameter element
   * marking the beginning of the contained password string.
   * @param length The number of characters, beginning at <i>offset</i>,
   * to be copied into this object's {@link password} field.
   */
  public Password (char[] password, int offset, int length)
  {
    this (password, offset, length, DEFAULT_TIMEOUT);
  }

  /**
   * Create a new expirable Password object that will expire after the
   * timeout denoted by constructor parameter, <i>delay</i>.
   *
   * @param password The character array password to associate with this
   * Password object.
   * @param offset The <i>password</i> character array parameter element
   * marking the beginning of the contained password string.
   * @param length The number of characters, beginning at <i>offset</i>,
   * to be copied into this object's {@link password} field.
   * @param delay The number of miliseconds before this Password object
   * will be automatically destroyed.
   */
  public Password (char[] password, int offset, int length, long delay)
  {
    super (delay);

    if (offset < 0 || length < 0 || offset + length > password.length)
      throw new ArrayIndexOutOfBoundsException ("off=" + offset + " length=" +
                                                length + " array.length=" +
                                                password.length);

    int i, j;
    this.password = new char[length];
    bPassword = new byte[length];

    for(i = 0, j = offset; i < length; i++, j++)
      {
        this.password[i] = (char) password[j];
        // XXX this should use character encodings, other than ASCII.
        bPassword[i] = (byte) (password[j] & 0x7F);
      }
  }

  /**
   * Create a new expirable Password object that will expire after the
   * default timeout {@link ExpirableObject#DEFAULT_TIMEOUT}.
   *
   * @param password The byte array password to associate with this
   * Password object.
   */
  public Password (byte[] password)
  {
    this (password, 0, password.length, DEFAULT_TIMEOUT);
  }

  /**
   * Create a new expirable Password object that will expire after the
   * timeout denoted by constructor parameter, <i>delay</i>.
   *
   * @param password The byte array password to associate with this
   * Password object.
   * @param delay The number of miliseconds before this Password object
   * will be automatically destroyed.
   */
  public Password (byte[] password, long delay)
  {
    this (password, 0, password.length, delay);
  }

  /**
   * Create a new expirable Password object that will expire after the
   * default timeout {@link ExpirableObject#DEFAULT_TIMEOUT}.
   *
   * @param password The byte array password to associate with this
   * Password object.
   * @param offset The <i>password</i> byte array parameter element
   * marking the beginning of the contained password string.
   * @param length The number of bytes, beginning at <i>offset</i>,
   * to be copied into this object's {@link password} field.
   */
  public Password (byte[] password, int offset, int length)
  {
    this (password, offset, length, DEFAULT_TIMEOUT);
  }

  /**
   * Create a new expirable Password object that will expire after the
   * timeout denoted by constructor parameter, <i>delay</i>.
   *
   * @param password The byte array password to associate with this
   * Password object.
   * @param offset The <i>password</i> byte array parameter element
   * marking the beginning of the contained password string.
   * @param length The number of bytes, beginning at <i>offset</i>,
   * to be copied into this object's {@link bPassword} field.
   * @param delay The number of miliseconds before this Password object
   * will be automatically destroyed.
   */
  public Password (byte[] password, int offset, int length, long delay)
  {
    super (delay);

    if (offset < 0 || length < 0 || offset + length > password.length)
      throw new ArrayIndexOutOfBoundsException ("off=" + offset + " length=" +
                                                length + " array.length=" +
                                                password.length);

    int i, j;
    this.password = new char[length];
    bPassword = new byte[length];

    for (i = 0, j = offset; i < length; i++, j++)
      {
        this.password[i] = (char) password[j];
        bPassword[i] = password[j];
      }
  }

  // Instance methods
  // -------------------------------------------------------------------------

  /**
   * Returns a reference to the {@link char[]} password storage field,
   * {@link password}.
   */
  public synchronized char[] getPassword()
  {
    if (mIsDestroyed)
      throw new IllegalStateException ("Attempted destroyed password access.");

    return password;
  }

  /**
   * Returns a reference to the {@link byte[]} password storage field,
   * {@link bPassword}.
   */
  public synchronized byte[] getBytes()
  {
    if (mIsDestroyed)
      throw new IllegalStateException ("Attempted destroyed password access.");

    return bPassword;
  }

  /**
   * Sets password field char[], and byte[] array elements to zero.
   * This method implements base class {@link ExpirableObject} abstract
   * method, {@link ExpirableObject#doDestroy()}.  See also,
   * {@link ExpirableObject#destroy()}.
   */
  protected synchronized void doDestroy()
  {
    if (isDestroyed())
      return;
    else
      {
        for (int i = 0; i < password.length; i++)
          password[i] = 0;
        for (int i = 0; i < bPassword.length; i++)
          bPassword[i] = 0;
        mIsDestroyed = true;
      }
  }

  /**
   * Returns true, or false relative to whether, or not this object's
   * {@link doDestroy()} method has been called.  See also,
   * {@ExpirableObject#destroy()}.
   */
  public synchronized boolean isDestroyed()
  {
    return (mIsDestroyed);
  }
}
