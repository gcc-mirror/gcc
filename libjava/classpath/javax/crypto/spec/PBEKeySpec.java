/* PBEKeySpec.java -- Wrapper for password-based keys.
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


package javax.crypto.spec;

import java.security.spec.KeySpec;

/**
 * A wrapper for a password-based key, used for password-based
 * encryption (PBE).
 *
 * <p>Examples of password-based encryption algorithms include:
 *
 * <ul>
 * <li><a href="http://www.rsasecurity.com/rsalabs/pkcs/pkcs-5/">PKCS #5
 * - Password-Based Cryptography Standard</a></li>
 * <li><a href="http://www.rsasecurity.com/rsalabs/pkcs/pkcs-12/">PKCS
 * #12 - Personal Information Exchange Syntax Standard</a></li>
 * </ul>
 *
 * @author Casey Marshall (csm@gnu.org)
 * @since 1.4
 * @see javax.crypto.SecretKeyFactory
 * @see PBEParameterSpec
 */
public class PBEKeySpec implements KeySpec
{

  // Fields.
  // ------------------------------------------------------------------------

  /** The iteration count. */
  private int iterationCount;

  /** The generated key length. */
  private int keyLength;

  /** The password. */
  private char[] password;

  /** The salt. */
  private byte[] salt;

  // Constructors.
  // ------------------------------------------------------------------------

  /**
   * Create a new PBE key spec with just a password.
   *
   * @param password The password char array.
   */
  public PBEKeySpec(char[] password)
  {
    this(password, null, 0, 0);
  }

  /**
   * Create a PBE key spec with a password, salt, and iteration count.
   *
   * @param password       The password char array.
   * @param salt           The salt bytes.
   * @param iterationCount The iteration count.
   */
  public PBEKeySpec(char[] password, byte[] salt, int iterationCount)
  {
    this(password, salt, iterationCount, 0);
  }

  /**
   * Create a PBE key spec with a password, salt, iteration count, and
   * key length.
   *
   * @param password       The password char array.
   * @param salt           The salt bytes.
   * @param iterationCount The iteration count.
   * @param keyLength      The generated key length.
   */
  public PBEKeySpec(char[] password, byte[] salt, int iterationCount,
                    int keyLength)
  {
    this.password = password;
    this.salt = salt;
    this.iterationCount = iterationCount;
    this.keyLength = keyLength;
  }

  // Instance methods.
  // ------------------------------------------------------------------------

  /**
   * Clear the password array by filling it with null characters.
   */
  public final void clearPassword()
  {
    if (password == null) return;
    for (int i = 0; i < password.length; i++)
      {
        password[i] = '\u0000';
      }
  }

  /**
   * Get the iteration count, or 0 if it has not been specified.
   *
   * @return The iteration count, or 0 if it has not been specified.
   */
  public final int getIterationCount()
  {
    return iterationCount;
  }

  /**
   * Get the generated key length, or 0 if it has not been specified.
   *
   * @return The key length, or 0 if it has not been specified.
   */
  public final int getKeyLength()
  {
    return keyLength;
  }

  /**
   * Get the password character array.
   *
   * @return The password.
   */
  public final char[] getPassword()
  {
    return password;
  }

  /**
   * Get the salt bytes.
   *
   * @return The salt.
   */
  public final byte[] getSalt()
  {
    return salt;
  }
}
