/* PBEKeySpec.java -- Wrapper for password-based keys.
   Copyright (C) 2004, 2006  Free Software Foundation, Inc.

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

  /** The password state */
  private boolean passwordValid = true;
  
  // Constructors.
  // ------------------------------------------------------------------------

  /**
   * Create a new PBE key spec with just a password.
   * <p>
   * A copy of the password argument is stored instead of the argument itself.
   * 
   * @param password The password char array.
   */
  public PBEKeySpec(char[] password)
  {
    setPassword(password);
    
    // load the default values for unspecified variables.
    salt = null;
    iterationCount = 0;
    keyLength = 0;
  }

  /**
   * Create a PBE key spec with a password, salt, and iteration count.
   * <p>
   * A copy of the password and salt arguments are stored instead of the
   * arguments themselves.
   * 
   * @param password The password char array.
   * @param salt The salt bytes.
   * @param iterationCount The iteration count.
   * @throws NullPointerException If salt is null
   * @throws IllegalArgumentException If salt is an empty array, or
   *           iterationCount is negative
   */
  public PBEKeySpec(char[] password, byte[] salt, int iterationCount)
  {
    setPassword(password);
    setSalt(salt);
    setIterationCount(iterationCount);

    // load default values into unspecified variables.
    keyLength = 0;
  }

  /**
   * Create a PBE key spec with a password, salt, iteration count, and key
   * length.
   * <p>
   * A copy of the password and salt arguments are stored instead of the
   * arguments themselves.
   * 
   * @param password The password char array.
   * @param salt The salt bytes.
   * @param iterationCount The iteration count.
   * @param keyLength The generated key length.
   * @throws NullPointerException If salt is null
   * @throws IllegalArgumentException If salt is an empty array, if
   *           iterationCount or keyLength is negative
   */
  public PBEKeySpec(char[] password, byte[] salt, int iterationCount,
                    int keyLength)
  {
    setPassword(password);
    setSalt(salt);
    setIterationCount(iterationCount);
    setKeyLength(keyLength);
  }

  // Instance methods.
  // ------------------------------------------------------------------------

  /**
   * Clear the password array by filling it with null characters.
   * <p>
   * This clears the stored copy of the password, not the original char array
   * used to create the password.
   */
  public final void clearPassword()
  {
    if (password == null)
      return;
    for (int i = 0; i < password.length; i++)
      password[i] = '\u0000';
    
    // since the password is cleared, it is no longer valid
    passwordValid = false;
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
   * Get the password character array copy.
   * <p>
   * This returns a copy of the password, not the password itself.
   * 
   * @return a clone of the password.
   * @throws IllegalStateException If {@link #clearPassword()} has already been
   *           called.
   */
  public final char[] getPassword()
  {
    if (! passwordValid)
      throw new IllegalStateException("clearPassword() has been called, the "
                                      + "password is no longer valid");
    return (char[]) password.clone();
  }

  /**
   * Get the salt bytes array copy.
   * <p>
   * This returns a copy of the salt, not the salt itself.
   * 
   * @return The salt.
   */
  public final byte[] getSalt()
  {
    if (salt != null)
      return (byte[]) salt.clone();
    return null;
  }

  /**
   * Set the password char array.
   * <p>
   * A copy of the password argument is stored instead of the argument itself.
   * 
   * @param password The password to be set
   */
  private void setPassword(char[] password)
  {
    if (password != null)
      this.password = (char[]) password.clone();
    else
      this.password = new char[0];

    passwordValid = true;
  }

  /**
   * Set the salt byte array.
   * <p>
   * A copy of the salt arguments is stored instead of the argument itself.
   * 
   * @param salt The salt to be set.
   * @throws NullPointerException If the salt is null.
   * @throws IllegalArgumentException If the salt is an empty array.
   */
  private void setSalt(byte[] salt)
  {
    if (salt.length == 0)
      throw new IllegalArgumentException("salt MUST NOT be an empty byte array");

    this.salt = (byte[]) salt.clone();
  }

  /**
   * Set the iterationCount.
   * 
   * @param iterationCount The iteration count to be set.
   * @throws IllegalArgumentException If the iterationCount is negative.
   */
  private void setIterationCount(int iterationCount)
  {
    if (iterationCount < 0)
      throw new IllegalArgumentException("iterationCount MUST be positive");

    this.iterationCount = iterationCount;
  }

  /**
   * Set the keyLength.
   * 
   * @param keyLength The keyLength to be set.
   * @throws IllegalArgumentException if the keyLength is negative.
   */
  private void setKeyLength(int keyLength)
  {
    if (keyLength < 0)
      throw new IllegalArgumentException("keyLength MUST be positive");

    this.keyLength = keyLength;
  }
}
