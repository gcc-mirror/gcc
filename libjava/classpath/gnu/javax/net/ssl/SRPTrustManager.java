/* SRPTrustManager.java -- interface to SRP trust managers.
   Copyright (C) 2006  Free Software Foundation, Inc.

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


package gnu.javax.net.ssl;

import gnu.javax.crypto.sasl.srp.PasswordFile;

import java.math.BigInteger;
import java.security.KeyPair;
import javax.net.ssl.TrustManager;

/**
 * A trust manager for secure remote password (SRP) key exchange cipher
 * suites. This is a read-only interface to the {@link
 * gnu.crypto.sasl.srp.PasswordFile} class, with convenience methods to
 * generate session key pairs.
 */
public interface SRPTrustManager extends TrustManager
{

  // Methods.
  // -------------------------------------------------------------------------

  /**
   * Tests if the configured password file contains the specified user name.
   *
   * @param user The user name.
   * @return True if the password file has an entry for <i>user</i>
   */
  boolean contains(String user);

  /**
   * Create and return a session SRP key pair for the given user name.
   *
   * @param user The user name to generate the key pair for.
   * @return The session key pair, or <code>null</code> if there is no
   *   entry for <i>user</i>.
   */
  KeyPair getKeyPair(String user);

  /**
   * Returns the salt value for the given user.
   *
   * @param user The user name.
   * @return The salt for <i>user</i>'s entry, or <code>null</code>.
   */
  byte[] getSalt(String user);

  /**
   * Returns the password verifier for the given user.
   *
   * @param user The user name.
   * @return <i>user</i>'s password verifier, or <code>null</code>.
   */
  BigInteger getVerifier(String user);

  /**
   * Returns a reference to the SRP {@link PasswordFile} used by this
   * {@link TrustManager}.
   *
   * @return a reference to the SRP password file in use.
   */
  PasswordFile getPasswordFile();
}
