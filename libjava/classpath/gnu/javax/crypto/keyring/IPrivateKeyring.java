/* IPrivateKeyring.java --
   Copyright (C) 2003, 2006 Free Software Foundation, Inc.

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


package gnu.javax.crypto.keyring;

import java.security.Key;
import java.security.PublicKey;
import java.security.UnrecoverableKeyException;
import java.security.cert.Certificate;

/**
 * An interface to private, or "personal", keyrings, which contain private
 * credentials. The contract is that each such entry is known by a unique
 * <i>alias</i>.
 * <p>
 * What about public keys? and certificate-path?
 */
public interface IPrivateKeyring
    extends IKeyring
{
  /**
   * Tests if this keyring contains a private key entry with the given
   * <code>alias</code>.
   *
   * @param alias The alias to check.
   * @return <code>true</code> if this keyring contains a private key with the
   *         given <code>alias</code>; <code>false</code> otherwise.
   */
  boolean containsPrivateKey(String alias);

  /**
   * Returns the private key with the given <code>alias</code>.
   *
   * @param alias The alias of the private key to find.
   * @param password The password of the private key.
   * @return The private, or secret, key if one is found; <code>null</code> if
   *         none were found.
   * @throws UnrecoverableKeyException If the private key could not be
   *           recovered, possibly due to a bad password.
   */
  Key getPrivateKey(String alias, char[] password)
      throws UnrecoverableKeyException;

  /**
   * Adds a private key to this keyring.
   *
   * @param alias The alias of the private key.
   * @param key The private key.
   * @param password The password used to protect this private key.
   */
  void putPrivateKey(String alias, Key key, char[] password);

  /**
   * Checks if this keyring contains a public key with the given
   * <code>alias</code>.
   *
   * @param alias The alias to test.
   * @return <code>true</code> if this keyring contains a public key entry
   *         with the given <code>alias</code>; <code>false</code>
   *         otherwise.
   */
  boolean containsPublicKey(String alias);

  /**
   * Returns the public key with the given <code>alias</code>, or
   * <code>null</code> if there is no such entry.
   *
   * @param alias The alias of the public key to find.
   * @return The public key; or <code>null</code> if none were found.
   */
  PublicKey getPublicKey(String alias);

  /**
   * Sets a public key entry.
   *
   * @param alias The alias for this public key.
   * @param key The public key.
   */
  void putPublicKey(String alias, PublicKey key);

  /**
   * Checks if this keyring contains a certificate path with the given
   * <code>alias</code>.
   *
   * @param alias The alias to check.
   * @return <code>true</code> if this keyring contains a certificate path
   *         with the given <code>alias</code>; <code>false</code>
   *         otherwise.
   */
  boolean containsCertPath(String alias);

  /**
   * Returns the certificate path with the given <code>alias</code>, or
   * <code>null</code> if there is no such entry.
   *
   * @param alias The alias of the certificate path to find.
   * @return The certificate path for the designated <code>alias</code>; or
   *         <code>null</code> if none were found.
   */
  Certificate[] getCertPath(String alias);

  /**
   * Sets a certificate path entry.
   *
   * @param alias The alias for this certificate path.
   * @param path The certificate path.
   */
  void putCertPath(String alias, Certificate[] path);
}
