/* Signer.java --- Signer Class
   Copyright (C) 1999, 2003, Free Software Foundation, Inc.

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

/**
 * <code>Signer</code> is a subclass of {@link Identity}. It is used to store a
 * digital signature key with an <i>Identity</i>.
 *
 * @author Mark Benvenuto (ivymccough@worldnet.att.net)
 * @deprecated Replaced by <code>java.security.KeyStore</code>, the
 * <code>java.security.cert</code> package, and <code>java.security.Principal</code>.
 */
public abstract class Signer extends Identity
{
  private static final long serialVersionUID = -1763464102261361480L;
  private PrivateKey privateKey = null;

  /** Trivial constructor for serialization purposes. */
  protected Signer()
  {
  }

  /**
   * Constructs a new instance of <code>Signer</code> with the specified
   * identity name.
   * 
   * @param name
   *          the name of the identity to use.
   */
  public Signer(String name)
  {
    super(name);
  }

  /**
   * Constructs a new instance of <code>Signer</code> with the specified
   * identity name and {@link IdentityScope}.
   * 
   * @param name
   *          the name of the the identity to use.
   * @param scope
   *          the {@link IdentityScope} to use.
   * @throws KeyManagementException
   *           if a duplicate identity <code>name</code> exists within
   *           <code>scope</code>.
   */
  public Signer(String name, IdentityScope scope) throws KeyManagementException
  {
    super(name, scope);
  }

  /**
   * Returns the private key of this <code>Signer</code>.
   * 
   * @returns the private key of this <code>Signer</code>.
   * @throws SecurityException
   *           if a {@link SecurityManager} is installed which disallows this
   *           operation.
   */
  public PrivateKey getPrivateKey()
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkSecurityAccess("getSignerPrivateKey");

    return privateKey;
  }

  /**
   * Specifies the {@link KeyPair} associated with this <code>Signer</code>.
   * 
   * @param pair
   *          the {@link KeyPair} to use.
   * @throws InvalidParameterException
   *           if the key-pair is invalid.
   * @throws KeyException
   *           if any another key-related error occurs.
   * @throws SecurityException
   *           if a {@link SecurityManager} is installed which disallows this
   *           operation.
   */
  public final void setKeyPair(KeyPair pair)
    throws InvalidParameterException, KeyException
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkSecurityAccess("setSignerKeyPair");

    try
      {
        if (pair.getPublic() != null)
          setPublicKey(pair.getPublic());
        else
          throw new InvalidParameterException();

      }
    catch (KeyManagementException kme)
      {
        throw new KeyException();
      }

    if (pair.getPrivate() != null)
        privateKey = pair.getPrivate();
    else
      throw new InvalidParameterException();
  }

  /** @returns a string representing this <code>Signer</code>. */
  public String toString()
  {
    return (getName() + ": " + privateKey);
  }
}
