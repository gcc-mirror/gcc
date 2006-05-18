/* KerberosKey.java -- kerberos key
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package javax.security.auth.kerberos;

import gnu.classpath.NotImplementedException;

import java.io.Serializable;

import javax.crypto.SecretKey;
import javax.security.auth.DestroyFailedException;
import javax.security.auth.Destroyable;

/**
 * This class represents a Kerberos key.  See the Kerberos
 * authentication RFC for more information:
 * <a href="http://www.ietf.org/rfc/rfc1510.txt">RFC 1510</a>.
 * 
 * @since 1.4
 */
public class KerberosKey
    implements Serializable, SecretKey, Destroyable
{
  private static final long serialVersionUID = -4625402278148246993L;

  private KerberosPrincipal principal;
  private int versionNum;
  private KeyImpl key;

  /**
   * Construct a new key with the indicated principal and key.
   * @param principal the principal
   * @param key the key's data
   * @param type the key's type
   * @param version the key's version number
   */
  public KerberosKey(KerberosPrincipal principal, byte[] key, int type,
                     int version)
  {
    this.principal = principal;
    this.versionNum = version;
    this.key = new KeyImpl(key, type);
  }

  /**
   * Construct a new key with the indicated principal and a password.
   * @param principal the principal
   * @param passwd the password to use
   * @param algo the algorithm; if null the "DES" algorithm is used
   */
  public KerberosKey(KerberosPrincipal principal, char[] passwd, String algo)
  // Not implemented because KeyImpl really does nothing here.
    throws NotImplementedException
  {
    this.principal = principal;
    this.versionNum = 0; // FIXME: correct?
    this.key = new KeyImpl(passwd, algo);
  }

  /**
   * Return the name of the algorithm used to create this key.
   */
  public final String getAlgorithm()
  {
    checkDestroyed();
    return key.algorithm;
  }

  /**
   * Return the format of this key.  This implementation always returns "RAW".
   */
  public final String getFormat()
  {
    checkDestroyed();
    // Silly, but specified.
    return "RAW";
  }

  /**
   * Return the principal associated with this key.
   */
  public final KerberosPrincipal getPrincipal()
  {
    checkDestroyed();
    return principal;
  }

  /**
   * Return the type of this key.
   */
  public final int getKeyType()
  {
    checkDestroyed();
    return key.type;
  }

  /**
   * Return the version number of this key.
   */
  public final int getVersionNumber()
  {
    checkDestroyed();
    return versionNum;
  }

  /**
   * Return the encoded form of this key.
   */
  public final byte[] getEncoded()
  {
    checkDestroyed();
    return (byte[]) key.key.clone();
  }

  /**
   * Destroy this key.
   */
  public void destroy() throws DestroyFailedException
  {
    if (key == null)
      throw new DestroyFailedException("already destroyed");
    key = null;
  }

  /**
   * Return true if this key has been destroyed.  After this has been
   * called, other methods on this object will throw IllegalStateException.
   */
  public boolean isDestroyed()
  {
    return key == null;
  }

  private void checkDestroyed()
  {
    if (key == null)
      throw new IllegalStateException("key is destroyed");
  }

  public String toString()
  {
    // FIXME: random choice here.
    return principal + ":" + versionNum;
  }
}
