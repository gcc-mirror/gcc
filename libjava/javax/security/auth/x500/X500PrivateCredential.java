/* X500PrivateCredential.java -- certificate and private key pair.
   Copyright (C) 2003, 2004  Free Software Foundation, Inc.

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


package javax.security.auth.x500;

import java.security.PrivateKey;
import java.security.cert.X509Certificate;

import javax.security.auth.Destroyable;

/**
 * A pairing of a {@link X509Certificate} and its corresponding {@link
 * PrivateKey}, with an optional keystore alias.
 */
public final class X500PrivateCredential implements Destroyable
{

  // Fields.
  // -------------------------------------------------------------------------

  private PrivateKey key;
  private X509Certificate certificate;
  private String alias;

  // Constructors.
  // -------------------------------------------------------------------------

  /**
   * Creates a new private credential with no associated keystore alias.
   *
   * @param certificate The X.509 certificate.
   * @param key The private key.
   * @throws IllegalArgumentException If either parameter is null.
   */
  public X500PrivateCredential (X509Certificate certificate, PrivateKey key)
  {
    if (certificate == null || key == null)
      throw new IllegalArgumentException();
    this.certificate = certificate;
    this.key = key;
  }

  /**
   * Creates a new private credential with a keystore alias.
   *
   * @param certificate The X.509 certificate.
   * @param key The private key.
   * @param alias The keystore alias for this credential.
   * @throws IllegalArgumentException If any parameter is null.
   */
  public X500PrivateCredential (X509Certificate certificate, PrivateKey key,
                                String alias)
  {
    this (certificate, key);
    if (alias == null)
      throw new IllegalArgumentException();
    this.alias = alias;
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  /**
   * Returns the certificate of this credential.
   *
   * @return The certificate of this credential.
   */
  public X509Certificate getCertificate()
  {
    return certificate;
  }

  /**
   * Returns the private key of this credential.
   *
   * @return The private key of this credential.
   */
  public PrivateKey getPrivateKey()
  {
    return key;
  }

  /**
   * Returns the keystore alias of this credential, or null if not present.
   *
   * @return The keystore alias, or null.
   */
  public String getAlias()
  {
    return alias;
  }

  /**
   * Destroy the sensitive data of this credential, setting the certificate,
   * private key, and keystore alias to null.
   */
  public void destroy()
  {
    certificate = null;
    key = null;
    alias = null;
  }

  /**
   * Tells whether or not this credential has been destroyed, and that
   * the certificate and private key fields are null.
   *
   * @return True if this object has been destroyed.
   */
  public boolean isDestroyed()
  {
    return certificate == null && key == null;
  }
}
