/* TrustAnchor.java -- an ultimately-trusted certificate.
   Copyright (C) 2003 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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


package java.security.cert;

import java.io.ByteArrayInputStream;
import java.io.IOException;

import java.security.PublicKey;

import gnu.java.security.x509.X500DistinguishedName;

/**
 * An ultimately-trusted certificate to serve as the root of a
 * certificate chain.
 *
 * @author Casey Marshall (rsdio@metastatic.org)
 */
public class TrustAnchor
{

  // Fields.
  // ------------------------------------------------------------------------

  /** The certificate authority's distinguished name. */
  private final X500DistinguishedName caName;

  /** The certficate authority's public key. */
  private final PublicKey caKey;

  /** The certficate authority's certificate. */
  private final X509Certificate trustedCert;

  /** The encoded name constraints bytes. */
  private final byte[] nameConstraints;

  // Constnuctors.
  // ------------------------------------------------------------------------

  /**
   * Create a new trust anchor from a certificate and (optional) name
   * constraints.
   *
   * <p>If the <i>nameConstraints</i> argument in non-null, it will be
   * copied to prevent modification.
   *
   * @param trustedCert The trusted certificate.
   * @param nameConstraints The encoded nameConstraints.
   */
  public TrustAnchor(X509Certificate trustedCert, byte[] nameConstraints)
  {
    if (trustedCert == null)
      throw new NullPointerException();
    this.trustedCert = trustedCert;
    caName = null;
    caKey = null;
    if (nameConstraints != null)
      this.nameConstraints = (byte[]) nameConstraints.clone();
    else
      this.nameConstraints = null;
  }

  /**
   * Create a new trust anchor from a certificate authority's
   * distinguished name, public key, and (optional) name constraints.
   *
   * <p>If the <i>nameConstraints</i> argument in non-null, it will be
   * copied to prevent modification.
   *
   * @params caName The CA's distinguished name.
   * @params caKey The CA's public key.
   * @params nameConstraints The encoded nameConstraints.
   */
  public TrustAnchor(String caName, PublicKey caKey, byte[] nameConstraints)
  {
    if (caName == null || caKey == null)
      throw new NullPointerException();
    if (caName.length() == 0)
      throw new IllegalArgumentException();
    trustedCert = null;
    this.caName = new X500DistinguishedName(caName);
    this.caKey = caKey;
    if (nameConstraints != null)
      this.nameConstraints = (byte[]) nameConstraints.clone();
    else
      this.nameConstraints = null;
  }

  // Instance methods.
  // ------------------------------------------------------------------------

  /**
   * Return the trusted certificate, or null if none was specified.
   *
   * @return The trusted certificate.
   */
  public final X509Certificate getTrustedCert()
  {
    return trustedCert;
  }

  /**
   * Return the certificate authority's distinguished name, or null if
   * none was specified.
   *
   * @return The CA's distinguished name.
   */
  public final String getCAName()
  {
    if (caName != null)
      return caName.toRFC2253();
    return null;
  }

  /**
   * Return the certificate authority's public key, or null if none was
   * specified.
   *
   * @return The CA's public key.
   */
  public final PublicKey getCAPublicKey()
  {
    return caKey;
  }

  /**
   * Return the encoded name constraints, or null if none was specified.
   *
   * <p>The name constraints byte array is copied when this method is
   * called to prevent modification.
   *
   * @return The encoded name constraints.
   */
  public final byte[] getNameConstraints()
  {
    if (nameConstraints == null)
      return null;
    return (byte[]) nameConstraints.clone();
  }

  /**
   * Return a printable representation of this trust anchor.
   *
   * @return The printable representation.
   */
  public String toString()
  {
    if (trustedCert == null)
      return "[ Trusted CA Public Key=" + caKey + ", Trusted CA Issuer Name="
        + caName.toRFC2253() + " ]";
    return "[ Trusted CA Certificate=" + trustedCert + " ]";
  }
}
