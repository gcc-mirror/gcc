/* Certificate.java -- base class of public-key certificates.
   Copyright (C) 2004 Free Software Foundation, Inc.

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


package javax.security.cert;

import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.PublicKey;
import java.security.SignatureException;

import java.util.Arrays;
import java.util.zip.Adler32;

/**
 * <p>The base class for public-key certificates.</p>
 *
 * <p><b>This class is deprecated in favor of the {@link
 * java.security.cert.Certificate} class. It should not be used in new
 * applications.</b></p>
 */
public abstract class Certificate
{

  // Constructors.
  // -------------------------------------------------------------------------

  public Certificate()
  {
    super();
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  /**
   * <p>Tests if this certificate equals another.</p>
   *
   * @param other The object to test.
   * @return True if the certificates are equal.
   */
  public boolean equals(Object other)
  {
    if (other == null || !(other instanceof Certificate))
      {
        return false;
      }
    if (other == this)
      {
        return true;
      }
    try
      {
        return Arrays.equals(getEncoded(), ((Certificate) other).getEncoded());
      }
    catch (CertificateEncodingException cee)
      {
        return false;
      }
  }

  /**
   * <p>Computes a hash code for this certificate.</p>
   *
   * @return The hash code.
   */
  public int hashCode()
  {
    try
      {
        Adler32 csum = new Adler32();
        csum.update(getEncoded());
        return (int) csum.getValue();
      }
    catch (CertificateEncodingException cee)
      {
        return 0;
      }
  }

  // Abstract methods.
  // -------------------------------------------------------------------------

  /**
   * <p>Return the encoded form of this certificate.</p>
   *
   * @return The encoded form.
   * @throws CertificateEncodingException If the certificate could not be
   *   encoded.
   */
  public abstract byte[] getEncoded() throws CertificateEncodingException;

  /**
   * <p>Verifies the signature of this certificate.</p>
   *
   * @param key The signer's public key.
   * @throws CertificateException
   * @throws NoSuchAlgorithmException If the algorithm used to sign the
   *   certificate is not available.
   * @throws InvalidKeyException If the supplied key is not appropriate for the
   *   certificate's signature algorithm.
   * @throws NoSuchProviderException
   * @throws SignatureException If the signature could not be verified.
   */
  public abstract void verify(PublicKey key)
    throws CertificateException, NoSuchAlgorithmException, InvalidKeyException,
           NoSuchProviderException, SignatureException;

  /**
   * <p>Verifies the signature of this certificate, using the specified security
   * provider.</p>
   *
   * @param key The signer's public key.
   * @param sigProvider The name of the signature provider.
   * @throws CertificateException
   * @throws NoSuchAlgorithmException If the algorithm used to sign the
   *   certificate is not available.
   * @throws InvalidKeyException If the supplied key is not appropriate for the
   *   certificate's signature algorithm.
   * @throws NoSuchProviderException If <i>sigProvider</i> is not the name of an
   *   installed provider.
   * @throws SignatureException If the signature could not be verified.
   */
  public abstract void verify(PublicKey key, String sigProvider)
    throws CertificateException, NoSuchAlgorithmException, InvalidKeyException,
           NoSuchProviderException, SignatureException;

  /**
   * <p>Returns a printable representation of this certificate.</p>
   *
   * @return The string.
   */
  public abstract String toString();

  /**
   * <p>Returns this certificate's public key.</p>
   *
   * @return The public key.
   */
  public abstract PublicKey getPublicKey();
}
