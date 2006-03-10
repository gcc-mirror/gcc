/* SecurityParameters.java -- SSL security parameters.
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


package gnu.javax.net.ssl.provider;

import javax.net.ssl.SSLException;

/**
 * The interface that all security parameters used by Jessie must implement.
 * Security parameters handle all transforming of data, including encryption,
 * authentication, and compression.
 */
interface SecurityParameters
{

  // Methods.
  // -------------------------------------------------------------------------

  /**
   * Decrypts, verifies, and inflates a fragment received. The fragment is
   * just the data field of a text object, without the version, type, and
   * length fields. An exception is thrown if any step fails.
   *
   * @param fragment The fragment being decrypted.
   * @param version  The version field of the received text.
   * @param type     The type field of the received text.
   * @return The decrypted fragment.
   * @throws MacException If the MAC could not be verified, or if the padding
   *         on the decrypted fragment is incorrect.
   * @throws OverflowException If the processed text overflows the configured
   *         maximum fragment size.
   * @throws SSLException If any other error occurs.
   */
  byte[] decrypt (byte[] fragment, ProtocolVersion version, ContentType type)
    throws MacException, OverflowException, SSLException;

  /**
   * Deflates, authenticates, and encrypts a fragment to be sent.
   *
   * @param buf The fragment being encrypted.
   * @param off The offset into the buffer to start at.
   * @param len The number of bytes in this fragment.
   * @param type The content type of this text.
   * @return The encrypted fragment.
   * @throws OverflowException If deflating increases the size of the fragment
   *         too much.
   * @throws SSLException If any other error occurs.
   */
  byte[] encrypt (byte[] buf, int off, int len, ContentType type)
    throws OverflowException, SSLException;

  /**
   * Set all crypto primitives to <code>null</code>, meaning that any calls
   * to {@link #encrypt(byte[],int,int,org.metastatic.jessie.provider.ContentType)} or
   * {@link #decrypt(byte[],org.metastatic.jessie.provider.ProtocolVersion,org.metastatic.jessie.provider.ContentType})
   * will perform the identity transformation.
   */
  void reset();

  /**
   * Returns the version of texts being sent.
   *
   * @return The version.
   */
  ProtocolVersion getVersion();

  /**
   * Sets the version of texts being sent. This affects the {@link
   * #encrypt(byte[],int,int,org.metastatic.jessie.provider.ContentType)}
   * method.
   *
   * @param version The version to set.
   */
  void setVersion (ProtocolVersion version);

  /**
   * Turns zlib deflating on or off.
   *
   * @param deflate Whether or not to deflate outgoing fragments.
   */
  void setDeflating (boolean deflate);

  /**
   * Turns zlib inflating on or off.
   *
   * @param inflate Whether or not to inflate incoming fragments.
   */
  void setInflating (boolean inflate);

  /**
   * Returns the maximum size that plaintext fragments may be.
   *
   * @return The fragment length.
   */
  int getFragmentLength();

  /**
   * Sets the maximum size that plaintext fragments may be.
   *
   * @param fragmentLength The new fragment length.
   */
  void setFragmentLength (int fragmentLength);

  /**
   * Set the cipher used to decrypt incoming fragments. The parameter must be
   * appropriate for the implementation.
   *
   * @param cipher The cipher.
   * @throws ClassCastException If the argument is not appropriate for the
   *         implementation.
   */
  void setInCipher (Object cipher);

  /**
   * Set the cipher used to encrypt outgoing fragments. The parameter must be
   * appropriate for the implementation.
   *
   * @param cipher The cipher.
   * @throws ClassCastException If the argument is not appropriate for the
   *         implementation.
   */
  void setOutCipher (Object cipher);

  /**
   * Set the MAC used to verify incoming fragments. The parameter must be
   * appropriate for the implementation.
   *
   * @param mac The MAC.
   * @throws ClassCastException If the argument is not appropriate for the
   *         implementation.
   */
  void setInMac (Object mac);

  /**
   * Set the MAC used to authenticating outgoinging fragments. The parameter
   * must be appropriate for the implementation.
   *
   * @param mac The MAC.
   * @throws ClassCastException If the argument is not appropriate for the
   *         implementation.
   */
  void setOutMac (Object mac);
}
