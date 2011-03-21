/* IKeyPairCodec.java --
   Copyright 2001, 2002, 2006 Free Software Foundation, Inc.

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


package gnu.java.security.key;

import gnu.java.security.Registry;

import java.security.PrivateKey;
import java.security.PublicKey;

/**
 * The visible methods of an object that knows how to encode and decode
 * cryptographic asymmetric keypairs. Codecs are useful for (a) externalising
 * public and private keys for storage and on-the-wire transmission, as well as
 * (b) re-creating their internal Java representation from external sources.
 */
public interface IKeyPairCodec
{
  /** Constant identifying the <i>Raw</i> encoding format. */
  int RAW_FORMAT = Registry.RAW_ENCODING_ID;

  /** Constant identifying the <i>X.509</i> encoding format. */
  int X509_FORMAT = Registry.X509_ENCODING_ID;

  /** Constant identifying the <i>PKCS#8</i> encoding format. */
  int PKCS8_FORMAT = Registry.PKCS8_ENCODING_ID;

  /**
   * Constant identifying the <i>ASN.1</i> encoding format: a combined encoding
   * of <i>X.509</i> for public keys, and <i>PKCS#8</i> for private ones.
   */
  int ASN1_FORMAT = Registry.ASN1_ENCODING_ID;

  /**
   * Returns the unique identifier (within this library) of the format used to
   * externalise public and private keys.
   *
   * @return the identifier of the format, the object supports.
   */
  int getFormatID();

  /**
   * Encodes an instance of a public key for storage or transmission purposes.
   *
   * @param key the non-null key to encode.
   * @return a byte sequence representing the encoding of the designated key
   *         according to the format supported by this codec.
   * @exception IllegalArgumentException if the designated key is not supported
   *              by this codec.
   */
  byte[] encodePublicKey(PublicKey key);

  /**
   * Encodes an instance of a private key for storage or transmission purposes.
   *
   * @param key the non-null key to encode.
   * @return a byte sequence representing the encoding of the designated key
   *         according to the format supported by this codec.
   * @exception IllegalArgumentException if the designated key is not supported
   *              by this codec.
   */
  byte[] encodePrivateKey(PrivateKey key);

  /**
   * Decodes an instance of an external public key into its native Java
   * representation.
   *
   * @param input the source of the externalised key to decode.
   * @return a concrete instance of a public key, reconstructed from the
   *         designated input.
   * @exception IllegalArgumentException if the designated input does not
   *              contain a known representation of a public key for the format
   *              supported by the concrete codec.
   */
  PublicKey decodePublicKey(byte[] input);

  /**
   * Decodes an instance of an external private key into its native Java
   * representation.
   *
   * @param input the source of the externalised key to decode.
   * @return a concrete instance of a private key, reconstructed from the
   *         designated input.
   * @exception IllegalArgumentException if the designated input does not
   *              contain a known representation of a private key for the format
   *              supported by the concrete codec.
   */
  PrivateKey decodePrivateKey(byte[] input);
}
