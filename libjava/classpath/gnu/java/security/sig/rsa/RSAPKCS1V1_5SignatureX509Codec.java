/* RSAPSSSignatureX509Codec.java -- X.509 encoder/decoder for RSA signatures
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


package gnu.java.security.sig.rsa;

import gnu.java.security.Registry;
import gnu.java.security.sig.ISignatureCodec;

import java.security.InvalidParameterException;

/**
 * An implementation of an {@link ISignatureCodec} that knows to encode and
 * decode RSA PKCS1 (v1.5) signatures into the raw bytes which would constitute
 * a DER-encoded form of the ASN.1 structure defined in RFC-2459, and RFC-2313
 * as described in the next paragraphs.
 * <p>
 * Digital signatures when transmitted in an X.509 certificates are encoded
 * in DER (Distinguished Encoding Rules) as a BIT STRING; i.e.
 * 
 * <pre>
 * Certificate ::= SEQUENCE {
 *   tbsCertificate       TBSCertificate,
 *   signatureAlgorithm   AlgorithmIdentifier,
 *   signature            BIT STRING
 * }
 * </pre>
 * <p>
 * The output of the encoder, and the input of the decoder, of this codec are
 * then the <i>raw</i> bytes of such a BIT STRING; i.e. not the DER-encoded
 * form itself.
 * <p>
 * Our implementation of the RSA PKCS1 signature algorithm outputs a byte array
 * as the result of generating a digital signature, in accordance with RFC-2313.
 * As a consequence, the encoder and decoder of this codec, simply pass through
 * such a byte array.
 * <p>
 * Client code that needs to build a DER BIT STRING <b>MUST</b> construct such
 * an ASN.1 value. The following is an example of how to do this:
 * <p>
 * <pre>
 * ...
 * import gnu.java.security.der.BitString;
 * import gnu.java.security.der.DER;
 * import gnu.java.security.der.DERValue;
 * ...
 * DERValue bitString = new DERValue(DER.BIT_STRING, new BitString(sigBytes));
 * ...
 * </pre>
 */
public class RSAPKCS1V1_5SignatureX509Codec
    implements ISignatureCodec
{
  // default 0-arguments constructor

  public int getFormatID()
  {
    return Registry.X509_ENCODING_ID;
  }

  /**
   * Encodes an RSA Signature output as a <i>signature</i> BIT STRING as
   * defined in the documentation of this class.
   * 
   * @param signature the output of the RSA PKCS1 (v1.5) signature algorithm;
   *          i.e. the value returned by the invocation of
   *          {@link gnu.java.security.sig.ISignature#sign()} method. In the
   *          case of the RSA PKCS1 (v1.5) signature this is an array of bytes.
   * @return the raw bytes of an RSA signature which could be then used as the
   *         contents of a BIT STRING as per rfc-2459.
   */
  public byte[] encodeSignature(Object signature)
  {
    byte[] result = (byte[]) signature;
    return result;
  }

  /**
   * Decodes a <i>signature</i> as defined in the documentation of this class.
   * 
   * @param input the byte array to unmarshall into a valid RSA PKCS1 (v1.5)
   *          signature instance; i.e. a byte array. MUST NOT be null.
   * @return an array of raw bytes decoded from the designated input. In the
   *         case of RSA PKCS1 (v1.5) this is the same as the input.
   * @throw InvalidParameterException if the <code>input</code> array is null.
   */
  public Object decodeSignature(byte[] input)
  {
    if (input == null)
      throw new InvalidParameterException("Input bytes MUST NOT be null");

    return input;
  }
}
