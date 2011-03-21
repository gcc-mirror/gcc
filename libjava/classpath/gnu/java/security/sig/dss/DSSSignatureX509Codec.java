/* DSSSignatureX509Codec.java -- X.509 encoder/decoder for DSS signatures
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


package gnu.java.security.sig.dss;

import gnu.java.security.Registry;
import gnu.java.security.der.DER;
import gnu.java.security.der.DERReader;
import gnu.java.security.der.DERValue;
import gnu.java.security.der.DERWriter;
import gnu.java.security.sig.ISignatureCodec;
import gnu.java.security.util.DerUtil;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.math.BigInteger;
import java.security.InvalidParameterException;
import java.util.ArrayList;

/**
 * An implementation of an {@link ISignatureCodec} that knows to encode and
 * decode DSS signatures into the raw bytes which would constitute a DER-encoded
 * form of the ASN.1 structure defined in RFC-2459, and RFC-2313 as described in
 * the next paragraphs.
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
 * RFC-2459 states that, for the Digital Signature Standard (DSS), which
 * generates two MPIs, commonly called <code>r</code> and <code>s</code>, as the
 * result of digitally signing a message, these two numbers will be transferred
 * as the following ASN.1 structure:
 *
 * <pre>
 *   Dss-Sig-Value ::= SEQUENCE {
 *     r  INTEGER,
 *     s  INTEGER
 *   }
 * </pre>
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
public class DSSSignatureX509Codec
    implements ISignatureCodec
{
  // implicit 0-arguments constructor

  public int getFormatID()
  {
    return Registry.X509_ENCODING_ID;
  }

  /**
   * Encodes a DSS Signature output as the <i>signature</i> raw bytes which can
   * be used to construct an ASN.1 DER-encoded BIT STRING as defined in the
   * documentation of this class.
   *
   * @param signature the output of the DSS signature algorithm; i.e. the value
   *          returned by the invocation of
   *          {@link gnu.java.security.sig.ISignature#sign()} method. In the
   *          case of a DSS signature this is an array of two MPIs called
   *          <code>r</code> and <code>s</code>.
   * @return the raw bytes of a DSS signature which could be then used as the
   *         contents of a BIT STRING as per rfc-2459.
   * @throws InvalidParameterException if an exception occurs during the
   *           marshalling process.
   */
  public byte[] encodeSignature(Object signature)
  {
    BigInteger[] rs = (BigInteger[]) signature;

    DERValue derR = new DERValue(DER.INTEGER, rs[0]);
    DERValue derS = new DERValue(DER.INTEGER, rs[1]);

    ArrayList dssSigValue = new ArrayList(2);
    dssSigValue.add(derR);
    dssSigValue.add(derS);
    DERValue derDssSigValue = new DERValue(DER.CONSTRUCTED | DER.SEQUENCE,
                                           dssSigValue);
    byte[] result;
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    try
      {
        DERWriter.write(baos, derDssSigValue);
        result = baos.toByteArray();
      }
    catch (IOException x)
      {
        InvalidParameterException y = new InvalidParameterException();
        y.initCause(x);
        throw y;
      }

    return result;
  }

  /**
   * Decodes a <i>signature</i> as defined in the documentation of this class.
   *
   * @param input the byte array to unmarshall into a valid DSS signature
   *          instance; i.e. an array of two MPIs. MUST NOT be null.
   * @return an array of two MPIs, <code>r</code> and <code>s</code> in this
   *         order, decoded from the designated <code>input</code>.
   * @throw InvalidParameterException if an exception occurs during the
   *        unmarshalling process.
   */
  public Object decodeSignature(byte[] input)
  {
    if (input == null)
      throw new InvalidParameterException("Input bytes MUST NOT be null");

    BigInteger r, s;
    DERReader der = new DERReader(input);
    try
      {
        DERValue derDssSigValue = der.read();
        DerUtil.checkIsConstructed(derDssSigValue, "Wrong Dss-Sig-Value field");

        DERValue val = der.read();
        DerUtil.checkIsBigInteger(val, "Wrong R field");
        r = (BigInteger) val.getValue();
        val = der.read();
        DerUtil.checkIsBigInteger(val, "Wrong S field");
        s = (BigInteger) val.getValue();
      }
    catch (IOException x)
      {
        InvalidParameterException y = new InvalidParameterException();
        y.initCause(x);
        throw y;
      }

    return new BigInteger[] { r, s };
  }
}
