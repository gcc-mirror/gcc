/* BlockCipherParameters.java -- 
   Copyright (C) 2002, 2003, 2006  Free Software Foundation, Inc.

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


package gnu.javax.crypto.jce.params;

import gnu.javax.crypto.jce.spec.BlockCipherParameterSpec;

import java.io.IOException;
import java.math.BigInteger;

import java.security.AlgorithmParametersSpi;
import java.security.spec.AlgorithmParameterSpec;
import java.security.spec.InvalidParameterSpecException;

/**
 * An implementation of algorithm parameters for the GNU Crypto block
 * ciphers. This encompasses the cipher's block size, its key size, and
 * an optional initialization vector (IV).
 */
public class BlockCipherParameters extends AlgorithmParametersSpi
{

  // Constants and variables.
  // -----------------------------------------------------------------------

  /**
   * The underlying block cipher specification.
   */
  protected BlockCipherParameterSpec cipherSpec;

  private static final String DEFAULT_FORMAT = "ASN.1";

  // Instance methods implementing AlgorithmParametersSpi.
  // -----------------------------------------------------------------------

  /**
   * Return these parameters encoded in ASN.1 (DER).
   *
   * <p>For GNU Crypto block ciphers we will define these parameters as
   *
   * <blockquote>
   * <pre>BlockCipherParameters ::= SEQUENCE {
   *   blockSize            INTEGER,
   *   keySize              INTEGER,
   *   initializationVector OCTET STRING OPTIONAL }</pre>
   * </blockquote>
   *
   * @return The parameters, encoded an an ASN.1 DER sequence.
   * @throws java.io.IOException If encoding these parameters fails.
   */
  protected byte[] engineGetEncoded() throws IOException
  {
    return engineGetEncoded(DEFAULT_FORMAT);
  }

  protected byte[] engineGetEncoded(String format) throws IOException
  {
    if (!format.equalsIgnoreCase(DEFAULT_FORMAT)
        && !format.equalsIgnoreCase("asn1"))
      {
        throw new IOException("unknown format \"" + format + "\"");
      }
    // This is probably a bad idea.
    /*
     int len = 12 + ((cipherSpec.getIV() != null)
     ? cipherSpec.getIV().length + 2 : 0);
     ByteArrayOutputStream out = new ByteArrayOutputStream();
     out.write(0x30);
     out.write(len);
     out.write(0x02);
     out.write(4);
     out.write(cipherSpec.getBlockSize() >>> 24 & 0xff);
     out.write(cipherSpec.getBlockSize() >>> 16 & 0xff);
     out.write(cipherSpec.getBlockSize() >>>  8 & 0xff);
     out.write(cipherSpec.getBlockSize() & 0xff);
     out.write(0x02);
     out.write(4);
     out.write(cipherSpec.getKeySize() >>> 24 & 0xff);
     out.write(cipherSpec.getKeySize() >>> 16 & 0xff);
     out.write(cipherSpec.getKeySize() >>>  8 & 0xff);
     out.write(cipherSpec.getKeySize() & 0xff);
     if (cipherSpec.getIV() != null) {
     out.write(0x04);
     len = cipherSpec.getIV().length;
     out.write(len & 0xff);
     out.write(cipherSpec.getIV());
     }
     out.write(0); out.write(0);
     return out.toByteArray();*/
    DERWriter writer = new DERWriter();
    return writer.joinarrays(
                             writer.writeBigInteger(BigInteger.valueOf(cipherSpec.getBlockSize())),
                             writer.writeBigInteger(BigInteger.valueOf(cipherSpec.getKeySize())),
                             (cipherSpec.getIV() != null) ? writer.writeBigInteger(new BigInteger(
                                                                                                  cipherSpec.getIV()))
                                                         : new byte[0]);
  }

  protected void engineInit(AlgorithmParameterSpec spec)
      throws InvalidParameterSpecException
  {
    if (spec instanceof BlockCipherParameterSpec)
      {
        cipherSpec = (BlockCipherParameterSpec) spec;
      }
    else
      {
        throw new InvalidParameterSpecException();
      }
  }

  protected void engineInit(byte[] encoded, String format) throws IOException
  {
    if (!format.equalsIgnoreCase(DEFAULT_FORMAT)
        && !format.equalsIgnoreCase("ASN1"))
      {
        throw new IOException("invalid format: only accepts ASN.1");
      }
    engineInit(encoded);
  }

  protected void engineInit(byte[] encoded) throws IOException
  {
    // This is probably an equally bad idea.
    /*if (encoded[0] != 0x30) {
     throw new IOException("malformed ASN.1 sequence");
     }
     if (encoded[2] != 0x02 || encoded[3] != 4) {
     throw new IOException("malformed ASN.1 sequence");
     }
     int blockSize = encoded[4] << 24 | encoded[5] << 16
     | encoded[6] <<  8 | encoded[7];
     if (encoded[8] != 0x02 || encoded[9] != 4) {
     throw new IOException("malformed ASN.1 sequence");
     }
     int keySize = encoded[10] << 24 | encoded[11] << 16
     | encoded[12] <<  8 | encoded[13];
     if (encoded[14] == 0x04) {
     int len = encoded[15] & 0xff;
     byte[] iv = new byte[len];
     System.arraycopy(encoded, 16, iv, 0, len);
     cipherSpec = new BlockCipherParameterSpec(iv, blockSize, keySize);
     } else if (encoded[14] == 0) {
     cipherSpec = new BlockCipherParameterSpec(blockSize, keySize);
     } else {
     throw new IOException("malformed ASN.1 sequence");
     }*/
    DERReader reader = new DERReader(encoded);
    int bs = reader.getBigInteger().intValue();
    int ks = reader.getBigInteger().intValue();
    byte[] iv = null;
    if (reader.hasMorePrimitives())
      {
        iv = reader.getBigInteger().toByteArray();
      }
    cipherSpec = new BlockCipherParameterSpec(iv, bs, ks);
    System.out.println(cipherSpec);
  }

  protected AlgorithmParameterSpec engineGetParameterSpec(Class c)
      throws InvalidParameterSpecException
  {
    if (c.isInstance(cipherSpec))
      {
        return cipherSpec;
      }
    throw new InvalidParameterSpecException();
  }

  protected String engineToString()
  {
    return cipherSpec.toString();
  }
}