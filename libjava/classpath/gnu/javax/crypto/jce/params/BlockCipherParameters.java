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

import gnu.java.security.Configuration;
import gnu.javax.crypto.jce.spec.BlockCipherParameterSpec;

import java.io.IOException;
import java.math.BigInteger;

import java.security.AlgorithmParametersSpi;
import java.security.spec.AlgorithmParameterSpec;
import java.security.spec.InvalidParameterSpecException;
import java.util.logging.Logger;

import javax.crypto.spec.IvParameterSpec;

/**
 * An implementation of algorithm parameters for the GNU block ciphers. This
 * encompasses the cipher's block size, its key size, and an optional
 * initialization vector (IV).
 */
public class BlockCipherParameters
    extends AlgorithmParametersSpi
{
  private static final Logger log = Logger.getLogger(BlockCipherParameters.class.getName());
  /** The underlying block cipher specification. */
  protected BlockCipherParameterSpec cipherSpec;
  private static final String DEFAULT_FORMAT = "ASN.1";

  /**
   * Return these parameters encoded in ASN.1 (DER).
   * <p>
   * For GNU block ciphers we will define these parameters as
   * <pre>
   * BlockCipherParameters ::= SEQUENCE {
   *    blockSize            INTEGER,
   *    keySize              INTEGER,
   *    initializationVector OCTET STRING OPTIONAL }
   * </pre>
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
    if (! format.equalsIgnoreCase(DEFAULT_FORMAT)
        && ! format.equalsIgnoreCase("asn1"))
      throw new IOException("unknown format \"" + format + "\"");
    DERWriter writer = new DERWriter();
    int cipherBlockSize = cipherSpec.getBlockSize();
    int cipherKeySize = cipherSpec.getKeySize();
    byte[] iv = cipherSpec.getIV();
    return writer.joinarrays(
        writer.writeBigInteger(BigInteger.valueOf(cipherBlockSize)),
        writer.writeBigInteger(BigInteger.valueOf(cipherKeySize)),
        (iv != null) ? writer.writeBigInteger(new BigInteger(iv))
                     : new byte[0]);
  }

  protected void engineInit(AlgorithmParameterSpec spec)
      throws InvalidParameterSpecException
  {
    if (spec instanceof BlockCipherParameterSpec)
      cipherSpec = (BlockCipherParameterSpec) spec;
    else
      throw new InvalidParameterSpecException();
  }

  protected void engineInit(byte[] encoded, String format) throws IOException
  {
    if (! format.equalsIgnoreCase(DEFAULT_FORMAT)
        && ! format.equalsIgnoreCase("ASN1"))
      throw new IOException("invalid format: only accepts ASN.1");
    engineInit(encoded);
  }

  protected void engineInit(byte[] encoded) throws IOException
  {
    DERReader reader = new DERReader(encoded);
    int bs = reader.getBigInteger().intValue();
    int ks = reader.getBigInteger().intValue();
    byte[] iv = null;
    if (reader.hasMorePrimitives())
      iv = reader.getBigInteger().toByteArray();
    cipherSpec = new BlockCipherParameterSpec(iv, bs, ks);
    if (Configuration.DEBUG)
      log.fine("cipherSpec: " + cipherSpec);
  }

  protected AlgorithmParameterSpec engineGetParameterSpec(Class c)
      throws InvalidParameterSpecException
  {
    if (c.isInstance(cipherSpec))
      return cipherSpec;
    if (IvParameterSpec.class.isAssignableFrom(c))
      {
        IvParameterSpec result = new IvParameterSpec(cipherSpec.getIV());
        return result;
      }
    throw new InvalidParameterSpecException();
  }

  protected String engineToString()
  {
    return cipherSpec.toString();
  }
}
