/* SRPPublicKey.java -- 
   Copyright (C) 2003, 2006 Free Software Foundation, Inc.

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


package gnu.javax.crypto.key.srp6;

import gnu.java.security.Registry;
import gnu.java.security.key.IKeyPairCodec;

import java.math.BigInteger;
import java.security.PublicKey;

/**
 * A representation of an SRP ephemeral public key.
 * <p>
 * Reference:
 * <ol>
 * <li><a href="http://srp.stanford.edu/design.html">SRP Protocol Design</a><br>
 * Thomas J. Wu.</li>
 * </ol>
 */
public class SRPPublicKey
    extends SRPKey
    implements PublicKey
{
  /**
   * The public exponent for either the server or the client engaged in the SRP
   * protocol exchange.
   */
  private final BigInteger Y;

  /**
   * Public constructor for use from outside this package.
   * 
   * @param N the public shared modulus.
   * @param g the generator.
   * @param Y the public exponent of the ephemeral key.
   */
  public SRPPublicKey(BigInteger N, BigInteger g, BigInteger Y)
  {
    super(N, g);

    SRPAlgorithm.checkParams(N, g);
    this.Y = Y;
  }

  /**
   * Default constructor. Assumes that N and g are already validated.
   * 
   * @param params an array of 3 values representing N, g and Y; the latter
   *          being the client's or server's public exponent.
   */
  SRPPublicKey(BigInteger[] params)
  {
    super(params[0], params[1]);

    this.Y = params[2];
  }

  /**
   * A class method that takes the output of the <code>encodePublicKey()</code>
   * method of an SRP keypair codec object (an instance implementing
   * {@link IKeyPairCodec} for SRP keys, and re-constructs an instance of this
   * object.
   * 
   * @param k the contents of a previously encoded instance of this object.
   * @throws ArrayIndexOutOfBoundsException if there is not enough bytes, in
   *           <code>k</code>, to represent a valid encoding of an instance
   *           of this object.
   * @throws IllegalArgumentException if the byte sequence does not represent a
   *           valid encoding of an instance of this object.
   */
  public static SRPPublicKey valueOf(byte[] k)
  {
    // check magic...
    // we should parse here enough bytes to know which codec to use, and
    // direct the byte array to the appropriate codec. since we only have one
    // codec, we could have immediately tried it; nevertheless since testing
    // one byte is cheaper than instatiating a codec that will fail we test
    // the first byte before we carry on.
    if (k[0] == Registry.MAGIC_RAW_SRP_PUBLIC_KEY[0])
      {
        // it's likely to be in raw format. get a raw codec and hand it over
        IKeyPairCodec codec = new SRPKeyPairRawCodec();
        return (SRPPublicKey) codec.decodePublicKey(k);
      }
    throw new IllegalArgumentException("magic");
  }

  /**
   * Returns the public exponent of the key as a {@link BigInteger}.
   * 
   * @return the public exponent of the key as a {@link BigInteger}.
   */
  public BigInteger getY()
  {
    return Y;
  }

  /**
   * Returns the encoded form of this public key according to the designated
   * format.
   * 
   * @param format the desired format identifier of the resulting encoding.
   * @return the byte sequence encoding this key according to the designated
   *         format.
   * @throws IllegalArgumentException if the format is not supported.
   */
  public byte[] getEncoded(int format)
  {
    byte[] result;
    switch (format)
      {
      case IKeyPairCodec.RAW_FORMAT:
        result = new SRPKeyPairRawCodec().encodePublicKey(this);
        break;
      default:
        throw new IllegalArgumentException("format");
      }
    return result;
  }

  /**
   * Returns <code>true</code> if the designated object is an instance of
   * <code>SRPPublicKey</code>and has the same SRP parameter values as this
   * one.
   * 
   * @param obj the other non-null SRP key to compare to.
   * @return <code>true</code> if the designated object is of the same type
   *         and value as this one.
   */
  public boolean equals(Object obj)
  {
    if (obj == null)
      return false;
    if (! (obj instanceof SRPPublicKey))
      return false;
    SRPPublicKey that = (SRPPublicKey) obj;
    return super.equals(that) && Y.equals(that.getY());
  }
}
