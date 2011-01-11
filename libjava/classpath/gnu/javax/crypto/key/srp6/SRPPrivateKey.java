/* SRPPrivateKey.java --
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
import java.security.PrivateKey;

/**
 * A representation of an SRP ephemeral private key.
 * <p>
 * Reference:
 * <ol>
 * <li><a href="http://srp.stanford.edu/design.html">SRP Protocol Design</a><br>
 * Thomas J. Wu.</li>
 * </ol>
 */
public class SRPPrivateKey
    extends SRPKey
    implements PrivateKey
{
  /**
   * The private exponent for either the server or the client engaged in the SRP
   * protocol exchange.
   */
  private final BigInteger X;
  /**
   * The user's verifier (v) --for the server-- also computed at the client side
   * as g.modPow(x, N), where x is the hashed output of the user name and
   * password .
   */
  private final BigInteger v;

  /**
   * Public constructor for use from outside this package.
   *
   * @param N the public shared modulus.
   * @param g the generator.
   * @param x the private exponent of the ephemeral key.
   */
  public SRPPrivateKey(BigInteger N, BigInteger g, BigInteger x)
  {
    this(N, g, x, null);
  }

  /**
   * Public constructor for use from outside this package.
   *
   * @param N the public shared modulus.
   * @param g the generator.
   * @param x the private exponent of the ephemeral key.
   * @param v the user's verifier value (for the server side only).
   */
  public SRPPrivateKey(BigInteger N, BigInteger g, BigInteger x, BigInteger v)
  {
    super(N, g);

    SRPAlgorithm.checkParams(N, g);
    this.X = x;
    this.v = v;
  }

  /**
   * Default constructor. Assumes N and g are already validated.
   *
   * @param params an array of either 3 or 4 values representing N, g, and
   *          either v and X for the server, or just X for the client. Those
   *          values represent the following:
   *          <ol>
   *          <li>v (server side): the user's verifier.</li>
   *          <li>X (both sides): the server's or client's ephemeral private
   *          exponent.</li>
   *          </ol>
   */
  SRPPrivateKey(BigInteger[] params)
  {
    super(params[0], params[1]);

    if (params.length == 3)
      {
        X = params[2];
        v = null;
      }
    else if (params.length == 4)
      {
        X = params[2];
        v = params[3];
      }
    else
      throw new IllegalArgumentException("invalid number of SRP parameters");
  }

  /**
   * A class method that takes the output of the <code>encodePrivateKey()</code>
   * method of an SRP keypair codec object (an instance implementing
   * {@link IKeyPairCodec} for DSS keys, and re-constructs an instance of this
   * object.
   *
   * @param k the contents of a previously encoded instance of this object.
   * @throws ArrayIndexOutOfBoundsException if there is not enough bytes, in
   *           <code>k</code>, to represent a valid encoding of an instance
   *           of this object.
   * @throws IllegalArgumentException if the byte sequence does not represent a
   *           valid encoding of an instance of this object.
   */
  public static SRPPrivateKey valueOf(byte[] k)
  {
    // check magic...
    // we should parse here enough bytes to know which codec to use, and
    // direct the byte array to the appropriate codec. since we only have one
    // codec, we could have immediately tried it; nevertheless since testing
    // one byte is cheaper than instatiating a codec that will fail we test
    // the first byte before we carry on.
    if (k[0] == Registry.MAGIC_RAW_SRP_PRIVATE_KEY[0])
      {
        // it's likely to be in raw format. get a raw codec and hand it over
        IKeyPairCodec codec = new SRPKeyPairRawCodec();
        return (SRPPrivateKey) codec.decodePrivateKey(k);
      }
    throw new IllegalArgumentException("magic");
  }

  /**
   * Returns the private exponent of the key as a {@link BigInteger}.
   *
   * @return the private exponent of the key as a {@link BigInteger}.
   */
  public BigInteger getX()
  {
    return X;
  }

  /**
   * Returns the user's verifier as a {@link BigInteger}.
   *
   * @return the user's verifier as a {@link BigInteger} if this is an SRP
   *         private key of a Host, or <code>null</code> if this is a private
   *         SRP key for a User.
   */
  public BigInteger getV()
  {
    return v;
  }

  /**
   * Returns the encoded form of this private key according to the designated
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
        result = new SRPKeyPairRawCodec().encodePrivateKey(this);
        break;
      default:
        throw new IllegalArgumentException("format");
      }
    return result;
  }

  /**
   * Returns <code>true</code> if the designated object is an instance of
   * <code>SRPPrivateKey</code> and has the same SRP parameter values as this
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
    if (! (obj instanceof SRPPrivateKey))
      return false;
    SRPPrivateKey that = (SRPPrivateKey) obj;
    boolean result = super.equals(that) && X.equals(that.getX());
    if (v != null)
      result = result && v.equals(that.getV());
    return result;
  }
}
