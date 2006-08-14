/* SRPAlgorithm.java -- 
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

import gnu.javax.crypto.sasl.srp.SRPRegistry;

import java.math.BigInteger;

/**
 * Utilities for use with SRP-6 based methods and protocols.
 * <p>
 * Reference:
 * <ol>
 *    <li><a href="http://srp.stanford.edu/design.html">SRP Protocol Design</a><br>
 *    Thomas J. Wu.</li>
 * </ol>
 */
public class SRPAlgorithm
{
  // lifted from draft-burdis-cat-srp-sasl-09
  public static final BigInteger N_2048 = new BigInteger(
      "AC6BDB41324A9A9BF166DE5E1389582FAF72B6651987EE07FC3192943DB56050"
    + "A37329CBB4A099ED8193E0757767A13DD52312AB4B03310DCD7F48A9DA04FD50"
    + "E8083969EDB767B0CF6095179A163AB3661A05FBD5FAAAE82918A9962F0B93B8"
    + "55F97993EC975EEAA80D740ADBF4FF747359D041D5C33EA71D281E446B14773B"
    + "CA97B43A23FB801676BD207A436C6481F1D2B9078717461A5B9D32E688F87748"
    + "544523B524B0D57D5EA77A2775D2ECFA032CFBDBF52FB3786160279004E57AE6"
    + "AF874E7303CE53299CCC041C7BC308D82A5698F3A8D0C38271AE35F8E9DBFBB6"
    + "94B5C803D89F7AE435DE236D525F54759B65E372FCD68EF20FA7111F9E4AFF73", 16);
  public static final BigInteger N_1536 = new BigInteger(
      "9DEF3CAFB939277AB1F12A8617A47BBBDBA51DF499AC4C80BEEEA9614B19CC4D"
    + "5F4F5F556E27CBDE51C6A94BE4607A291558903BA0D0F84380B655BB9A22E8DC"
    + "DF028A7CEC67F0D08134B1C8B97989149B609E0BE3BAB63D47548381DBC5B1FC"
    + "764E3F4B53DD9DA1158BFD3E2B9C8CF56EDF019539349627DB2FD53D24B7C486"
    + "65772E437D6C7F8CE442734AF7CCB7AE837C264AE3A9BEB87F8A2FE9B8B5292E"
    + "5A021FFF5E91479E8CE7A28C2442C6F315180F93499A234DCF76E3FED135F9BB", 16);
  public static final BigInteger N_1280 = new BigInteger(
      "D77946826E811914B39401D56A0A7843A8E7575D738C672A090AB1187D690DC4"
    + "3872FC06A7B6A43F3B95BEAEC7DF04B9D242EBDC481111283216CE816E004B78"
    + "6C5FCE856780D41837D95AD787A50BBE90BD3A9C98AC0F5FC0DE744B1CDE1891"
    + "690894BC1F65E00DE15B4B2AA6D87100C9ECC2527E45EB849DEB14BB2049B163"
    + "EA04187FD27C1BD9C7958CD40CE7067A9C024F9B7C5A0B4F5003686161F0605B", 16);
  public static final BigInteger N_1024 = new BigInteger(
      "EEAF0AB9ADB38DD69C33F80AFA8FC5E86072618775FF3C0B9EA2314C9C256576"
    + "D674DF7496EA81D3383B4813D692C6E0E0D5D8E250B98BE48E495C1D6089DAD1"
    + "5DC7D7B46154D6B6CE8EF4AD69B15D4982559B297BCF1885C529F566660E57EC"
    + "68EDBC3C05726CC02FD4CBF4976EAA9AFD5138FE8376435B9FC61D2FC0EB06E3", 16);
  public static final BigInteger N_768 = new BigInteger(
      "B344C7C4F8C495031BB4E04FF8F84EE95008163940B9558276744D91F7CC9F40"
    + "2653BE7147F00F576B93754BCDDF71B636F2099E6FFF90E79575F3D0DE694AFF"
    + "737D9BE9713CEF8D837ADA6380B1093E94B6A529A8C6C2BE33E0867C60C3262B", 16);
  public static final BigInteger N_640 = new BigInteger(
      "C94D67EB5B1A2346E8AB422FC6A0EDAEDA8C7F894C9EEEC42F9ED250FD7F0046"
    + "E5AF2CF73D6B2FA26BB08033DA4DE322E144E7A8E9B12A0E4637F6371F34A207"
    + "1C4B3836CBEEAB15034460FAA7ADF483", 16);
  public static final BigInteger N_512 = new BigInteger(
      "D4C7F8A2B32C11B8FBA9581EC4BA4F1B04215642EF7355E37C0FC0443EF756EA"
    + "2C6B8EEB755A1C723027663CAA265EF785B8FF6A9B35227A52D86633DBDFCA43", 16);
  public static final BigInteger N_384 = new BigInteger(
      "8025363296FB943FCE54BE717E0E2958A02A9672EF561953B2BAA3BAACC3ED57"
    + "54EB764C7AB7184578C57D5949CCB41B", 16);
  public static final BigInteger N_264 = new BigInteger(
      "115B8B692E0E045692CF280B436735C77A5A9E8A9E7ED56C965F87DB5B2A2ECE3", 16);
  private static final BigInteger ZERO = BigInteger.ZERO;
  private static final BigInteger ONE = BigInteger.ONE;
  private static final BigInteger TWO = BigInteger.valueOf(2L);

  /** Trivial constructor to enforce usage through class methods. */
  private SRPAlgorithm()
  {
    super();
  }

  public static void checkParams(final BigInteger N, final BigInteger g)
  {
    // 1. N should be at least 512-bit long
    final int blen = N.bitLength();
    if (blen < SRPRegistry.MINIMUM_MODULUS_BITLENGTH)
      throw new IllegalArgumentException("Bit length of N ("
                                         + blen
                                         + ") is too low. Should be at least "
                                         + SRPRegistry.MINIMUM_MODULUS_BITLENGTH);
    // 2. N should be a prime
    if (! N.isProbablePrime(80))
      throw new IllegalArgumentException("N should be prime but isn't");
    // 3. N should be of the form 2*q + 1, where q is prime
    final BigInteger q = N.subtract(ONE).divide(TWO);
    if (! q.isProbablePrime(80))
      throw new IllegalArgumentException("(N-1)/2 should be prime but isn't");
    // 4. g**q should be -1 mod N
    final BigInteger gq = g.modPow(q, N).add(ONE).mod(N);
    if (gq.compareTo(ZERO) != 0)
      throw new IllegalArgumentException("g**q should be -1 (mod N) but isn't");
  }
}
