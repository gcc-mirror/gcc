/* RFC2631.java -- 
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


package gnu.javax.crypto.key.dh;

import gnu.java.security.hash.Sha160;
import gnu.java.security.util.PRNG;

import java.math.BigInteger;
import java.security.SecureRandom;

/**
 * An implementation of the Diffie-Hellman parameter generation as defined in
 * RFC-2631.
 * <p>
 * Reference:
 * <ol>
 * <li><a href="http://www.ietf.org/rfc/rfc2631.txt">Diffie-Hellman Key
 * Agreement Method</a><br>
 * Eric Rescorla.</li>
 * </ol>
 */
public class RFC2631
{
  public static final int DH_PARAMS_SEED = 0;
  public static final int DH_PARAMS_COUNTER = 1;
  public static final int DH_PARAMS_Q = 2;
  public static final int DH_PARAMS_P = 3;
  public static final int DH_PARAMS_J = 4;
  public static final int DH_PARAMS_G = 5;
  private static final BigInteger TWO = BigInteger.valueOf(2L);
  /** The SHA instance to use. */
  private Sha160 sha = new Sha160();
  /** Length of private modulus and of q. */
  private int m;
  /** Length of public modulus p. */
  private int L;
  /** The optional {@link SecureRandom} instance to use. */
  private SecureRandom rnd = null;
  /** Our default source of randomness. */
  private PRNG prng = null;

  public RFC2631(int m, int L, SecureRandom rnd)
  {
    super();

    this.m = m;
    this.L = L;
    this.rnd = rnd;
  }

  public BigInteger[] generateParameters()
  {
    int i, j, counter;
    byte[] u1, u2, v;
    byte[] seedBytes = new byte[m / 8];
    BigInteger SEED, U, q, R, V, W, X, p, g;
    // start by genrating p and q, where q is of length m and p is of length L
    // 1. Set m' = m/160 where / represents integer division with rounding
    //    upwards. I.e. 200/160 = 2.
    int m_ = (m + 159) / 160;
    // 2. Set L'=  L/160
    int L_ = (L + 159) / 160;
    // 3. Set N'=  L/1024
    int N_ = (L + 1023) / 1024;
    algorithm: while (true)
      {
        step4: while (true)
          {
            // 4. Select an arbitrary bit string SEED such that length of
            //    SEED >= m
            nextRandomBytes(seedBytes);
            SEED = new BigInteger(1, seedBytes).setBit(m - 1).setBit(0);
            // 5. Set U = 0
            U = BigInteger.ZERO;
            // 6. For i = 0 to m' - 1
            //    U = U + (SHA1[SEED + i] XOR SHA1[(SEED + m' + i)) * 2^(160 * i)
            //    Note that for m=160, this reduces to the algorithm of FIPS-186
            //    U = SHA1[SEED] XOR SHA1[(SEED+1) mod 2^160 ].
            for (i = 0; i < m_; i++)
              {
                u1 = SEED.add(BigInteger.valueOf(i)).toByteArray();
                u2 = SEED.add(BigInteger.valueOf(m_ + i)).toByteArray();
                sha.update(u1, 0, u1.length);
                u1 = sha.digest();
                sha.update(u2, 0, u2.length);
                u2 = sha.digest();
                for (j = 0; j < u1.length; j++)
                  u1[j] ^= u2[j];
                U = U.add(new BigInteger(1, u1).multiply(TWO.pow(160 * i)));
              }
            // 5. Form q from U by computing U mod (2^m) and setting the most
            //    significant bit (the 2^(m-1) bit) and the least significant
            //    bit to 1. In terms of boolean operations, q = U OR 2^(m-1) OR
            //    1. Note that 2^(m-1) < q < 2^m
            q = U.setBit(m - 1).setBit(0);
            // 6. Use a robust primality algorithm to test whether q is prime.
            // 7. If q is not prime then go to 4.
            if (q.isProbablePrime(80))
              break step4;
          }
        // 8. Let counter = 0
        counter = 0;
        step9: while (true)
          {
            // 9. Set R = seed + 2*m' + (L' * counter)
            R = SEED
                .add(BigInteger.valueOf(2 * m_))
                .add(BigInteger.valueOf(L_ * counter));
            // 10. Set V = 0
            V = BigInteger.ZERO;
            // 12. For i = 0 to L'-1 do: V = V + SHA1(R + i) * 2^(160 * i)
            for (i = 0; i < L_; i++)
              {
                v = R.toByteArray();
                sha.update(v, 0, v.length);
                v = sha.digest();
                V = V.add(new BigInteger(1, v).multiply(TWO.pow(160 * i)));
              }
            // 13. Set W = V mod 2^L
            W = V.mod(TWO.pow(L));
            // 14. Set X = W OR 2^(L-1)
            //     Note that 0 <= W < 2^(L-1) and hence X >= 2^(L-1)
            X = W.setBit(L - 1);
            // 15. Set p = X - (X mod (2*q)) + 1
            p = X.add(BigInteger.ONE).subtract(X.mod(TWO.multiply(q)));
            // 16. If p > 2^(L-1) use a robust primality test to test whether p
            //     is prime. Else go to 18.
            // 17. If p is prime output p, q, seed, counter and stop.
            if (p.isProbablePrime(80))
              {
                break algorithm;
              }
            // 18. Set counter = counter + 1
            counter++;
            // 19. If counter < (4096 * N) then go to 8.
            // 20. Output "failure"
            if (counter >= 4096 * N_)
              continue algorithm;
          }
      }
    // compute g. from FIPS-186, Appendix 4:
    // 1. Generate p and q as specified in Appendix 2.
    // 2. Let e = (p - 1) / q
    BigInteger e = p.subtract(BigInteger.ONE).divide(q);
    BigInteger h = TWO;
    BigInteger p_minus_1 = p.subtract(BigInteger.ONE);
    g = TWO;
    // 3. Set h = any integer, where 1 < h < p - 1 and h differs from any
    //    value previously tried
    for (; h.compareTo(p_minus_1) < 0; h = h.add(BigInteger.ONE))
      {
        // 4. Set g = h**e mod p
        g = h.modPow(e, p);
        // 5. If g = 1, go to step 3
        if (! g.equals(BigInteger.ONE))
          break;
      }
    return new BigInteger[] { SEED, BigInteger.valueOf(counter), q, p, e, g };
  }

  /**
   * Fills the designated byte array with random data.
   *
   * @param buffer the byte array to fill with random data.
   */
  private void nextRandomBytes(byte[] buffer)
  {
    if (rnd != null)
      rnd.nextBytes(buffer);
    else
      getDefaultPRNG().nextBytes(buffer);
  }

  private PRNG getDefaultPRNG()
  {
    if (prng == null)
      prng = PRNG.getInstance();

    return prng;
  }
}
