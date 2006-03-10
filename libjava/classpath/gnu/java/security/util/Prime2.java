/* Prime2.java -- 
   Copyright (C) 2001, 2002, 2003, 2006 Free Software Foundation, Inc.

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


package gnu.java.security.util;

import java.io.PrintWriter;
import java.lang.ref.WeakReference;
import java.math.BigInteger;
import java.util.Map;
import java.util.WeakHashMap;

/**
 * <p>A collection of prime number related utilities used in this library.</p>
 */
public class Prime2
{

  // Debugging methods and variables
  // -------------------------------------------------------------------------

  private static final String NAME = "prime";

  private static final boolean DEBUG = false;

  private static final int debuglevel = 5;

  private static final PrintWriter err = new PrintWriter(System.out, true);

  private static void debug(String s)
  {
    err.println(">>> " + NAME + ": " + s);
  }

  // Constants and variables
  // -------------------------------------------------------------------------

  private static final int DEFAULT_CERTAINTY = 20; // XXX is this a good value?

  private static final BigInteger ZERO = BigInteger.ZERO;

  private static final BigInteger ONE = BigInteger.ONE;

  private static final BigInteger TWO = BigInteger.valueOf(2L);

  /**
   * The first SMALL_PRIME primes: Algorithm P, section 1.3.2, The Art of
   * Computer Programming, Donald E. Knuth.
   */
  private static final int SMALL_PRIME_COUNT = 1000;

  private static final BigInteger[] SMALL_PRIME = new BigInteger[SMALL_PRIME_COUNT];
  static
    {
      long time = -System.currentTimeMillis();
      SMALL_PRIME[0] = TWO;
      int N = 3;
      int J = 0;
      int prime;
      P2: while (true)
        {
          SMALL_PRIME[++J] = BigInteger.valueOf(N);
          if (J >= 999)
            {
              break P2;
            }
          P4: while (true)
            {
              N += 2;
              P6: for (int K = 1; true; K++)
                {
                  prime = SMALL_PRIME[K].intValue();
                  if ((N % prime) == 0)
                    {
                      continue P4;
                    }
                  else if ((N / prime) <= prime)
                    {
                      continue P2;
                    }
                }
            }
        }
      time += System.currentTimeMillis();
      if (DEBUG && debuglevel > 8)
        {
          StringBuffer sb;
          for (int i = 0; i < (SMALL_PRIME_COUNT / 10); i++)
            {
              sb = new StringBuffer();
              for (int j = 0; j < 10; j++)
                {
                  sb.append(String.valueOf(SMALL_PRIME[i * 10 + j])).append(" ");
                }
              debug(sb.toString());
            }
        }
      if (DEBUG && debuglevel > 4)
        {
          debug("Generating first " + String.valueOf(SMALL_PRIME_COUNT)
                + " primes took: " + String.valueOf(time) + " ms.");
        }
    }

  private static final Map knownPrimes = new WeakHashMap();

  // Constructor(s)
  // -------------------------------------------------------------------------

  /** Trivial constructor to enforce Singleton pattern. */
  private Prime2()
  {
    super();
  }

  // Class methods
  // -------------------------------------------------------------------------

  /**
   * <p>Trial division for the first 1000 small primes.</p>
   *
   * <p>Returns <code>true</code> if at least one small prime, among the first
   * 1000 ones, was found to divide the designated number. Retuens <code>false</code>
   * otherwise.</p>
   *
   * @param w the number to test.
   * @return <code>true</code> if at least one small prime was found to divide
   * the designated number.
   */
  public static boolean hasSmallPrimeDivisor(BigInteger w)
  {
    BigInteger prime;
    for (int i = 0; i < SMALL_PRIME_COUNT; i++)
      {
        prime = SMALL_PRIME[i];
        if (w.mod(prime).equals(ZERO))
          {
            if (DEBUG && debuglevel > 4)
              {
                debug(prime.toString(16) + " | " + w.toString(16) + "...");
              }
            return true;
          }
      }
    if (DEBUG && debuglevel > 4)
      {
        debug(w.toString(16) + " has no small prime divisors...");
      }
    return false;
  }

  /**
   * <p>Java port of Colin Plumb primality test (Euler Criterion)
   * implementation for a base of 2 --from bnlib-1.1 release, function
   * primeTest() in prime.c. this is his comments.</p>
   *
   * <p>"Now, check that bn is prime. If it passes to the base 2, it's prime
   * beyond all reasonable doubt, and everything else is just gravy, but it
   * gives people warm fuzzies to do it.</p>
   *
   * <p>This starts with verifying Euler's criterion for a base of 2. This is
   * the fastest pseudoprimality test that I know of, saving a modular squaring
   * over a Fermat test, as well as being stronger. 7/8 of the time, it's as
   * strong as a strong pseudoprimality test, too. (The exception being when
   * <code>bn == 1 mod 8</code> and <code>2</code> is a quartic residue, i.e.
   * <code>bn</code> is of the form <code>a^2 + (8*b)^2</code>.) The precise
   * series of tricks used here is not documented anywhere, so here's an
   * explanation. Euler's criterion states that if <code>p</code> is prime
   * then <code>a^((p-1)/2)</code> is congruent to <code>Jacobi(a,p)</code>,
   * modulo <code>p</code>. <code>Jacobi(a, p)</code> is a function which is
   * <code>+1</code> if a is a square modulo <code>p</code>, and <code>-1</code>
   * if it is not. For <code>a = 2</code>, this is particularly simple. It's
   * <code>+1</code> if <code>p == +/-1 (mod 8)</code>, and <code>-1</code> if
   * <code>m == +/-3 (mod 8)</code>. If <code>p == 3 (mod 4)</code>, then all
   * a strong test does is compute <code>2^((p-1)/2)</code>. and see if it's
   * <code>+1</code> or <code>-1</code>. (Euler's criterion says <i>which</i>
   * it should be.) If <code>p == 5 (mod 8)</code>, then <code>2^((p-1)/2)</code>
   * is <code>-1</code>, so the initial step in a strong test, looking at
   * <code>2^((p-1)/4)</code>, is wasted --you're not going to find a
   * <code>+/-1</code> before then if it <b>is</b> prime, and it shouldn't
   * have either of those values if it isn't. So don't bother.</p>
   *
   * <p>The remaining case is <code>p == 1 (mod 8)</code>. In this case, we
   * expect <code>2^((p-1)/2) == 1 (mod p)</code>, so we expect that the
   * square root of this, <code>2^((p-1)/4)</code>, will be <code>+/-1 (mod p)
   * </code>. Evaluating this saves us a modular squaring 1/4 of the time. If
   * it's <code>-1</code>, a strong pseudoprimality test would call <code>p</code>
   * prime as well. Only if the result is <code>+1</code>, indicating that
   * <code>2</code> is not only a quadratic residue, but a quartic one as well,
   * does a strong pseudoprimality test verify more things than this test does.
   * Good enough.</p>
   *
   * <p>We could back that down another step, looking at <code>2^((p-1)/8)</code>
   * if there was a cheap way to determine if <code>2</code> were expected to
   * be a quartic residue or not. Dirichlet proved that <code>2</code> is a
   * quadratic residue iff <code>p</code> is of the form <code>a^2 + (8*b^2)</code>.
   * All primes <code>== 1 (mod 4)</code> can be expressed as <code>a^2 +
   * (2*b)^2</code>, but I see no cheap way to evaluate this condition."</p>
   *
   * @param bn the number to test.
   * @return <code>true</code> iff the designated number passes Euler criterion
   * as implemented by Colin Plumb in his <i>bnlib</i> version 1.1.
   */
  public static boolean passEulerCriterion(final BigInteger bn)
  {
    BigInteger bn_minus_one = bn.subtract(ONE);
    BigInteger e = bn_minus_one;
    // l is the 3 least-significant bits of e
    int l = e.and(BigInteger.valueOf(7L)).intValue();
    int j = 1; // Where to start in prime array for strong prime tests
    BigInteger a;
    int k;

    if (l != 0)
      {
        e = e.shiftRight(1);
        a = TWO.modPow(e, bn);
        if (l == 6) // bn == 7 mod 8, expect +1
          {
            if (a.bitLength() != 1)
              {
                debugBI("Fails Euler criterion #1", bn);
                return false; // Not prime
              }
            k = 1;
          }
        else // bn == 3 or 5 mod 8, expect -1 == bn-1
          {
            a = a.add(ONE);
            if (a.compareTo(bn) != 0)
              {
                debugBI("Fails Euler criterion #2", bn);
                return false; // Not prime
              }
            k = 1;
            if ((l & 4) != 0) // bn == 5 mod 8, make odd for strong tests
              {
                e = e.shiftRight(1);
                k = 2;
              }
          }
      }
    else // bn == 1 mod 8, expect 2^((bn-1)/4) == +/-1 mod bn
      {
        e = e.shiftRight(2);
        a = TWO.modPow(e, bn);
        if (a.bitLength() == 1)
          j = 0; // Re-do strong prime test to base 2
        else
          {
            a = a.add(ONE);
            if (a.compareTo(bn) != 0)
              {
                debugBI("Fails Euler criterion #3", bn);
                return false; // Not prime
              }
          }
        // bnMakeOdd(n) = d * 2^s. Replaces n with d and returns s.
        k = e.getLowestSetBit();
        e = e.shiftRight(k);
        k += 2;
      }
    // It's prime!  Now go on to confirmation tests

    // Now, e = (bn-1)/2^k is odd.  k >= 1, and has a given value with
    // probability 2^-k, so its expected value is 2.  j = 1 in the usual case
    // when the previous test was as good as a strong prime test, but 1/8 of
    // the time, j = 0 because the strong prime test to the base 2 needs to
    // be re-done.
    for (int i = j; i < 7; i++) // try only the first 7 primes
      {
        a = SMALL_PRIME[i];
        a = a.modPow(e, bn);
        if (a.bitLength() == 1)
          continue; // Passed this test

        l = k;
        while (true)
          {
//            a = a.add(ONE);
//            if (a.compareTo(w) == 0) { // Was result bn-1?
            if (a.compareTo(bn_minus_one) == 0) // Was result bn-1?
              break; // Prime

            if (--l == 0) // Reached end, not -1? luck?
              {
                debugBI("Fails Euler criterion #4", bn);
                return false; // Failed, not prime
              }
            // This portion is executed, on average, once
//            a = a.subtract(ONE); // Put a back where it was
            a = a.modPow(TWO, bn);
            if (a.bitLength() == 1)
              {
                debugBI("Fails Euler criterion #5", bn);
                return false; // Failed, not prime
              }
          }
        // It worked (to the base primes[i])
      }
    debugBI("Passes Euler criterion", bn);
    return true;
  }

  public static boolean isProbablePrime(BigInteger w)
  {
    return isProbablePrime(w, DEFAULT_CERTAINTY);
  }

  /**
   * Wrapper around {@link BigInteger#isProbablePrime(int)} with few pre-checks.
   *
   * @param w the integer to test.
   * @param certainty the certainty with which to compute the test.
   */
  public static boolean isProbablePrime(BigInteger w, int certainty)
  {
    // Nonnumbers are not prime.
    if (w == null)
        return false;

    // eliminate trivial cases when w == 0 or 1
    if (w.equals(ZERO) || w.equals(ONE))
        return false;

    // Test if w is a known small prime.
    for (int i = 0; i < SMALL_PRIME_COUNT; i++)
        if (w.equals(SMALL_PRIME[i]))
          {
            if (DEBUG && debuglevel > 4)
                debug(w.toString(16) + " is a small prime");
            return true;
          }

    // Check if it's already a known prime
    WeakReference obj = (WeakReference) knownPrimes.get(w);
    if (obj != null && w.equals(obj.get()))
      {
        if (DEBUG && debuglevel > 4)
            debug("found in known primes");
        return true;
      }

    // trial division with first 1000 primes
    if (hasSmallPrimeDivisor(w))
      {
        if (DEBUG && debuglevel > 4)
            debug(w.toString(16) + " has a small prime divisor. Rejected...");
        return false;
      }

//     Euler's criterion.
//           if (passEulerCriterion(w)) {
//              if (DEBUG && debuglevel > 4) {
//                 debug(w.toString(16)+" passes Euler's criterion...");
//              }
//           } else {
//              if (DEBUG && debuglevel > 4) {
//                 debug(w.toString(16)+" fails Euler's criterion. Rejected...");
//              }
//              return false;
//           }
//
//    if (DEBUG && debuglevel > 4)
//      {
//        debug(w.toString(16) + " is probable prime. Accepted...");
//      }

    boolean result = w.isProbablePrime(certainty);
    if (result && certainty > 0) // store it in the known primes weak hash-map
      knownPrimes.put(w, new WeakReference(w));

    return result;
  }

  // helper methods -----------------------------------------------------------

  private static final void debugBI(String msg, BigInteger bn)
  {
    if (DEBUG && debuglevel > 4)
      debug("*** " + msg + ": 0x" + bn.toString(16));
  }
}
