/* java.util.Random
   Copyright (C) 1998, 1999, 2000, 2001 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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


package java.util;

/**
 * This class generates pseudorandom numbers.  It uses the same
 * algorithm as the original JDK-class, so that your programs behave
 * exactly the same way, if started with the same seed.
 *
 * The algorithm is described in <em>The Art of Computer Programming,
 * Volume 2</em> by Donald Knuth in Section 3.2.1.
 *
 * If two instances of this class are created with the same seed and
 * the same calls to these classes are made, they behave exactly the
 * same way.  This should be even true for foreign implementations
 * (like this), so every port must use the same algorithm as described
 * here.
 *
 * If you want to implement your own pseudorandom algorithm, you
 * should extend this class and overload the <code>next()</code> and
 * <code>setSeed(long)</code> method.  In that case the above
 * paragraph doesn't apply to you.
 *
 * This class shouldn't be used for security sensitive purposes (like 
 * generating passwords or encryption keys.  See <code>SecureRandom</code>
 * in package <code>java.security</code> for this purpose.
 *
 * For simple random doubles between 0.0 and 1.0, you may consider using
 * Math.random instead.
 *
 * @see java.security.SecureRandom
 * @see Math#random()
 * @author Jochen Hoenicke */
public class Random implements java.io.Serializable
{
  /**
   * True if the next nextGaussian is available.  This is used by
   * nextGaussian, which generates two gaussian numbers by one call,
   * and returns the second on the second call.  
   * @see #nextGaussian.  */
  private boolean haveNextNextGaussian;
  /**
   * The next nextGaussian if available.  This is used by nextGaussian,
   * which generates two gaussian numbers by one call, and returns the
   * second on the second call.
   * @see #nextGaussian.
   */
  private double nextNextGaussian;
  /**
   * The seed.  This is the number set by setSeed and which is used
   * in next.
   * @see #next
   */
  private long seed;

  private static final long serialVersionUID = 3905348978240129619L;

  /**
   * Creates a new pseudorandom number generator.  The seed is initialized
   * to the current time as follows.
   * <pre>
   * setSeed(System.currentTimeMillis());
   * </pre>
   * @see System#currentTimeMillis()
   */
  public Random()
  {
    setSeed(System.currentTimeMillis());
  }

  /**
   * Creates a new pseudorandom number generator, starting with the
   * specified seed. This does:
   * <pre>
   * setSeed(seed);
   * </pre>
   * @param seed the initial seed.
   */
  public Random(long seed)
  {
    setSeed(seed);
  }

  /**
   * Sets the seed for this pseudorandom number generator.  As described
   * above, two instances of the same random class, starting with the
   * same seed, should produce the same results, if the same methods
   * are called.  The implementation for java.util.Random is:
   * <pre>
   * public synchronized void setSeed(long seed) {
   *     this.seed = (seed ^ 0x5DEECE66DL) & ((1L << 48) - 1);
   *     haveNextNextGaussian = false;
   * }
   * </pre>
   */
  public synchronized void setSeed(long seed)
  {
    this.seed = (seed ^ 0x5DEECE66DL) & ((1L << 48) - 1);
    haveNextNextGaussian = false;
  }

  /**
   * Generates the next pseudorandom number.  This returns
   * an int value whose <code>bits</code> low order bits are
   * independent chosen random bits (0 and 1 are equally likely).
   * The implementation for java.util.Random is:
   * <pre>
   * protected synchronized int next(int bits) {
   *     seed = (seed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1);
   *     return (int) (seed >>> (48 - bits));
   * }
   * </pre>
   * @param bits the number of random bits to generate.  Must be in range
   * 1..32.
   * @return the next pseudorandom value.
   * @since JDK1.1
   */
  protected synchronized int next(int bits)
    /*{ require { 1 <= bits && bits <=32 :: 
       "bits "+bits+" not in range [1..32]" } } */
  {
    seed = (seed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1);
    return (int) (seed >>> (48 - bits));
  }

  /**
   * Fills an array of bytes with random numbers.  All possible values
   * are (approximately) equally likely.
   * The JDK documentation gives no implementation, but it seems to be:
   * <pre>
   * public void nextBytes(byte[] bytes) {
   *     for (int i=0; i< bytes.length; i+=4) {
   *         int random = next(32);
   *         for (int j=0; i+j< bytes.length && j<4; j++)
   *             bytes[i+j] = (byte) (random & 0xff)
   *             random >>= 8;
   *         }
   *     }
   * }
   * </pre>
   * @param bytes The byte array that should be filled.
   * @since JDK1.1
   */
  public void nextBytes(byte[] bytes)
    /*{ require { bytes != null :: "bytes is null"; } } */
  {
    int random;
    /* Do a little bit unrolling of the above algorithm. */
    int max = bytes.length & ~0x3;
    for (int i = 0; i < max; i += 4)
      {
	random = next(32);
	bytes[i] = (byte) random;
	bytes[i + 1] = (byte) (random >> 8);
	bytes[i + 2] = (byte) (random >> 16);
	bytes[i + 3] = (byte) (random >> 24);
      }
    if (max < bytes.length)
      {
	random = next(32);
	for (int j = max; j < bytes.length; j++)
	  {
	    bytes[j] = (byte) random;
	    random >>= 8;
	  }
      }
  }

  /**
   * Generates the next pseudorandom number.  This returns
   * an int value whose 32 bits are independent chosen random bits
   * (0 and 1 are equally likely).  The implementation for
   * java.util.Random is:
   * <pre>
   * public int nextInt() {
   *     return next(32);
   * }
   * </pre>
   *
   * @return the next pseudorandom value.  */
  public int nextInt()
  {
    return next(32);
  }

  /**
   * Generates the next pseudorandom number.  This returns
   * a value between 0(inclusive) and <code>n</code>(exclusive), and
   * each value has the same likelihodd (1/<code>n</code>).
   * (0 and 1 are equally likely).  The implementation for
   * java.util.Random is:
   * <pre>
   * public int nextInt(int n) {
   *     if (n<=0)
   *         throw new IllegalArgumentException("n must be positive");
   *     if ((n & -n) == n)  // i.e., n is a power of 2
   *         return (int)((n * (long)next(31)) >> 31);
   *     int bits, val;
   *     do {
   *         bits = next(32);
   *         val = bits % n;
   *     } while(bits - val + (n-1) < 0);
   *     return val;
   * }
   * </pre>
   * This algorithm would return every value with exactly the same 
   * probability, if the next()-method would be a perfect random number
   * generator.
   * 
   * The loop at the bottom only accepts a value, if the random
   * number was between 0 and the highest number less then 1<<31,
   * which is divisible by n.  The probability for this is high for small
   * n, and the worst case is 1/2 (for n=(1<<30)+1).
   *
   * The special treatment for n = power of 2, selects the high bits of 
   * the random number (the loop at the bottom would select the low order
   * bits).  This is done, because the low order bits of linear congruential
   * number generators (like the one used in this class) are known to be 
   * ``less random'' than the high order bits.
   *
   * @param n the upper bound.
   * @exception IllegalArgumentException if the given upper bound is negative
   * @return the next pseudorandom value.  
   */
  public int nextInt(int n)
    /*{ require { n > 0 :: "n must be positive"; } } */
  {
    if (n <= 0)
      throw new IllegalArgumentException("n must be positive");
    if ((n & -n) == n)		// i.e., n is a power of 2
      return (int) ((n * (long) next(31)) >> 31);
    int bits, val;
    do
      {
	bits = next(32);
	val = bits % n;
      }
    while (bits - val + (n - 1) < 0);
    return val;
  }

  /**
   * Generates the next pseudorandom long number.  All bits of this
   * long are independently chosen and 0 and 1 have equal likelihood.
   * The implementation for java.util.Random is:
   * <pre>
   * public long nextLong() {
   *     return ((long)next(32) << 32) + next(32);
   * }
   * </pre>
   * @return the next pseudorandom value.  
   */
  public long nextLong()
  {
    return ((long) next(32) << 32) + next(32);
  }

  /**
   * Generates the next pseudorandom boolean.  True and false have
   * the same probability.  The implementation is:
   * <pre>
   * public boolean nextBoolean() {
   *     return next(1) != 0;
   * }
   * </pre>
   * @return the next pseudorandom boolean.
   */
  public boolean nextBoolean()
  {
    return next(1) != 0;
  }

  /**
   * Generates the next pseudorandom float uniformly distributed
   * between 0.0f (inclusive) and 1.0 (exclusive).  The
   * implementation is as follows.
   * <pre>
   * public float nextFloat() {
   *     return next(24) / ((float)(1 << 24));
   * }
   * </pre>
   * @return the next pseudorandom float.  */
  public float nextFloat()
  {
    return next(24) / ((float) (1 << 24));
  }

  /**
   * Generates the next pseudorandom double uniformly distributed
   * between 0.0f (inclusive) and 1.0 (exclusive).  The
   * implementation is as follows.
   * <pre>
   * public double nextDouble() {
   *     return (((long)next(26) << 27) + next(27)) / (double)(1 << 53);
   * }
   * </pre>
   * @return the next pseudorandom double.  */
  public double nextDouble()
  {
    return (((long) next(26) << 27) + next(27)) / (double) (1L << 53);
  }

  /**
   * Generates the next pseudorandom, Gaussian (normally) distributed 
   * double value, with mean 0.0 and standard deviation 1.0.
   * The algorithm is as follows.
   * <pre>
   * public synchronized double nextGaussian() {
   *     if (haveNextNextGaussian) {
   *         haveNextNextGaussian = false;
   *         return nextNextGaussian;
   *     } else {
   *         double v1, v2, s;
   *         do {
   *             v1 = 2 * nextDouble() - 1; // between -1.0 and 1.0
   *             v2 = 2 * nextDouble() - 1; // between -1.0 and 1.0
   *             s = v1 * v1 + v2 * v2;
   *         } while (s >= 1);
   *         double norm = Math.sqrt(-2 * Math.log(s)/s);
   *         nextNextGaussian = v2 * norm;
   *         haveNextNextGaussian = true;
   *         return v1 * norm;
   *     }
   * }
   * </pre>
   * This is described in section 3.4.1 of <em>The Art of Computer
   * Programming, Volume 2</em> by Donald Knuth.
   *
   * @return the next pseudorandom Gaussian distributed double.  
   */
  public synchronized double nextGaussian()
  {
    if (haveNextNextGaussian)
      {
	haveNextNextGaussian = false;
	return nextNextGaussian;
      }
    else
      {
	double v1, v2, s;
	do
	  {
	    v1 = 2 * nextDouble() - 1;	// between -1.0 and 1.0
	    v2 = 2 * nextDouble() - 1;	// between -1.0 and 1.0
	    s = v1 * v1 + v2 * v2;
	  }
	while (s >= 1);
	double norm = Math.sqrt(-2 * Math.log(s) / s);
	nextNextGaussian = v2 * norm;
	haveNextNextGaussian = true;
	return v1 * norm;
      }
  }
}
