/* Copyright (C) 1998, 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.util;

import java.io.Serializable;

/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date August 25, 1998.
 */
/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct
 */

/* This class is completely specified by the spec to ensure absolute
 * portability between all implementations of Java 
 */
public class Random implements Serializable
{
  /* Used by next() to hold the state of the pseudorandom number generator */
  protected long seed;

  /* Used by nextGaussian() to hold a precomputed value */
  /* to be delivered by that method the next time it is called */
  protected double nextNextGaussian;

  /* Used by nextGaussian() to keep track of whether it is has precomputed */
  /* and stashed away the next value to be delivered by that method */
  protected boolean haveNextNextGaussian = false;

  public Random()
  {
    this(System.currentTimeMillis());
  }

  public Random(long seed)
  {
    setSeed(seed);
  }

  protected synchronized int next(int bits)
  {
    seed = (seed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1);
    return (int)(seed >>> (48 - bits));
  }

  // JDK1.2
  public boolean nextBoolean()
  {
    return next(1) != 0;
  }

  /* The method nextBytes() is not fully specified in the published specs.
   * At first I implemented it simply via:
   *	for (int i = 0; i < buf.length; i++)
   *	  buf[i] = (byte)next(8);
   * but a simple test did not yield the same results as the std implementation.
   * There seemed to be a relationship where each i byte above was at pos 4*i+3
   * in the std.  For efficiency, by reducing calls to the expensive math
   * routines, the std probably was calling next(32) once rather than next(8)
   * 4 times.  Changing the algorithm to the one below based on that assumption
   * then yielded identical results to the std.
   */
  public void nextBytes(byte[] buf)
  {
    int randInt = 0;

    for (int i = 0;  i < buf.length;  i++)
      {
	int shift = (i % 4) * 8;
        if (shift == 0)
            randInt = next(32);
        buf[i] = (byte) (randInt >> shift);
      }
  }

  public double nextDouble()
  {
    return (((long)next(26) << 27) + next(27)) / (double)(1L << 53);
  }

  public float nextFloat()
  {
    return next(24) / ((float)(1 << 24));
  }

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
            v1 = 2 * nextDouble() - 1;   // between -1.0 and 1.0
            v2 = 2 * nextDouble() - 1;   // between -1.0 and 1.0
            s = v1 * v1 + v2 * v2;
          } while (s >= 1);
        double norm = Math.sqrt(-2 * Math.log(s)/s);
        nextNextGaussian = v2 * norm;
        haveNextNextGaussian = true;
        return v1 * norm;
      }
  }

  public int nextInt()
  {
    return next(32);
  }

  // JDK1.2
  public int nextInt(int n)
  {
    if (n <= 0)
      throw new IllegalArgumentException("n must be positive");

    int bits, val;
    do
      {
        bits = next(31);
        val = bits % n;
      } while (bits - val + (n-1) < 0);
    return val;
  }

  public long nextLong()
  {
    return ((long)next(32) << 32) + next(32);
  }

  public synchronized void setSeed(long seed)
  {
    this.seed = (seed ^ 0x5DEECE66DL) & ((1L << 48) - 1);
    haveNextNextGaussian = false;
  }
}
