// BitSet - A vector of bits.

/* Copyright (C) 1998, 1999, 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.util;
import java.io.Serializable;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date October 23, 1998.
 */

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * hashCode algorithm taken from JDK 1.2 docs.
 */

public final class BitSet implements Cloneable, Serializable
{
  public void and (BitSet bs)
    {
      int max = Math.min(bits.length, bs.bits.length);
      int i;
      for (i = 0; i < max; ++i)
	bits[i] &= bs.bits[i];
      for ( ; i < bits.length; ++i)
	bits[i] = 0;
    }

  public BitSet ()
    {
      this (64);
    }

  public BitSet (int nbits)
    {
      if (nbits < 0)
	throw new NegativeArraySizeException ();
      int length = nbits / 64;
      if (nbits % 64 != 0)
	++length;
      bits = new long[length];
    }

  public void clear (int pos)
    {
      if (pos < 0)
	throw new IndexOutOfBoundsException ();
      int bit = pos % 64;
      int offset = pos / 64;
      ensure (offset);
      bits[offset] &= ~ (1L << bit);
    }

  public Object clone ()
    {
      BitSet bs = new BitSet (bits.length * 64);
      System.arraycopy(bits, 0, bs.bits, 0, bits.length);
      return bs;
    }

  public boolean equals (Object obj)
    {
      if (! (obj instanceof BitSet))
	return false;
      BitSet bs = (BitSet) obj;
      int max = Math.min(bits.length, bs.bits.length);
      int i;
      for (i = 0; i < max; ++i)
	if (bits[i] != bs.bits[i])
	  return false;
      // If one is larger, check to make sure all extra bits are 0.
      for (int j = i; j < bits.length; ++j)
	if (bits[j] != 0)
	  return false;
      for (int j = i; j < bs.bits.length; ++j)
	if (bs.bits[j] != 0)
	  return false;
      return true;
    }

  public boolean get (int pos)
    {
      if (pos < 0)
	throw new IndexOutOfBoundsException ();

      int bit = pos % 64;
      int offset = pos / 64;

      if (offset >= bits.length)
	return false;

      return (bits[offset] & (1L << bit)) == 0 ? false : true;
    }

  public int hashCode ()
    {
      long h = 1234;
      for (int i = bits.length - 1; i >= 0; --i)
	h ^= bits[i] * (i + 1);
      return (int) ((h >> 32) ^ h);
    }

  public void or (BitSet bs)
    {
      ensure (bs.bits.length - 1);
      int i;
      for (i = 0; i < bs.bits.length; ++i)
	bits[i] |= bs.bits[i];
    }

  public void set (int pos)
    {
      if (pos < 0)
	throw new IndexOutOfBoundsException ();
      int bit = pos % 64;
      int offset = pos / 64;
      ensure (offset);
      bits[offset] |= 1L << bit;
    }

  public int size ()
    {
      return bits.length * 64;
    }

  public String toString ()
    {
      StringBuffer result = new StringBuffer ("{");
      boolean first = true;
      for (int i = 0; i < bits.length; ++i)
	{
	  int bit = 1;
	  long word = bits[i];
	  for (int j = 0; j < 64; ++j)
	    {
	      if ((word & bit) != 0)
		{
		  if (! first)
		    result.append(", ");
		  result.append(64 * i + j);
		  first = false;
		}
	      bit <<= 1;
	    }
	}

      return result.append("}").toString();
    }

  public void xor (BitSet bs)
    {
      ensure (bs.bits.length - 1);
      int i;
      for (i = 0; i < bs.bits.length; ++i)
	bits[i] ^= bs.bits[i];
    }

  // Make sure the vector is big enough.
  private final void ensure (int lastElt)
    {
      if (lastElt + 1 > bits.length)
	{
	  long[] nd = new long[lastElt + 1];
	  System.arraycopy(bits, 0, nd, 0, bits.length);
	  bits = nd;
	}
    }

  // The actual bits.
  private long[] bits;
}
