// BitSet - A vector of bits.

/* Copyright (C) 1998, 1999, 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.util;
import java.io.Serializable;

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * hashCode algorithm taken from JDK 1.2 docs.
 */

/**
 * This class can be thought of in two ways.  You can see it as a
 * vector of bits or as a set of non-negative integers.  The name
 * <code>BitSet</code> is a bit misleading.
 *
 * It is implemented by a bit vector, but its equally possible to see
 * it as set of non-negative integer; each integer in the set is
 * represented by a set bit at the corresponding index.  The size of
 * this structure is determined by the highest integer in the set.
 *
 * You can union, intersect and build (symmetric) remainders, by
 * invoking the logical operations and, or, andNot, resp. xor.
 *
 * This implementation is NOT synchronized against concurrent access from
 * multiple threads. Specifically, if one thread is reading from a bitset
 * while another thread is simultaneously modifying it, the results are
 * undefined.
 *
 * @specnote There is some confusion as to whether or not this class should
 *           be synchronized. JDK 1.1 javadocs explicitly state that the 
 *           class is NOT synchronized, however the code listed in the JDK 1.3
 *           javadoc for the hashCode() method implies that it is. It is not
 *           stated elsewhere in the 1.2 javadoc that the class is 
 *           synchronized, unlike Hashtable and Vector. From an efficiency 
 *           perspective, it is very undesirable to synchronize this class
 *           because multiple locks and explicit lock ordering are required
 *           to safely synchronize some methods. For this reason we're going
 *           with the unsynchronized implementation unless the specs are 
 *           changed to explicitly say otherwise.
 *
 * @author Jochen Hoenicke
 * @author Tom Tromey <tromey@cygnus.com>
 * @date October 23, 1998.
 * @status API complete to JDK 1.3.
 */
public final class BitSet implements Cloneable, Serializable
{
  /**
   * Create a new empty bit set.
   */
  public BitSet()
  {
    this(64);
  }

  /**
   * Create a new empty bit set, with a given size.  This
   * constructor reserves enough space to represent the integers
   * from <code>0</code> to <code>nbits-1</code>.  
   * @param nbits the initial size of the bit set.
   * @throws NegativeArraySizeException if the specified initial
   * size is negative.  
   * @require nbits >= 0
   */
  public BitSet(int nbits)
  {
    if (nbits < 0)
      throw new NegativeArraySizeException();
    int length = nbits / 64;
    if (nbits % 64 != 0)
      ++length;
    bits = new long[length];
  }

  /**
   * Performs the logical AND operation on this bit set and the
   * given <code>set</code>.  This means it builds the intersection
   * of the two sets.  The result is stored into this bit set.
   * @param set the second bit set.
   * @require set != null
   */
  public void and(BitSet bs)
  {
    int max = Math.min(bits.length, bs.bits.length);
    int i;
    for (i = 0; i < max; ++i)
      bits[i] &= bs.bits[i];
    for (; i < bits.length; ++i)
      bits[i] = 0;
  }

  /**
   * Performs the logical AND operation on this bit set and the
   * complement of the given <code>set</code>.  This means it
   * selects every element in the first set, that isn't in the
   * second set.  The result is stored into this bit set.  
   * @param set the second bit set.  
   * @require set != null
   * @since JDK1.2
   */
  public void andNot(BitSet bs)
  {
    int max = Math.min(bits.length, bs.bits.length);
    int i;
    for (i = 0; i < max; ++i)
      bits[i] &= ~bs.bits[i];
  }

  /**
   * Removes the integer <code>bitIndex</code> from this set. That is
   * the corresponding bit is cleared.  If the index is not in the set,
   * this method does nothing.
   * @param bitIndex a non-negative integer.
   * @exception ArrayIndexOutOfBoundsException if the specified bit index
   * is negative.
   * @require bitIndex >= 0
   */
  public void clear(int pos)
  {
    if (pos < 0)
      throw new IndexOutOfBoundsException();
    int bit = pos % 64;
    int offset = pos / 64;
    ensure(offset);
    bits[offset] &= ~(1L << bit);
  }

  /**
   * Create a clone of this bit set, that is an instance of the same
   * class and contains the same elements.  But it doesn't change when
   * this bit set changes.
   * @return the clone of this object.
   */
  public Object clone()
  {
    BitSet bs = new BitSet(bits.length * 64);
    System.arraycopy(bits, 0, bs.bits, 0, bits.length);
    return bs;
  }

  /**
   * Returns true if the <code>obj</code> is a bit set that contains
   * exactly the same elements as this bit set, otherwise false.
   * @return true if obj equals this bit set.
   */
  public boolean equals(Object obj)
  {
    if (!(obj instanceof BitSet))
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

  /**
   * Returns true if the integer <code>bitIndex</code> is in this bit
   * set, otherwise false.
   * @param bitIndex a non-negative integer
   * @return the value of the bit at the specified index.
   * @exception ArrayIndexOutOfBoundsException if the specified bit index
   * is negative.
   * @require bitIndex >= 0
   */
  public boolean get(int pos)
  {
    if (pos < 0)
      throw new IndexOutOfBoundsException();

    int bit = pos % 64;
    int offset = pos / 64;

    if (offset >= bits.length)
      return false;

    return (bits[offset] & (1L << bit)) == 0 ? false : true;
  }

  /**
   * Returns a hash code value for this bit set.  The hash code of 
   * two bit sets containing the same integers is identical.  The algorithm
   * used to compute it is as follows:
   *
   * Suppose the bits in the BitSet were to be stored in an array of
   * long integers called <code>bits</code>, in such a manner that
   * bit <code>k</code> is set in the BitSet (for non-negative values
   * of <code>k</code>) if and only if
   *
   * <pre>
   * ((k/64) < bits.length) && ((bits[k/64] & (1L << (bit % 64))) != 0)
   * </pre>
   *
   * Then the following definition of the hashCode method
   * would be a correct implementation of the actual algorithm:
   *
   * <pre>
   * public int hashCode() {
   *     long h = 1234;
   *     for (int i = bits.length-1; i>=0; i--) {
   *         h ^= bits[i] * (i + 1);
   *     }
   *     return (int)((h >> 32) ^ h);
   * }
   * </pre>
   *
   * Note that the hash code values changes, if the set is changed.
   * @return the hash code value for this bit set.
   */
  public int hashCode()
  {
    long h = 1234;
    for (int i = bits.length - 1; i >= 0; --i)
      h ^= bits[i] * (i + 1);
    return (int) ((h >> 32) ^ h);
  }

  /**
   * Returns the logical number of bits actually used by this bit
   * set.  It returns the index of the highest set bit plus one.
   * Note that this method doesn't return the number of set bits.
   * @return the index of the highest set bit plus one.  
   */
  public int length()
  {
    // Set i to highest index that contains a non-zero value.
    int i;
    for (i = bits.length - 1; i >= 0 && bits[i] == 0; --i)
      ;

    // if i < 0 all bits are cleared.
    if (i < 0)
      return 0;

    // Now determine the exact length.
    long b = bits[i];
    int len = (i + 1) * 64;
    // b >= 0 checks if the highest bit is zero.
    while (b >= 0)
      {
	--len;
	b <<= 1;
      }

    return len;
  }

  /**
   * Performs the logical OR operation on this bit set and the
   * given <code>set</code>.  This means it builds the union
   * of the two sets.  The result is stored into this bit set, which
   * grows as necessary.
   * @param set the second bit set.
   * @exception OutOfMemoryError if the current set can't grow.
   * @require set != null
   */
  public void or(BitSet bs)
  {
    ensure(bs.bits.length - 1);
    int i;
    for (i = 0; i < bs.bits.length; ++i)
      bits[i] |= bs.bits[i];
  }

  /**
   * Add the integer <code>bitIndex</code> to this set.  That is 
   * the corresponding bit is set to true.  If the index was already in
   * the set, this method does nothing.  The size of this structure
   * is automatically increased as necessary.
   * @param bitIndex a non-negative integer.
   * @exception ArrayIndexOutOfBoundsException if the specified bit index
   * is negative.
   * @require bitIndex >= 0
   */
  public void set(int pos)
  {
    if (pos < 0)
      throw new IndexOutOfBoundsException();
    int bit = pos % 64;
    int offset = pos / 64;
    ensure(offset);
    bits[offset] |= 1L << bit;
  }

  /**
   * Returns the number of bits actually used by this bit set.  Note
   * that this method doesn't return the number of set bits.
   * @returns the number of bits currently used.  
   */
  public int size()
  {
    return bits.length * 64;
  }

  /**
   * Returns the string representation of this bit set.  This
   * consists of a comma separated list of the integers in this set
   * surrounded by curly braces.  There is a space after each comma.
   * @return the string representation.
   */
  public String toString()
  {
    String r = "{";
    boolean first = true;
    for (int i = 0; i < bits.length; ++i)
      {
	long bit = 1;
	long word = bits[i];
	if (word == 0)
	  continue;
	for (int j = 0; j < 64; ++j)
	  {
	    if ((word & bit) != 0)
	      {
		if (!first)
		  r += ", ";
		r += Integer.toString(64 * i + j);
		first = false;
	      }
	    bit <<= 1;
	  }
      }

    return r += "}";
  }

  /**
   * Performs the logical XOR operation on this bit set and the
   * given <code>set</code>.  This means it builds the symmetric
   * remainder of the two sets (the elements that are in one set,
   * but not in the other).  The result is stored into this bit set,
   * which grows as necessary.  
   * @param set the second bit set.
   * @exception OutOfMemoryError if the current set can't grow.  
   * @require set != null
   */
  public void xor(BitSet bs)
  {
    ensure(bs.bits.length - 1);
    int i;
    for (i = 0; i < bs.bits.length; ++i)
      bits[i] ^= bs.bits[i];
  }

  // Make sure the vector is big enough.
  private final void ensure(int lastElt)
  {
    if (lastElt + 1 > bits.length)
      {
	long[] nd = new long[lastElt + 1];
	System.arraycopy(bits, 0, nd, 0, bits.length);
	bits = nd;
      }
  }

  // The actual bits.
  long[] bits;

  private static final long serialVersionUID = 7997698588986878753L;
}
