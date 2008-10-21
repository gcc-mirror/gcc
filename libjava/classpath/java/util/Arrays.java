/* Arrays.java -- Utility class with methods to operate on arrays
   Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
   Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

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

import gnu.java.lang.CPStringBuilder;

import java.io.Serializable;
import java.lang.reflect.Array;

/**
 * This class contains various static utility methods performing operations on
 * arrays, and a method to provide a List "view" of an array to facilitate
 * using arrays with Collection-based APIs. All methods throw a
 * {@link NullPointerException} if the parameter array is null.
 * <p>
 *
 * Implementations may use their own algorithms, but must obey the general
 * properties; for example, the sort must be stable and n*log(n) complexity.
 * Sun's implementation of sort, and therefore ours, is a tuned quicksort,
 * adapted from Jon L. Bentley and M. Douglas McIlroy's "Engineering a Sort
 * Function", Software-Practice and Experience, Vol. 23(11) P. 1249-1265
 * (November 1993). This algorithm offers n*log(n) performance on many data
 * sets that cause other quicksorts to degrade to quadratic performance.
 *
 * @author Original author unknown
 * @author Bryce McKinlay
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see Comparable
 * @see Comparator
 * @since 1.2
 * @status updated to 1.4
 */
public class Arrays
{
  /**
   * This class is non-instantiable.
   */
  private Arrays()
  {
  }


// binarySearch
  /**
   * Perform a binary search of a byte array for a key. The array must be
   * sorted (as by the sort() method) - if it is not, the behaviour of this
   * method is undefined, and may be an infinite loop. If the array contains
   * the key more than once, any one of them may be found. Note: although the
   * specification allows for an infinite loop if the array is unsorted, it
   * will not happen in this implementation.
   *
   * @param a the array to search (must be sorted)
   * @param key the value to search for
   * @return the index at which the key was found, or -n-1 if it was not
   *         found, where n is the index of the first value higher than key or
   *         a.length if there is no such value.
   */
  public static int binarySearch(byte[] a, byte key)
  {
    if (a.length == 0)
      return -1;
    return binarySearch(a, 0, a.length - 1, key);
  }

  /**
   * Perform a binary search of a range of a byte array for a key. The range
   * must be sorted (as by the <code>sort(byte[], int, int)</code> method) -
   * if it is not, the behaviour of this method is undefined, and may be an
   * infinite loop. If the array contains the key more than once, any one of
   * them may be found. Note: although the specification allows for an infinite
   * loop if the array is unsorted, it will not happen in this implementation.
   *
   * @param a the array to search (must be sorted)
   * @param low the lowest index to search from.
   * @param hi the highest index to search to.
   * @param key the value to search for
   * @return the index at which the key was found, or -n-1 if it was not
   *         found, where n is the index of the first value higher than key or
   *         a.length if there is no such value.
   * @throws IllegalArgumentException if <code>low > hi</code>
   * @throws ArrayIndexOutOfBoundsException if <code>low < 0</code> or
   *                                        <code>hi > a.length</code>.
   */
  public static int binarySearch(byte[] a, int low, int hi, byte key)
  {
    if (low > hi)
      throw new IllegalArgumentException("The start index is higher than " +
					 "the finish index.");
    if (low < 0 || hi > a.length)
      throw new ArrayIndexOutOfBoundsException("One of the indices is out " +
					       "of bounds.");
    int mid = 0;
    while (low <= hi)
      {
        mid = (low + hi) >>> 1;
        final byte d = a[mid];
        if (d == key)
          return mid;
        else if (d > key)
          hi = mid - 1;
        else
          // This gets the insertion point right on the last loop.
          low = ++mid;
      }
    return -mid - 1;
  }

  /**
   * Perform a binary search of a char array for a key. The array must be
   * sorted (as by the sort() method) - if it is not, the behaviour of this
   * method is undefined, and may be an infinite loop. If the array contains
   * the key more than once, any one of them may be found. Note: although the
   * specification allows for an infinite loop if the array is unsorted, it
   * will not happen in this implementation.
   *
   * @param a the array to search (must be sorted)
   * @param key the value to search for
   * @return the index at which the key was found, or -n-1 if it was not
   *         found, where n is the index of the first value higher than key or
   *         a.length if there is no such value.
   */
  public static int binarySearch(char[] a, char key)
  {
    if (a.length == 0)
      return -1;
    return binarySearch(a, 0, a.length - 1, key);
  }

  /**
   * Perform a binary search of a range of a char array for a key. The range
   * must be sorted (as by the <code>sort(char[], int, int)</code> method) -
   * if it is not, the behaviour of this method is undefined, and may be an
   * infinite loop. If the array contains the key more than once, any one of
   * them may be found. Note: although the specification allows for an infinite
   * loop if the array is unsorted, it will not happen in this implementation.
   *
   * @param a the array to search (must be sorted)
   * @param low the lowest index to search from.
   * @param hi the highest index to search to.
   * @param key the value to search for
   * @return the index at which the key was found, or -n-1 if it was not
   *         found, where n is the index of the first value higher than key or
   *         a.length if there is no such value.
   * @throws IllegalArgumentException if <code>low > hi</code>
   * @throws ArrayIndexOutOfBoundsException if <code>low < 0</code> or
   *                                        <code>hi > a.length</code>.
   */
  public static int binarySearch(char[] a, int low, int hi, char key)
  {
    if (low > hi)
      throw new IllegalArgumentException("The start index is higher than " +
					 "the finish index.");
    if (low < 0 || hi > a.length)
      throw new ArrayIndexOutOfBoundsException("One of the indices is out " +
					       "of bounds.");
    int mid = 0;
    while (low <= hi)
      {
        mid = (low + hi) >>> 1;
        final char d = a[mid];
        if (d == key)
          return mid;
        else if (d > key)
          hi = mid - 1;
        else
          // This gets the insertion point right on the last loop.
          low = ++mid;
      }
    return -mid - 1;
  }

  /**
   * Perform a binary search of a short array for a key. The array must be
   * sorted (as by the sort() method) - if it is not, the behaviour of this
   * method is undefined, and may be an infinite loop. If the array contains
   * the key more than once, any one of them may be found. Note: although the
   * specification allows for an infinite loop if the array is unsorted, it
   * will not happen in this implementation.
   *
   * @param a the array to search (must be sorted)
   * @param key the value to search for
   * @return the index at which the key was found, or -n-1 if it was not
   *         found, where n is the index of the first value higher than key or
   *         a.length if there is no such value.
   */
  public static int binarySearch(short[] a, short key)
  {
    if (a.length == 0)
      return -1;
    return binarySearch(a, 0, a.length - 1, key);
  }

  /**
   * Perform a binary search of a range of a short array for a key. The range
   * must be sorted (as by the <code>sort(short[], int, int)</code> method) -
   * if it is not, the behaviour of this method is undefined, and may be an
   * infinite loop. If the array contains the key more than once, any one of
   * them may be found. Note: although the specification allows for an infinite
   * loop if the array is unsorted, it will not happen in this implementation.
   *
   * @param a the array to search (must be sorted)
   * @param low the lowest index to search from.
   * @param hi the highest index to search to.
   * @param key the value to search for
   * @return the index at which the key was found, or -n-1 if it was not
   *         found, where n is the index of the first value higher than key or
   *         a.length if there is no such value.
   * @throws IllegalArgumentException if <code>low > hi</code>
   * @throws ArrayIndexOutOfBoundsException if <code>low < 0</code> or
   *                                        <code>hi > a.length</code>.
   */
  public static int binarySearch(short[] a, int low, int hi, short key)
  {
    if (low > hi)
      throw new IllegalArgumentException("The start index is higher than " +
					 "the finish index.");
    if (low < 0 || hi > a.length)
      throw new ArrayIndexOutOfBoundsException("One of the indices is out " +
					       "of bounds.");
    int mid = 0;
    while (low <= hi)
      {
        mid = (low + hi) >>> 1;
        final short d = a[mid];
        if (d == key)
          return mid;
        else if (d > key)
          hi = mid - 1;
        else
          // This gets the insertion point right on the last loop.
          low = ++mid;
      }
    return -mid - 1;
  }

  /**
   * Perform a binary search of an int array for a key. The array must be
   * sorted (as by the sort() method) - if it is not, the behaviour of this
   * method is undefined, and may be an infinite loop. If the array contains
   * the key more than once, any one of them may be found. Note: although the
   * specification allows for an infinite loop if the array is unsorted, it
   * will not happen in this implementation.
   *
   * @param a the array to search (must be sorted)
   * @param key the value to search for
   * @return the index at which the key was found, or -n-1 if it was not
   *         found, where n is the index of the first value higher than key or
   *         a.length if there is no such value.
   */
  public static int binarySearch(int[] a, int key)
  {
    if (a.length == 0)
      return -1;
    return binarySearch(a, 0, a.length - 1, key);
  }

  /**
   * Perform a binary search of a range of an integer array for a key. The range
   * must be sorted (as by the <code>sort(int[], int, int)</code> method) -
   * if it is not, the behaviour of this method is undefined, and may be an
   * infinite loop. If the array contains the key more than once, any one of
   * them may be found. Note: although the specification allows for an infinite
   * loop if the array is unsorted, it will not happen in this implementation.
   *
   * @param a the array to search (must be sorted)
   * @param low the lowest index to search from.
   * @param hi the highest index to search to.
   * @param key the value to search for
   * @return the index at which the key was found, or -n-1 if it was not
   *         found, where n is the index of the first value higher than key or
   *         a.length if there is no such value.
   * @throws IllegalArgumentException if <code>low > hi</code>
   * @throws ArrayIndexOutOfBoundsException if <code>low < 0</code> or
   *                                        <code>hi > a.length</code>.
   */
  public static int binarySearch(int[] a, int low, int hi, int key)
  {
    if (low > hi)
      throw new IllegalArgumentException("The start index is higher than " +
					 "the finish index.");
    if (low < 0 || hi > a.length)
      throw new ArrayIndexOutOfBoundsException("One of the indices is out " +
					       "of bounds.");
    int mid = 0;
    while (low <= hi)
      {
        mid = (low + hi) >>> 1;
        final int d = a[mid];
        if (d == key)
          return mid;
        else if (d > key)
          hi = mid - 1;
        else
          // This gets the insertion point right on the last loop.
          low = ++mid;
      }
    return -mid - 1;
  }

  /**
   * Perform a binary search of a long array for a key. The array must be
   * sorted (as by the sort() method) - if it is not, the behaviour of this
   * method is undefined, and may be an infinite loop. If the array contains
   * the key more than once, any one of them may be found. Note: although the
   * specification allows for an infinite loop if the array is unsorted, it
   * will not happen in this implementation.
   *
   * @param a the array to search (must be sorted)
   * @param key the value to search for
   * @return the index at which the key was found, or -n-1 if it was not
   *         found, where n is the index of the first value higher than key or
   *         a.length if there is no such value.
   */
  public static int binarySearch(long[] a, long key)
  {
    if (a.length == 0)
      return -1;
    return binarySearch(a, 0, a.length - 1, key);
  }

  /**
   * Perform a binary search of a range of a long array for a key. The range
   * must be sorted (as by the <code>sort(long[], int, int)</code> method) -
   * if it is not, the behaviour of this method is undefined, and may be an
   * infinite loop. If the array contains the key more than once, any one of
   * them may be found. Note: although the specification allows for an infinite
   * loop if the array is unsorted, it will not happen in this implementation.
   *
   * @param a the array to search (must be sorted)
   * @param low the lowest index to search from.
   * @param hi the highest index to search to.
   * @param key the value to search for
   * @return the index at which the key was found, or -n-1 if it was not
   *         found, where n is the index of the first value higher than key or
   *         a.length if there is no such value.
   * @throws IllegalArgumentException if <code>low > hi</code>
   * @throws ArrayIndexOutOfBoundsException if <code>low < 0</code> or
   *                                        <code>hi > a.length</code>.
   */
  public static int binarySearch(long[] a, int low, int hi, long key)
  {
    if (low > hi)
      throw new IllegalArgumentException("The start index is higher than " +
					 "the finish index.");
    if (low < 0 || hi > a.length)
      throw new ArrayIndexOutOfBoundsException("One of the indices is out " +
					       "of bounds.");
    int mid = 0;
    while (low <= hi)
      {
        mid = (low + hi) >>> 1;
        final long d = a[mid];
        if (d == key)
          return mid;
        else if (d > key)
          hi = mid - 1;
        else
          // This gets the insertion point right on the last loop.
          low = ++mid;
      }
    return -mid - 1;
  }

  /**
   * Perform a binary search of a float array for a key. The array must be
   * sorted (as by the sort() method) - if it is not, the behaviour of this
   * method is undefined, and may be an infinite loop. If the array contains
   * the key more than once, any one of them may be found. Note: although the
   * specification allows for an infinite loop if the array is unsorted, it
   * will not happen in this implementation.
   *
   * @param a the array to search (must be sorted)
   * @param key the value to search for
   * @return the index at which the key was found, or -n-1 if it was not
   *         found, where n is the index of the first value higher than key or
   *         a.length if there is no such value.
   */
  public static int binarySearch(float[] a, float key)
  {
    if (a.length == 0)
      return -1;
    return binarySearch(a, 0, a.length - 1, key);
  }

  /**
   * Perform a binary search of a range of a float array for a key. The range
   * must be sorted (as by the <code>sort(float[], int, int)</code> method) -
   * if it is not, the behaviour of this method is undefined, and may be an
   * infinite loop. If the array contains the key more than once, any one of
   * them may be found. Note: although the specification allows for an infinite
   * loop if the array is unsorted, it will not happen in this implementation.
   *
   * @param a the array to search (must be sorted)
   * @param low the lowest index to search from.
   * @param hi the highest index to search to.
   * @param key the value to search for
   * @return the index at which the key was found, or -n-1 if it was not
   *         found, where n is the index of the first value higher than key or
   *         a.length if there is no such value.
   * @throws IllegalArgumentException if <code>low > hi</code>
   * @throws ArrayIndexOutOfBoundsException if <code>low < 0</code> or
   *                                        <code>hi > a.length</code>.
   */
  public static int binarySearch(float[] a, int low, int hi, float key)
  {
    if (low > hi)
      throw new IllegalArgumentException("The start index is higher than " +
					 "the finish index.");
    if (low < 0 || hi > a.length)
      throw new ArrayIndexOutOfBoundsException("One of the indices is out " +
					       "of bounds.");
    // Must use Float.compare to take into account NaN, +-0.
    int mid = 0;
    while (low <= hi)
      {
        mid = (low + hi) >>> 1;
        final int r = Float.compare(a[mid], key);
        if (r == 0)
          return mid;
        else if (r > 0)
          hi = mid - 1;
        else
          // This gets the insertion point right on the last loop
          low = ++mid;
      }
    return -mid - 1;
  }

  /**
   * Perform a binary search of a double array for a key. The array must be
   * sorted (as by the sort() method) - if it is not, the behaviour of this
   * method is undefined, and may be an infinite loop. If the array contains
   * the key more than once, any one of them may be found. Note: although the
   * specification allows for an infinite loop if the array is unsorted, it
   * will not happen in this implementation.
   *
   * @param a the array to search (must be sorted)
   * @param key the value to search for
   * @return the index at which the key was found, or -n-1 if it was not
   *         found, where n is the index of the first value higher than key or
   *         a.length if there is no such value.
   */
  public static int binarySearch(double[] a, double key)
  {
    if (a.length == 0)
      return -1;
    return binarySearch(a, 0, a.length - 1, key);
  }

  /**
   * Perform a binary search of a range of a double array for a key. The range
   * must be sorted (as by the <code>sort(double[], int, int)</code> method) -
   * if it is not, the behaviour of this method is undefined, and may be an
   * infinite loop. If the array contains the key more than once, any one of
   * them may be found. Note: although the specification allows for an infinite
   * loop if the array is unsorted, it will not happen in this implementation.
   *
   * @param a the array to search (must be sorted)
   * @param low the lowest index to search from.
   * @param hi the highest index to search to.
   * @param key the value to search for
   * @return the index at which the key was found, or -n-1 if it was not
   *         found, where n is the index of the first value higher than key or
   *         a.length if there is no such value.
   * @throws IllegalArgumentException if <code>low > hi</code>
   * @throws ArrayIndexOutOfBoundsException if <code>low < 0</code> or
   *                                        <code>hi > a.length</code>.
   */
  public static int binarySearch(double[] a, int low, int hi, double key)
  {
    if (low > hi)
      throw new IllegalArgumentException("The start index is higher than " +
					 "the finish index.");
    if (low < 0 || hi > a.length)
      throw new ArrayIndexOutOfBoundsException("One of the indices is out " +
					       "of bounds.");
    // Must use Double.compare to take into account NaN, +-0.
    int mid = 0;
    while (low <= hi)
      {
        mid = (low + hi) >>> 1;
        final int r = Double.compare(a[mid], key);
        if (r == 0)
          return mid;
        else if (r > 0)
          hi = mid - 1;
        else
          // This gets the insertion point right on the last loop
          low = ++mid;
      }
    return -mid - 1;
  }

  /**
   * Perform a binary search of an Object array for a key, using the natural
   * ordering of the elements. The array must be sorted (as by the sort()
   * method) - if it is not, the behaviour of this method is undefined, and may
   * be an infinite loop. Further, the key must be comparable with every item
   * in the array. If the array contains the key more than once, any one of
   * them may be found. Note: although the specification allows for an infinite
   * loop if the array is unsorted, it will not happen in this (JCL)
   * implementation.
   *
   * @param a the array to search (must be sorted)
   * @param key the value to search for
   * @return the index at which the key was found, or -n-1 if it was not
   *         found, where n is the index of the first value higher than key or
   *         a.length if there is no such value.
   * @throws ClassCastException if key could not be compared with one of the
   *         elements of a
   * @throws NullPointerException if a null element in a is compared
   */
  public static int binarySearch(Object[] a, Object key)
  {
    if (a.length == 0)
      return -1;
    return binarySearch(a, key, null);
  }

  /**
   * Perform a binary search of a range of an Object array for a key. The range
   * must be sorted (as by the <code>sort(Object[], int, int)</code> method) -
   * if it is not, the behaviour of this method is undefined, and may be an
   * infinite loop. If the array contains the key more than once, any one of
   * them may be found. Note: although the specification allows for an infinite
   * loop if the array is unsorted, it will not happen in this implementation.
   *
   * @param a the array to search (must be sorted)
   * @param low the lowest index to search from.
   * @param hi the highest index to search to.
   * @param key the value to search for
   * @return the index at which the key was found, or -n-1 if it was not
   *         found, where n is the index of the first value higher than key or
   *         a.length if there is no such value.
   */
  public static int binarySearch(Object[] a, int low, int hi, Object key)
  {
    return binarySearch(a, low, hi, key, null);
  }

  /**
   * Perform a binary search of an Object array for a key, using a supplied
   * Comparator. The array must be sorted (as by the sort() method with the
   * same Comparator) - if it is not, the behaviour of this method is
   * undefined, and may be an infinite loop. Further, the key must be
   * comparable with every item in the array. If the array contains the key
   * more than once, any one of them may be found. Note: although the
   * specification allows for an infinite loop if the array is unsorted, it
   * will not happen in this (JCL) implementation.
   *
   * @param a the array to search (must be sorted)
   * @param key the value to search for
   * @param c the comparator by which the array is sorted; or null to
   *        use the elements' natural order
   * @return the index at which the key was found, or -n-1 if it was not
   *         found, where n is the index of the first value higher than key or
   *         a.length if there is no such value.
   * @throws ClassCastException if key could not be compared with one of the
   *         elements of a
   * @throws NullPointerException if a null element is compared with natural
   *         ordering (only possible when c is null)
   */
  public static <T> int binarySearch(T[] a, T key, Comparator<? super T> c)
  {
    if (a.length == 0)
      return -1;
    return binarySearch(a, 0, a.length - 1, key, c);
  }

  /**
   * Perform a binary search of a range of an Object array for a key using
   * a {@link Comparator}. The range must be sorted (as by the
   * <code>sort(Object[], int, int)</code> method) - if it is not, the
   * behaviour of this method is undefined, and may be an infinite loop. If
   * the array contains the key more than once, any one of them may be found.
   * Note: although the specification allows for an infinite loop if the array
   * is unsorted, it will not happen in this implementation.
   *
   * @param a the array to search (must be sorted)
   * @param low the lowest index to search from.
   * @param hi the highest index to search to.
   * @param key the value to search for
   * @param c the comparator by which the array is sorted; or null to
   *        use the elements' natural order
   * @return the index at which the key was found, or -n-1 if it was not
   *         found, where n is the index of the first value higher than key or
   *         a.length if there is no such value.
   * @throws ClassCastException if key could not be compared with one of the
   *         elements of a
   * @throws IllegalArgumentException if <code>low > hi</code>
   * @throws ArrayIndexOutOfBoundsException if <code>low < 0</code> or
   *                                        <code>hi > a.length</code>.
   */
  public static <T> int binarySearch(T[] a, int low, int hi, T key,
				     Comparator<? super T> c)
  {
    if (low > hi)
      throw new IllegalArgumentException("The start index is higher than " +
					 "the finish index.");
    if (low < 0 || hi > a.length)
      throw new ArrayIndexOutOfBoundsException("One of the indices is out " +
					       "of bounds.");
    int mid = 0;
    while (low <= hi)
      {
        mid = (low + hi) >>> 1;
	// NOTE: Please keep the order of a[mid] and key.  Although
	// not required by the specs, the RI has it in this order as
	// well, and real programs (erroneously) depend on it.
        final int d = Collections.compare(a[mid], key, c);
        if (d == 0)
          return mid;
        else if (d > 0)
          hi = mid - 1;
        else
          // This gets the insertion point right on the last loop
          low = ++mid;
      }
    return -mid - 1;
  }


// equals
  /**
   * Compare two boolean arrays for equality.
   *
   * @param a1 the first array to compare
   * @param a2 the second array to compare
   * @return true if a1 and a2 are both null, or if a2 is of the same length
   *         as a1, and for each 0 <= i < a1.length, a1[i] == a2[i]
   */
  public static boolean equals(boolean[] a1, boolean[] a2)
  {
    // Quick test which saves comparing elements of the same array, and also
    // catches the case that both are null.
    if (a1 == a2)
      return true;

    if (null == a1 || null == a2)
      return false;
    
    // If they're the same length, test each element
    if (a1.length == a2.length)
      {
	int i = a1.length;
	while (--i >= 0)
	  if (a1[i] != a2[i])
	    return false;
	return true;
      }
    return false;
  }

  /**
   * Compare two byte arrays for equality.
   *
   * @param a1 the first array to compare
   * @param a2 the second array to compare
   * @return true if a1 and a2 are both null, or if a2 is of the same length
   *         as a1, and for each 0 <= i < a1.length, a1[i] == a2[i]
   */
  public static boolean equals(byte[] a1, byte[] a2)
  {
    // Quick test which saves comparing elements of the same array, and also
    // catches the case that both are null.
    if (a1 == a2)
      return true;

    if (null == a1 || null == a2)
      return false;

    // If they're the same length, test each element
    if (a1.length == a2.length)
      {
	int i = a1.length;
	while (--i >= 0)
	  if (a1[i] != a2[i])
	    return false;
	return true;
      }
    return false;
  }

  /**
   * Compare two char arrays for equality.
   *
   * @param a1 the first array to compare
   * @param a2 the second array to compare
   * @return true if a1 and a2 are both null, or if a2 is of the same length
   *         as a1, and for each 0 <= i < a1.length, a1[i] == a2[i]
   */
  public static boolean equals(char[] a1, char[] a2)
  {
    // Quick test which saves comparing elements of the same array, and also
    // catches the case that both are null.
    if (a1 == a2)
      return true;

    if (null == a1 || null == a2)
      return false;
    
    // If they're the same length, test each element
    if (a1.length == a2.length)
      {
	int i = a1.length;
	while (--i >= 0)
	  if (a1[i] != a2[i])
	    return false;
	return true;
      }
    return false;
  }

  /**
   * Compare two short arrays for equality.
   *
   * @param a1 the first array to compare
   * @param a2 the second array to compare
   * @return true if a1 and a2 are both null, or if a2 is of the same length
   *         as a1, and for each 0 <= i < a1.length, a1[i] == a2[i]
   */
  public static boolean equals(short[] a1, short[] a2)
  {
    // Quick test which saves comparing elements of the same array, and also
    // catches the case that both are null.
    if (a1 == a2)
      return true;

    if (null == a1 || null == a2)
      return false;

    // If they're the same length, test each element
    if (a1.length == a2.length)
      {
	int i = a1.length;
	while (--i >= 0)
	  if (a1[i] != a2[i])
	    return false;
	return true;
      }
    return false;
  }

  /**
   * Compare two int arrays for equality.
   *
   * @param a1 the first array to compare
   * @param a2 the second array to compare
   * @return true if a1 and a2 are both null, or if a2 is of the same length
   *         as a1, and for each 0 <= i < a1.length, a1[i] == a2[i]
   */
  public static boolean equals(int[] a1, int[] a2)
  {
    // Quick test which saves comparing elements of the same array, and also
    // catches the case that both are null.
    if (a1 == a2)
      return true;

    if (null == a1 || null == a2)
      return false;

    // If they're the same length, test each element
    if (a1.length == a2.length)
      {
	int i = a1.length;
	while (--i >= 0)
	  if (a1[i] != a2[i])
	    return false;
	return true;
      }
    return false;
  }

  /**
   * Compare two long arrays for equality.
   *
   * @param a1 the first array to compare
   * @param a2 the second array to compare
   * @return true if a1 and a2 are both null, or if a2 is of the same length
   *         as a1, and for each 0 <= i < a1.length, a1[i] == a2[i]
   */
  public static boolean equals(long[] a1, long[] a2)
  {
    // Quick test which saves comparing elements of the same array, and also
    // catches the case that both are null.
    if (a1 == a2)
      return true;

    if (null == a1 || null == a2)
      return false;

    // If they're the same length, test each element
    if (a1.length == a2.length)
      {
	int i = a1.length;
	while (--i >= 0)
	  if (a1[i] != a2[i])
	    return false;
	return true;
      }
    return false;
  }

  /**
   * Compare two float arrays for equality.
   *
   * @param a1 the first array to compare
   * @param a2 the second array to compare
   * @return true if a1 and a2 are both null, or if a2 is of the same length
   *         as a1, and for each 0 <= i < a1.length, a1[i] == a2[i]
   */
  public static boolean equals(float[] a1, float[] a2)
  {
    // Quick test which saves comparing elements of the same array, and also
    // catches the case that both are null.
    if (a1 == a2)
      return true;

    if (null == a1 || null == a2)
      return false;

    // Must use Float.compare to take into account NaN, +-0.
    // If they're the same length, test each element
    if (a1.length == a2.length)
      {
	int i = a1.length;
	while (--i >= 0)
	  if (Float.compare(a1[i], a2[i]) != 0)
	    return false;
	return true;
      }
    return false;
  }

  /**
   * Compare two double arrays for equality.
   *
   * @param a1 the first array to compare
   * @param a2 the second array to compare
   * @return true if a1 and a2 are both null, or if a2 is of the same length
   *         as a1, and for each 0 <= i < a1.length, a1[i] == a2[i]
   */
  public static boolean equals(double[] a1, double[] a2)
  {
    // Quick test which saves comparing elements of the same array, and also
    // catches the case that both are null.
    if (a1 == a2)
      return true;

    if (null == a1 || null == a2)
      return false;
    
    // Must use Double.compare to take into account NaN, +-0.
    // If they're the same length, test each element
    if (a1.length == a2.length)
      {
	int i = a1.length;
	while (--i >= 0)
	  if (Double.compare(a1[i], a2[i]) != 0)
	    return false;
	return true;
      }
    return false;
  }

  /**
   * Compare two Object arrays for equality.
   *
   * @param a1 the first array to compare
   * @param a2 the second array to compare
   * @return true if a1 and a2 are both null, or if a1 is of the same length
   *         as a2, and for each 0 <= i < a.length, a1[i] == null ?
   *         a2[i] == null : a1[i].equals(a2[i]).
   */
  public static boolean equals(Object[] a1, Object[] a2)
  {
    // Quick test which saves comparing elements of the same array, and also
    // catches the case that both are null.
    if (a1 == a2)
      return true;

    if (null == a1 || null == a2)
      return false;
    
    // If they're the same length, test each element
    if (a1.length == a2.length)
      {
	int i = a1.length;
	while (--i >= 0)
	  if (! AbstractCollection.equals(a1[i], a2[i]))
	    return false;
	return true;
      }
    return false;
  }


// fill
  /**
   * Fill an array with a boolean value.
   *
   * @param a the array to fill
   * @param val the value to fill it with
   */
  public static void fill(boolean[] a, boolean val)
  {
    fill(a, 0, a.length, val);
  }

  /**
   * Fill a range of an array with a boolean value.
   *
   * @param a the array to fill
   * @param fromIndex the index to fill from, inclusive
   * @param toIndex the index to fill to, exclusive
   * @param val the value to fill with
   * @throws IllegalArgumentException if fromIndex &gt; toIndex
   * @throws ArrayIndexOutOfBoundsException if fromIndex &lt; 0
   *         || toIndex &gt; a.length
   */
  public static void fill(boolean[] a, int fromIndex, int toIndex, boolean val)
  {
    if (fromIndex > toIndex)
      throw new IllegalArgumentException();
    for (int i = fromIndex; i < toIndex; i++)
      a[i] = val;
  }

  /**
   * Fill an array with a byte value.
   *
   * @param a the array to fill
   * @param val the value to fill it with
   */
  public static void fill(byte[] a, byte val)
  {
    fill(a, 0, a.length, val);
  }

  /**
   * Fill a range of an array with a byte value.
   *
   * @param a the array to fill
   * @param fromIndex the index to fill from, inclusive
   * @param toIndex the index to fill to, exclusive
   * @param val the value to fill with
   * @throws IllegalArgumentException if fromIndex &gt; toIndex
   * @throws ArrayIndexOutOfBoundsException if fromIndex &lt; 0
   *         || toIndex &gt; a.length
   */
  public static void fill(byte[] a, int fromIndex, int toIndex, byte val)
  {
    if (fromIndex > toIndex)
      throw new IllegalArgumentException();
    for (int i = fromIndex; i < toIndex; i++)
      a[i] = val;
  }

  /**
   * Fill an array with a char value.
   *
   * @param a the array to fill
   * @param val the value to fill it with
   */
  public static void fill(char[] a, char val)
  {
    fill(a, 0, a.length, val);
  }

  /**
   * Fill a range of an array with a char value.
   *
   * @param a the array to fill
   * @param fromIndex the index to fill from, inclusive
   * @param toIndex the index to fill to, exclusive
   * @param val the value to fill with
   * @throws IllegalArgumentException if fromIndex &gt; toIndex
   * @throws ArrayIndexOutOfBoundsException if fromIndex &lt; 0
   *         || toIndex &gt; a.length
   */
  public static void fill(char[] a, int fromIndex, int toIndex, char val)
  {
    if (fromIndex > toIndex)
      throw new IllegalArgumentException();
    for (int i = fromIndex; i < toIndex; i++)
      a[i] = val;
  }

  /**
   * Fill an array with a short value.
   *
   * @param a the array to fill
   * @param val the value to fill it with
   */
  public static void fill(short[] a, short val)
  {
    fill(a, 0, a.length, val);
  }

  /**
   * Fill a range of an array with a short value.
   *
   * @param a the array to fill
   * @param fromIndex the index to fill from, inclusive
   * @param toIndex the index to fill to, exclusive
   * @param val the value to fill with
   * @throws IllegalArgumentException if fromIndex &gt; toIndex
   * @throws ArrayIndexOutOfBoundsException if fromIndex &lt; 0
   *         || toIndex &gt; a.length
   */
  public static void fill(short[] a, int fromIndex, int toIndex, short val)
  {
    if (fromIndex > toIndex)
      throw new IllegalArgumentException();
    for (int i = fromIndex; i < toIndex; i++)
      a[i] = val;
  }

  /**
   * Fill an array with an int value.
   *
   * @param a the array to fill
   * @param val the value to fill it with
   */
  public static void fill(int[] a, int val)
  {
    fill(a, 0, a.length, val);
  }

  /**
   * Fill a range of an array with an int value.
   *
   * @param a the array to fill
   * @param fromIndex the index to fill from, inclusive
   * @param toIndex the index to fill to, exclusive
   * @param val the value to fill with
   * @throws IllegalArgumentException if fromIndex &gt; toIndex
   * @throws ArrayIndexOutOfBoundsException if fromIndex &lt; 0
   *         || toIndex &gt; a.length
   */
  public static void fill(int[] a, int fromIndex, int toIndex, int val)
  {
    if (fromIndex > toIndex)
      throw new IllegalArgumentException();
    for (int i = fromIndex; i < toIndex; i++)
      a[i] = val;
  }

  /**
   * Fill an array with a long value.
   *
   * @param a the array to fill
   * @param val the value to fill it with
   */
  public static void fill(long[] a, long val)
  {
    fill(a, 0, a.length, val);
  }

  /**
   * Fill a range of an array with a long value.
   *
   * @param a the array to fill
   * @param fromIndex the index to fill from, inclusive
   * @param toIndex the index to fill to, exclusive
   * @param val the value to fill with
   * @throws IllegalArgumentException if fromIndex &gt; toIndex
   * @throws ArrayIndexOutOfBoundsException if fromIndex &lt; 0
   *         || toIndex &gt; a.length
   */
  public static void fill(long[] a, int fromIndex, int toIndex, long val)
  {
    if (fromIndex > toIndex)
      throw new IllegalArgumentException();
    for (int i = fromIndex; i < toIndex; i++)
      a[i] = val;
  }

  /**
   * Fill an array with a float value.
   *
   * @param a the array to fill
   * @param val the value to fill it with
   */
  public static void fill(float[] a, float val)
  {
    fill(a, 0, a.length, val);
  }

  /**
   * Fill a range of an array with a float value.
   *
   * @param a the array to fill
   * @param fromIndex the index to fill from, inclusive
   * @param toIndex the index to fill to, exclusive
   * @param val the value to fill with
   * @throws IllegalArgumentException if fromIndex &gt; toIndex
   * @throws ArrayIndexOutOfBoundsException if fromIndex &lt; 0
   *         || toIndex &gt; a.length
   */
  public static void fill(float[] a, int fromIndex, int toIndex, float val)
  {
    if (fromIndex > toIndex)
      throw new IllegalArgumentException();
    for (int i = fromIndex; i < toIndex; i++)
      a[i] = val;
  }

  /**
   * Fill an array with a double value.
   *
   * @param a the array to fill
   * @param val the value to fill it with
   */
  public static void fill(double[] a, double val)
  {
    fill(a, 0, a.length, val);
  }

  /**
   * Fill a range of an array with a double value.
   *
   * @param a the array to fill
   * @param fromIndex the index to fill from, inclusive
   * @param toIndex the index to fill to, exclusive
   * @param val the value to fill with
   * @throws IllegalArgumentException if fromIndex &gt; toIndex
   * @throws ArrayIndexOutOfBoundsException if fromIndex &lt; 0
   *         || toIndex &gt; a.length
   */
  public static void fill(double[] a, int fromIndex, int toIndex, double val)
  {
    if (fromIndex > toIndex)
      throw new IllegalArgumentException();
    for (int i = fromIndex; i < toIndex; i++)
      a[i] = val;
  }

  /**
   * Fill an array with an Object value.
   *
   * @param a the array to fill
   * @param val the value to fill it with
   * @throws ClassCastException if val is not an instance of the element
   *         type of a.
   */
  public static void fill(Object[] a, Object val)
  {
    fill(a, 0, a.length, val);
  }

  /**
   * Fill a range of an array with an Object value.
   *
   * @param a the array to fill
   * @param fromIndex the index to fill from, inclusive
   * @param toIndex the index to fill to, exclusive
   * @param val the value to fill with
   * @throws ClassCastException if val is not an instance of the element
   *         type of a.
   * @throws IllegalArgumentException if fromIndex &gt; toIndex
   * @throws ArrayIndexOutOfBoundsException if fromIndex &lt; 0
   *         || toIndex &gt; a.length
   */
  public static void fill(Object[] a, int fromIndex, int toIndex, Object val)
  {
    if (fromIndex > toIndex)
      throw new IllegalArgumentException();
    for (int i = fromIndex; i < toIndex; i++)
      a[i] = val;
  }


// sort
  // Thanks to Paul Fisher (rao@gnu.org) for finding this quicksort algorithm
  // as specified by Sun and porting it to Java. The algorithm is an optimised
  // quicksort, as described in Jon L. Bentley and M. Douglas McIlroy's
  // "Engineering a Sort Function", Software-Practice and Experience, Vol.
  // 23(11) P. 1249-1265 (November 1993). This algorithm gives n*log(n)
  // performance on many arrays that would take quadratic time with a standard
  // quicksort.

  /**
   * Performs a stable sort on the elements, arranging them according to their
   * natural order.
   *
   * @param a the byte array to sort
   */
  public static void sort(byte[] a)
  {
    qsort(a, 0, a.length);
  }

  /**
   * Performs a stable sort on the elements, arranging them according to their
   * natural order.
   *
   * @param a the byte array to sort
   * @param fromIndex the first index to sort (inclusive)
   * @param toIndex the last index to sort (exclusive)
   * @throws IllegalArgumentException if fromIndex &gt; toIndex
   * @throws ArrayIndexOutOfBoundsException if fromIndex &lt; 0
   *         || toIndex &gt; a.length
   */
  public static void sort(byte[] a, int fromIndex, int toIndex)
  {
    if (fromIndex > toIndex)
      throw new IllegalArgumentException();
    if (fromIndex < 0)
      throw new ArrayIndexOutOfBoundsException();
    qsort(a, fromIndex, toIndex - fromIndex);
  }

  /**
   * Finds the index of the median of three array elements.
   *
   * @param a the first index
   * @param b the second index
   * @param c the third index
   * @param d the array
   * @return the index (a, b, or c) which has the middle value of the three
   */
  private static int med3(int a, int b, int c, byte[] d)
  {
    return (d[a] < d[b]
            ? (d[b] < d[c] ? b : d[a] < d[c] ? c : a)
            : (d[b] > d[c] ? b : d[a] > d[c] ? c : a));
  }

  /**
   * Swaps the elements at two locations of an array
   *
   * @param i the first index
   * @param j the second index
   * @param a the array
   */
  private static void swap(int i, int j, byte[] a)
  {
    byte c = a[i];
    a[i] = a[j];
    a[j] = c;
  }

  /**
   * Swaps two ranges of an array.
   *
   * @param i the first range start
   * @param j the second range start
   * @param n the element count
   * @param a the array
   */
  private static void vecswap(int i, int j, int n, byte[] a)
  {
    for ( ; n > 0; i++, j++, n--)
      swap(i, j, a);
  }

  /**
   * Performs a recursive modified quicksort.
   *
   * @param array the array to sort
   * @param from the start index (inclusive)
   * @param count the number of elements to sort
   */
  private static void qsort(byte[] array, int from, int count)
  {
    // Use an insertion sort on small arrays.
    if (count <= 7)
      {
        for (int i = from + 1; i < from + count; i++)
          for (int j = i; j > from && array[j - 1] > array[j]; j--)
            swap(j, j - 1, array);
        return;
      }

    // Determine a good median element.
    int mid = from + count / 2;
    int lo = from;
    int hi = from + count - 1;

    if (count > 40)
      { // big arrays, pseudomedian of 9
        int s = count / 8;
        lo = med3(lo, lo + s, lo + 2 * s, array);
        mid = med3(mid - s, mid, mid + s, array);
        hi = med3(hi - 2 * s, hi - s, hi, array);
      }
    mid = med3(lo, mid, hi, array);

    int a, b, c, d;
    int comp;

    // Pull the median element out of the fray, and use it as a pivot.
    swap(from, mid, array);
    a = b = from;
    c = d = from + count - 1;

    // Repeatedly move b and c to each other, swapping elements so
    // that all elements before index b are less than the pivot, and all
    // elements after index c are greater than the pivot. a and b track
    // the elements equal to the pivot.
    while (true)
      {
        while (b <= c && (comp = array[b] - array[from]) <= 0)
          {
            if (comp == 0)
              {
                swap(a, b, array);
                a++;
              }
            b++;
          }
        while (c >= b && (comp = array[c] - array[from]) >= 0)
          {
            if (comp == 0)
              {
                swap(c, d, array);
                d--;
              }
            c--;
          }
        if (b > c)
          break;
        swap(b, c, array);
        b++;
        c--;
      }

    // Swap pivot(s) back in place, the recurse on left and right sections.
    hi = from + count;
    int span;
    span = Math.min(a - from, b - a);
    vecswap(from, b - span, span, array);

    span = Math.min(d - c, hi - d - 1);
    vecswap(b, hi - span, span, array);

    span = b - a;
    if (span > 1)
      qsort(array, from, span);

    span = d - c;
    if (span > 1)
      qsort(array, hi - span, span);
  }

  /**
   * Performs a stable sort on the elements, arranging them according to their
   * natural order.
   *
   * @param a the char array to sort
   */
  public static void sort(char[] a)
  {
    qsort(a, 0, a.length);
  }

  /**
   * Performs a stable sort on the elements, arranging them according to their
   * natural order.
   *
   * @param a the char array to sort
   * @param fromIndex the first index to sort (inclusive)
   * @param toIndex the last index to sort (exclusive)
   * @throws IllegalArgumentException if fromIndex &gt; toIndex
   * @throws ArrayIndexOutOfBoundsException if fromIndex &lt; 0
   *         || toIndex &gt; a.length
   */
  public static void sort(char[] a, int fromIndex, int toIndex)
  {
    if (fromIndex > toIndex)
      throw new IllegalArgumentException();
    if (fromIndex < 0)
      throw new ArrayIndexOutOfBoundsException();
    qsort(a, fromIndex, toIndex - fromIndex);
  }

  /**
   * Finds the index of the median of three array elements.
   *
   * @param a the first index
   * @param b the second index
   * @param c the third index
   * @param d the array
   * @return the index (a, b, or c) which has the middle value of the three
   */
  private static int med3(int a, int b, int c, char[] d)
  {
    return (d[a] < d[b]
            ? (d[b] < d[c] ? b : d[a] < d[c] ? c : a)
            : (d[b] > d[c] ? b : d[a] > d[c] ? c : a));
  }

  /**
   * Swaps the elements at two locations of an array
   *
   * @param i the first index
   * @param j the second index
   * @param a the array
   */
  private static void swap(int i, int j, char[] a)
  {
    char c = a[i];
    a[i] = a[j];
    a[j] = c;
  }

  /**
   * Swaps two ranges of an array.
   *
   * @param i the first range start
   * @param j the second range start
   * @param n the element count
   * @param a the array
   */
  private static void vecswap(int i, int j, int n, char[] a)
  {
    for ( ; n > 0; i++, j++, n--)
      swap(i, j, a);
  }

  /**
   * Performs a recursive modified quicksort.
   *
   * @param array the array to sort
   * @param from the start index (inclusive)
   * @param count the number of elements to sort
   */
  private static void qsort(char[] array, int from, int count)
  {
    // Use an insertion sort on small arrays.
    if (count <= 7)
      {
        for (int i = from + 1; i < from + count; i++)
          for (int j = i; j > from && array[j - 1] > array[j]; j--)
            swap(j, j - 1, array);
        return;
      }

    // Determine a good median element.
    int mid = from + count / 2;
    int lo = from;
    int hi = from + count - 1;

    if (count > 40)
      { // big arrays, pseudomedian of 9
        int s = count / 8;
        lo = med3(lo, lo + s, lo + 2 * s, array);
        mid = med3(mid - s, mid, mid + s, array);
        hi = med3(hi - 2 * s, hi - s, hi, array);
      }
    mid = med3(lo, mid, hi, array);

    int a, b, c, d;
    int comp;

    // Pull the median element out of the fray, and use it as a pivot.
    swap(from, mid, array);
    a = b = from;
    c = d = from + count - 1;

    // Repeatedly move b and c to each other, swapping elements so
    // that all elements before index b are less than the pivot, and all
    // elements after index c are greater than the pivot. a and b track
    // the elements equal to the pivot.
    while (true)
      {
        while (b <= c && (comp = array[b] - array[from]) <= 0)
          {
            if (comp == 0)
              {
                swap(a, b, array);
                a++;
              }
            b++;
          }
        while (c >= b && (comp = array[c] - array[from]) >= 0)
          {
            if (comp == 0)
              {
                swap(c, d, array);
                d--;
              }
            c--;
          }
        if (b > c)
          break;
        swap(b, c, array);
        b++;
        c--;
      }

    // Swap pivot(s) back in place, the recurse on left and right sections.
    hi = from + count;
    int span;
    span = Math.min(a - from, b - a);
    vecswap(from, b - span, span, array);

    span = Math.min(d - c, hi - d - 1);
    vecswap(b, hi - span, span, array);

    span = b - a;
    if (span > 1)
      qsort(array, from, span);

    span = d - c;
    if (span > 1)
      qsort(array, hi - span, span);
  }

  /**
   * Performs a stable sort on the elements, arranging them according to their
   * natural order.
   *
   * @param a the short array to sort
   */
  public static void sort(short[] a)
  {
    qsort(a, 0, a.length);
  }

  /**
   * Performs a stable sort on the elements, arranging them according to their
   * natural order.
   *
   * @param a the short array to sort
   * @param fromIndex the first index to sort (inclusive)
   * @param toIndex the last index to sort (exclusive)
   * @throws IllegalArgumentException if fromIndex &gt; toIndex
   * @throws ArrayIndexOutOfBoundsException if fromIndex &lt; 0
   *         || toIndex &gt; a.length
   */
  public static void sort(short[] a, int fromIndex, int toIndex)
  {
    if (fromIndex > toIndex)
      throw new IllegalArgumentException();
    if (fromIndex < 0)
      throw new ArrayIndexOutOfBoundsException();
    qsort(a, fromIndex, toIndex - fromIndex);
  }

  /**
   * Finds the index of the median of three array elements.
   *
   * @param a the first index
   * @param b the second index
   * @param c the third index
   * @param d the array
   * @return the index (a, b, or c) which has the middle value of the three
   */
  private static int med3(int a, int b, int c, short[] d)
  {
    return (d[a] < d[b]
            ? (d[b] < d[c] ? b : d[a] < d[c] ? c : a)
            : (d[b] > d[c] ? b : d[a] > d[c] ? c : a));
  }

  /**
   * Swaps the elements at two locations of an array
   *
   * @param i the first index
   * @param j the second index
   * @param a the array
   */
  private static void swap(int i, int j, short[] a)
  {
    short c = a[i];
    a[i] = a[j];
    a[j] = c;
  }

  /**
   * Swaps two ranges of an array.
   *
   * @param i the first range start
   * @param j the second range start
   * @param n the element count
   * @param a the array
   */
  private static void vecswap(int i, int j, int n, short[] a)
  {
    for ( ; n > 0; i++, j++, n--)
      swap(i, j, a);
  }

  /**
   * Performs a recursive modified quicksort.
   *
   * @param array the array to sort
   * @param from the start index (inclusive)
   * @param count the number of elements to sort
   */
  private static void qsort(short[] array, int from, int count)
  {
    // Use an insertion sort on small arrays.
    if (count <= 7)
      {
        for (int i = from + 1; i < from + count; i++)
	  for (int j = i; j > from && array[j - 1] > array[j]; j--)
	    swap(j, j - 1, array);
        return;
      }

    // Determine a good median element.
    int mid = from + count / 2;
    int lo = from;
    int hi = from + count - 1;

    if (count > 40)
      { // big arrays, pseudomedian of 9
        int s = count / 8;
        lo = med3(lo, lo + s, lo + 2 * s, array);
        mid = med3(mid - s, mid, mid + s, array);
        hi = med3(hi - 2 * s, hi - s, hi, array);
      }
    mid = med3(lo, mid, hi, array);

    int a, b, c, d;
    int comp;

    // Pull the median element out of the fray, and use it as a pivot.
    swap(from, mid, array);
    a = b = from;
    c = d = from + count - 1;

    // Repeatedly move b and c to each other, swapping elements so
    // that all elements before index b are less than the pivot, and all
    // elements after index c are greater than the pivot. a and b track
    // the elements equal to the pivot.
    while (true)
      {
        while (b <= c && (comp = array[b] - array[from]) <= 0)
          {
            if (comp == 0)
              {
                swap(a, b, array);
                a++;
              }
            b++;
          }
        while (c >= b && (comp = array[c] - array[from]) >= 0)
          {
            if (comp == 0)
              {
                swap(c, d, array);
                d--;
              }
            c--;
          }
        if (b > c)
          break;
        swap(b, c, array);
        b++;
        c--;
      }

    // Swap pivot(s) back in place, the recurse on left and right sections.
    hi = from + count;
    int span;
    span = Math.min(a - from, b - a);
    vecswap(from, b - span, span, array);

    span = Math.min(d - c, hi - d - 1);
    vecswap(b, hi - span, span, array);

    span = b - a;
    if (span > 1)
      qsort(array, from, span);

    span = d - c;
    if (span > 1)
      qsort(array, hi - span, span);
  }

  /**
   * Performs a stable sort on the elements, arranging them according to their
   * natural order.
   *
   * @param a the int array to sort
   */
  public static void sort(int[] a)
  {
    qsort(a, 0, a.length);
  }

  /**
   * Performs a stable sort on the elements, arranging them according to their
   * natural order.
   *
   * @param a the int array to sort
   * @param fromIndex the first index to sort (inclusive)
   * @param toIndex the last index to sort (exclusive)
   * @throws IllegalArgumentException if fromIndex &gt; toIndex
   * @throws ArrayIndexOutOfBoundsException if fromIndex &lt; 0
   *         || toIndex &gt; a.length
   */
  public static void sort(int[] a, int fromIndex, int toIndex)
  {
    if (fromIndex > toIndex)
      throw new IllegalArgumentException();
    if (fromIndex < 0)
      throw new ArrayIndexOutOfBoundsException();
    qsort(a, fromIndex, toIndex - fromIndex);
  }

  /**
   * Finds the index of the median of three array elements.
   *
   * @param a the first index
   * @param b the second index
   * @param c the third index
   * @param d the array
   * @return the index (a, b, or c) which has the middle value of the three
   */
  private static int med3(int a, int b, int c, int[] d)
  {
    return (d[a] < d[b]
            ? (d[b] < d[c] ? b : d[a] < d[c] ? c : a)
            : (d[b] > d[c] ? b : d[a] > d[c] ? c : a));
  }

  /**
   * Swaps the elements at two locations of an array
   *
   * @param i the first index
   * @param j the second index
   * @param a the array
   */
  private static void swap(int i, int j, int[] a)
  {
    int c = a[i];
    a[i] = a[j];
    a[j] = c;
  }

  /**
   * Swaps two ranges of an array.
   *
   * @param i the first range start
   * @param j the second range start
   * @param n the element count
   * @param a the array
   */
  private static void vecswap(int i, int j, int n, int[] a)
  {
    for ( ; n > 0; i++, j++, n--)
      swap(i, j, a);
  }

  /**
   * Compares two integers in natural order, since a - b is inadequate.
   *
   * @param a the first int
   * @param b the second int
   * @return &lt; 0, 0, or &gt; 0 accorting to the comparison
   */
  private static int compare(int a, int b)
  {
    return a < b ? -1 : a == b ? 0 : 1;
  }

  /**
   * Performs a recursive modified quicksort.
   *
   * @param array the array to sort
   * @param from the start index (inclusive)
   * @param count the number of elements to sort
   */
  private static void qsort(int[] array, int from, int count)
  {
    // Use an insertion sort on small arrays.
    if (count <= 7)
      {
        for (int i = from + 1; i < from + count; i++)
          for (int j = i; j > from && array[j - 1] > array[j]; j--)
            swap(j, j - 1, array);
        return;
      }

    // Determine a good median element.
    int mid = from + count / 2;
    int lo = from;
    int hi = from + count - 1;

    if (count > 40)
      { // big arrays, pseudomedian of 9
        int s = count / 8;
        lo = med3(lo, lo + s, lo + 2 * s, array);
        mid = med3(mid - s, mid, mid + s, array);
        hi = med3(hi - 2 * s, hi - s, hi, array);
      }
    mid = med3(lo, mid, hi, array);

    int a, b, c, d;
    int comp;

    // Pull the median element out of the fray, and use it as a pivot.
    swap(from, mid, array);
    a = b = from;
    c = d = from + count - 1;

    // Repeatedly move b and c to each other, swapping elements so
    // that all elements before index b are less than the pivot, and all
    // elements after index c are greater than the pivot. a and b track
    // the elements equal to the pivot.
    while (true)
      {
        while (b <= c && (comp = compare(array[b], array[from])) <= 0)
          {
            if (comp == 0)
              {
                swap(a, b, array);
                a++;
              }
            b++;
          }
        while (c >= b && (comp = compare(array[c], array[from])) >= 0)
          {
            if (comp == 0)
              {
                swap(c, d, array);
                d--;
              }
            c--;
          }
        if (b > c)
          break;
        swap(b, c, array);
        b++;
        c--;
      }

    // Swap pivot(s) back in place, the recurse on left and right sections.
    hi = from + count;
    int span;
    span = Math.min(a - from, b - a);
    vecswap(from, b - span, span, array);

    span = Math.min(d - c, hi - d - 1);
    vecswap(b, hi - span, span, array);

    span = b - a;
    if (span > 1)
      qsort(array, from, span);

    span = d - c;
    if (span > 1)
      qsort(array, hi - span, span);
  }

  /**
   * Performs a stable sort on the elements, arranging them according to their
   * natural order.
   *
   * @param a the long array to sort
   */
  public static void sort(long[] a)
  {
    qsort(a, 0, a.length);
  }

  /**
   * Performs a stable sort on the elements, arranging them according to their
   * natural order.
   *
   * @param a the long array to sort
   * @param fromIndex the first index to sort (inclusive)
   * @param toIndex the last index to sort (exclusive)
   * @throws IllegalArgumentException if fromIndex &gt; toIndex
   * @throws ArrayIndexOutOfBoundsException if fromIndex &lt; 0
   *         || toIndex &gt; a.length
   */
  public static void sort(long[] a, int fromIndex, int toIndex)
  {
    if (fromIndex > toIndex)
      throw new IllegalArgumentException();
    if (fromIndex < 0)
      throw new ArrayIndexOutOfBoundsException();
    qsort(a, fromIndex, toIndex - fromIndex);
  }

  /**
   * Finds the index of the median of three array elements.
   *
   * @param a the first index
   * @param b the second index
   * @param c the third index
   * @param d the array
   * @return the index (a, b, or c) which has the middle value of the three
   */
  private static int med3(int a, int b, int c, long[] d)
  {
    return (d[a] < d[b]
            ? (d[b] < d[c] ? b : d[a] < d[c] ? c : a)
            : (d[b] > d[c] ? b : d[a] > d[c] ? c : a));
  }

  /**
   * Swaps the elements at two locations of an array
   *
   * @param i the first index
   * @param j the second index
   * @param a the array
   */
  private static void swap(int i, int j, long[] a)
  {
    long c = a[i];
    a[i] = a[j];
    a[j] = c;
  }

  /**
   * Swaps two ranges of an array.
   *
   * @param i the first range start
   * @param j the second range start
   * @param n the element count
   * @param a the array
   */
  private static void vecswap(int i, int j, int n, long[] a)
  {
    for ( ; n > 0; i++, j++, n--)
      swap(i, j, a);
  }

  /**
   * Compares two longs in natural order, since a - b is inadequate.
   *
   * @param a the first long
   * @param b the second long
   * @return &lt; 0, 0, or &gt; 0 accorting to the comparison
   */
  private static int compare(long a, long b)
  {
    return a < b ? -1 : a == b ? 0 : 1;
  }

  /**
   * Performs a recursive modified quicksort.
   *
   * @param array the array to sort
   * @param from the start index (inclusive)
   * @param count the number of elements to sort
   */
  private static void qsort(long[] array, int from, int count)
  {
    // Use an insertion sort on small arrays.
    if (count <= 7)
      {
        for (int i = from + 1; i < from + count; i++)
          for (int j = i; j > from && array[j - 1] > array[j]; j--)
            swap(j, j - 1, array);
        return;
      }

    // Determine a good median element.
    int mid = from + count / 2;
    int lo = from;
    int hi = from + count - 1;

    if (count > 40)
      { // big arrays, pseudomedian of 9
        int s = count / 8;
        lo = med3(lo, lo + s, lo + 2 * s, array);
        mid = med3(mid - s, mid, mid + s, array);
        hi = med3(hi - 2 * s, hi - s, hi, array);
      }
    mid = med3(lo, mid, hi, array);

    int a, b, c, d;
    int comp;

    // Pull the median element out of the fray, and use it as a pivot.
    swap(from, mid, array);
    a = b = from;
    c = d = from + count - 1;

    // Repeatedly move b and c to each other, swapping elements so
    // that all elements before index b are less than the pivot, and all
    // elements after index c are greater than the pivot. a and b track
    // the elements equal to the pivot.
    while (true)
      {
        while (b <= c && (comp = compare(array[b], array[from])) <= 0)
          {
            if (comp == 0)
              {
                swap(a, b, array);
                a++;
              }
            b++;
          }
        while (c >= b && (comp = compare(array[c], array[from])) >= 0)
          {
            if (comp == 0)
              {
                swap(c, d, array);
                d--;
              }
            c--;
          }
        if (b > c)
          break;
        swap(b, c, array);
        b++;
        c--;
      }

    // Swap pivot(s) back in place, the recurse on left and right sections.
    hi = from + count;
    int span;
    span = Math.min(a - from, b - a);
    vecswap(from, b - span, span, array);

    span = Math.min(d - c, hi - d - 1);
    vecswap(b, hi - span, span, array);

    span = b - a;
    if (span > 1)
      qsort(array, from, span);

    span = d - c;
    if (span > 1)
      qsort(array, hi - span, span);
  }

  /**
   * Performs a stable sort on the elements, arranging them according to their
   * natural order.
   *
   * @param a the float array to sort
   */
  public static void sort(float[] a)
  {
    qsort(a, 0, a.length);
  }

  /**
   * Performs a stable sort on the elements, arranging them according to their
   * natural order.
   *
   * @param a the float array to sort
   * @param fromIndex the first index to sort (inclusive)
   * @param toIndex the last index to sort (exclusive)
   * @throws IllegalArgumentException if fromIndex &gt; toIndex
   * @throws ArrayIndexOutOfBoundsException if fromIndex &lt; 0
   *         || toIndex &gt; a.length
   */
  public static void sort(float[] a, int fromIndex, int toIndex)
  {
    if (fromIndex > toIndex)
      throw new IllegalArgumentException();
    if (fromIndex < 0)
      throw new ArrayIndexOutOfBoundsException();
    qsort(a, fromIndex, toIndex - fromIndex);
  }

  /**
   * Finds the index of the median of three array elements.
   *
   * @param a the first index
   * @param b the second index
   * @param c the third index
   * @param d the array
   * @return the index (a, b, or c) which has the middle value of the three
   */
  private static int med3(int a, int b, int c, float[] d)
  {
    return (Float.compare(d[a], d[b]) < 0
            ? (Float.compare(d[b], d[c]) < 0 ? b
               : Float.compare(d[a], d[c]) < 0 ? c : a)
            : (Float.compare(d[b], d[c]) > 0 ? b
               : Float.compare(d[a], d[c]) > 0 ? c : a));
  }

  /**
   * Swaps the elements at two locations of an array
   *
   * @param i the first index
   * @param j the second index
   * @param a the array
   */
  private static void swap(int i, int j, float[] a)
  {
    float c = a[i];
    a[i] = a[j];
    a[j] = c;
  }

  /**
   * Swaps two ranges of an array.
   *
   * @param i the first range start
   * @param j the second range start
   * @param n the element count
   * @param a the array
   */
  private static void vecswap(int i, int j, int n, float[] a)
  {
    for ( ; n > 0; i++, j++, n--)
      swap(i, j, a);
  }

  /**
   * Performs a recursive modified quicksort.
   *
   * @param array the array to sort
   * @param from the start index (inclusive)
   * @param count the number of elements to sort
   */
  private static void qsort(float[] array, int from, int count)
  {
    // Use an insertion sort on small arrays.
    if (count <= 7)
      {
        for (int i = from + 1; i < from + count; i++)
          for (int j = i;
               j > from && Float.compare(array[j - 1], array[j]) > 0;
               j--)
            {
              swap(j, j - 1, array);
            }
        return;
      }

    // Determine a good median element.
    int mid = from + count / 2;
    int lo = from;
    int hi = from + count - 1;

    if (count > 40)
      { // big arrays, pseudomedian of 9
        int s = count / 8;
        lo = med3(lo, lo + s, lo + 2 * s, array);
        mid = med3(mid - s, mid, mid + s, array);
        hi = med3(hi - 2 * s, hi - s, hi, array);
      }
    mid = med3(lo, mid, hi, array);

    int a, b, c, d;
    int comp;

    // Pull the median element out of the fray, and use it as a pivot.
    swap(from, mid, array);
    a = b = from;
    c = d = from + count - 1;

    // Repeatedly move b and c to each other, swapping elements so
    // that all elements before index b are less than the pivot, and all
    // elements after index c are greater than the pivot. a and b track
    // the elements equal to the pivot.
    while (true)
      {
        while (b <= c && (comp = Float.compare(array[b], array[from])) <= 0)
          {
            if (comp == 0)
              {
                swap(a, b, array);
                a++;
              }
            b++;
          }
        while (c >= b && (comp = Float.compare(array[c], array[from])) >= 0)
          {
            if (comp == 0)
              {
                swap(c, d, array);
                d--;
              }
            c--;
          }
        if (b > c)
          break;
        swap(b, c, array);
        b++;
        c--;
      }

    // Swap pivot(s) back in place, the recurse on left and right sections.
    hi = from + count;
    int span;
    span = Math.min(a - from, b - a);
    vecswap(from, b - span, span, array);

    span = Math.min(d - c, hi - d - 1);
    vecswap(b, hi - span, span, array);

    span = b - a;
    if (span > 1)
      qsort(array, from, span);

    span = d - c;
    if (span > 1)
      qsort(array, hi - span, span);
  }

  /**
   * Performs a stable sort on the elements, arranging them according to their
   * natural order.
   *
   * @param a the double array to sort
   */
  public static void sort(double[] a)
  {
    qsort(a, 0, a.length);
  }

  /**
   * Performs a stable sort on the elements, arranging them according to their
   * natural order.
   *
   * @param a the double array to sort
   * @param fromIndex the first index to sort (inclusive)
   * @param toIndex the last index to sort (exclusive)
   * @throws IllegalArgumentException if fromIndex &gt; toIndex
   * @throws ArrayIndexOutOfBoundsException if fromIndex &lt; 0
   *         || toIndex &gt; a.length
   */
  public static void sort(double[] a, int fromIndex, int toIndex)
  {
    if (fromIndex > toIndex)
      throw new IllegalArgumentException();
    if (fromIndex < 0)
      throw new ArrayIndexOutOfBoundsException();
    qsort(a, fromIndex, toIndex - fromIndex);
  }

  /**
   * Finds the index of the median of three array elements.
   *
   * @param a the first index
   * @param b the second index
   * @param c the third index
   * @param d the array
   * @return the index (a, b, or c) which has the middle value of the three
   */
  private static int med3(int a, int b, int c, double[] d)
  {
    return (Double.compare(d[a], d[b]) < 0
            ? (Double.compare(d[b], d[c]) < 0 ? b
               : Double.compare(d[a], d[c]) < 0 ? c : a)
            : (Double.compare(d[b], d[c]) > 0 ? b
               : Double.compare(d[a], d[c]) > 0 ? c : a));
  }

  /**
   * Swaps the elements at two locations of an array
   *
   * @param i the first index
   * @param j the second index
   * @param a the array
   */
  private static void swap(int i, int j, double[] a)
  {
    double c = a[i];
    a[i] = a[j];
    a[j] = c;
  }

  /**
   * Swaps two ranges of an array.
   *
   * @param i the first range start
   * @param j the second range start
   * @param n the element count
   * @param a the array
   */
  private static void vecswap(int i, int j, int n, double[] a)
  {
    for ( ; n > 0; i++, j++, n--)
      swap(i, j, a);
  }

  /**
   * Performs a recursive modified quicksort.
   *
   * @param array the array to sort
   * @param from the start index (inclusive)
   * @param count the number of elements to sort
   */
  private static void qsort(double[] array, int from, int count)
  {
    // Use an insertion sort on small arrays.
    if (count <= 7)
      {
        for (int i = from + 1; i < from + count; i++)
          for (int j = i;
               j > from && Double.compare(array[j - 1], array[j]) > 0;
               j--)
            {
              swap(j, j - 1, array);
            }
        return;
      }

    // Determine a good median element.
    int mid = from + count / 2;
    int lo = from;
    int hi = from + count - 1;

    if (count > 40)
      { // big arrays, pseudomedian of 9
        int s = count / 8;
        lo = med3(lo, lo + s, lo + 2 * s, array);
        mid = med3(mid - s, mid, mid + s, array);
        hi = med3(hi - 2 * s, hi - s, hi, array);
      }
    mid = med3(lo, mid, hi, array);

    int a, b, c, d;
    int comp;

    // Pull the median element out of the fray, and use it as a pivot.
    swap(from, mid, array);
    a = b = from;
    c = d = from + count - 1;

    // Repeatedly move b and c to each other, swapping elements so
    // that all elements before index b are less than the pivot, and all
    // elements after index c are greater than the pivot. a and b track
    // the elements equal to the pivot.
    while (true)
      {
        while (b <= c && (comp = Double.compare(array[b], array[from])) <= 0)
          {
            if (comp == 0)
              {
                swap(a, b, array);
                a++;
              }
            b++;
          }
        while (c >= b && (comp = Double.compare(array[c], array[from])) >= 0)
          {
            if (comp == 0)
              {
                swap(c, d, array);
                d--;
              }
            c--;
          }
        if (b > c)
          break;
        swap(b, c, array);
        b++;
        c--;
      }

    // Swap pivot(s) back in place, the recurse on left and right sections.
    hi = from + count;
    int span;
    span = Math.min(a - from, b - a);
    vecswap(from, b - span, span, array);

    span = Math.min(d - c, hi - d - 1);
    vecswap(b, hi - span, span, array);

    span = b - a;
    if (span > 1)
      qsort(array, from, span);

    span = d - c;
    if (span > 1)
      qsort(array, hi - span, span);
  }

  /**
   * Sort an array of Objects according to their natural ordering. The sort is
   * guaranteed to be stable, that is, equal elements will not be reordered.
   * The sort algorithm is a mergesort with the merge omitted if the last
   * element of one half comes before the first element of the other half. This
   * algorithm gives guaranteed O(n*log(n)) time, at the expense of making a
   * copy of the array.
   *
   * @param a the array to be sorted
   * @throws ClassCastException if any two elements are not mutually
   *         comparable
   * @throws NullPointerException if an element is null (since
   *         null.compareTo cannot work)
   * @see Comparable
   */
  public static void sort(Object[] a)
  {
    sort(a, 0, a.length, null);
  }

  /**
   * Sort an array of Objects according to a Comparator. The sort is
   * guaranteed to be stable, that is, equal elements will not be reordered.
   * The sort algorithm is a mergesort with the merge omitted if the last
   * element of one half comes before the first element of the other half. This
   * algorithm gives guaranteed O(n*log(n)) time, at the expense of making a
   * copy of the array.
   *
   * @param a the array to be sorted
   * @param c a Comparator to use in sorting the array; or null to indicate
   *        the elements' natural order
   * @throws ClassCastException if any two elements are not mutually
   *         comparable by the Comparator provided
   * @throws NullPointerException if a null element is compared with natural
   *         ordering (only possible when c is null)
   */
  public static <T> void sort(T[] a, Comparator<? super T> c)
  {
    sort(a, 0, a.length, c);
  }

  /**
   * Sort an array of Objects according to their natural ordering. The sort is
   * guaranteed to be stable, that is, equal elements will not be reordered.
   * The sort algorithm is a mergesort with the merge omitted if the last
   * element of one half comes before the first element of the other half. This
   * algorithm gives guaranteed O(n*log(n)) time, at the expense of making a
   * copy of the array.
   *
   * @param a the array to be sorted
   * @param fromIndex the index of the first element to be sorted
   * @param toIndex the index of the last element to be sorted plus one
   * @throws ClassCastException if any two elements are not mutually
   *         comparable
   * @throws NullPointerException if an element is null (since
   *         null.compareTo cannot work)
   * @throws ArrayIndexOutOfBoundsException if fromIndex and toIndex
   *         are not in range.
   * @throws IllegalArgumentException if fromIndex &gt; toIndex
   */
  public static void sort(Object[] a, int fromIndex, int toIndex)
  {
    sort(a, fromIndex, toIndex, null);
  }

  /**
   * Sort an array of Objects according to a Comparator. The sort is
   * guaranteed to be stable, that is, equal elements will not be reordered.
   * The sort algorithm is a mergesort with the merge omitted if the last
   * element of one half comes before the first element of the other half. This
   * algorithm gives guaranteed O(n*log(n)) time, at the expense of making a
   * copy of the array.
   *
   * @param a the array to be sorted
   * @param fromIndex the index of the first element to be sorted
   * @param toIndex the index of the last element to be sorted plus one
   * @param c a Comparator to use in sorting the array; or null to indicate
   *        the elements' natural order
   * @throws ClassCastException if any two elements are not mutually
   *         comparable by the Comparator provided
   * @throws ArrayIndexOutOfBoundsException if fromIndex and toIndex
   *         are not in range.
   * @throws IllegalArgumentException if fromIndex &gt; toIndex
   * @throws NullPointerException if a null element is compared with natural
   *         ordering (only possible when c is null)
   */
  public static <T> void sort(T[] a, int fromIndex, int toIndex,
			      Comparator<? super T> c)
  {
    if (fromIndex > toIndex)
      throw new IllegalArgumentException("fromIndex " + fromIndex
                                         + " > toIndex " + toIndex);
    if (fromIndex < 0)
      throw new ArrayIndexOutOfBoundsException();

    // In general, the code attempts to be simple rather than fast, the
    // idea being that a good optimising JIT will be able to optimise it
    // better than I can, and if I try it will make it more confusing for
    // the JIT. First presort the array in chunks of length 6 with insertion
    // sort. A mergesort would give too much overhead for this length.
    for (int chunk = fromIndex; chunk < toIndex; chunk += 6)
      {
        int end = Math.min(chunk + 6, toIndex);
        for (int i = chunk + 1; i < end; i++)
          {
            if (Collections.compare(a[i - 1], a[i], c) > 0)
              {
                // not already sorted
                int j = i;
                T elem = a[j];
                do
                  {
                    a[j] = a[j - 1];
                    j--;
                  }
                while (j > chunk
                       && Collections.compare(a[j - 1], elem, c) > 0);
                a[j] = elem;
              }
          }
      }

    int len = toIndex - fromIndex;
    // If length is smaller or equal 6 we are done.
    if (len <= 6)
      return;

    T[] src = a;
    T[] dest = (T[]) new Object[len];
    T[] t = null; // t is used for swapping src and dest

    // The difference of the fromIndex of the src and dest array.
    int srcDestDiff = -fromIndex;

    // The merges are done in this loop
    for (int size = 6; size < len; size <<= 1)
      {
        for (int start = fromIndex; start < toIndex; start += size << 1)
          {
            // mid is the start of the second sublist;
            // end the start of the next sublist (or end of array).
            int mid = start + size;
            int end = Math.min(toIndex, mid + size);

            // The second list is empty or the elements are already in
            // order - no need to merge
            if (mid >= end
                || Collections.compare(src[mid - 1], src[mid], c) <= 0)
              {
                System.arraycopy(src, start,
                                 dest, start + srcDestDiff, end - start);

                // The two halves just need swapping - no need to merge
              }
            else if (Collections.compare(src[start], src[end - 1], c) > 0)
              {
                System.arraycopy(src, start,
                                 dest, end - size + srcDestDiff, size);
                System.arraycopy(src, mid,
                                 dest, start + srcDestDiff, end - mid);

              }
            else
              {
                // Declare a lot of variables to save repeating
                // calculations.  Hopefully a decent JIT will put these
                // in registers and make this fast
                int p1 = start;
                int p2 = mid;
                int i = start + srcDestDiff;

                // The main merge loop; terminates as soon as either
                // half is ended
                while (p1 < mid && p2 < end)
                  {
                    dest[i++] =
                      src[(Collections.compare(src[p1], src[p2], c) <= 0
                           ? p1++ : p2++)];
                  }

                // Finish up by copying the remainder of whichever half
                // wasn't finished.
                if (p1 < mid)
                  System.arraycopy(src, p1, dest, i, mid - p1);
                else
                  System.arraycopy(src, p2, dest, i, end - p2);
              }
          }
        // swap src and dest ready for the next merge
        t = src;
        src = dest;
        dest = t;
        fromIndex += srcDestDiff;
        toIndex += srcDestDiff;
        srcDestDiff = -srcDestDiff;
      }

    // make sure the result ends up back in the right place.  Note
    // that src and dest may have been swapped above, so src
    // contains the sorted array.
    if (src != a)
      {
        // Note that fromIndex == 0.
        System.arraycopy(src, 0, a, srcDestDiff, toIndex);
      }
  }

  /**
   * Returns a list "view" of the specified array. This method is intended to
   * make it easy to use the Collections API with existing array-based APIs and
   * programs. Changes in the list or the array show up in both places. The
   * list does not support element addition or removal, but does permit
   * value modification. The returned list implements both Serializable and
   * RandomAccess.
   *
   * @param a the array to return a view of (<code>null</code> not permitted)
   * @return a fixed-size list, changes to which "write through" to the array
   * 
   * @throws NullPointerException if <code>a</code> is <code>null</code>.
   * @see Serializable
   * @see RandomAccess
   * @see Arrays.ArrayList
   */
  public static <T> List<T> asList(final T... a)
  {
    return new Arrays.ArrayList(a);
  }

  /** 
   * Returns the hashcode of an array of long numbers.  If two arrays
   * are equal, according to <code>equals()</code>, they should have the
   * same hashcode.  The hashcode returned by the method is equal to that
   * obtained by the corresponding <code>List</code> object.  This has the same
   * data, but represents longs in their wrapper class, <code>Long</code>.
   * For <code>null</code>, 0 is returned.
   *
   * @param v an array of long numbers for which the hash code should be
   *          computed.
   * @return the hash code of the array, or 0 if null was given.
   * @since 1.5 
   */
  public static int hashCode(long[] v)
  {
    if (v == null)
      return 0;
    int result = 1;
    for (int i = 0; i < v.length; ++i)
      {
	int elt = (int) (v[i] ^ (v[i] >>> 32));
	result = 31 * result + elt;
      }
    return result;
  }

  /** 
   * Returns the hashcode of an array of integer numbers.  If two arrays
   * are equal, according to <code>equals()</code>, they should have the
   * same hashcode.  The hashcode returned by the method is equal to that
   * obtained by the corresponding <code>List</code> object.  This has the same
   * data, but represents ints in their wrapper class, <code>Integer</code>.
   * For <code>null</code>, 0 is returned.
   *
   * @param v an array of integer numbers for which the hash code should be
   *          computed.
   * @return the hash code of the array, or 0 if null was given.
   * @since 1.5 
   */
  public static int hashCode(int[] v)
  {
    if (v == null)
      return 0;
    int result = 1;
    for (int i = 0; i < v.length; ++i)
      result = 31 * result + v[i];
    return result;
  }

  /** 
   * Returns the hashcode of an array of short numbers.  If two arrays
   * are equal, according to <code>equals()</code>, they should have the
   * same hashcode.  The hashcode returned by the method is equal to that
   * obtained by the corresponding <code>List</code> object.  This has the same
   * data, but represents shorts in their wrapper class, <code>Short</code>.
   * For <code>null</code>, 0 is returned.
   *
   * @param v an array of short numbers for which the hash code should be
   *          computed.
   * @return the hash code of the array, or 0 if null was given.
   * @since 1.5 
   */
  public static int hashCode(short[] v)
  {
    if (v == null)
      return 0;
    int result = 1;
    for (int i = 0; i < v.length; ++i)
      result = 31 * result + v[i];
    return result;
  }

  /** 
   * Returns the hashcode of an array of characters.  If two arrays
   * are equal, according to <code>equals()</code>, they should have the
   * same hashcode.  The hashcode returned by the method is equal to that
   * obtained by the corresponding <code>List</code> object.  This has the same
   * data, but represents chars in their wrapper class, <code>Character</code>.
   * For <code>null</code>, 0 is returned.
   *
   * @param v an array of characters for which the hash code should be
   *          computed.
   * @return the hash code of the array, or 0 if null was given.
   * @since 1.5 
   */
  public static int hashCode(char[] v)
  {
    if (v == null)
      return 0;
    int result = 1;
    for (int i = 0; i < v.length; ++i)
      result = 31 * result + v[i];
    return result;
  }

  /** 
   * Returns the hashcode of an array of bytes.  If two arrays
   * are equal, according to <code>equals()</code>, they should have the
   * same hashcode.  The hashcode returned by the method is equal to that
   * obtained by the corresponding <code>List</code> object.  This has the same
   * data, but represents bytes in their wrapper class, <code>Byte</code>.
   * For <code>null</code>, 0 is returned.
   *
   * @param v an array of bytes for which the hash code should be
   *          computed.
   * @return the hash code of the array, or 0 if null was given.
   * @since 1.5 
   */
  public static int hashCode(byte[] v)
  {
    if (v == null)
      return 0;
    int result = 1;
    for (int i = 0; i < v.length; ++i)
      result = 31 * result + v[i];
    return result;
  }

  /** 
   * Returns the hashcode of an array of booleans.  If two arrays
   * are equal, according to <code>equals()</code>, they should have the
   * same hashcode.  The hashcode returned by the method is equal to that
   * obtained by the corresponding <code>List</code> object.  This has the same
   * data, but represents booleans in their wrapper class,
   * <code>Boolean</code>.  For <code>null</code>, 0 is returned.
   *
   * @param v an array of booleans for which the hash code should be
   *          computed.
   * @return the hash code of the array, or 0 if null was given.
   * @since 1.5 
   */
  public static int hashCode(boolean[] v)
  {
    if (v == null)
      return 0;
    int result = 1;
    for (int i = 0; i < v.length; ++i)
      result = 31 * result + (v[i] ? 1231 : 1237);
    return result;
  }

  /** 
   * Returns the hashcode of an array of floats.  If two arrays
   * are equal, according to <code>equals()</code>, they should have the
   * same hashcode.  The hashcode returned by the method is equal to that
   * obtained by the corresponding <code>List</code> object.  This has the same
   * data, but represents floats in their wrapper class, <code>Float</code>.
   * For <code>null</code>, 0 is returned.
   *
   * @param v an array of floats for which the hash code should be
   *          computed.
   * @return the hash code of the array, or 0 if null was given.
   * @since 1.5 
   */
  public static int hashCode(float[] v)
  {
    if (v == null)
      return 0;
    int result = 1;
    for (int i = 0; i < v.length; ++i)
      result = 31 * result + Float.floatToIntBits(v[i]);
    return result;
  }

  /** 
   * Returns the hashcode of an array of doubles.  If two arrays
   * are equal, according to <code>equals()</code>, they should have the
   * same hashcode.  The hashcode returned by the method is equal to that
   * obtained by the corresponding <code>List</code> object.  This has the same
   * data, but represents doubles in their wrapper class, <code>Double</code>.
   * For <code>null</code>, 0 is returned.
   *
   * @param v an array of doubles for which the hash code should be
   *          computed.
   * @return the hash code of the array, or 0 if null was given.
   * @since 1.5 
   */
  public static int hashCode(double[] v)
  {
    if (v == null)
      return 0;
    int result = 1;
    for (int i = 0; i < v.length; ++i)
      {
	long l = Double.doubleToLongBits(v[i]);
	int elt = (int) (l ^ (l >>> 32));
	result = 31 * result + elt;
      }
    return result;
  }

  /** 
   * Returns the hashcode of an array of objects.  If two arrays
   * are equal, according to <code>equals()</code>, they should have the
   * same hashcode.  The hashcode returned by the method is equal to that
   * obtained by the corresponding <code>List</code> object.  
   * For <code>null</code>, 0 is returned.
   *
   * @param v an array of integer numbers for which the hash code should be
   *          computed.
   * @return the hash code of the array, or 0 if null was given.
   * @since 1.5 
   */
  public static int hashCode(Object[] v)
  {
    if (v == null)
      return 0;
    int result = 1;
    for (int i = 0; i < v.length; ++i)
      {
	int elt = v[i] == null ? 0 : v[i].hashCode();
	result = 31 * result + elt;
      }
    return result;
  }

  public static int deepHashCode(Object[] v)
  {
    if (v == null)
      return 0;
    int result = 1;
    for (int i = 0; i < v.length; ++i)
      {
	int elt;
	if (v[i] == null)
	  elt = 0;
	else if (v[i] instanceof boolean[])
	  elt = hashCode((boolean[]) v[i]);
	else if (v[i] instanceof byte[])
	  elt = hashCode((byte[]) v[i]);
	else if (v[i] instanceof char[])
	  elt = hashCode((char[]) v[i]);
	else if (v[i] instanceof short[])
	  elt = hashCode((short[]) v[i]);
	else if (v[i] instanceof int[])
	  elt = hashCode((int[]) v[i]);
	else if (v[i] instanceof long[])
	  elt = hashCode((long[]) v[i]);
	else if (v[i] instanceof float[])
	  elt = hashCode((float[]) v[i]);
	else if (v[i] instanceof double[])
	  elt = hashCode((double[]) v[i]);
	else if (v[i] instanceof Object[])
	  elt = hashCode((Object[]) v[i]);
	else
	  elt = v[i].hashCode();
	result = 31 * result + elt;
      }
    return result;
  }

  /** @since 1.5 */
  public static boolean deepEquals(Object[] v1, Object[] v2)
  {
    if (v1 == null)
      return v2 == null;
    if (v2 == null || v1.length != v2.length)
      return false;

    for (int i = 0; i < v1.length; ++i)
      {
	Object e1 = v1[i];
	Object e2 = v2[i];

	if (e1 == e2)
	  continue;
	if (e1 == null || e2 == null)
	  return false;

	boolean check;
	if (e1 instanceof boolean[] && e2 instanceof boolean[])
	  check = equals((boolean[]) e1, (boolean[]) e2);
	else if (e1 instanceof byte[] && e2 instanceof byte[])
	  check = equals((byte[]) e1, (byte[]) e2);
	else if (e1 instanceof char[] && e2 instanceof char[])
	  check = equals((char[]) e1, (char[]) e2);
	else if (e1 instanceof short[] && e2 instanceof short[])
	  check = equals((short[]) e1, (short[]) e2);
	else if (e1 instanceof int[] && e2 instanceof int[])
	  check = equals((int[]) e1, (int[]) e2);
	else if (e1 instanceof long[] && e2 instanceof long[])
	  check = equals((long[]) e1, (long[]) e2);
	else if (e1 instanceof float[] && e2 instanceof float[])
	  check = equals((float[]) e1, (float[]) e2);
	else if (e1 instanceof double[] && e2 instanceof double[])
	  check = equals((double[]) e1, (double[]) e2);
	else if (e1 instanceof Object[] && e2 instanceof Object[])
	  check = equals((Object[]) e1, (Object[]) e2);
	else
	  check = e1.equals(e2);
	if (! check)
	  return false;
      }

    return true;
  }

  /**
   * Returns a String representation of the argument array.  Returns "null"
   * if <code>a</code> is null.
   * @param v the array to represent
   * @return a String representing this array
   * @since 1.5
   */
  public static String toString(boolean[] v)
  {
    if (v == null)
      return "null";
    CPStringBuilder b = new CPStringBuilder("[");
    for (int i = 0; i < v.length; ++i)
      {
	if (i > 0)
	  b.append(", ");
	b.append(v[i]);
      }
    b.append("]");
    return b.toString();
  }

  /**
   * Returns a String representation of the argument array.  Returns "null"
   * if <code>a</code> is null.
   * @param v the array to represent
   * @return a String representing this array
   * @since 1.5
   */
  public static String toString(byte[] v)
  {
    if (v == null)
      return "null";
    CPStringBuilder b = new CPStringBuilder("[");
    for (int i = 0; i < v.length; ++i)
      {
	if (i > 0)
	  b.append(", ");
	b.append(v[i]);
      }
    b.append("]");
    return b.toString();
  }

  /**
   * Returns a String representation of the argument array.  Returns "null"
   * if <code>a</code> is null.
   * @param v the array to represent
   * @return a String representing this array
   * @since 1.5
   */
  public static String toString(char[] v)
  {
    if (v == null)
      return "null";
    CPStringBuilder b = new CPStringBuilder("[");
    for (int i = 0; i < v.length; ++i)
      {
	if (i > 0)
	  b.append(", ");
	b.append(v[i]);
      }
    b.append("]");
    return b.toString();
  }

  /**
   * Returns a String representation of the argument array.  Returns "null"
   * if <code>a</code> is null.
   * @param v the array to represent
   * @return a String representing this array
   * @since 1.5
   */
  public static String toString(short[] v)
  {
    if (v == null)
      return "null";
    CPStringBuilder b = new CPStringBuilder("[");
    for (int i = 0; i < v.length; ++i)
      {
	if (i > 0)
	  b.append(", ");
	b.append(v[i]);
      }
    b.append("]");
    return b.toString();
  }

  /**
   * Returns a String representation of the argument array.  Returns "null"
   * if <code>a</code> is null.
   * @param v the array to represent
   * @return a String representing this array
   * @since 1.5
   */
  public static String toString(int[] v)
  {
    if (v == null)
      return "null";
    CPStringBuilder b = new CPStringBuilder("[");
    for (int i = 0; i < v.length; ++i)
      {
	if (i > 0)
	  b.append(", ");
	b.append(v[i]);
      }
    b.append("]");
    return b.toString();
  }

  /**
   * Returns a String representation of the argument array.  Returns "null"
   * if <code>a</code> is null.
   * @param v the array to represent
   * @return a String representing this array
   * @since 1.5
   */
  public static String toString(long[] v)
  {
    if (v == null)
      return "null";
    CPStringBuilder b = new CPStringBuilder("[");
    for (int i = 0; i < v.length; ++i)
      {
	if (i > 0)
	  b.append(", ");
	b.append(v[i]);
      }
    b.append("]");
    return b.toString();
  }

  /**
   * Returns a String representation of the argument array.  Returns "null"
   * if <code>a</code> is null.
   * @param v the array to represent
   * @return a String representing this array
   * @since 1.5
   */
  public static String toString(float[] v)
  {
    if (v == null)
      return "null";
    CPStringBuilder b = new CPStringBuilder("[");
    for (int i = 0; i < v.length; ++i)
      {
	if (i > 0)
	  b.append(", ");
	b.append(v[i]);
      }
    b.append("]");
    return b.toString();
  }

  /**
   * Returns a String representation of the argument array.  Returns "null"
   * if <code>a</code> is null.
   * @param v the array to represent
   * @return a String representing this array
   * @since 1.5
   */
  public static String toString(double[] v)
  {
    if (v == null)
      return "null";
    CPStringBuilder b = new CPStringBuilder("[");
    for (int i = 0; i < v.length; ++i)
      {
	if (i > 0)
	  b.append(", ");
	b.append(v[i]);
      }
    b.append("]");
    return b.toString();
  }

  /**
   * Returns a String representation of the argument array.  Returns "null"
   * if <code>a</code> is null.
   * @param v the array to represent
   * @return a String representing this array
   * @since 1.5
   */
  public static String toString(Object[] v)
  {
    if (v == null)
      return "null";
    CPStringBuilder b = new CPStringBuilder("[");
    for (int i = 0; i < v.length; ++i)
      {
	if (i > 0)
	  b.append(", ");
	b.append(v[i]);
      }
    b.append("]");
    return b.toString();
  }

  private static void deepToString(Object[] v, CPStringBuilder b, HashSet seen)
  {
    b.append("[");
    for (int i = 0; i < v.length; ++i)
      {
	if (i > 0)
	  b.append(", ");
	Object elt = v[i];
	if (elt == null)
	  b.append("null");
	else if (elt instanceof boolean[])
	  b.append(toString((boolean[]) elt));
	else if (elt instanceof byte[])
	  b.append(toString((byte[]) elt));
	else if (elt instanceof char[])
	  b.append(toString((char[]) elt));
	else if (elt instanceof short[])
	  b.append(toString((short[]) elt));
	else if (elt instanceof int[])
	  b.append(toString((int[]) elt));
	else if (elt instanceof long[])
	  b.append(toString((long[]) elt));
	else if (elt instanceof float[])
	  b.append(toString((float[]) elt));
	else if (elt instanceof double[])
	  b.append(toString((double[]) elt));
	else if (elt instanceof Object[])
	  {
	    Object[] os = (Object[]) elt;
	    if (seen.contains(os))
	      b.append("[...]");
	    else
	      {
		seen.add(os);
		deepToString(os, b, seen);
	      }
	  }
	else
	  b.append(elt);
      }
    b.append("]");
  }

  /** @since 1.5 */
  public static String deepToString(Object[] v)
  {
    if (v == null)
      return "null";
    HashSet seen = new HashSet();
    CPStringBuilder b = new CPStringBuilder();
    deepToString(v, b, seen);
    return b.toString();
  }

  /**
   * Inner class used by {@link #asList(Object[])} to provide a list interface
   * to an array. The name, though it clashes with java.util.ArrayList, is
   * Sun's choice for Serialization purposes. Element addition and removal
   * is prohibited, but values can be modified.
   *
   * @author Eric Blake (ebb9@email.byu.edu)
   * @status updated to 1.4
   */
  private static final class ArrayList<E> extends AbstractList<E>
    implements Serializable, RandomAccess
  {
    // We override the necessary methods, plus others which will be much
    // more efficient with direct iteration rather than relying on iterator().

    /**
     * Compatible with JDK 1.4.
     */
    private static final long serialVersionUID = -2764017481108945198L;

    /**
     * The array we are viewing.
     * @serial the array
     */
    private final E[] a;

    /**
     * Construct a list view of the array.
     * @param a the array to view
     * @throws NullPointerException if a is null
     */
    ArrayList(E[] a)
    {
      // We have to explicitly check.
      if (a == null)
        throw new NullPointerException();
      this.a = a;
    }

    /**
     * Returns the object at the specified index in
     * the array.
     *
     * @param index The index to retrieve an object from.
     * @return The object at the array index specified.
     */ 
    public E get(int index)
    {
      return a[index];
    }

    /**
     * Returns the size of the array.
     *
     * @return The size.
     */
    public int size()
    {
      return a.length;
    }

    /**
     * Replaces the object at the specified index
     * with the supplied element.
     *
     * @param index The index at which to place the new object.
     * @param element The new object.
     * @return The object replaced by this operation.
     */
    public E set(int index, E element)
    {
      E old = a[index];
      a[index] = element;
      return old;
    }

    /**
     * Returns true if the array contains the
     * supplied object.
     *
     * @param o The object to look for.
     * @return True if the object was found.
     */
    public boolean contains(Object o)
    {
      return lastIndexOf(o) >= 0;
    }

    /**
     * Returns the first index at which the
     * object, o, occurs in the array.
     *
     * @param o The object to search for.
     * @return The first relevant index.
     */
    public int indexOf(Object o)
    {
      int size = a.length;
      for (int i = 0; i < size; i++)
        if (ArrayList.equals(o, a[i]))
          return i;
      return -1;
    }

    /**
     * Returns the last index at which the
     * object, o, occurs in the array.
     *
     * @param o The object to search for.
     * @return The last relevant index.
     */
    public int lastIndexOf(Object o)
    {
      int i = a.length;
      while (--i >= 0)
        if (ArrayList.equals(o, a[i]))
          return i;
      return -1;
    }

    /**
     * Transforms the list into an array of
     * objects, by simplying cloning the array
     * wrapped by this list.
     *
     * @return A clone of the internal array.
     */
    public Object[] toArray()
    {
      return (Object[]) a.clone();
    }

    /**
     * Copies the objects from this list into
     * the supplied array.  The supplied array
     * is shrunk or enlarged to the size of the
     * internal array, and filled with its objects.
     *
     * @param array The array to fill with the objects in this list.
     * @return The array containing the objects in this list,
     *         which may or may not be == to array.
     */
    public <T> T[] toArray(T[] array)
    {
      int size = a.length;
      if (array.length < size)
        array = (T[]) Array.newInstance(array.getClass().getComponentType(),
					size);
      else if (array.length > size)
        array[size] = null;

      System.arraycopy(a, 0, array, 0, size);
      return array;
    }
  }

  /**
   * Returns a copy of the supplied array, truncating or padding as
   * necessary with <code>false</code> to obtain the specified length.
   * Indices that are valid for both arrays will return the same value.
   * Indices that only exist in the returned array (due to the new length
   * being greater than the original length) will return <code>false</code>.
   * This is equivalent to calling
   * <code>copyOfRange(original, 0, newLength)</code>.
   *
   * @param original the original array to be copied.
   * @param newLength the length of the returned array.
   * @return a copy of the original array, truncated or padded with
   *         <code>false</code> to obtain the required length.
   * @throws NegativeArraySizeException if <code>newLength</code> is negative.
   * @throws NullPointerException if <code>original</code> is <code>null</code>.
   * @since 1.6
   * @see #copyOfRange(boolean[],int,int)
   */
  public static boolean[] copyOf(boolean[] original, int newLength)
  {
    if (newLength < 0)
      throw new NegativeArraySizeException("The array size is negative.");
    return copyOfRange(original, 0, newLength);
  }

  /**
   * Copies the specified range of the supplied array to a new
   * array, padding as necessary with <code>false</code>
   * if <code>to</code> is greater than the length of the original
   * array.  <code>from</code> must be in the range zero to
   * <code>original.length</code> and can not be greater than
   * <code>to</code>.  The initial element of the
   * returned array will be equal to <code>original[from]</code>,
   * except where <code>from</code> is equal to <code>to</code>
   * (where a zero-length array will be returned) or <code>
   * <code>from</code> is equal to <code>original.length</code>
   * (where an array padded with <code>false</code> will be
   * returned).  The returned array is always of length
   * <code>to - from</code>.
   *
   * @param original the array from which to copy.
   * @param from the initial index of the range, inclusive.
   * @param to the final index of the range, exclusive.
   * @return a copy of the specified range, with padding to
   *         obtain the required length.
   * @throws ArrayIndexOutOfBoundsException if <code>from < 0</code>
   *                                        or <code>from > original.length</code>
   * @throws IllegalArgumentException if <code>from > to</code>
   * @throws NullPointerException if <code>original</code> is <code>null</code>.
   * @since 1.6
   * @see #copyOf(boolean[],int)
   */
  public static boolean[] copyOfRange(boolean[] original, int from, int to)
  {
    if (from > to)
      throw new IllegalArgumentException("The initial index is after " +
					 "the final index.");
    boolean[] newArray = new boolean[to - from];
    if (to > original.length)
      {
	System.arraycopy(original, from, newArray, 0,
			 original.length - from);
	fill(newArray, original.length, newArray.length, false);
      }
    else
      System.arraycopy(original, from, newArray, 0, to - from);
    return newArray;
  }

  /**
   * Returns a copy of the supplied array, truncating or padding as
   * necessary with <code>(byte)0</code> to obtain the specified length.
   * Indices that are valid for both arrays will return the same value.
   * Indices that only exist in the returned array (due to the new length
   * being greater than the original length) will return <code>(byte)0</code>.
   * This is equivalent to calling
   * <code>copyOfRange(original, 0, newLength)</code>.
   *
   * @param original the original array to be copied.
   * @param newLength the length of the returned array.
   * @return a copy of the original array, truncated or padded with
   *         <code>(byte)0</code> to obtain the required length.
   * @throws NegativeArraySizeException if <code>newLength</code> is negative.
   * @throws NullPointerException if <code>original</code> is <code>null</code>.
   * @since 1.6
   * @see #copyOfRange(byte[],int,int)
   */
  public static byte[] copyOf(byte[] original, int newLength)
  {
    if (newLength < 0)
      throw new NegativeArraySizeException("The array size is negative.");
    return copyOfRange(original, 0, newLength);
  }

  /**
   * Copies the specified range of the supplied array to a new
   * array, padding as necessary with <code>(byte)0</code>
   * if <code>to</code> is greater than the length of the original
   * array.  <code>from</code> must be in the range zero to
   * <code>original.length</code> and can not be greater than
   * <code>to</code>.  The initial element of the
   * returned array will be equal to <code>original[from]</code>,
   * except where <code>from</code> is equal to <code>to</code>
   * (where a zero-length array will be returned) or <code>
   * <code>from</code> is equal to <code>original.length</code>
   * (where an array padded with <code>(byte)0</code> will be
   * returned).  The returned array is always of length
   * <code>to - from</code>.
   *
   * @param original the array from which to copy.
   * @param from the initial index of the range, inclusive.
   * @param to the final index of the range, exclusive.
   * @return a copy of the specified range, with padding to
   *         obtain the required length.
   * @throws ArrayIndexOutOfBoundsException if <code>from < 0</code>
   *                                        or <code>from > original.length</code>
   * @throws IllegalArgumentException if <code>from > to</code>
   * @throws NullPointerException if <code>original</code> is <code>null</code>.
   * @since 1.6
   * @see #copyOf(byte[],int)
   */
  public static byte[] copyOfRange(byte[] original, int from, int to)
  {
    if (from > to)
      throw new IllegalArgumentException("The initial index is after " +
					 "the final index.");
    byte[] newArray = new byte[to - from];
    if (to > original.length)
      {
	System.arraycopy(original, from, newArray, 0,
			 original.length - from);
	fill(newArray, original.length, newArray.length, (byte)0);
      }
    else
      System.arraycopy(original, from, newArray, 0, to - from);
    return newArray;
  }

  /**
   * Returns a copy of the supplied array, truncating or padding as
   * necessary with <code>'\0'</code> to obtain the specified length.
   * Indices that are valid for both arrays will return the same value.
   * Indices that only exist in the returned array (due to the new length
   * being greater than the original length) will return <code>'\0'</code>.
   * This is equivalent to calling
   * <code>copyOfRange(original, 0, newLength)</code>.
   *
   * @param original the original array to be copied.
   * @param newLength the length of the returned array.
   * @return a copy of the original array, truncated or padded with
   *         <code>'\0'</code> to obtain the required length.
   * @throws NegativeArraySizeException if <code>newLength</code> is negative.
   * @throws NullPointerException if <code>original</code> is <code>null</code>.
   * @since 1.6
   * @see #copyOfRange(char[],int,int)
   */
  public static char[] copyOf(char[] original, int newLength)
  {
    if (newLength < 0)
      throw new NegativeArraySizeException("The array size is negative.");
    return copyOfRange(original, 0, newLength);
  }

  /**
   * Copies the specified range of the supplied array to a new
   * array, padding as necessary with <code>'\0'</code>
   * if <code>to</code> is greater than the length of the original
   * array.  <code>from</code> must be in the range zero to
   * <code>original.length</code> and can not be greater than
   * <code>to</code>.  The initial element of the
   * returned array will be equal to <code>original[from]</code>,
   * except where <code>from</code> is equal to <code>to</code>
   * (where a zero-length array will be returned) or <code>
   * <code>from</code> is equal to <code>original.length</code>
   * (where an array padded with <code>'\0'</code> will be
   * returned).  The returned array is always of length
   * <code>to - from</code>.
   *
   * @param original the array from which to copy.
   * @param from the initial index of the range, inclusive.
   * @param to the final index of the range, exclusive.
   * @return a copy of the specified range, with padding to
   *         obtain the required length.
   * @throws ArrayIndexOutOfBoundsException if <code>from < 0</code>
   *                                        or <code>from > original.length</code>
   * @throws IllegalArgumentException if <code>from > to</code>
   * @throws NullPointerException if <code>original</code> is <code>null</code>.
   * @since 1.6
   * @see #copyOf(char[],int)
   */
  public static char[] copyOfRange(char[] original, int from, int to)
  {
    if (from > to)
      throw new IllegalArgumentException("The initial index is after " +
					 "the final index.");
    char[] newArray = new char[to - from];
    if (to > original.length)
      {
	System.arraycopy(original, from, newArray, 0,
			 original.length - from);
	fill(newArray, original.length, newArray.length, '\0');
      }
    else
      System.arraycopy(original, from, newArray, 0, to - from);
    return newArray;
  }

  /**
   * Returns a copy of the supplied array, truncating or padding as
   * necessary with <code>0d</code> to obtain the specified length.
   * Indices that are valid for both arrays will return the same value.
   * Indices that only exist in the returned array (due to the new length
   * being greater than the original length) will return <code>0d</code>.
   * This is equivalent to calling
   * <code>copyOfRange(original, 0, newLength)</code>.
   *
   * @param original the original array to be copied.
   * @param newLength the length of the returned array.
   * @return a copy of the original array, truncated or padded with
   *         <code>0d</code> to obtain the required length.
   * @throws NegativeArraySizeException if <code>newLength</code> is negative.
   * @throws NullPointerException if <code>original</code> is <code>null</code>.
   * @since 1.6
   * @see #copyOfRange(double[],int,int)
   */
  public static double[] copyOf(double[] original, int newLength)
  {
    if (newLength < 0)
      throw new NegativeArraySizeException("The array size is negative.");
    return copyOfRange(original, 0, newLength);
  }

  /**
   * Copies the specified range of the supplied array to a new
   * array, padding as necessary with <code>0d</code>
   * if <code>to</code> is greater than the length of the original
   * array.  <code>from</code> must be in the range zero to
   * <code>original.length</code> and can not be greater than
   * <code>to</code>.  The initial element of the
   * returned array will be equal to <code>original[from]</code>,
   * except where <code>from</code> is equal to <code>to</code>
   * (where a zero-length array will be returned) or <code>
   * <code>from</code> is equal to <code>original.length</code>
   * (where an array padded with <code>0d</code> will be
   * returned).  The returned array is always of length
   * <code>to - from</code>.
   *
   * @param original the array from which to copy.
   * @param from the initial index of the range, inclusive.
   * @param to the final index of the range, exclusive.
   * @return a copy of the specified range, with padding to
   *         obtain the required length.
   * @throws ArrayIndexOutOfBoundsException if <code>from < 0</code>
   *                                        or <code>from > original.length</code>
   * @throws IllegalArgumentException if <code>from > to</code>
   * @throws NullPointerException if <code>original</code> is <code>null</code>.
   * @since 1.6
   * @see #copyOf(double[],int)
   */
  public static double[] copyOfRange(double[] original, int from, int to)
  {
    if (from > to)
      throw new IllegalArgumentException("The initial index is after " +
					 "the final index.");
    double[] newArray = new double[to - from];
    if (to > original.length)
      {
	System.arraycopy(original, from, newArray, 0,
			 original.length - from);
	fill(newArray, original.length, newArray.length, 0d);
      }
    else
      System.arraycopy(original, from, newArray, 0, to - from);
    return newArray;
  }

  /**
   * Returns a copy of the supplied array, truncating or padding as
   * necessary with <code>0f</code> to obtain the specified length.
   * Indices that are valid for both arrays will return the same value.
   * Indices that only exist in the returned array (due to the new length
   * being greater than the original length) will return <code>0f</code>.
   * This is equivalent to calling
   * <code>copyOfRange(original, 0, newLength)</code>.
   *
   * @param original the original array to be copied.
   * @param newLength the length of the returned array.
   * @return a copy of the original array, truncated or padded with
   *         <code>0f</code> to obtain the required length.
   * @throws NegativeArraySizeException if <code>newLength</code> is negative.
   * @throws NullPointerException if <code>original</code> is <code>null</code>.
   * @since 1.6
   * @see #copyOfRange(float[],int,int)
   */
  public static float[] copyOf(float[] original, int newLength)
  {
    if (newLength < 0)
      throw new NegativeArraySizeException("The array size is negative.");
    return copyOfRange(original, 0, newLength);
  }

  /**
   * Copies the specified range of the supplied array to a new
   * array, padding as necessary with <code>0f</code>
   * if <code>to</code> is greater than the length of the original
   * array.  <code>from</code> must be in the range zero to
   * <code>original.length</code> and can not be greater than
   * <code>to</code>.  The initial element of the
   * returned array will be equal to <code>original[from]</code>,
   * except where <code>from</code> is equal to <code>to</code>
   * (where a zero-length array will be returned) or <code>
   * <code>from</code> is equal to <code>original.length</code>
   * (where an array padded with <code>0f</code> will be
   * returned).  The returned array is always of length
   * <code>to - from</code>.
   *
   * @param original the array from which to copy.
   * @param from the initial index of the range, inclusive.
   * @param to the final index of the range, exclusive.
   * @return a copy of the specified range, with padding to
   *         obtain the required length.
   * @throws ArrayIndexOutOfBoundsException if <code>from < 0</code>
   *                                        or <code>from > original.length</code>
   * @throws IllegalArgumentException if <code>from > to</code>
   * @throws NullPointerException if <code>original</code> is <code>null</code>.
   * @since 1.6
   * @see #copyOf(float[],int)
   */
  public static float[] copyOfRange(float[] original, int from, int to)
  {
    if (from > to)
      throw new IllegalArgumentException("The initial index is after " +
					 "the final index.");
    float[] newArray = new float[to - from];
    if (to > original.length)
      {
	System.arraycopy(original, from, newArray, 0,
			 original.length - from);
	fill(newArray, original.length, newArray.length, 0f);
      }
    else
      System.arraycopy(original, from, newArray, 0, to - from);
    return newArray;
  }

  /**
   * Returns a copy of the supplied array, truncating or padding as
   * necessary with <code>0</code> to obtain the specified length.
   * Indices that are valid for both arrays will return the same value.
   * Indices that only exist in the returned array (due to the new length
   * being greater than the original length) will return <code>0</code>.
   * This is equivalent to calling
   * <code>copyOfRange(original, 0, newLength)</code>.
   *
   * @param original the original array to be copied.
   * @param newLength the length of the returned array.
   * @return a copy of the original array, truncated or padded with
   *         <code>0</code> to obtain the required length.
   * @throws NegativeArraySizeException if <code>newLength</code> is negative.
   * @throws NullPointerException if <code>original</code> is <code>null</code>.
   * @since 1.6
   * @see #copyOfRange(int[],int,int)
   */
  public static int[] copyOf(int[] original, int newLength)
  {
    if (newLength < 0)
      throw new NegativeArraySizeException("The array size is negative.");
    return copyOfRange(original, 0, newLength);
  }

  /**
   * Copies the specified range of the supplied array to a new
   * array, padding as necessary with <code>0</code>
   * if <code>to</code> is greater than the length of the original
   * array.  <code>from</code> must be in the range zero to
   * <code>original.length</code> and can not be greater than
   * <code>to</code>.  The initial element of the
   * returned array will be equal to <code>original[from]</code>,
   * except where <code>from</code> is equal to <code>to</code>
   * (where a zero-length array will be returned) or <code>
   * <code>from</code> is equal to <code>original.length</code>
   * (where an array padded with <code>0</code> will be
   * returned).  The returned array is always of length
   * <code>to - from</code>.
   *
   * @param original the array from which to copy.
   * @param from the initial index of the range, inclusive.
   * @param to the final index of the range, exclusive.
   * @return a copy of the specified range, with padding to
   *         obtain the required length.
   * @throws ArrayIndexOutOfBoundsException if <code>from < 0</code>
   *                                        or <code>from > original.length</code>
   * @throws IllegalArgumentException if <code>from > to</code>
   * @throws NullPointerException if <code>original</code> is <code>null</code>.
   * @since 1.6
   * @see #copyOf(int[],int)
   */
  public static int[] copyOfRange(int[] original, int from, int to)
  {
    if (from > to)
      throw new IllegalArgumentException("The initial index is after " +
					 "the final index.");
    int[] newArray = new int[to - from];
    if (to > original.length)
      {
	System.arraycopy(original, from, newArray, 0,
			 original.length - from);
	fill(newArray, original.length, newArray.length, 0);
      }
    else
      System.arraycopy(original, from, newArray, 0, to - from);
    return newArray;
  }

  /**
   * Returns a copy of the supplied array, truncating or padding as
   * necessary with <code>0L</code> to obtain the specified length.
   * Indices that are valid for both arrays will return the same value.
   * Indices that only exist in the returned array (due to the new length
   * being greater than the original length) will return <code>0L</code>.
   * This is equivalent to calling
   * <code>copyOfRange(original, 0, newLength)</code>.
   *
   * @param original the original array to be copied.
   * @param newLength the length of the returned array.
   * @return a copy of the original array, truncated or padded with
   *         <code>0L</code> to obtain the required length.
   * @throws NegativeArraySizeException if <code>newLength</code> is negative.
   * @throws NullPointerException if <code>original</code> is <code>null</code>.
   * @since 1.6
   * @see #copyOfRange(long[],int,int)
   */
  public static long[] copyOf(long[] original, int newLength)
  {
    if (newLength < 0)
      throw new NegativeArraySizeException("The array size is negative.");
    return copyOfRange(original, 0, newLength);
  }

  /**
   * Copies the specified range of the supplied array to a new
   * array, padding as necessary with <code>0L</code>
   * if <code>to</code> is greater than the length of the original
   * array.  <code>from</code> must be in the range zero to
   * <code>original.length</code> and can not be greater than
   * <code>to</code>.  The initial element of the
   * returned array will be equal to <code>original[from]</code>,
   * except where <code>from</code> is equal to <code>to</code>
   * (where a zero-length array will be returned) or <code>
   * <code>from</code> is equal to <code>original.length</code>
   * (where an array padded with <code>0L</code> will be
   * returned).  The returned array is always of length
   * <code>to - from</code>.
   *
   * @param original the array from which to copy.
   * @param from the initial index of the range, inclusive.
   * @param to the final index of the range, exclusive.
   * @return a copy of the specified range, with padding to
   *         obtain the required length.
   * @throws ArrayIndexOutOfBoundsException if <code>from < 0</code>
   *                                        or <code>from > original.length</code>
   * @throws IllegalArgumentException if <code>from > to</code>
   * @throws NullPointerException if <code>original</code> is <code>null</code>.
   * @since 1.6
   * @see #copyOf(long[],int)
   */
  public static long[] copyOfRange(long[] original, int from, int to)
  {
    if (from > to)
      throw new IllegalArgumentException("The initial index is after " +
					 "the final index.");
    long[] newArray = new long[to - from];
    if (to > original.length)
      {
	System.arraycopy(original, from, newArray, 0,
			 original.length - from);
	fill(newArray, original.length, newArray.length, 0L);
      }
    else
      System.arraycopy(original, from, newArray, 0, to - from);
    return newArray;
  }

  /**
   * Returns a copy of the supplied array, truncating or padding as
   * necessary with <code>(short)0</code> to obtain the specified length.
   * Indices that are valid for both arrays will return the same value.
   * Indices that only exist in the returned array (due to the new length
   * being greater than the original length) will return <code>(short)0</code>.
   * This is equivalent to calling
   * <code>copyOfRange(original, 0, newLength)</code>.
   *
   * @param original the original array to be copied.
   * @param newLength the length of the returned array.
   * @return a copy of the original array, truncated or padded with
   *         <code>(short)0</code> to obtain the required length.
   * @throws NegativeArraySizeException if <code>newLength</code> is negative.
   * @throws NullPointerException if <code>original</code> is <code>null</code>.
   * @since 1.6
   * @see #copyOfRange(short[],int,int)
   */
  public static short[] copyOf(short[] original, int newLength)
  {
    if (newLength < 0)
      throw new NegativeArraySizeException("The array size is negative.");
    return copyOfRange(original, 0, newLength);
  }

  /**
   * Copies the specified range of the supplied array to a new
   * array, padding as necessary with <code>(short)0</code>
   * if <code>to</code> is greater than the length of the original
   * array.  <code>from</code> must be in the range zero to
   * <code>original.length</code> and can not be greater than
   * <code>to</code>.  The initial element of the
   * returned array will be equal to <code>original[from]</code>,
   * except where <code>from</code> is equal to <code>to</code>
   * (where a zero-length array will be returned) or <code>
   * <code>from</code> is equal to <code>original.length</code>
   * (where an array padded with <code>(short)0</code> will be
   * returned).  The returned array is always of length
   * <code>to - from</code>.
   *
   * @param original the array from which to copy.
   * @param from the initial index of the range, inclusive.
   * @param to the final index of the range, exclusive.
   * @return a copy of the specified range, with padding to
   *         obtain the required length.
   * @throws ArrayIndexOutOfBoundsException if <code>from < 0</code>
   *                                        or <code>from > original.length</code>
   * @throws IllegalArgumentException if <code>from > to</code>
   * @throws NullPointerException if <code>original</code> is <code>null</code>.
   * @since 1.6
   * @see #copyOf(short[],int)
   */
  public static short[] copyOfRange(short[] original, int from, int to)
  {
    if (from > to)
      throw new IllegalArgumentException("The initial index is after " +
					 "the final index.");
    short[] newArray = new short[to - from];
    if (to > original.length)
      {
	System.arraycopy(original, from, newArray, 0,
			 original.length - from);
	fill(newArray, original.length, newArray.length, (short)0);
      }
    else
      System.arraycopy(original, from, newArray, 0, to - from);
    return newArray;
  }

  /**
   * Returns a copy of the supplied array, truncating or padding as
   * necessary with <code>null</code> to obtain the specified length.
   * Indices that are valid for both arrays will return the same value.
   * Indices that only exist in the returned array (due to the new length
   * being greater than the original length) will return <code>null</code>.
   * This is equivalent to calling
   * <code>copyOfRange(original, 0, newLength)</code>.
   *
   * @param original the original array to be copied.
   * @param newLength the length of the returned array.
   * @return a copy of the original array, truncated or padded with
   *         <code>null</code> to obtain the required length.
   * @throws NegativeArraySizeException if <code>newLength</code> is negative.
   * @throws NullPointerException if <code>original</code> is <code>null</code>.
   * @since 1.6
   * @see #copyOfRange(T[],int,int)
   */
  public static <T> T[] copyOf(T[] original, int newLength)
  {
    if (newLength < 0)
      throw new NegativeArraySizeException("The array size is negative.");
    return copyOfRange(original, 0, newLength);
  }

  /**
   * Copies the specified range of the supplied array to a new
   * array, padding as necessary with <code>null</code>
   * if <code>to</code> is greater than the length of the original
   * array.  <code>from</code> must be in the range zero to
   * <code>original.length</code> and can not be greater than
   * <code>to</code>.  The initial element of the
   * returned array will be equal to <code>original[from]</code>,
   * except where <code>from</code> is equal to <code>to</code>
   * (where a zero-length array will be returned) or <code>
   * <code>from</code> is equal to <code>original.length</code>
   * (where an array padded with <code>null</code> will be
   * returned).  The returned array is always of length
   * <code>to - from</code>.
   *
   * @param original the array from which to copy.
   * @param from the initial index of the range, inclusive.
   * @param to the final index of the range, exclusive.
   * @return a copy of the specified range, with padding to
   *         obtain the required length.
   * @throws ArrayIndexOutOfBoundsException if <code>from < 0</code>
   *                                        or <code>from > original.length</code>
   * @throws IllegalArgumentException if <code>from > to</code>
   * @throws NullPointerException if <code>original</code> is <code>null</code>.
   * @since 1.6
   * @see #copyOf(T[],int)
   */
  public static <T> T[] copyOfRange(T[] original, int from, int to)
  {
    if (from > to)
      throw new IllegalArgumentException("The initial index is after " +
					 "the final index.");
    Class elemType = original.getClass().getComponentType();
    T[] newArray = (T[]) Array.newInstance(elemType, to - from);
    if (to > original.length)
      {
	System.arraycopy(original, from, newArray, 0,
			 original.length - from);
	fill(newArray, original.length, newArray.length, null);
      }
    else
      System.arraycopy(original, from, newArray, 0, to - from);
    return newArray;
  }

  /**
   * Returns a copy of the supplied array, truncating or padding as
   * necessary with <code>null</code> to obtain the specified length.
   * Indices that are valid for both arrays will return the same value.
   * Indices that only exist in the returned array (due to the new length
   * being greater than the original length) will return <code>null</code>.
   * This is equivalent to calling
   * <code>copyOfRange(original, 0, newLength, newType)</code>.  The returned
   * array will be of the specified type, <code>newType</code>.
   *
   * @param original the original array to be copied.
   * @param newLength the length of the returned array.
   * @param newType the type of the returned array.
   * @return a copy of the original array, truncated or padded with
   *         <code>null</code> to obtain the required length.
   * @throws NegativeArraySizeException if <code>newLength</code> is negative.
   * @throws NullPointerException if <code>original</code> is <code>null</code>.
   * @since 1.6
   * @see #copyOfRange(U[],int,int,Class)
   */
  public static <T,U> T[] copyOf(U[] original, int newLength,
				 Class<? extends T[]> newType)
  {
    if (newLength < 0)
      throw new NegativeArraySizeException("The array size is negative.");
    return copyOfRange(original, 0, newLength, newType);
  }

  /**
   * Copies the specified range of the supplied array to a new
   * array, padding as necessary with <code>null</code>
   * if <code>to</code> is greater than the length of the original
   * array.  <code>from</code> must be in the range zero to
   * <code>original.length</code> and can not be greater than
   * <code>to</code>.  The initial element of the
   * returned array will be equal to <code>original[from]</code>,
   * except where <code>from</code> is equal to <code>to</code>
   * (where a zero-length array will be returned) or <code>
   * <code>from</code> is equal to <code>original.length</code>
   * (where an array padded with <code>null</code> will be
   * returned).  The returned array is always of length
   * <code>to - from</code> and will be of the specified type,
   * <code>newType</code>.
   *
   * @param original the array from which to copy.
   * @param from the initial index of the range, inclusive.
   * @param to the final index of the range, exclusive.
   * @param newType the type of the returned array.
   * @return a copy of the specified range, with padding to
   *         obtain the required length.
   * @throws ArrayIndexOutOfBoundsException if <code>from < 0</code>
   *                                        or <code>from > original.length</code>
   * @throws IllegalArgumentException if <code>from > to</code>
   * @throws NullPointerException if <code>original</code> is <code>null</code>.
   * @since 1.6
   * @see #copyOf(T[],int)
   */
  public static <T,U> T[] copyOfRange(U[] original, int from, int to,
				      Class<? extends T[]> newType)
  {
    if (from > to)
      throw new IllegalArgumentException("The initial index is after " +
					 "the final index.");
    T[] newArray = (T[]) Array.newInstance(newType.getComponentType(),
					   to - from);
    if (to > original.length)
      {
	System.arraycopy(original, from, newArray, 0,
			 original.length - from);
	fill(newArray, original.length, newArray.length, null);
      }
    else
      System.arraycopy(original, from, newArray, 0, to - from);
    return newArray;
  }
}
