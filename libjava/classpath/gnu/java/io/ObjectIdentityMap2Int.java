/* ObjectIdentityMapToInt.java -- Helper class for faster serialization
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package gnu.java.io;

/**
 * This class provides a map from Object to non-negative int values.
 * Objects are considered equal only if their references are equal.
 *
 * This can be used to equip objects with an integer id.  This class
 * is implemented to use as little memory as possible, particularly
 * not to create hashtable buckets and Integer instances for each
 * mapping.
 *
 * @author Fridtjof Siebert (siebert@aicas.com)
 */
public class ObjectIdentityMap2Int
{ 


  /**
   * Prime numbers used as size of array. We need the size to be a
   * prime number since the delta used for conflict resulution must
   * not have any common divisors with the length.
   */
  private static final int[] PRIMES = { 
    0x1f,
    0x3d,
    0x7f,
    0xfb,
    0x1fd,
    0x3fd,
    0x7f7,
    0xffd,
    0x1fff,
    0x3ffd,
    0x7fed,
    0xfff1,
    0x1ffff,
    0x3fffb,
    0x7ffff,
    0xffffd,
    0x1ffff7,
    0x3ffffd,
    0x7ffff1,
    0xfffffd,
    0x1ffffd9,
    0x3fffffb,
    0x7ffffd9,
    0xfffffc7,
    0x1ffffffd,
    0x3fffffdd,
    0x7fffffff};


  /**
   * Object to be used instead of "null"
   */
  private static final Object NIL = new Object();

  
  /**
   * The objects in this map:
   *
   * invariant
   *   objectTable.size == PRIMES[cap]
   */
  private Object[] objectTable;


  /**
   * The corresponding integer ids.
   *
   * invariant
   *   intTable.size == PRIMES[cap]
   */
  private int[] intTable;


  /**
   * The number of entries in this map.
   *
   * invariant
   *   size < limit
   */
  private int size = 0; 


  /**
   * The index in primes of the size of the tables.
   */
  private int cap = 0; 


  /**
   * The limit for size at which the table size is increased.
   *
   * invariant
   *   limit = PRIMES[cap] / 4 * 3;
   */
  private int limit = 0; 


  /**
   * Constructs an empty <code>ObjectIdentityMap2Int</code>.
   */
  public ObjectIdentityMap2Int()
  {
    alloc(0);
  }


  /**
   * Helper function to alloc the object and int array for the given
   * capacity.  Set limit, reset size to 0.
   * 
   * No elements will be stored in the newly allocated arrays.
   *
   * @param c the capacity: this is an index in PRIMES, PRIMES[c]
   * gives the size of the arrays.
   *
   * @throws InternalError if c >= PRIMES.length (in this case, a
   * normal Hashtable would throw an OutOfMemoryError or a
   * NegativeArraySizeException since the array size exceeds the range
   * of positive integers).
   */
  private void alloc(int c)
  {
    if (c >= PRIMES.length) 
      throw new InternalError("Hash table size overflow"); 

    cap = c;
    int len = PRIMES[c];
    objectTable = new Object[len];
    intTable    = new int[len];
    limit = len / 4 * 3;

    size = 0; 
  }


  /**
   * Add a mapping to this Map.
   *
   * ensures
   *   (get(o) == i);
   *
   * @param o object reference or null that is to be mapped. 
   *
   * @param i the integer id to be associated with o
   *
   * @throws IllegalArgumentException if i<0
   *
   * @throws InternalError if hash tables has grown to more then
   * 0x7fffffff entries (ie., size >= 0x7fffffff*3/4).
   */
  public void put(Object o, int i) 
  {
    if (i < 0) 
      throw new IllegalArgumentException("int argument must be postive: "+i);

    o = (o == null) ? NIL : o;
    int s = slot(o);
    Object[] ot = objectTable;
    intTable[s] = i;
    if (objectTable[s] == null)
      {
        objectTable[s] = o; 
        size++; 
        if (size >= limit)
          {
            rehash();
          }
      }
  }


  /**
   * Helper function to find the index of a free or existing slot for
   * object o
   *
   * ensure
   *   ((objectTable[result] != null) IMPLIES (objectTable[result] == o));
   *
   * @param o an object, must not be null.
   *
   * @return an index of o 
   */
  private int slot(Object o)
  {
    Object[] ot = objectTable;
    int hc     = System.identityHashCode(o);
    int len    = ot.length;
    int result = hc % len;  
    result = result < 0 ? -result : result;
    int delta  = 16 - (hc & 15);
    Object existing = ot[result];
    while ((existing != null) && (existing != o))
      {
        result += delta;
        if (result >= len)
          result -= len;
        existing = ot[result];
      }
    return result;
  }


  /**
   * Helper function for put() to increaes the capacity of this table
   * to the next size (approx. double the size).  Keep the mapping and
   * the size unchanged.
   *
   * ensure
   *   (cap == \old cap+1);
   */
  private void rehash() 
  {
    Object[] ot = objectTable; 
    int   [] it = intTable;
    alloc(cap + 1);

    for (int i = 0; i < ot.length; i++)
      put(ot[i], it[i]);
  }


  /**
   * Obtain an element from this map
   *
   * @param o an object or null
   *
   * @return the corresponding integer id for o or -1 if o has not
   * been put into this map.
   */
  public int get(Object o) 
  {
    o = (o == null) ? NIL : o;
    int s = slot(o);
    return objectTable[s] == null ? -1 : intTable[s];
  }

  /**
   * Clear this map
   *
   * ensures
   *   ((size == 0) && \forall Object o: get(o) == -1)
   */
  public void clear() 
  {
    Object[] ot = objectTable; 
    size = 0; 
    for (int i = 0; i < ot.length; i++)
      ot[i] = null;
  }

}
