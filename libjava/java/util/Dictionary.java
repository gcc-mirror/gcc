/* Dictionary.java -- an abstract (and essentially worthless) 
   class which is Hashtable's superclass
   Copyright (C) 1998, 2001 Free Software Foundation, Inc.

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

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */


package java.util;

/**
 * A Dictionary maps keys to values; <i>how</i> it does that is
 * implementation-specific.
 * 
 * This is an abstract class which has really gone by the wayside.
 * People at Javasoft are probably embarrassed by it.  At this point,
 * it might as well be an interface rather than a class, but it remains
 * this poor, laughable skeleton for the sake of backwards compatibility.
 * At any rate, this was what came before the <pre>Map</pre> interface 
 * in the Collections framework.
 *
 * @author Jon Zeppieri
 * @author Eric Blake <ebb9@email.byu.edu>
 * @see Map
 * @see Hashtable
 * @since 1.0
 * @status updated to 1.4
 */
public abstract class Dictionary extends Object
{
  /**
   * Sole constructor (often called implicitly).
   */
  public Dictionary()
  {
  }

  /**
   * Returns an Enumeration of the values in this Dictionary.
   *
   * @return an Enumeration of the values
   * @see #keys()
   */
  public abstract Enumeration elements();

  /** 
   * Returns the value associated with the supplied key, or null
   * if no such value exists. Since Dictionaries are not allowed null keys
   * or elements, a null result always means the key is not present.
   *
   * @param key the key to use to fetch the value
   * @return the mapped value
   * @throws NullPointerException if key is null
   * @see #put(Object, Object)
   */
  public abstract Object get(Object key);

  /**
   * Returns true when there are no elements in this Dictionary.
   *
   * @return <code>size() == 0</code>
   */
  public abstract boolean isEmpty();

  /**
   * Returns an Enumeration of the keys in this Dictionary
   *
   * @return an Enumeration of the keys
   * @see #elements()
   */
  public abstract Enumeration keys();

  /**
   * Inserts a new value into this Dictionary, located by the
   * supplied key. Dictionary does not support null keys or values, so
   * a null return can safely be interpreted as adding a new key.
   *
   * @param key the key which locates the value
   * @param value the value to put into the Dictionary
   * @return the previous value of the key, or null if there was none
   * @throws NullPointerException if key or value is null
   * @see #get(Object)
   */
  public abstract Object put(Object key, Object value);

  /**
   * Removes from the Dictionary the value located by the given key. A null
   * return safely means that the key was not mapped in the Dictionary.
   *
   * @param key the key used to locate the value to be removed
   * @return the value associated with the removed key
   * @throws NullPointerException if key is null
   */
  public abstract Object remove(Object key);

  /**
   * Returns the number of values currently in this Dictionary.
   *
   * @return the number of keys in the Dictionary
   */
  public abstract int size();
}
