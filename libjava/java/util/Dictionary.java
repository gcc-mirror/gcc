/* Dictionary.java -- an abstract (and essentially worthless) 
   class which is Hashtable's superclass
   Copyright (C) 1998, 2001, 2002 Free Software Foundation, Inc.

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
 * A Dictionary maps keys to values; <i>how</i> it does that is
 * implementation-specific.
 * 
 * This is an abstract class which has really gone by the wayside.
 * People at Javasoft are probably embarrassed by it.  At this point,
 * it might as well be an interface rather than a class, but it remains
 * this poor, laughable skeleton for the sake of backwards compatibility.
 * At any rate, this was what came before the {@link Map} interface 
 * in the Collections framework.
 *
 * @author Jon Zeppieri
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see Map
 * @see Hashtable
 * @since 1.0
 * @status updated to 1.4
 */
public abstract class Dictionary
{
  // WARNING: Dictionary is a CORE class in the bootstrap cycle. See the
  // comments in vm/reference/java/lang/Runtime for implications of this fact.

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
} // class Dictionary
