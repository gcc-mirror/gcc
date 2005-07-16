/* AccessibleRelationSet.java -- the combined relations of an accessible object
   Copyright (C) 2002, 2005 Free Software Foundation

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

package javax.accessibility;

import java.util.Locale;
import java.util.Vector;

/**
 * Describes all relations of an accessible object. For example, an object
 * by labeled by one object and control another.
 *
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see AccessibleRelation
 * @since 1.2
 * @status updated to 1.4
 */
public class AccessibleRelationSet
{
  /**
   * The list of relations, should be instances of AccessibleRelation. Don't
   * set this to null.
   *
   * @see #add(AccessibleRelation)
   * @see #addAll(AccessibleRelation[])
   * @see #remove(AccessibleRelation)
   * @see #contains(String)
   * @see #get(String)
   * @see #size()
   * @see #toArray()
   * @see #clear()
   */
  protected Vector relations = new Vector();

  /**
   * Create an empty relation set.
   */
  public AccessibleRelationSet()
  {
  }

  /**
   * Create a relation set initialized with the given relations, duplicates are
   * ignored.
   *
   * @param relations the relations to insert
   * @throws NullPointerException if relations is null
   */
  public AccessibleRelationSet(AccessibleRelation[] relations)
  {
    addAll(relations);
  }

  /**
   * Add a new relation to the current set. If the relation is already in
   * the set, the targets are merged with the existing relation, possibly
   * resulting in an object being in the target list more than once. Do not
   * add a relation with a null key, as it will cause problems later.
   *
   * @param relation the relation to add
   * @return true if the set was modified, which is always the case
   * @throws NullPointerException if relation is null
   */
  public boolean add(AccessibleRelation relation)
  {
    AccessibleRelation old = get(relation.key);
    if (old == null)
      return relations.add(relation);
    if (old.targets.length == 0)
      old.targets = relation.targets;
    else if (relation.targets.length != 0)
      {
        Object[] t = new Object[old.targets.length + relation.targets.length];
        System.arraycopy(old.targets, 0, t, 0, old.targets.length);
        System.arraycopy(relation.targets, 0, t, old.targets.length,
                         relation.targets.length);
        old.targets = t;
      }
    return true;
  }

  /**
   * Add all of the relations to the current set. Duplicates are ignored.
   *
   * @param array the array of relations to add
   * @throws NullPointerException if array is null or has null entries
   */
  public void addAll(AccessibleRelation[] array)
  {
    int i = array.length;
    while (--i >= 0)
      add(array[i]);
  }

  /**
   * Remove a relation from the set. If a relation was removed, return true.
   * Note that this uses AccessibleRelation.equals, which defaults to ==, so a
   * relation with the same key may still exist in the set afterwords.
   *
   * @param relation the state to remove
   * @return true if the set changed
   */
  public boolean remove(AccessibleRelation relation)
  {
    return relations.remove(relation);
  }

  /**
   * Clear all relations in the set.
   */
  public void clear()
  {
    relations.clear();
  }

  /**
   * Return the number of relations in the set.
   *
   * @return the set size
   */
  public int size()
  {
    return relations.size();
  }

  /**
   * Check if the relation key is in the set.
   *
   * @param key the relation to locate
   * @return true if it is in the set
   */
  public boolean contains(String key)
  {
    int i = relations.size();
    while (--i >= 0)
      if (((AccessibleRelation) relations.get(i)).key.equals(key))
        return true;
    return false;
  }

  /**
   * Get the relation that matches the key.
   *
   * @param key the relation to locate
   * @return the relation in the set, or null
   */
  public AccessibleRelation get(String key)
  {
    int i = relations.size();
    while (--i >= 0)
      {
        AccessibleRelation r = (AccessibleRelation) relations.get(i);
        if (r.key.equals(key))
          return r;
      }
    return null;
  }

  /**
   * Return the relation set as an array.
   *
   * @return an array of the current relations
   */
  public AccessibleRelation[] toArray()
  {
    AccessibleRelation[] result = new AccessibleRelation[relations.size()];
    relations.toArray(result);
    return result;
  }

  /**
   * Return a localized, comma-separated string representing all relations
   * in the set. This is in arbitrary order.
   *
   * @return the string representation
   * @see AccessibleBundle#toDisplayString(String, Locale)
   */
  public String toString()
  {
    int i = relations.size();
    if (i == 0)
      return "";
    // Pre-allocate an average of 10 chars per state.
    StringBuffer b = new StringBuffer(i * 10);
    while (--i >= 0)
      b.append(relations.get(i)).append(',');
    return b.substring(0, b.length() - 1);
  }
} // class AccessibleRelationSet
