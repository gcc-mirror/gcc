/* AttributeSet.java --
   Copyright (C) 2002, 2005 Free Software Foundation, Inc.

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

package javax.print.attribute;

/**
 * <code>AttributeSet</code> is the top-level interface for sets of printing
 * attributes in the Java Print Service API.
 * <p>
 * There are no duplicate values allowed in an attribute set and there is
 * at most one attribute object contained per category type. Based on the
 * {@link java.util.Map} interface the values of attribute sets are objects
 * of type {@link javax.print.attribute.Attribute} and the entries are the
 * categories as {@link java.lang.Class} instances.
 * </p>
 * <p>
 * The following specialized types of <code>AttributeSet</code> are available:
 * <ul>
 *  <li>{@link javax.print.attribute.DocAttributeSet}</li>
 *  <li>{@link javax.print.attribute.PrintRequestAttributeSet}</li>
 *  <li>{@link javax.print.attribute.PrintJobAttributeSet}</li>
 *  <li>{@link javax.print.attribute.PrintServiceAttributeSet}</li>
 * </ul>
 * </p>
 * <p>
 * Attribute sets may be unmodifiable depending on the context of usage. If
 * used as read-only attribute set modifying operations throw an
 * {@link javax.print.attribute.UnmodifiableSetException}.
 * </p>
 * <p>
 * The Java Print Service API provides implementation classes for the existing
 * attribute set interfaces but applications may use their own implementations.
 * </p>
 *
 * @author Michael Koch (konqueror@gmx.de)
 */
public interface AttributeSet
{
  /**
   * Adds the specified attribute value to this attribute set
   * if it is not already present.
   *
   * This operation removes any existing attribute of the same category
   * before adding the given attribute to the set.
   *
   * @param attribute the attribute to add.
   * @return <code>true</code> if the set is changed, false otherwise.
   * @throws NullPointerException if the attribute is <code>null</code>.
   * @throws UnmodifiableSetException if the set does not support modification.
   */
  boolean add (Attribute attribute);

  /**
   * Adds all of the elements in the specified set to this attribute set.
   *
   * @param attributes the set of attributes to add.
   * @return <code>true</code> if the set is changed, false otherwise.
   * @throws UnmodifiableSetException if the set does not support modification.
   *
   * @see #add(Attribute)
   */
  boolean addAll (AttributeSet attributes);

  /**
   * Removes all attributes from this attribute set.
   *
   * @throws UnmodifiableSetException if the set does not support modification.
   */
  void clear ();

  /**
   * Checks if this attributes set contains an attribute with the given
   * category.
   *
   * @param category the category to test for.
   * @return <code>true</code> if an attribute of the category is contained
   * in the set, <code>false</code> otherwise.
   */
  boolean containsKey (Class<?> category);

  /**
   * Checks if this attribute set contains the given attribute.
   *
   * @param attribute the attribute to test for.
   * @return <code>true</code> if the attribute is contained in the set,
   * <code>false</code> otherwise.
   */
  boolean containsValue (Attribute attribute);

  /**
   * Tests this set for equality with the given object. <code>true</code> is
   * returned, if the given object is also of type <code>AttributeSet</code>
   * and the contained attributes are the same as in this set.
   *
   * @param obj the Object to test.
   * @return <code>true</code> if equal, false otherwise.
   */
  boolean equals (Object obj);

  /**
   * Returns the attribute object contained in this set for the given attribute
   * category.
   *
   * @param category the category of the attribute. A <code>Class</code>
   * instance of a class implementing the <code>Attribute</code> interface.
   * @return The attribute for this category or <code>null</code> if no
   * attribute is contained for the given category.
   * @throws NullPointerException if category is null.
   * @throws ClassCastException if category is not implementing
   * <code>Attribute</code>.
   */
  Attribute get (Class<?> category);

  /**
   * Returns the hashcode value. The hashcode value is the sum of all hashcodes
   * of the attributes contained in this set.
   *
   * @return The hashcode for this attribute set.
   */
  int hashCode ();

  /**
   * Checks if the attribute set is empty.
   *
   * @return <code>true</code> if the attribute set is empty, false otherwise.
   */
  boolean isEmpty ();

  /**
   * Removes the given attribute from the set. If the given attribute is <code>null</code>
   * nothing is done and <code>false</code> is returned.
   *
   * @param attribute the attribute to remove.
   * @return <code>true</code> if removed, false in all other cases.
   * @throws UnmodifiableSetException if the set does not support modification.
   */
  boolean remove (Attribute attribute);

  /**
   * Removes the attribute entry of the given category from the set. If the given
   * category is <code>null</code> nothing is done and <code>false</code> is returned.
   *
   * @param category the category of the entry to be removed.
   * @return <code>true</code> if an attribute is removed, false in all other cases.
   * @throws UnmodifiableSetException if the set does not support modification.
   */
  boolean remove (Class<?> category);

  /**
   * Returns the number of elements in this attribute set.
   *
   * @return The number of elements.
   */
  int size ();

  /**
   * Returns the content of the attribute set as an array
   *
   * @return An array of attributes.
   */
  Attribute[] toArray ();
}
