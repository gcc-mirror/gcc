/* AttributeSet.java -- 
   Copyright (C) 2002, 2004, 2005 Free Software Foundation, Inc.

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

package javax.swing.text;

import java.util.Enumeration;

/**
 * A set of attributes. An attribute has a key and a value. They typically
 * describe features of a piece of text that make up its graphical
 * representation.
 *
 * An <code>AttributeSet</code> may have a resolving parent,
 * that is another <code>AttributeSet</code> that is searched for attribute
 * keys that are not stored locally in this <code>AttributeSet</code>.
 *
 * @author original author unknown
 * @author Roman Kennke (roman@kennke.org)
 */
public interface AttributeSet
{ 
  /**
   * Used as keys to identify character-run attributes.
   */
  static interface CharacterAttribute
  {
  }

  /**
   * Used as keys to identify color attributes.
   */
  static interface ColorAttribute
  {
  }

  /**
   * Used as keys to identify font attributes.
   */
  static interface FontAttribute
  {
  }

  /**
   * Used as keys to identify paragraph level attributes.
   */
  static interface ParagraphAttribute
  {
  }

  /**
   * Key of the attribute that is used to describe the name of an
   * <code>AttributeSet</code>.
   */
  Object NameAttribute = StyleConstants.NameAttribute;

  /**
   * Key of the attribute that is used to identify the resolving parent of
   * an <code>AttributeSet</code>.
   */
  Object ResolveAttribute = StyleConstants.ResolveAttribute;

  /**
   * Returns <code>true</code> if this <code>AttributeSet</code> contains
   * an attribute with the specified <code>name</code> and <code>value</code>,
   * <code>false</code> otherwise.
   *
   * @param name the name of the requested attribute
   * @param the value of the requested attribute
   *
   * @return <code>true</code> if this <code>AttributeSet</code> contains
   *         an attribute with the specified <code>name</code> and
   *         <code>value</code>, <code>false</code> otherwise
   */
  boolean containsAttribute(Object name, Object value);

  /**
   * Returns <code>true</code> of this <code>AttributeSet</code> contains all
   * of the specified <code>attributes</code>.
   *
   * @param attributes the requested attributes
   *
   * @return <code>true</code> of this <code>AttributeSet</code> contains all
   *         of the specified <code>attributes</code>
   */
  boolean containsAttributes(AttributeSet attributes);

  /**
   * Creates and returns a copy of this <code>AttributeSet</code>.
   *
   * @return a copy of this <code>AttributeSet</code>
   */
  AttributeSet copyAttributes();

  /**
   * Returns the attribute with the specified <code>key</code> or
   * <code>null</code> if no such attribute is defined in this
   * <code>AttributeSet</code> and its resolving parents.
   *
   * @param key the key of the attribute that is looked up
   *
   * @return the attribute with the specified <code>key</code> or
   *         <code>null</code> if no such attribute is defined in this
   *         <code>AttributeSet</code> and its resolving parents
   */
  Object getAttribute(Object key);

  /**
   * Returns the number of attributes that are stored locally in this
   * <code>AttributeSet</code>.
   *
   * @return the number of attributes that are stored locally in this
   * <code>AttributeSet</code>
   */
  int getAttributeCount();

  /**
   * Returns the names of the attributes that are stored in this
   * <code>AttributeSet</code>.
   *
   * @return the names of the attributes that are stored in this
   *         <code>AttributeSet</code>
   */
  Enumeration getAttributeNames();

  /**
   * Returns the resolving parent of this <code>AttributeSet</code>.
   * If a key is not stored locally, then a {@link #getAttribute(Object)}
   * request is resolved up in the resolving parent of this
   * <code>AttributeSet</code>.
   *
   * @return the resolving parent of this <code>AttributeSet</code>
   */
  AttributeSet getResolveParent();

  /**
   * Returns <code>true</code> if an attribute with the specified name is
   * defined locally in this <code>AttributeSet</code>, without resolving
   * through the resolving parents.
   *
   * @return <code>true</code> if an attribute with the specified name is
   *          defined locally in this <code>AttributeSet</code>
   */
  boolean isDefined(Object attrName);

  /**
   * Returns <code>true</code> if all of the attributes in <code>attr</code>
   * are equal to the attributes in this <code>AttributeSet</code>,
   * <code>false</code> otherwise.
   *
   * @param attr the attributes to be compared to <code>this</code>
   *
   * @return <code>true</code> if all of the attributes in <code>attr</code>
   *         are equal to the attributes in this <code>AttributeSet</code>,
   *         <code>false</code> otherwise
   */
  boolean isEqual(AttributeSet attr);     
}
