/* MutableAttributeSet.java --
   Copyright (C) 2002, 2004, 2006 Free Software Foundation, Inc.

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
 * An {@link AttributeSet} that supports modification of the stored
 * attributes.
 *
 * @author      Andrew Selkirk
 * @since 1.2
 */
public interface MutableAttributeSet extends AttributeSet
{
  /**
   * Adds an attribute with the given <code>name</code> and <code>value</code>
   * to the set.  If the set already contains an attribute with the given
   * <code>name</code>, the attribute value is updated.
   *
   * @param name  the attribute name (<code>null</code> not permitted).
   * @param value  the value (<code>null</code> not permitted).
   *
   * @throws NullPointerException if either argument is <code>null</code>.
   */
  void addAttribute(Object name, Object value);

  /**
   * Adds all the attributes from <code>attributes</code> to this set.
   *
   * @param attributes  the set of attributes to add (<code>null</code> not
   *                    permitted).
   *
   * @throws NullPointerException if <code>attributes</code> is
   *         <code>null</code>.
   */
  void addAttributes(AttributeSet attributes);

  /**
   * Removes the attribute with the specified <code>name</code>, if this
   * attribute is defined.  This method will only remove an attribute from
   * this set, not from the resolving parent.
   *
   * @param name  the attribute name (<code>null</code> not permitted).
   *
   * @throws NullPointerException if <code>name</code> is <code>null</code>.
   */
  void removeAttribute(Object name);

  /**
   * Removes the attributes listed in <code>names</code>.
   *
   * @param names  the attribute names (<code>null</code> not permitted).
   *
   * @throws NullPointerException if <code>names</code> is <code>null</code>
   *         or contains any <code>null</code> values.
   */
  void removeAttributes(Enumeration<?> names);

  /**
   * Removes attributes from this set if they are found in the
   * given set.  Only attributes whose key AND value are removed.
   * Removes attributes only from this set, not from the resolving parent.
   * Since the resolving parent is stored as an attribute, if
   * <code>attributes</code> has the same resolving parent as this set, the
   * parent will be removed from this set.
   *
   * @param attributes  the attributes (<code>null</code> not permitted).
   */
  void removeAttributes(AttributeSet attributes);

  /**
   * Sets the reolving parent for this set.  When looking up an attribute, if
   * it is not found in this set, then the resolving parent is also used for
   * the lookup.
   *
   * @param parent  the parent attribute set (<code>null</code> not permitted).
   *
   * @throws NullPointerException if <code>parent</code> is <code>null</code>.
   */
  void setResolveParent(AttributeSet parent);
}
