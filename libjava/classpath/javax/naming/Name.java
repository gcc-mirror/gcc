/* Name.java -- Name build up from different components
   Copyright (C) 2000, 2001, 2004, 2005 Free Software Foundation, Inc.

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

package javax.naming;

import java.io.Serializable;
import java.util.Enumeration;

/**
 * Interface descriping a name build up from different components.
 * The components are represented as <code>String</code>s which are
 * ordered from most significant to least significant. There are methods to
 * get the number of components. Methods to get a particular component or group
 * of components. Components can be added as <code>String</code>s or
 * <code>Name</code>s and a component can be removed from any position in the
 * <code>Name</code>.
 * A <code>Name</code> can be compared to another <code>Name</code> and it can
 * be checked if a particular <code>Name</code> starts or ends with the same
 * components as another <code>Name</code>. Finally <code>Name</code>s can be
 * serialized and cloned.
 * <p>
 * Since <code>Name</code>s can be empty (have no components) methods that
 * return a <code>Name</code> will never return <code>null</code>.
 *
 * @since 1.3
 * @author Anthony Green (green@redhat.com)
 * @author Mark Wielaard (mark@klomp.org)
 */
public interface Name extends Cloneable, Serializable, Comparable<Object>
{
  // This class is implemented as gnu.javax.naming.ictxImpl.trans.GnuName

  long serialVersionUID = -3617482732056931635L;

  /**
   * Returns the number of components of this <code>Name</code>.
   * The returned number can be zero.
   */
  int size();

  /**
   * Returns <code>true</code> if the number of components of this
   * <code>Name</code> is zero, <code>false</code> otherwise.
   */
  boolean isEmpty();

  /**
   * Returns a non-null (but possibly empty) <code>Enumeration</code> of the
   * components of the <code>Name</code> as <code>String</code>s.
   */
  Enumeration<String> getAll();

  /**
   * Gets the component at the given index.
   *
   * @exception ArrayIndexOutOfBoundsException if the given index is smaller
   *            then zero or greater then or equal to <code>size()</code>.
   */
  String get(int i);

  /**
   * Returns the components till the given index as a <code>Name</code>.
   * The returned <code>Name</code> can be modified without changing the
   * original.
   *
   * @param posn the ending position, exclusive
   *
   * @exception ArrayIndexOutOfBoundsException if the given index is smaller
   *            then zero or greater then or equal to <code>size()</code>.
   */
  Name getPrefix(int posn);

  /**
   * Returns the components from the given index till the end as a
   * <code>Name</code>.
   * The returned <code>Name</code> can be modified without changing the
   * original.
   *
   * @param posn the starting position, inclusive. If it is equal to the size
   *        of the name, the empty name is returned.
   *
   * @exception ArrayIndexOutOfBoundsException if the given index is smaller
   *            then zero or greater then or equal to <code>size()</code>.
   */
  Name getSuffix(int posn);

  /**
   * Adds the given <code>String</code> component to the end of this
   * <code>Name</code>. The method modifies the current <code>Name</code> and
   * then returns it.
   *
   * @exception InvalidNameException if the given <code>String</code> is not a
   *            valid component for this <code>Name</code>.
   */
  Name add(String comp) throws InvalidNameException;

  /**
   * Inserts the given <code>String</code> component to this <code>Name</code>
   * at the given index. The method modifies the current <code>Name</code> and
   * then returns it.
   *
   * @exception ArrayIndexOutOfBoundsException if the given index is smaller
   *            then zero or greater then or equal to <code>size()</code>.
   * @exception InvalidNameException if the given <code>String</code> is not a
   *            valid component for this <code>Name</code>.
   */
  Name add(int posn, String comp) throws InvalidNameException;

  /**
   * Adds all the components of the given <code>Name</code> to the end of this
   * <code>Name</code>. The method modifies the current <code>Name</code> and
   * then returns it.
   *
   * @exception InvalidNameException if any of the given components is not a
   *            valid component for this <code>Name</code>.
   */
  Name addAll(Name suffix) throws InvalidNameException;

  /**
   * Inserts all the components of the given <code>Name</code> to this
   * <code>Name</code> at the given index. Components after this index
   * (if any) are shifted up. The method modifies the current
   * <code>Name</code> and then returns it.
   *
   * @exception ArrayIndexOutOfBoundsException if the given index is smaller
   *            then zero or greater then or equal to <code>size()</code>.
   * @exception InvalidNameException if any of the given components is not a
   *            valid component for this <code>Name</code>.
   */
  Name addAll(int posn, Name n) throws InvalidNameException;

  /**
   * Removes the component at the given index from this <code>Name</code>.
   * The method modifies the current <code>Name</code> and then returns it.
   *
   * @exception InvalidNameException if the given <code>String</code> is not a
   *            valid component for this <code>Name</code>.
   */
  Object remove(int posn) throws InvalidNameException;

  /**
   * Returns <code>true</code> if this <code>Name</code> starts with the
   * components of the given <code>Name</code>, <code>false</code> otherwise.
   */
  boolean startsWith(Name name);

  /**
   * Returns <code>true</code> if this <code>Name</code> ends with the
   * components of the given <code>Name</code>, <code>false</code> otherwise.
   */
  boolean endsWith(Name name);

  /**
   * Compares the given object to this <code>Name</code>.
   * Returns a negative value if the given <code>Object</code> is smaller then
   * this <code>Name</code>, a positive value if the <code>Object</code> is
   * bigger, and zero if the are equal. If the <code>Object</code> is not of
   * a class that can be compared to the class of this <code>Name</code> then
   * a <code>ClassCastException</code> is thrown. Note that it is not
   * guaranteed that <code>Name</code>s implemented in different classes can
   * be compared. The definition of smaller, bigger and equal is up to the
   * actual implementing class.
   */
  int compareTo(Object obj);

  /**
   * Returns a clone of this <code>Name</code>. It will be a deep copy of
   * all the components of the <code>Name</code> so that changes to components
   * of the components does not change the component in this <code>Name</code>.
   */
  Object clone();
}
