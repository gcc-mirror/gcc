/* AttributeValue.java -- parent of type-safe enums of attributes
   Copyright (C) 2002 Free Software Foundation, Inc.

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


package java.awt;

/**
 * This class is undocumented by Sun, but it is the parent of several other
 * classes, all of which are type-safe enumerations. This takes care of
 * <code>equals</code>, <code>toString</code>, and <code>hashCode</code>, so
 * that you don't have to (although hashCode is commonly overridden).
 *
 * @author Eric Blake <ebb9@email.byu.edu>
 */
class AttributeValue
{
  /** The value of the enumeration. Package visible for speed. */
  final int value;

  /** The list of enumeration names for the given subclass. */
  private final String[] names;

  /**
   * Construct a type-safe enumeration element. For example,<br>
   * <pre>
   * class Foo extends AttributeValue
   * {
   *   private static final String[] names = { "one", "two" }
   *   public static final Foo ONE = new Foo(0);
   *   public static final Foo TWO = new Foo(1);
   *   private Foo(int value) { super(value, names); }
   * }
   * </pre>
   *
   * @param value the position of this enumeration element, consecutive from 0
   * @param names the constant list of enumeration names for the subclass
   */
  AttributeValue(int value, String[] names)
  {
    this.value = value;
    this.names = names;
  }

  /**
   * Returns the hashcode of this element. This is the index of the element
   * in the enumeration. Note that equals defaults to the == relation.
   *
   * @return the hashcode
   */
  public int hashCode()
  {
    return value;
  }

  /**
   * Returns the name of this enumeration element.
   *
   * @return the element name
   */
  public String toString()
  {
    return names[value];
  }
} // class AttributeValue
