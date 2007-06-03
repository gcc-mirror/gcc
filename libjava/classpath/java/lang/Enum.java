/* Enum.java - Base class for all enums
   Copyright (C) 2004, 2005 Free Software Foundation

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

package java.lang;

import java.io.Serializable;
import java.lang.reflect.Field;

/**
 * This class represents a Java enumeration.  All enumerations are
 * subclasses of this class.
 *
 * @author Tom Tromey (tromey@redhat.com)
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public abstract class Enum<T extends Enum<T>>
  implements Comparable<T>, Serializable
{

  /**
   * For compatability with Sun's JDK
   */
  private static final long serialVersionUID = -4300926546619394005L;

  /**
   * The name of this enum constant.
   */
  final String name;

  /**
   * The number of this enum constant.  Each constant is given a number
   * which matches the order in which it was declared, starting with zero.
   */
  final int ordinal;

  /**
   * This constructor is used by the compiler to create enumeration constants.
   *
   * @param name the name of the enumeration constant.
   * @param ordinal the number of the enumeration constant, based on the
   *                declaration order of the constants and starting from zero.
   */
  protected Enum(String name, int ordinal)
  {
    this.name = name;
    this.ordinal = ordinal;
  }

  /**
   * Returns an Enum for a enum class given a description string of
   * the enum constant.
   *
   * @exception NullPointerException when etype or s are null.
   * @exception IllegalArgumentException when there is no value s in
   * the enum etype.
   */
  @SuppressWarnings("unchecked")
  public static <S extends Enum<S>> S valueOf(Class<S> etype, String s)
  {
    if (etype == null || s == null)
      throw new NullPointerException();

    try
      {
        // XXX We should not use getDeclaredField, because it does
        // an unnecessary security check.
        Field f = etype.getDeclaredField(s);
        if (! f.isEnumConstant())
          throw new IllegalArgumentException(s);
        Class.setAccessible(f);
        return (S) f.get(null);
      }
    catch (NoSuchFieldException exception)
      {
	throw new IllegalArgumentException(s);
      }
    catch (IllegalAccessException exception)
      {
	throw new Error("Unable to access Enum class");
      }
  }

  /**
   * Returns true if this enumeration is equivalent to the supplied object,
   * <code>o</code>.  Only one instance of an enumeration constant exists,
   * so the comparison is simply done using <code>==</code>.
   *
   * @param o the object to compare to this.
   * @return true if <code>this == o</code>.
   */
  public final boolean equals(Object o)
  {
    // Enum constants are singular, so we need only compare `=='.
    return this == o;
  }

  /**
   * Returns the hash code of this constant.  This is simply the ordinal.
   *
   * @return the hash code of this enumeration constant.
   */
  public final int hashCode()
  {
    return ordinal;
  }

  /**
   * Returns a textual representation of this enumeration constant.
   * By default, this is simply the declared name of the constant, but
   * specific enumeration types may provide an implementation more suited
   * to the data being stored.
   *
   * @return a textual representation of this constant.
   */
  public String toString()
  {
    return name;
  }

  /**
   * Returns an integer which represents the relative ordering of this
   * enumeration constant.  Enumeration constants are ordered by their
   * ordinals, which represents their declaration order.  So, comparing
   * two identical constants yields zero, while one declared prior to
   * this returns a positive integer and one declared after yields a
   * negative integer.
   *
   * @param e the enumeration constant to compare.
   * @return a negative integer if <code>e.ordinal < this.ordinal</code>,
   *         zero if <code>e.ordinal == this.ordinal</code> and a positive
   *         integer if <code>e.ordinal > this.ordinal</code>.
   * @throws ClassCastException if <code>e</code> is not an enumeration
   *                            constant of the same class.
   */ 
  public final int compareTo(T e)
  {
    if (getDeclaringClass() != e.getDeclaringClass())
      throw new ClassCastException();
    return ordinal - e.ordinal;
  }

  /**
   * Cloning of enumeration constants is prevented, to maintain their
   * singleton status.
   *
   * @return the cloned object.
   * @throws CloneNotSupportedException as enumeration constants can't be
   *         cloned.
   */
  protected final Object clone() throws CloneNotSupportedException
  {
    throw new CloneNotSupportedException("can't clone an enum constant");
  }

  /**
   * Returns the name of this enumeration constant.
   *
   * @return the name of the constant.
   */
  public final String name()
  {
    return name;
  }

  /**
   * Returns the number of this enumeration constant, which represents
   * the order in which it was originally declared, starting from zero.
   * 
   * @return the number of this constant.
   */
  public final int ordinal()
  {
    return ordinal;
  }

  /**
   * Returns the type of this enumeration constant.  This is the class
   * corresponding to the declaration of the enumeration.
   *
   * @return the type of this enumeration constant.
   */
  public final Class<T> getDeclaringClass()
  {
    Class k = getClass();
    // We might be in an anonymous subclass of the enum class, so go
    // up one more level.
    if (k.getSuperclass() != Enum.class)
      k = k.getSuperclass();
    return k;
  }

  /**
   * Enumerations can not have finalization methods.
   *
   * @since 1.6
   */
  protected final void finalize()
  {
  }

}
