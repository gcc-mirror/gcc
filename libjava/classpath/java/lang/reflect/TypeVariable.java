/* TypeVariable.java
   Copyright (C) 2004 Free Software Foundation, Inc.

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


package java.lang.reflect;

/**
 * <p>
 * This is a common interface for all type variables provided by
 * the Java language.  Instances are created the first time a type
 * variable is needed by one of the reflective methods declared in
 * this package.
 * </p>
 * <p>
 * Creating a type variable requires resolving the appropriate type.
 * This may involve resolving other classes as a side effect (e.g.
 * if the type is nested inside other classes).  Creation should not
 * involve resolving the bounds.  Repeated creation has no effect; an
 * equivalent instance is returned.  Caching is not required, but all
 * instances must be <code>equal()</code> to each other.
 * </p>
 *
 * @author Tom Tromey (tromey@redhat.com)
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public interface TypeVariable<T extends GenericDeclaration> extends Type
{

  /**
   * Returns an array of <code>Type</code> objects which represent the upper
   * bounds of this type variable.  There is always a default bound of
   * <code>Object</code>.  Any <code>ParameterizedType</code>s will be
   * created as necessary, and other types resolved.
   *
   * @return an array of <code>Type</code> objects representing the upper
   *         bounds.
   * @throws TypeNotPresentException if any of the bounds refer to a
   *         non-existant type.
   * @throws MalformedParameterizedTypeException if the creation of a
   *         <code>ParameterizedType</code> fails.
   */
  Type[] getBounds();


  /**
   * Returns a representation of the declaration used to declare this
   * type variable.
   *
   * @return the <code>GenericDeclaration</code> object for this type
   *         variable.
   */
  T getGenericDeclaration();

  /**
   * Returns the name of the type variable, as written in the source
   * code.
   *
   * @return the name of the type variable.
   */
  String getName();
}
