/* ParameterizedType.java -- Represents parameterized types e.g. List<String>
   Copyright (C) 2004, 2005 Free Software Foundation, Inc.

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
 * Represents a type which is parameterized over one or more other
 * types.  For example, <code>List&lt;Integer&gt;</code> is a parameterized
 * type, with <code>List</code> parameterized over the type
 * <code>Integer</code>.
 * </p>
 * <p>
 * Instances of this classes are created as needed, during reflection.
 * On creating a parameterized type, <code>p</code>, the
 * <code>GenericTypeDeclaration</code> corresponding to <code>p</code>
 * is created and resolved.  Each type argument of <code>p</code>
 * is then created recursively; details of this process are availble
 * in the documentation of <code>TypeVariable</code>.  This creation
 * process only happens once; repetition has no effect.
 * </p>
 * <p>
 * Implementors of this interface must implement an appropriate
 * <code>equals()</code> method.  This method should equate any
 * two instances of the implementing class that have the same
 * <code>GenericTypeDeclaration</code> and <code>Type</code>
 * parameters.
 *
 * @author Tom Tromey (tromey@redhat.com)
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @see GenericDeclaration
 * @see TypeVariable
 * @since 1.5
 */
public interface ParameterizedType
  extends Type
{

  /**
   * <p>
   * Returns an array of <code>Type</code> objects, which gives
   * the parameters of this type.
   * </p>
   * <p>
   * <strong>Note</code>: the returned array may be empty.  This
   * occurs if the supposed <code>ParameterizedType</code> is simply
   * a normal type wrapped inside a parameterized type.
   * </p>
   *
   * @return an array of <code>Type</code>s, representing the arguments
   *         of this type.
   * @throws TypeNotPresentException if any of the types referred to by
   *         the parameters of this type do not actually exist.
   * @throws MalformedParameterizedTypeException if any of the types
   *         refer to a type which can not be instantiated.
   */
  Type[] getActualTypeArguments();

  /**
   * Returns the type of which this type is a member.  For example,
   * in <code>Top&lt;String&gt;.Bottom&lt;Integer&gt;</code>,
   * <code>Bottom&lt;Integer&gt;</code> is a member of
   * <code>Top&lt;String&gt;</code>, and so the latter is returned
   * by this method.  Calling this method on top-level types (such as
   * <code>Top&lt;String&gt;</code>) returns null.
   *
   * @return the type which owns this type.
   * @throws TypeNotPresentException if the owner type referred to by
   *         this type do not actually exist.
   * @throws MalformedParameterizedTypeException if the owner type
   *         referred to by this type can not be instantiated.
   */
  Type getOwnerType();

  /**
   * Returns a version of this type without parameters, which corresponds
   * to the class or interface which declared the type.  For example,
   * the raw type corresponding to <code>List&lt;Double&gt;</code>
   * is <code>List</code>, which was declared by the <code>List</code>
   * class.
   *
   * @return the raw variant of this type (i.e. the type without
   *         parameters).
   */
  Type getRawType();

}
