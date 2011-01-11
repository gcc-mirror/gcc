/* TypeNotPresentException.java -- Thrown when a string-based type is missing
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


package java.lang;

/**
 * <p>
 * Thrown when a type is accessed using a <code>String</code>-based
 * representation, but no definition of the supplied type is found.
 * This is effectively an unchecked equivalent of the existing
 * <code>ClassNotFound</code> exception.
 * </p>
 * <p>
 * It may occur due to an attempt to load a missing class, interface or
 * annotation, or when an undefined type variable is accessed.
 * </p>
 *
 * @author Tom Tromey (tromey@redhat.com)
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @see ClassNotFoundException
 * @since 1.5
 */
public class TypeNotPresentException
  extends RuntimeException
{
  private static final long serialVersionUID = -5101214195716534496L;

  /**
   * Constructs a <code>TypeNotPresentException</code> for
   * the supplied type.  The specified cause <code>Throwable</code>
   * may be used to provide additional history, with regards to the
   * root of the problem.  It is perfectly valid for this to be null,
   * if the cause of the problem is unknown.
   *
   * @param typeName the name of the missing type.
   * @param cause the cause of this exception, or null if the cause
   *              is unknown.
   */
  public TypeNotPresentException(String typeName, Throwable cause)
  {
    super("type \"" + typeName + "\" not found", cause);
    this.typeName = typeName;
  }

  /**
   * Returns the name of the missing type.
   *
   * @return the missing type's name.
   */
  public String typeName()
  {
    return typeName;
  }

  /**
   * The name of the missing type.
   *
   * @serial the missing type's name.
   */
  // Name fixed by serialization.
  private String typeName;

}
