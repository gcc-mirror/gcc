/* AnnotationTypeMismatchException.java - Thrown when annotation has changed
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

package java.lang.annotation;

import java.lang.reflect.Method;

/**
 * Thrown when accessing an element within an annotation for
 * which the type has changed, since compilation or serialization
 * took place.  The mismatch between the compiled or serialized
 * type and the current type causes this exception to be thrown.
 * 
 * @author Tom Tromey (tromey@redhat.com)
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public class AnnotationTypeMismatchException extends RuntimeException
{

  /**
   * For compatability with Sun's JDK
   */
  private static final long serialVersionUID = 8125925355765570191L;

  /**
   * Constructs an <code>AnnotationTypeMismatchException</code>
   * which is due to a mismatched type in the annotation
   * element, <code>m</code>. The erroneous type used for the
   * data in <code>m</code> is represented by the string,
   * <code>type</code>.  This string is of an undefined format,
   * and may contain the value as well as the type.
   *
   * @param m the element from the annotation.
   * @param type the name of the erroneous type found in <code>m</code>.
   */
  public AnnotationTypeMismatchException(Method m, String type)
  {
    this.element = m;
    this.foundType = type;
  }

  /**
   * Returns the element from the annotation, for which a
   * mismatch occurred.
   *
   * @return the element with the mismatched type.
   */
  public Method element()
  {
    return element;
  }

  /**
   * Returns the erroneous type used by the element,
   * represented as a <code>String</code>.  The format
   * of this <code>String</code> is not explicitly specified,
   * and may contain the value as well as the type.
   *
   * @return the type found in the element.
   */
  public String foundType()
  {
    return foundType;
  }

  // Names are chosen from serialization spec.
  /**
   * The element from the annotation.
   *
   * @serial the element with the mismatched type.
   */
  private Method element;

  /**
   * The erroneous type used by the element.
   *
   * @serial the type found in the element.
   */
  private String foundType;

}
