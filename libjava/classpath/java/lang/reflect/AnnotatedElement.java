/* AnnotatedElement.java
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

import java.lang.annotation.Annotation;

/**
 * <p>
 * Represents an element that can be annotated.  The methods of this interface
 * provide reflection-based access to the annotations associated with a
 * particular element, such as a class, field, method or package.  Each
 * annotation returned by these methods is both immutable and serializable.
 * The returned arrays may be freely modified, without any effect on the
 * arrays returned to future callers.
 * </p>
 * <p>
 * If an annotation refers to a type or enumeration constant that is
 * inaccessible, then a <code>TypeNotPresentException</code> or
 * <code>EnumConstantNotPresentException</code> will be thrown.  Likewise,
 * invalid annotations will produce either a
 * <code>AnnotationTypeMismatchException</code> or
 * <code>IncompleteAnnotationException</code>.
 * </p>
 * 
 * @author Tom Tromey (tromey@redhat.com)
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public interface AnnotatedElement
{

  /**
   * Returns the element's annotation for the specified annotation type,
   * or <code>null</code> if no such annotation exists.
   *
   * @param annotationClass the type of annotation to look for.
   * @return this element's annotation for the specified type, or
   *         <code>null</code> if no such annotation exists.
   * @throws NullPointerException if the annotation class is <code>null</code>.
   */
  <T extends Annotation> T getAnnotation(Class<T> annotationClass);

  /**
   * Returns all annotations associated with the element.  If there are
   * no annotations associated with the element, then a zero-length array
   * will be returned.  The returned array may be modified by the client
   * code, but this will have no effect on the annotation content of the
   * element, and hence no effect on the return value of this method for
   * future callers.
   *
   * @return this element's annotations.
   */
  Annotation[] getAnnotations();

  /**
   * Returns all annotations directly defined by the element.  If there are
   * no annotations directly associated with the element, then a zero-length
   * array will be returned.  The returned array may be modified by the client
   * code, but this will have no effect on the annotation content of this
   * class, and hence no effect on the return value of this method for
   * future callers.
   *
   * @return the annotations directly defined by the element.
   * @since 1.5
   */
  Annotation[] getDeclaredAnnotations();

  /**
   * Returns true if an annotation for the specified type is associated
   * with the element.  This is primarily a short-hand for using marker
   * annotations.
   *
   * @param annotationClass the type of annotation to look for.
   * @return true if an annotation exists for the specified type.
   * @since 1.5
   */
  boolean isAnnotationPresent(Class<? extends Annotation> annotationClass);

}
