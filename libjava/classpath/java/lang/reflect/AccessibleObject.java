/* java.lang.reflect.AccessibleObject
   Copyright (C) 2001, 2005, 2006  Free Software Foundation, Inc.

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
 * This class is the superclass of various reflection classes, and
 * allows sufficiently trusted code to bypass normal restrictions to
 * do necessary things like invoke private methods outside of the
 * class during Serialization.  If you don't have a good reason
 * to mess with this, don't try. Fortunately, there are adequate
 * security checks before you can set a reflection object as accessible.
 *
 * @author Tom Tromey (tromey@cygnus.com)
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see Field
 * @see Constructor
 * @see Method
 * @see ReflectPermission
 * @since 1.2
 * @status updated to 1.5
 */
public class AccessibleObject
    implements AnnotatedElement
{
  /**
   * True if this object is marked accessible, which means the reflected
   * object bypasses normal security checks.
   */
  // default visibility for use by inherited classes
  boolean flag = false;

  /**
   * Only the three reflection classes that extend this can create an
   * accessible object.  This is not serializable for security reasons.
   */
  protected AccessibleObject()
  {
  }

  /**
   * Return the accessibility status of this object.
   *
   * @return true if this object bypasses security checks
   */
  public boolean isAccessible()
  {
    return flag;
  }

  /**
   * Convenience method to set the flag on a number of objects with a single
   * security check. If a security manager exists, it is checked for
   * <code>ReflectPermission("suppressAccessChecks")</code>.<p>
   *
   * It is forbidden to set the accessibility flag to true on any constructor
   * for java.lang.Class. This will result in a SecurityException. If the 
   * SecurityException is thrown for any of the passed AccessibleObjects,
   * the accessibility flag will be set on AccessibleObjects in the array prior 
   * to the one which resulted in the exception.
   *
   * @param array the array of accessible objects
   * @param flag the desired state of accessibility, true to bypass security
   * @throws NullPointerException if array is null
   * @throws SecurityException if the request is denied
   * @see SecurityManager#checkPermission(java.security.Permission)
   * @see RuntimePermission
   */
  public static void setAccessible(AccessibleObject[] array, boolean flag)
  {
    checkPermission();
    for (int i = 0; i < array.length; i++)
      array[i].secureSetAccessible(flag);
  }

  /**
   * Sets the accessibility flag for this reflection object. If a security
   * manager exists, it is checked for
   * <code>ReflectPermission("suppressAccessChecks")</code>.<p>
   *
   * It is forbidden to set the accessibility flag to true on any constructor for 
   * java.lang.Class. This will result in a SecurityException.
   *
   * @param flag the desired state of accessibility, true to bypass security
   * @throws NullPointerException if array is null
   * @throws SecurityException if the request is denied
   * @see SecurityManager#checkPermission(java.security.Permission)
   * @see RuntimePermission
   */
  public void setAccessible(boolean flag)
  {
    checkPermission();
    secureSetAccessible(flag);
  }

  /**
   * Performs the specified security check, for
   * <code>ReflectPermission("suppressAccessChecks")</code>.
   *
   * @throws SecurityException if permission is denied
   */
  private static void checkPermission()
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkPermission(new ReflectPermission("suppressAccessChecks"));
  }

  /**
   * Performs the actual accessibility change, this must always be invoked
   * after calling checkPermission.
   *
   * @param flag the desired status
   * @throws SecurityException if flag is true and this is a constructor
   * for <code>java.lang.Class</code>.
   */
  private void secureSetAccessible(boolean flag)
  {
    if (flag &&
        (this instanceof Constructor
          && ((Constructor) this).getDeclaringClass() == Class.class))
      throw new SecurityException("Cannot make object accessible: " + this);
    this.flag = flag;
  }

  public <T extends Annotation> T getAnnotation(Class<T> annotationClass)
  {
    throw new AssertionError("Subclass must override this method");
  }

  public Annotation[] getAnnotations()
  {
    return getDeclaredAnnotations();
  }

  public Annotation[] getDeclaredAnnotations()
  {
    throw new AssertionError("Subclass must override this method");
  }

  public boolean isAnnotationPresent(Class<? extends Annotation> annotationClass)
  {
    return getAnnotation(annotationClass) != null;
  }
}
