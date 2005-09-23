/* VMClass.java -- VM Specific Class methods
   Copyright (C) 2003, 2004 Free Software Foundation

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

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;

/*
 * This class is a reference version, mainly for compiling a class library
 * jar.  It is likely that VM implementers replace this with their own
 * version that can communicate effectively with the VM.
 */

/**
 *
 * @author Etienne Gagnon <etienne.gagnon@uqam.ca>
 * @author Archie Cobbs <archie@dellroad.org>
 * @author C. Brian Jones <cbj@gnu.org>
 */
final class VMClass 
{

  // Only static methods. Cannot be instantiated.
  private VMClass()
  {
  }

  /**
   * Discover whether an Object is an instance of this Class.  Think of it
   * as almost like <code>o instanceof (this class)</code>.
   *
   * @param klass the Class object that's calling us
   * @param o the Object to check
   * @return whether o is an instance of this class
   * @since 1.1
   */
  static native boolean isInstance(Class klass, Object o);

  /**
   * Discover whether an instance of the Class parameter would be an
   * instance of this Class as well.  Think of doing
   * <code>isInstance(c.newInstance())</code> or even
   * <code>c.newInstance() instanceof (this class)</code>. While this
   * checks widening conversions for objects, it must be exact for primitive
   * types.
   *
   * @param klass the Class object that's calling us
   * @param c the class to check
   * @return whether an instance of c would be an instance of this class
   *         as well
   * @throws NullPointerException if c is null
   * @since 1.1
   */
  static native boolean isAssignableFrom(Class klass, Class c);

  /**
   * Check whether this class is an interface or not.  Array types are not
   * interfaces.
   *
   * @param klass the Class object that's calling us
   * @return whether this class is an interface or not
   */
  static native boolean isInterface(Class klass);

  /**
   * Return whether this class is a primitive type.  A primitive type class
   * is a class representing a kind of "placeholder" for the various
   * primitive types, or void.  You can access the various primitive type
   * classes through java.lang.Boolean.TYPE, java.lang.Integer.TYPE, etc.,
   * or through boolean.class, int.class, etc.
   *
   * @param klass the Class object that's calling us
   * @return whether this class is a primitive type
   * @see Boolean#TYPE
   * @see Byte#TYPE
   * @see Character#TYPE
   * @see Short#TYPE
   * @see Integer#TYPE
   * @see Long#TYPE
   * @see Float#TYPE
   * @see Double#TYPE
   * @see Void#TYPE
   * @since 1.1
   */
  static native boolean isPrimitive(Class klass);

  /**
   * Get the name of this class, separated by dots for package separators.
   * Primitive types and arrays are encoded as:
   * <pre>
   * boolean             Z
   * byte                B
   * char                C
   * short               S
   * int                 I
   * long                J
   * float               F
   * double              D
   * void                V
   * array type          [<em>element type</em>
   * class or interface, alone: &lt;dotted name&gt;
   * class or interface, as element type: L&lt;dotted name&gt;;
   *
   * @param klass the Class object that's calling us
   * @return the name of this class
   */
  static native String getName(Class klass);

  /**
   * Get the direct superclass of this class.  If this is an interface,
   * Object, a primitive type, or void, it will return null. If this is an
   * array type, it will return Object.
   *
   * @param klass the Class object that's calling us
   * @return the direct superclass of this class
   */
  static native Class getSuperclass(Class klass);

  /**
   * Get the interfaces this class <EM>directly</EM> implements, in the
   * order that they were declared. This returns an empty array, not null,
   * for Object, primitives, void, and classes or interfaces with no direct
   * superinterface. Array types return Cloneable and Serializable.
   *
   * @param klass the Class object that's calling us
   * @return the interfaces this class directly implements
   */
  static native Class[] getInterfaces(Class klass);

  /**
   * If this is an array, get the Class representing the type of array.
   * Examples: "[[Ljava.lang.String;" would return "[Ljava.lang.String;", and
   * calling getComponentType on that would give "java.lang.String".  If
   * this is not an array, returns null.
   *
   * @param klass the Class object that's calling us
   * @return the array type of this class, or null
   * @see Array
   * @since 1.1
   */
  static native Class getComponentType(Class klass);

  /**
   * Get the modifiers of this class.  These can be decoded using Modifier,
   * and is limited to one of public, protected, or private, and any of
   * final, static, abstract, or interface. An array class has the same
   * public, protected, or private modifier as its component type, and is
   * marked final but not an interface. Primitive types and void are marked
   * public and final, but not an interface.
   *
   * @param klass the Class object that's calling us
   * @param ignoreInnerClassesAttrib if set, return the real modifiers, not
   * the ones specified in the InnerClasses attribute.
   * @return the modifiers of this class
   * @see Modifer
   * @since 1.1
   */
  static native int getModifiers(Class klass, boolean ignoreInnerClassesAttrib);

  /**
   * If this is a nested or inner class, return the class that declared it.
   * If not, return null.
   *
   * @param klass the Class object that's calling us
   * @return the declaring class of this class
   * @since 1.1
   */
  static native Class getDeclaringClass(Class klass);

  /**
   * Like <code>getDeclaredClasses()</code> but without the security checks.
   *
   * @param klass the Class object that's calling us
   * @param pulicOnly Only public classes should be returned
   */
  static native Class[] getDeclaredClasses(Class klass, boolean publicOnly);

  /**
   * Like <code>getDeclaredFields()</code> but without the security checks.
   *
   * @param klass the Class object that's calling us
   * @param pulicOnly Only public fields should be returned
   */
  static native Field[] getDeclaredFields(Class klass, boolean publicOnly);

  /**
   * Like <code>getDeclaredMethods()</code> but without the security checks.
   *
   * @param klass the Class object that's calling us
   * @param pulicOnly Only public methods should be returned
   */
  static native Method[] getDeclaredMethods(Class klass, boolean publicOnly);

  /**
   * Like <code>getDeclaredConstructors()</code> but without
   * the security checks.
   *
   * @param klass the Class object that's calling us
   * @param pulicOnly Only public constructors should be returned
   */
  static native Constructor[] getDeclaredConstructors(Class klass, boolean publicOnly);

  /**
   * Return the class loader of this class.
   *
   * @param klass the Class object that's calling us
   * @return the class loader
   */
  static native ClassLoader getClassLoader(Class klass);

  /**
   * Load the requested class and record the specified loader as the
   * initiating class loader.
   *
   * @param name the name of the class to find
   * @param initialize should the class initializer be run?
   * @param loader the class loader to use (or null for the bootstrap loader)
   * @return the Class object representing the class or null for noop
   * @throws ClassNotFoundException if the class was not found by the
   *         class loader
   * @throws LinkageError if linking the class fails
   * @throws ExceptionInInitializerError if the class loads, but an exception
   *         occurs during initialization
   */
  static native Class forName(String name, boolean initialize,
                              ClassLoader loader)
    throws ClassNotFoundException;

  /**
   * Return whether this class is an array type.
   *
   * @param klass the Class object that's calling us
   * @return true if this class is an array type
   * operation
   */
  static native boolean isArray(Class klass);

  /**
   * Throw a checked exception without declaring it.
   */
  static native void throwException(Throwable t);

} // class VMClass
