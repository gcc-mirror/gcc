/* Class.java -- Representation of a Java class.
   Copyright (C) 1998, 1999, 2000, 2002, 2003, 2004, 2005, 2006, 2007
   Free Software Foundation

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

import gnu.java.lang.reflect.ClassSignatureParser;
import java.io.InputStream;
import java.io.Serializable;
import java.lang.annotation.Annotation;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.GenericDeclaration;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Member;
import java.lang.reflect.Method;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.net.URL;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.security.ProtectionDomain;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.HashMap;
import java.util.Collection;
import java.lang.reflect.AnnotatedElement;
import java.lang.annotation.Annotation;
import java.lang.annotation.Inherited;
import java.lang.reflect.AccessibleObject;

/**
 * A Class represents a Java type.  There will never be multiple Class
 * objects with identical names and ClassLoaders. Primitive types, array
 * types, and void also have a Class object.
 *
 * <p>Arrays with identical type and number of dimensions share the same class.
 * The array class ClassLoader is the same as the ClassLoader of the element
 * type of the array (which can be null to indicate the bootstrap classloader).
 * The name of an array class is <code>[&lt;signature format&gt;;</code>.
 * <p> For example,
 * String[]'s class is <code>[Ljava.lang.String;</code>. boolean, byte,
 * short, char, int, long, float and double have the "type name" of
 * Z,B,S,C,I,J,F,D for the purposes of array classes.  If it's a
 * multidimensioned array, the same principle applies:
 * <code>int[][][]</code> == <code>[[[I</code>.
 *
 * <p>There is no public constructor - Class objects are obtained only through
 * the virtual machine, as defined in ClassLoaders.
 *
 * @serialData Class objects serialize specially:
 * <code>TC_CLASS ClassDescriptor</code>. For more serialization information,
 * see {@link ObjectStreamClass}.
 *
 * @author John Keiser
 * @author Eric Blake (ebb9@email.byu.edu)
 * @author Tom Tromey (tromey@cygnus.com)
 * @since 1.0
 * @see ClassLoader
 */
public final class Class<T>
  implements Type, AnnotatedElement, GenericDeclaration, Serializable
{
  /**
   * Class is non-instantiable from Java code; only the VM can create
   * instances of this class.
   */
  private Class ()
  {
  }

  // Initialize the class.
  private native void initializeClass ();

  // finalization
  protected native void finalize () throws Throwable;

  /**
   * Use the classloader of the current class to load, link, and initialize
   * a class. This is equivalent to your code calling
   * <code>Class.forName(name, true, getClass().getClassLoader())</code>.
   *
   * @param name the name of the class to find
   * @return the Class object representing the class
   * @throws ClassNotFoundException if the class was not found by the
   *         classloader
   * @throws LinkageError if linking the class fails
   * @throws ExceptionInInitializerError if the class loads, but an exception
   *         occurs during initialization
   */
  public static native Class<?> forName (String className)
    throws ClassNotFoundException;

  // A private internal method that is called by compiler-generated code.
  private static Class forName (String className, Class caller)
    throws ClassNotFoundException
  {
    return forName(className, true, caller.getClassLoaderInternal());
  }


  /**
   * Use the specified classloader to load and link a class. If the loader
   * is null, this uses the bootstrap class loader (provide the security
   * check succeeds). Unfortunately, this method cannot be used to obtain
   * the Class objects for primitive types or for void, you have to use
   * the fields in the appropriate java.lang wrapper classes.
   *
   * <p>Calls <code>classloader.loadclass(name, initialize)</code>.
   *
   * @param name the name of the class to find
   * @param initialize whether or not to initialize the class at this time
   * @param classloader the classloader to use to find the class; null means
   *        to use the bootstrap class loader
   * @throws ClassNotFoundException if the class was not found by the
   *         classloader
   * @throws LinkageError if linking the class fails
   * @throws ExceptionInInitializerError if the class loads, but an exception
   *         occurs during initialization
   * @throws SecurityException if the <code>classloader</code> argument
   *         is <code>null</code> and the caller does not have the
   *         <code>RuntimePermission("getClassLoader")</code> permission
   * @see ClassLoader
   * @since 1.2
   */
  public static native Class<?> forName (String className, boolean initialize,
					 ClassLoader loader)
    throws ClassNotFoundException;
  
  /**
   * Get all the public member classes and interfaces declared in this
   * class or inherited from superclasses. This returns an array of length
   * 0 if there are no member classes, including for primitive types. A
   * security check may be performed, with
   * <code>checkMemberAccess(this, Member.PUBLIC)</code> as well as
   * <code>checkPackageAccess</code> both having to succeed.
   *
   * @return all public member classes in this class
   * @throws SecurityException if the security check fails
   * @since 1.1
   */
  public Class<?>[] getClasses()
  {
    memberAccessCheck(Member.PUBLIC);
    return internalGetClasses();
  }

  /**
   * Like <code>getClasses()</code> but without the security checks.
   */
  private Class<?>[] internalGetClasses()
  {
    ArrayList<Class> list = new ArrayList<Class>();
    list.addAll(Arrays.asList(getDeclaredClasses(true)));
    Class superClass = getSuperclass();
    if (superClass != null)
      list.addAll(Arrays.asList(superClass.internalGetClasses()));
    return list.toArray(new Class<?>[list.size()]);
  }

  /**
   * Get the ClassLoader that loaded this class.  If the class was loaded
   * by the bootstrap classloader, this method will return null.
   * If there is a security manager, and the caller's class loader is not
   * an ancestor of the requested one, a security check of
   * <code>RuntimePermission("getClassLoader")</code>
   * must first succeed. Primitive types and void return null.
   *
   * @return the ClassLoader that loaded this class
   * @throws SecurityException if the security check fails
   * @see ClassLoader
   * @see RuntimePermission
   */
  public native ClassLoader getClassLoader ();

  // A private internal method that is called by compiler-generated code.
  private final native ClassLoader getClassLoader (Class caller);

  /**
   *  Internal method that circumvents the usual security checks when 
   *  getting the class loader.
   */
  private native ClassLoader getClassLoaderInternal ();

  /**
   * If this is an array, get the Class representing the type of array.
   * Examples: "[[Ljava.lang.String;" would return "[Ljava.lang.String;", and
   * calling getComponentType on that would give "java.lang.String".  If
   * this is not an array, returns null.
   *
   * @return the array type of this class, or null
   * @see Array
   * @since 1.1
   */
  public native Class<?> getComponentType ();

  /**
   * Get a public constructor declared in this class. If the constructor takes
   * no argument, an array of zero elements and null are equivalent for the
   * types argument. A security check may be performed, with
   * <code>checkMemberAccess(this, Member.PUBLIC)</code> as well as
   * <code>checkPackageAccess</code> both having to succeed.
   *
   * @param types the type of each parameter
   * @return the constructor
   * @throws NoSuchMethodException if the constructor does not exist
   * @throws SecurityException if the security check fails
   * @see #getConstructors()
   * @since 1.1
   */
  public native Constructor<T> getConstructor(Class<?>... args)
    throws NoSuchMethodException;

  /**
   * Get all the public constructors of this class. This returns an array of
   * length 0 if there are no constructors, including for primitive types,
   * arrays, and interfaces. It does, however, include the default
   * constructor if one was supplied by the compiler. A security check may
   * be performed, with <code>checkMemberAccess(this, Member.PUBLIC)</code>
   * as well as <code>checkPackageAccess</code> both having to succeed.
   *
   * @return all public constructors in this class
   * @throws SecurityException if the security check fails
   * @since 1.1
   */
  public Constructor<?>[] getConstructors()
  {
    memberAccessCheck(Member.PUBLIC);
    return getDeclaredConstructors(true);
  }

  /**
   * Get a constructor declared in this class. If the constructor takes no
   * argument, an array of zero elements and null are equivalent for the
   * types argument. A security check may be performed, with
   * <code>checkMemberAccess(this, Member.DECLARED)</code> as well as
   * <code>checkPackageAccess</code> both having to succeed.
   *
   * @param types the type of each parameter
   * @return the constructor
   * @throws NoSuchMethodException if the constructor does not exist
   * @throws SecurityException if the security check fails
   * @see #getDeclaredConstructors()
   * @since 1.1
   */
  public native Constructor<T> getDeclaredConstructor(Class<?>... args)
    throws NoSuchMethodException;

  /**
   * Get all the declared member classes and interfaces in this class, but
   * not those inherited from superclasses. This returns an array of length
   * 0 if there are no member classes, including for primitive types. A
   * security check may be performed, with
   * <code>checkMemberAccess(this, Member.DECLARED)</code> as well as
   * <code>checkPackageAccess</code> both having to succeed.
   *
   * @return all declared member classes in this class
   * @throws SecurityException if the security check fails
   * @since 1.1
   */
  public Class<?>[] getDeclaredClasses()
  {
    memberAccessCheck(Member.DECLARED);
    return getDeclaredClasses(false);
  }

  native Class<?>[] getDeclaredClasses (boolean publicOnly);

  /**
   * Get all the declared constructors of this class. This returns an array of
   * length 0 if there are no constructors, including for primitive types,
   * arrays, and interfaces. It does, however, include the default
   * constructor if one was supplied by the compiler. A security check may
   * be performed, with <code>checkMemberAccess(this, Member.DECLARED)</code>
   * as well as <code>checkPackageAccess</code> both having to succeed.
   *
   * @return all constructors in this class
   * @throws SecurityException if the security check fails
   * @since 1.1
   */
  public Constructor<?>[] getDeclaredConstructors()
  {
    memberAccessCheck(Member.DECLARED);
    return getDeclaredConstructors(false);
  }

  native Constructor<?>[] getDeclaredConstructors (boolean publicOnly);

  /**
   * Get a field declared in this class, where name is its simple name. The
   * implicit length field of arrays is not available. A security check may
   * be performed, with <code>checkMemberAccess(this, Member.DECLARED)</code>
   * as well as <code>checkPackageAccess</code> both having to succeed.
   *
   * @param name the name of the field
   * @return the field
   * @throws NoSuchFieldException if the field does not exist
   * @throws SecurityException if the security check fails
   * @see #getDeclaredFields()
   * @since 1.1
   */
  public native Field getDeclaredField(String fieldName)
    throws NoSuchFieldException;

  /**
   * Get all the declared fields in this class, but not those inherited from
   * superclasses. This returns an array of length 0 if there are no fields,
   * including for primitive types. This does not return the implicit length
   * field of arrays. A security check may be performed, with
   * <code>checkMemberAccess(this, Member.DECLARED)</code> as well as
   * <code>checkPackageAccess</code> both having to succeed.
   *
   * @return all declared fields in this class
   * @throws SecurityException if the security check fails
   * @since 1.1
   */
  public Field[] getDeclaredFields()
  {
    memberAccessCheck(Member.DECLARED);
    return getDeclaredFields(false);
  }

  native Field[] getDeclaredFields (boolean publicOnly);

  private native Method _getDeclaredMethod(String methodName, Class[] args);

  /**
   * Get a method declared in this class, where name is its simple name. The
   * implicit methods of Object are not available from arrays or interfaces.
   * Constructors (named "&lt;init&gt;" in the class file) and class initializers
   * (name "&lt;clinit&gt;") are not available.  The Virtual Machine allows
   * multiple methods with the same signature but differing return types; in
   * such a case the most specific return types are favored, then the final
   * choice is arbitrary. If the method takes no argument, an array of zero
   * elements and null are equivalent for the types argument. A security
   * check may be performed, with
   * <code>checkMemberAccess(this, Member.DECLARED)</code> as well as
   * <code>checkPackageAccess</code> both having to succeed.
   *
   * @param methodName the name of the method
   * @param types the type of each parameter
   * @return the method
   * @throws NoSuchMethodException if the method does not exist
   * @throws SecurityException if the security check fails
   * @see #getDeclaredMethods()
   * @since 1.1
   */
  public Method getDeclaredMethod(String methodName, Class<?>... args)
    throws NoSuchMethodException
  {
    memberAccessCheck(Member.DECLARED);

    if ("<init>".equals(methodName) || "<clinit>".equals(methodName))
      throw new NoSuchMethodException(methodName);

    Method match = _getDeclaredMethod(methodName, args);
    if (match == null)
      throw new NoSuchMethodException(methodName);
    return match;
  }

  /**
   * Get all the declared methods in this class, but not those inherited from
   * superclasses. This returns an array of length 0 if there are no methods,
   * including for primitive types. This does include the implicit methods of
   * arrays and interfaces which mirror methods of Object, nor does it
   * include constructors or the class initialization methods. The Virtual
   * Machine allows multiple methods with the same signature but differing
   * return types; all such methods are in the returned array. A security
   * check may be performed, with
   * <code>checkMemberAccess(this, Member.DECLARED)</code> as well as
   * <code>checkPackageAccess</code> both having to succeed.
   *
   * @return all declared methods in this class
   * @throws SecurityException if the security check fails
   * @since 1.1
   */
  public native Method[] getDeclaredMethods();
 
  /**
   * If this is a nested or inner class, return the class that declared it.
   * If not, return null.
   *
   * @return the declaring class of this class
   * @since 1.1
   */
  // This is marked as unimplemented in the JCL book.
  public native Class<?> getDeclaringClass ();

  private native Field getField (String fieldName, int hash)
    throws NoSuchFieldException;

  /**
   * Get a public field declared or inherited in this class, where name is
   * its simple name. If the class contains multiple accessible fields by
   * that name, an arbitrary one is returned. The implicit length field of
   * arrays is not available. A security check may be performed, with
   * <code>checkMemberAccess(this, Member.PUBLIC)</code> as well as
   * <code>checkPackageAccess</code> both having to succeed.
   *
   * @param fieldName the name of the field
   * @return the field
   * @throws NoSuchFieldException if the field does not exist
   * @throws SecurityException if the security check fails
   * @see #getFields()
   * @since 1.1
   */
  public Field getField(String fieldName)
    throws NoSuchFieldException
  {
    memberAccessCheck(Member.PUBLIC);
    Field field = getField(fieldName, fieldName.hashCode());
    if (field == null)
      throw new NoSuchFieldException(fieldName);
    return field;
  }

  /**
   * Get all the public fields declared in this class or inherited from
   * superclasses. This returns an array of length 0 if there are no fields,
   * including for primitive types. This does not return the implicit length
   * field of arrays. A security check may be performed, with
   * <code>checkMemberAccess(this, Member.PUBLIC)</code> as well as
   * <code>checkPackageAccess</code> both having to succeed.
   *
   * @return all public fields in this class
   * @throws SecurityException if the security check fails
   * @since 1.1
   */
  public Field[] getFields()
  {
    memberAccessCheck(Member.PUBLIC);
    return internalGetFields();
  }

  /**
   * Like <code>getFields()</code> but without the security checks.
   */
  private Field[] internalGetFields()
  {
    LinkedHashSet set = new LinkedHashSet();
    set.addAll(Arrays.asList(getDeclaredFields(true)));
    Class[] interfaces = getInterfaces();
    for (int i = 0; i < interfaces.length; i++)
      set.addAll(Arrays.asList(interfaces[i].internalGetFields()));
    Class superClass = getSuperclass();
    if (superClass != null)
      set.addAll(Arrays.asList(superClass.internalGetFields()));
    return (Field[])set.toArray(new Field[set.size()]);
  }

  /**
   * Returns the <code>Package</code> in which this class is defined
   * Returns null when this information is not available from the
   * classloader of this class.
   *
   * @return the package for this class, if it is available
   * @since 1.2
   */
  public Package getPackage()
  {
    ClassLoader cl = getClassLoaderInternal();
    if (cl != null)
      return cl.getPackage(getPackagePortion(getName()));
    else
      return VMClassLoader.getPackage(getPackagePortion(getName()));
  }

  /**
   * Get the interfaces this class <em>directly</em> implements, in the
   * order that they were declared. This returns an empty array, not null,
   * for Object, primitives, void, and classes or interfaces with no direct
   * superinterface. Array types return Cloneable and Serializable.
   *
   * @return the interfaces this class directly implements
   */
  public native Class<?>[] getInterfaces ();

  private final native void getSignature(StringBuffer buffer);
  private static final native String getSignature(Class[] args,
						  boolean is_construtor);

  public native Method _getMethod(String methodName, Class[] args);

  /**
   * Get a public method declared or inherited in this class, where name is
   * its simple name. The implicit methods of Object are not available from
   * interfaces.  Constructors (named "&lt;init&gt;" in the class file) and class
   * initializers (name "&lt;clinit&gt;") are not available.  The Virtual
   * Machine allows multiple methods with the same signature but differing
   * return types, and the class can inherit multiple methods of the same
   * return type; in such a case the most specific return types are favored,
   * then the final choice is arbitrary. If the method takes no argument, an
   * array of zero elements and null are equivalent for the types argument.
   * A security check may be performed, with
   * <code>checkMemberAccess(this, Member.PUBLIC)</code> as well as
   * <code>checkPackageAccess</code> both having to succeed.
   *
   * @param methodName the name of the method
   * @param types the type of each parameter
   * @return the method
   * @throws NoSuchMethodException if the method does not exist
   * @throws SecurityException if the security check fails
   * @see #getMethods()
   * @since 1.1
   */
  public Method getMethod(String methodName, Class<?>... args)
    throws NoSuchMethodException
  {
    memberAccessCheck(Member.PUBLIC);

    if ("<init>".equals(methodName) || "<clinit>".equals(methodName))
      throw new NoSuchMethodException(methodName);

    Method method = _getMethod(methodName, args);
    if (method == null)
      throw new NoSuchMethodException(methodName);
    return method;
  }

  private native int _getMethods (Method[] result, int offset);
  
  /**
   * Get all the public methods declared in this class or inherited from
   * superclasses. This returns an array of length 0 if there are no methods,
   * including for primitive types. This does not include the implicit
   * methods of interfaces which mirror methods of Object, nor does it
   * include constructors or the class initialization methods. The Virtual
   * Machine allows multiple methods with the same signature but differing
   * return types; all such methods are in the returned array. A security
   * check may be performed, with
   * <code>checkMemberAccess(this, Member.PUBLIC)</code> as well as
   * <code>checkPackageAccess</code> both having to succeed.
   *
   * @return all public methods in this class
   * @throws SecurityException if the security check fails
   * @since 1.1
   */
  public native Method[] getMethods();

  /**
   * Get the modifiers of this class.  These can be decoded using Modifier,
   * and is limited to one of public, protected, or private, and any of
   * final, static, abstract, or interface. An array class has the same
   * public, protected, or private modifier as its component type, and is
   * marked final but not an interface. Primitive types and void are marked
   * public and final, but not an interface.
   *
   * @return the modifiers of this class
   * @see Modifer
   * @since 1.1
   */
  public native int getModifiers ();
  
  /**
   * Get the name of this class, separated by dots for package separators.
   * If the class represents a primitive type, or void, then the
   * name of the type as it appears in the Java programming language
   * is returned.  For instance, <code>Byte.TYPE.getName()</code>
   * returns "byte".
   *
   * Arrays are specially encoded as shown on this table.
   * <pre>
   * array type          [<em>element type</em>
   *                     (note that the element type is encoded per
   *                      this table)
   * boolean             Z
   * byte                B
   * char                C
   * short               S
   * int                 I
   * long                J
   * float               F
   * double              D
   * void                V
   * class or interface, alone: &lt;dotted name&gt;
   * class or interface, as element type: L&lt;dotted name&gt;;
   * </pre>
   *
   * @return the name of this class
   */
  public native String getName ();

  /**
   * Get a resource URL using this class's package using the
   * getClassLoader().getResource() method.  If this class was loaded using
   * the system classloader, ClassLoader.getSystemResource() is used instead.
   *
   * <p>If the name you supply is absolute (it starts with a <code>/</code>),
   * then the leading <code>/</code> is removed and it is passed on to
   * getResource(). If it is relative, the package name is prepended, and
   * <code>.</code>'s are replaced with <code>/</code>.
   *
   * <p>The URL returned is system- and classloader-dependent, and could
   * change across implementations.
   *
   * @param resourceName the name of the resource, generally a path
   * @return the URL to the resource
   * @throws NullPointerException if name is null
   * @since 1.1
   */
  public URL getResource(String resourceName)
  {
    String name = resourcePath(resourceName);
    ClassLoader loader = getClassLoaderInternal();
    if (loader == null)
      return ClassLoader.getSystemResource(name);
    return loader.getResource(name);
  }

  /**
   * Get a resource using this class's package using the
   * getClassLoader().getResourceAsStream() method.  If this class was loaded
   * using the system classloader, ClassLoader.getSystemResource() is used
   * instead.
   *
   * <p>If the name you supply is absolute (it starts with a <code>/</code>),
   * then the leading <code>/</code> is removed and it is passed on to
   * getResource(). If it is relative, the package name is prepended, and
   * <code>.</code>'s are replaced with <code>/</code>.
   *
   * <p>The URL returned is system- and classloader-dependent, and could
   * change across implementations.
   *
   * @param resourceName the name of the resource, generally a path
   * @return an InputStream with the contents of the resource in it, or null
   * @throws NullPointerException if name is null
   * @since 1.1
   */
  public InputStream getResourceAsStream(String resourceName)
  {
    String name = resourcePath(resourceName);
    ClassLoader loader = getClassLoaderInternal();
    if (loader == null)
      return ClassLoader.getSystemResourceAsStream(name);
    return loader.getResourceAsStream(name);
  }

  private String resourcePath(String resourceName)
  {
    if (resourceName.length() > 0)
      {
	if (resourceName.charAt(0) != '/')
	  {
	    String pkg = getPackagePortion(getName());
	    if (pkg.length() > 0)
	      resourceName = pkg.replace('.','/') + '/' + resourceName;
	  }
	else
	  {
	    resourceName = resourceName.substring(1);
	  }
      }
    return resourceName;
  }

  /**
   * Get the signers of this class. This returns null if there are no signers,
   * such as for primitive types or void.
   *
   * @return the signers of this class
   * @since 1.1
   */
  public native Object[] getSigners ();
  
  /**
   * Set the signers of this class.
   *
   * @param signers the signers of this class
   */
  native void setSigners(Object[] signers);

  /**
   * Get the direct superclass of this class.  If this is an interface,
   * Object, a primitive type, or void, it will return null. If this is an
   * array type, it will return Object.
   *
   * @return the direct superclass of this class
   */
  public native Class<? super T> getSuperclass ();
  
  /**
   * Return whether this class is an array type.
   *
   * @return whether this class is an array type
   * @since 1.1
   */
  public native boolean isArray ();
  
  /**
   * Discover whether an instance of the Class parameter would be an
   * instance of this Class as well.  Think of doing
   * <code>isInstance(c.newInstance())</code> or even
   * <code>c.newInstance() instanceof (this class)</code>. While this
   * checks widening conversions for objects, it must be exact for primitive
   * types.
   *
   * @param c the class to check
   * @return whether an instance of c would be an instance of this class
   *         as well
   * @throws NullPointerException if c is null
   * @since 1.1
   */
  public native boolean isAssignableFrom (Class<?> c);
 
  /**
   * Discover whether an Object is an instance of this Class.  Think of it
   * as almost like <code>o instanceof (this class)</code>.
   *
   * @param o the Object to check
   * @return whether o is an instance of this class
   * @since 1.1
   */
  public native boolean isInstance (Object o);
  
  /**
   * Check whether this class is an interface or not.  Array types are not
   * interfaces.
   *
   * @return whether this class is an interface or not
   */
  public native boolean isInterface ();
  
  /**
   * Return whether this class is a primitive type.  A primitive type class
   * is a class representing a kind of "placeholder" for the various
   * primitive types, or void.  You can access the various primitive type
   * classes through java.lang.Boolean.TYPE, java.lang.Integer.TYPE, etc.,
   * or through boolean.class, int.class, etc.
   *
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
  public native boolean isPrimitive ();
  
  /**
   * Get a new instance of this class by calling the no-argument constructor.
   * The class is initialized if it has not been already. A security check
   * may be performed, with <code>checkMemberAccess(this, Member.PUBLIC)</code>
   * as well as <code>checkPackageAccess</code> both having to succeed.
   *
   * @return a new instance of this class
   * @throws InstantiationException if there is not a no-arg constructor
   *         for this class, including interfaces, abstract classes, arrays,
   *         primitive types, and void; or if an exception occurred during
   *         the constructor
   * @throws IllegalAccessException if you are not allowed to access the
   *         no-arg constructor because of scoping reasons
   * @throws SecurityException if the security check fails
   * @throws ExceptionInInitializerError if class initialization caused by
   *         this call fails with an exception
   */
  public native T newInstance ()
    throws InstantiationException, IllegalAccessException;

  // We need a native method to retrieve the protection domain, because we
  // can't add fields to java.lang.Class that are accessible from Java.
  private native ProtectionDomain getProtectionDomain0();

  /**
   * Returns the protection domain of this class. If the classloader did not
   * record the protection domain when creating this class the unknown
   * protection domain is returned which has a <code>null</code> code source
   * and all permissions. A security check may be performed, with
   * <code>RuntimePermission("getProtectionDomain")</code>.
   *
   * @return the protection domain
   * @throws SecurityException if the security manager exists and the caller
   * does not have <code>RuntimePermission("getProtectionDomain")</code>.
   * @see RuntimePermission
   * @since 1.2
   */
  public ProtectionDomain getProtectionDomain()
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkPermission(VMClassLoader.protectionDomainPermission);
    
    ProtectionDomain protectionDomain = getProtectionDomain0();

    if (protectionDomain == null)
      return VMClassLoader.unknownProtectionDomain;
    else
      return protectionDomain;
  }

  /**
   * Return the human-readable form of this Object.  For an object, this
   * is either "interface " or "class " followed by <code>getName()</code>,
   * for primitive types and void it is just <code>getName()</code>.
   *
   * @return the human-readable form of this Object
   */
  public String toString()
  {
    if (isPrimitive())
      return getName();
    return (isInterface() ? "interface " : "class ") + getName();
  }

  /**
   * Returns the desired assertion status of this class, if it were to be
   * initialized at this moment. The class assertion status, if set, is
   * returned; the backup is the default package status; then if there is
   * a class loader, that default is returned; and finally the system default
   * is returned. This method seldom needs calling in user code, but exists
   * for compilers to implement the assert statement. Note that there is no
   * guarantee that the result of this method matches the class's actual
   * assertion status.
   *
   * @return the desired assertion status
   * @see ClassLoader#setClassAssertionStatus(String, boolean)
   * @see ClassLoader#setPackageAssertionStatus(String, boolean)
   * @see ClassLoader#setDefaultAssertionStatus(boolean)
   * @since 1.4
   */
  public boolean desiredAssertionStatus()
  {
    ClassLoader c = getClassLoaderInternal();
    Object status;
    if (c == null)
      return VMClassLoader.defaultAssertionStatus();
    if (c.classAssertionStatus != null)
      synchronized (c)
        {
          status = c.classAssertionStatus.get(getName());
          if (status != null)
            return status.equals(Boolean.TRUE);
        }
    else
      {
        status = ClassLoader.systemClassAssertionStatus.get(getName());
        if (status != null)
          return status.equals(Boolean.TRUE);
      }
    if (c.packageAssertionStatus != null)
      synchronized (c)
        {
          String name = getPackagePortion(getName());
          if ("".equals(name))
            status = c.packageAssertionStatus.get(null);
          else
            do
              {
                status = c.packageAssertionStatus.get(name);
                name = getPackagePortion(name);
              }
            while (! "".equals(name) && status == null);
          if (status != null)
            return status.equals(Boolean.TRUE);
        }
    else
      {
        String name = getPackagePortion(getName());
        if ("".equals(name))
          status = ClassLoader.systemPackageAssertionStatus.get(null);
        else
          do
            {
              status = ClassLoader.systemPackageAssertionStatus.get(name);
              name = getPackagePortion(name);
            }
          while (! "".equals(name) && status == null);
        if (status != null)
          return status.equals(Boolean.TRUE);
      }
    return c.defaultAssertionStatus;
  }

  /**
   * Strip the last portion of the name (after the last dot).
   *
   * @param name the name to get package of
   * @return the package name, or "" if no package
   */
  private static String getPackagePortion(String name)
  {
    int lastInd = name.lastIndexOf('.');
    if (lastInd == -1)
      return "";
    return name.substring(0, lastInd);
  }

  /**
   * Perform security checks common to all of the methods that
   * get members of this Class.
   */
  private void memberAccessCheck(int which)
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      {
	sm.checkMemberAccess(this, which);
	Package pkg = getPackage();
	if (pkg != null)
	  sm.checkPackageAccess(pkg.getName());
      }
  }


  /**
   * <p>
   * Casts this class to represent a subclass of the specified class.
   * This method is useful for `narrowing' the type of a class so that
   * the class object, and instances of that class, can match the contract
   * of a more restrictive method.  For example, if this class has the
   * static type of <code>Class&lt;Object&gt;</code>, and a dynamic type of
   * <code>Class&lt;Rectangle&gt;</code>, then, assuming <code>Shape</code> is
   * a superclass of <code>Rectangle</code>, this method can be used on
   * this class with the parameter, <code>Class&lt;Shape&gt;</code>, to retain
   * the same instance but with the type
   * <code>Class&lt;? extends Shape&gt;</code>.
   * </p>
   * <p>
   * If this class can be converted to an instance which is parameterised
   * over a subtype of the supplied type, <code>U</code>, then this method
   * returns an appropriately cast reference to this object.  Otherwise,
   * a <code>ClassCastException</code> is thrown.
   * </p>
   * 
   * @param klass the class object, the parameterized type (<code>U</code>) of
   *              which should be a superclass of the parameterized type of
   *              this instance.
   * @return a reference to this object, appropriately cast.
   * @throws ClassCastException if this class can not be converted to one
   *                            which represents a subclass of the specified
   *                            type, <code>U</code>. 
   * @since 1.5
   */
  public <U> Class<? extends U> asSubclass(Class<U> klass)
  {
    if (! klass.isAssignableFrom(this))
      throw new ClassCastException();
    return (Class<? extends U>) this;
  }

  /**
   * Returns the specified object, cast to this <code>Class</code>' type.
   *
   * @param obj the object to cast
   * @throws ClassCastException  if obj is not an instance of this class
   * @since 1.5
   */
  public T cast(Object obj)
  {
    if (obj != null && ! isInstance(obj))
      throw new ClassCastException();
    return (T) obj;
  }

  /**
   * Returns the enumeration constants of this class, or
   * null if this class is not an <code>Enum</code>.
   *
   * @return an array of <code>Enum</code> constants
   *         associated with this class, or null if this
   *         class is not an <code>enum</code>.
   * @since 1.5
   */
  public T[] getEnumConstants()
  {
    if (isEnum())
      {
	try
	  {
            Method m = getMethod("values");
            setAccessible(m);
	    return (T[]) m.invoke(null);
	  }
	catch (NoSuchMethodException exception)
	  {
	    throw new Error("Enum lacks values() method");
	  }
	catch (IllegalAccessException exception)
	  {
	    throw new Error("Unable to access Enum class");
	  }
	catch (InvocationTargetException exception)
	  {
	    throw new
	      RuntimeException("The values method threw an exception",
			       exception);
	  }
      }
    else
      {
	return null;
      }
  }

  /**
   * Returns true if this class is an <code>Enum</code>.
   *
   * @return true if this is an enumeration class.
   * @since 1.5
   */
  public native boolean isEnum();


  /**
   * Returns true if this class is a synthetic class, generated by
   * the compiler.
   *
   * @return true if this is a synthetic class.
   * @since 1.5
   */
  public native boolean isSynthetic();


  /**
   * Returns true if this class is an <code>Annotation</code>.
   *
   * @return true if this is an annotation class.
   * @since 1.5
   */
  public native boolean isAnnotation();


  /**
   * Returns the simple name for this class, as used in the source
   * code.  For normal classes, this is the content returned by
   * <code>getName()</code> which follows the last ".".  Anonymous
   * classes have no name, and so the result of calling this method is
   * "".  The simple name of an array consists of the simple name of
   * its component type, followed by "[]".  Thus, an array with the
   * component type of an anonymous class has a simple name of simply
   * "[]".
   *
   * @return the simple name for this class.
   * @since 1.5
   */
  public String getSimpleName()
  {
    StringBuffer sb = new StringBuffer();
    Class klass = this;
    int arrayCount = 0;
    while (klass.isArray())
      {
	klass = klass.getComponentType();
	++arrayCount;
      }
    if (! klass.isAnonymousClass())
      {
	String fullName = klass.getName();
	sb.append(fullName, fullName.lastIndexOf(".") + 1, fullName.length());
      }
    while (arrayCount-- > 0)
      sb.append("[]");
    return sb.toString();
  }

  /**
   * Returns the class which immediately encloses this class.  If this class
   * is a top-level class, this method returns <code>null</code>.
   *
   * @return the immediate enclosing class, or <code>null</code> if this is
   *         a top-level class.
   * @since 1.5
   */
  public native Class<?> getEnclosingClass();

  /**
   * Returns the constructor which immediately encloses this class.  If
   * this class is a top-level class, or a local or anonymous class
   * immediately enclosed by a type definition, instance initializer
   * or static initializer, then <code>null</code> is returned.
   *
   * @return the immediate enclosing constructor if this class is
   *         declared within a constructor.  Otherwise, <code>null</code>
   *         is returned.
   * @since 1.5
   */
  public native Constructor<T> getEnclosingConstructor();

  /**
   * Returns the method which immediately encloses this class.  If
   * this class is a top-level class, or a local or anonymous class
   * immediately enclosed by a type definition, instance initializer
   * or static initializer, then <code>null</code> is returned.
   *
   * @return the immediate enclosing method if this class is
   *         declared within a method.  Otherwise, <code>null</code>
   *         is returned.
   * @since 1.5
   */
  public native Method getEnclosingMethod();

  private native String getClassSignature();

  /**
   * <p>
   * Returns an array of <code>Type</code> objects which represent the
   * interfaces directly implemented by this class or extended by this
   * interface.
   * </p>
   * <p>
   * If one of the superinterfaces is a parameterized type, then the
   * object returned for this interface reflects the actual type
   * parameters used in the source code.  Type parameters are created
   * using the semantics specified by the <code>ParameterizedType</code>
   * interface, and only if an instance has not already been created.
   * </p>
   * <p>
   * The order of the interfaces in the array matches the order in which
   * the interfaces are declared.  For classes which represent an array,
   * an array of two interfaces, <code>Cloneable</code> and
   * <code>Serializable</code>, is always returned, with the objects in
   * that order.  A class representing a primitive type or void always
   * returns an array of zero size.
   * </p>
   *
   * @return an array of interfaces implemented or extended by this class.
   * @throws GenericSignatureFormatError if the generic signature of one
   *         of the interfaces does not comply with that specified by the Java
   *         Virtual Machine specification, 3rd edition.
   * @throws TypeNotPresentException if any of the superinterfaces refers
   *         to a non-existant type.
   * @throws MalformedParameterizedTypeException if any of the interfaces
   *         refer to a parameterized type that can not be instantiated for
   *         some reason.
   * @since 1.5
   * @see java.lang.reflect.ParameterizedType
   */
  public Type[] getGenericInterfaces()
  {
    if (isPrimitive())
      return new Type[0];

    String sig = getClassSignature();
    if (sig == null)
      return getInterfaces();

    ClassSignatureParser p = new ClassSignatureParser(this, sig);
    return p.getInterfaceTypes();
  }

  /**
   * <p>
   * Returns a <code>Type</code> object representing the direct superclass,
   * whether class, interface, primitive type or void, of this class.
   * If this class is an array class, then a class instance representing
   * the <code>Object</code> class is returned.  If this class is primitive,
   * an interface, or a representation of either the <code>Object</code>
   * class or void, then <code>null</code> is returned.
   * </p>
   * <p>
   * If the superclass is a parameterized type, then the
   * object returned for this interface reflects the actual type
   * parameters used in the source code.  Type parameters are created
   * using the semantics specified by the <code>ParameterizedType</code>
   * interface, and only if an instance has not already been created.
   * </p>
   *
   * @return the superclass of this class.
   * @throws GenericSignatureFormatError if the generic signature of the
   *         class does not comply with that specified by the Java
   *         Virtual Machine specification, 3rd edition.
   * @throws TypeNotPresentException if the superclass refers
   *         to a non-existant type.
   * @throws MalformedParameterizedTypeException if the superclass
   *         refers to a parameterized type that can not be instantiated for
   *         some reason.
   * @since 1.5
   * @see java.lang.reflect.ParameterizedType
   */
  public Type getGenericSuperclass()
  {
    if (isArray())
      return Object.class;

    if (isPrimitive() || isInterface() || this == Object.class)
      return null;

    String sig = getClassSignature();
    if (sig == null)
      return getSuperclass();

    ClassSignatureParser p = new ClassSignatureParser(this, sig);
    return p.getSuperclassType();
  }

  /**
   * Returns an array of <code>TypeVariable</code> objects that represents
   * the type variables declared by this class, in declaration order.
   * An array of size zero is returned if this class has no type
   * variables.
   *
   * @return the type variables associated with this class. 
   * @throws GenericSignatureFormatError if the generic signature does
   *         not conform to the format specified in the Virtual Machine
   *         specification, version 3.
   * @since 1.5
   */
  public TypeVariable<Class<T>>[] getTypeParameters()
  {
    String sig = getClassSignature();
    if (sig == null)
      return (TypeVariable<Class<T>>[])new TypeVariable[0];

    ClassSignatureParser p = new ClassSignatureParser(this, sig);
    return p.getTypeParameters();
  }

  /**
   * Returns this class' annotation for the specified annotation type,
   * or <code>null</code> if no such annotation exists.
   *
   * @param annotationClass the type of annotation to look for.
   * @return this class' annotation for the specified type, or
   *         <code>null</code> if no such annotation exists.
   * @since 1.5
   */
  public <A extends Annotation> A getAnnotation(Class<A> annotationClass)
  {
    A foundAnnotation = null;
    Annotation[] annotations = getAnnotations();
    for (Annotation annotation : annotations)
      if (annotation.annotationType() == annotationClass)
	foundAnnotation = (A) annotation;
    return foundAnnotation;
  }

  /**
   * Returns all annotations associated with this class.  If there are
   * no annotations associated with this class, then a zero-length array
   * will be returned.  The returned array may be modified by the client
   * code, but this will have no effect on the annotation content of this
   * class, and hence no effect on the return value of this method for
   * future callers.
   *
   * @return this class' annotations.
   * @since 1.5
   */
  public Annotation[] getAnnotations()
  {
    HashMap<Class, Annotation> map = new HashMap<Class, Annotation>();
    for (Annotation a : getDeclaredAnnotations())
      map.put((Class) a.annotationType(), a);
    for (Class<? super T> s = getSuperclass();
	 s != null;
	 s = s.getSuperclass())
      {
	for (Annotation a : s.getDeclaredAnnotations())
	  {
	    Class k = (Class) a.annotationType();
	    if (! map.containsKey(k) && k.isAnnotationPresent(Inherited.class))
	      map.put(k, a);
	  }
      }
    Collection<Annotation> v = map.values();
    return v.toArray(new Annotation[v.size()]);
  }

  /**
   * <p>
   * Returns the canonical name of this class, as defined by section
   * 6.7 of the Java language specification.  Each package, top-level class,
   * top-level interface and primitive type has a canonical name.  A member
   * class has a canonical name, if its parent class has one.  Likewise,
   * an array type has a canonical name, if its component type does.
   * Local or anonymous classes do not have canonical names.
   * </p>
   * <p>
   * The canonical name for top-level classes, top-level interfaces and
   * primitive types is always the same as the fully-qualified name.
   * For array types, the canonical name is the canonical name of its
   * component type with `[]' appended.  
   * </p>
   * <p>
   * The canonical name of a member class always refers to the place where
   * the class was defined, and is composed of the canonical name of the
   * defining class and the simple name of the member class, joined by `.'.
   *  For example, if a <code>Person</code> class has an inner class,
   * <code>M</code>, then both its fully-qualified name and canonical name
   * is <code>Person.M</code>.  A subclass, <code>Staff</code>, of
   * <code>Person</code> refers to the same inner class by the fully-qualified
   * name of <code>Staff.M</code>, but its canonical name is still
   * <code>Person.M</code>.
   * </p>
   * <p>
   * Where no canonical name is present, <code>null</code> is returned.
   * </p>
   *
   * @return the canonical name of the class, or <code>null</code> if the
   *         class doesn't have a canonical name.
   * @since 1.5
   */
  public String getCanonicalName()
  {
    if (isArray())
      {
	String componentName = getComponentType().getCanonicalName();
	if (componentName != null)
	  return componentName + "[]";
      }
    if (isMemberClass())
      {
	String memberName = getDeclaringClass().getCanonicalName();
	if (memberName != null)
	  return memberName + "." + getSimpleName();
      }
    if (isLocalClass() || isAnonymousClass())
      return null;
    return getName();
  }

  /**
   * Returns all annotations directly defined by this class.  If there are
   * no annotations associated with this class, then a zero-length array
   * will be returned.  The returned array may be modified by the client
   * code, but this will have no effect on the annotation content of this
   * class, and hence no effect on the return value of this method for
   * future callers.
   *
   * @return the annotations directly defined by this class.
   * @since 1.5
   */
  public Annotation[] getDeclaredAnnotations()
  {
    Annotation[] result = getDeclaredAnnotationsInternal();
    if (result == null)
      result = new Annotation[0];
    return result;
  }

  private native Annotation[] getDeclaredAnnotationsInternal();

  /**
   * Returns true if an annotation for the specified type is associated
   * with this class.  This is primarily a short-hand for using marker
   * annotations.
   *
   * @param annotationClass the type of annotation to look for.
   * @return true if an annotation exists for the specified type.
   * @since 1.5
   */
  public boolean isAnnotationPresent(Class<? extends Annotation> 
				     annotationClass)
  {
    return getAnnotation(annotationClass) != null;
  }

  /**
   * Returns true if this object represents an anonymous class.
   *
   * @return true if this object represents an anonymous class.
   * @since 1.5
   */
  public native boolean isAnonymousClass();

  /**
   * Returns true if this object represents an local class.
   *
   * @return true if this object represents an local class.
   * @since 1.5
   */
  public native boolean isLocalClass();

  /**
   * Returns true if this object represents an member class.
   *
   * @return true if this object represents an member class.
   * @since 1.5
   */
  public native boolean isMemberClass();

  /**
   * Utility method for use by classes in this package.
   */
  static void setAccessible(final AccessibleObject obj)
  {
    AccessController.doPrivileged(new PrivilegedAction()
      {
        public Object run()
          {
            obj.setAccessible(true);
            return null;
          }
      });
  }
}
