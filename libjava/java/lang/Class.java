/* Class.java -- Representation of a Java class.
   Copyright (C) 1998, 1999, 2000, 2002, 2003, 2004, 2005
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

package java.lang;

import java.io.InputStream;
import java.io.Serializable;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Member;
import java.lang.reflect.Method;
import java.net.URL;
import java.security.ProtectionDomain;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;

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
public final class Class implements Serializable
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
  public static native Class forName (String className)
    throws ClassNotFoundException;

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
  public static native Class forName (String className, boolean initialize,
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
  public Class[] getClasses()
  {
    memberAccessCheck(Member.PUBLIC);
    return internalGetClasses();
  }

  /**
   * Like <code>getClasses()</code> but without the security checks.
   */
  private Class[] internalGetClasses()
  {
    ArrayList list = new ArrayList();
    list.addAll(Arrays.asList(getDeclaredClasses(true)));
    Class superClass = getSuperclass();
    if (superClass != null)
      list.addAll(Arrays.asList(superClass.internalGetClasses()));
    return (Class[])list.toArray(new Class[list.size()]);
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
  public native Class getComponentType ();

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
  public native Constructor getConstructor(Class[] args)
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
  public Constructor[] getConstructors()
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
  public native Constructor getDeclaredConstructor(Class[] args)
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
  public Class[] getDeclaredClasses()
  {
    memberAccessCheck(Member.DECLARED);
    return getDeclaredClasses(false);
  }

  native Class[] getDeclaredClasses (boolean publicOnly);

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
  public Constructor[] getDeclaredConstructors()
  {
    memberAccessCheck(Member.DECLARED);
    return getDeclaredConstructors(false);
  }

  native Constructor[] getDeclaredConstructors (boolean publicOnly);

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
  public Method getDeclaredMethod(String methodName, Class[] args)
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
  public native Class getDeclaringClass ();

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
    HashSet set = new HashSet();
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
   * classloader of this class or when the classloader of this class
   * is null.
   *
   * @return the package for this class, if it is available
   * @since 1.2
   */
  public Package getPackage()
  {
    ClassLoader cl = getClassLoader();
    if (cl != null)
      return cl.getPackage(getPackagePortion(getName()));
    return null;
  }

  /**
   * Get the interfaces this class <em>directly</em> implements, in the
   * order that they were declared. This returns an empty array, not null,
   * for Object, primitives, void, and classes or interfaces with no direct
   * superinterface. Array types return Cloneable and Serializable.
   *
   * @return the interfaces this class directly implements
   */
  public native Class[] getInterfaces ();

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
  public Method getMethod(String methodName, Class[] args)
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
    ClassLoader loader = getClassLoader();
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
    ClassLoader loader = getClassLoader();
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
  public native Class getSuperclass ();
  
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
  public native boolean isAssignableFrom (Class c);
 
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
  public native Object newInstance ()
    throws InstantiationException, IllegalAccessException;

  // We need a native method to retrieve the protection domain, because we
  // can't add fields to java.lang.Class that are accessible from Java.
  private native ProtectionDomain getProtectionDomain0();

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
    ClassLoader c = getClassLoader();
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
}
