// Class.java - Representation of a Java class.

/* Copyright (C) 1998, 1999, 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.lang;
import java.io.Serializable;
import java.io.InputStream;
import java.lang.reflect.*;
import java.security.*;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date October 1, 1998 
 */

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * plus gcj compiler sources (to determine object layout)
 * Status:  Sufficient for our purposes, but some methods missing
 * and some not implemented.
 */

public final class Class implements Serializable
{
  public static native Class forName (String className)
    throws ClassNotFoundException;
  /** @since 1.2 */
  public static native Class forName (String className, boolean initialize,
				      ClassLoader loader)
    throws ClassNotFoundException;
  public native Class[] getClasses ();
  public native ClassLoader getClassLoader ();
  public native Class getComponentType ();

  public native Constructor getConstructor (Class[] parameterTypes)
    throws NoSuchMethodException, SecurityException;

  // This is used to implement getConstructors and
  // getDeclaredConstructors.
  private native Constructor[] _getConstructors (boolean declared)
    throws SecurityException;

  public Constructor[] getConstructors () throws SecurityException
  {
    return _getConstructors (false);
  }

  public native Constructor getDeclaredConstructor (Class[] parameterTypes)
    throws NoSuchMethodException, SecurityException;

  public native Class[] getDeclaredClasses () throws SecurityException;

  public Constructor[] getDeclaredConstructors () throws SecurityException
  {
    return _getConstructors (true);
  }

  public native Field getDeclaredField (String fieldName)
    throws NoSuchFieldException, SecurityException;
  public native Field[] getDeclaredFields () throws SecurityException;
  public native Method getDeclaredMethod (String methodName,
					  Class[] parameterTypes)
    throws NoSuchMethodException, SecurityException;
  public native Method[] getDeclaredMethods () throws SecurityException;

  // This is marked as unimplemented in the JCL book.
  public native Class getDeclaringClass ();

  private native Field getField (String fieldName, int hash)
    throws NoSuchFieldException, SecurityException;

  public Field getField (String fieldName)
    throws NoSuchFieldException, SecurityException
  {
    SecurityManager s = System.getSecurityManager();
    if (s != null)
      s.checkMemberAccess (this, java.lang.reflect.Member.DECLARED);
    Field fld = getField(fieldName, fieldName.hashCode());
    if (fld == null)
      throw new NoSuchFieldException(fieldName);
    return fld;
  }

  private native Field[] _getFields (Field[] result, int offset);
  public native Field[] getFields () throws SecurityException;

  /**
   * Returns the <code>Package</code> in which this class is defined
   * Returns null when this information is not available from the
   * classloader of this class or when the classloader of this class
   * is null.
   *
   * @since 1.2
   */
  public Package getPackage()
  {
    ClassLoader cl = getClassLoader();
    if (cl != null)
      {
        String name = getName();
	String pkg = "";
	int idx = name.lastIndexOf('.');
	if (idx >= 0)
	  pkg = name.substring(0, idx);
	return cl.getPackage(pkg);
      }
    else
      return null;
  }

  public native Class[] getInterfaces ();

  private final native void getSignature (StringBuffer buffer);
  private static final native String getSignature (Class[] parameterTypes,
						   boolean is_construtor);

  public native Method getMethod (String methodName, Class[] parameterTypes)
    throws NoSuchMethodException, SecurityException;
  private native int _getMethods (Method[] result, int offset);
  public native Method[] getMethods () throws SecurityException;

  public native int getModifiers ();
  public native String getName ();

  public java.net.URL getResource (String resourceName)
  {
    String name = resourcePath (resourceName);
    ClassLoader loader = getClassLoader ();
    if (loader == null)
      return ClassLoader.getSystemResource (name);
    else
      return loader.getResource (name);
  }

  public java.io.InputStream getResourceAsStream (String resourceName)
  {
    String name = resourcePath (resourceName);
    ClassLoader loader = getClassLoader ();
    if (loader == null)
      return ClassLoader.getSystemResourceAsStream (name);
    else
      return loader.getResourceAsStream (name);
  }

  private String resourcePath (String resourceName)
  {
    if (resourceName.startsWith ("/"))
      return resourceName.substring (1);

    Class c = this;
    while (c.isArray ())
      c = c.getComponentType ();

    String packageName = c.getName ().replace ('.', '/');
    int end = packageName.lastIndexOf ('/');
    if (end == -1)
      return resourceName;
    else
      return packageName.substring (0, end+1) + resourceName;
  }

  // FIXME: implement.  Requires java.security.
  public Object[] getSigners ()
  {
    return null;
  }

  public native Class getSuperclass ();
  public native boolean isArray ();
  public native boolean isAssignableFrom (Class cls);
  public native boolean isInstance (Object obj);
  public native boolean isInterface ();
  public native boolean isPrimitive ();
  public native Object newInstance ()
    throws InstantiationException, IllegalAccessException;

  // We need a native method to retrieve the protection domain, because we
  // can't add fields to java.lang.Class that are accessible from Java.
  private native ProtectionDomain getProtectionDomain0();

  /**
   * Returns the protection domain of this class. If the classloader
   * did not record the protection domain when creating this class
   * the unknown protection domain is returned which has a <code>null</code>
   * code source and all permissions.
   *
   * @exception SecurityException if a security manager exists and the caller
   * does not have <code>RuntimePermission("getProtectionDomain")</code>.
   *
   * @since 1.2
   */
  public ProtectionDomain getProtectionDomain()
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkPermission(ClassLoader.protectionDomainPermission);
    
    ProtectionDomain protectionDomain = getProtectionDomain0();

    if (protectionDomain == null)
      return ClassLoader.unknownProtectionDomain;
    else
      return protectionDomain;
  }

  public String toString ()
  {
    if (isPrimitive ())
      return getName ();
    return (isInterface () ? "interface " : "class ") + getName ();
  }

  // Don't allow new classes to be made.
  private Class ()
  {
  }

  // Do a security check.
  private void checkMemberAccess (int flags)
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkMemberAccess(this, flags);
  }

  // Initialize the class.
  private native void initializeClass ();

  // finalization
  protected native void finalize ();
}
