// Class.java - Representation of a Java class.

/* Copyright (C) 1998, 1999, 2000, 2002  Free Software Foundation

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

  private native Method _getDeclaredMethod (String methodName,
					    Class[] parameterTypes);

  public Method getDeclaredMethod (String methodName, Class[] parameterTypes)
    throws NoSuchMethodException, SecurityException
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      {
	sm.checkMemberAccess(this, Member.DECLARED);
	Package p = getPackage();
	if (p != null)
	  sm.checkPackageAccess(p.getName());
      }

    if ("<init>".equals(methodName) || "<clinit>".equals(methodName))
      throw new NoSuchMethodException(methodName);

    Method m = _getDeclaredMethod(methodName, parameterTypes);
    if (m == null)
      throw new NoSuchMethodException (methodName);
    return m;
  }

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

  public native Method _getMethod (String methodName, Class[] parameterTypes);

  public Method getMethod (String methodName, Class[] parameterTypes)
    throws NoSuchMethodException, SecurityException
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      {
	sm.checkMemberAccess(this, Member.PUBLIC);
	Package p = getPackage();
	if (p != null)
	  sm.checkPackageAccess(p.getName());
      }

    if ("<init>".equals(methodName) || "<clinit>".equals(methodName))
      throw new NoSuchMethodException(methodName);

    Method m = _getMethod(methodName, parameterTypes);
    if (m == null)
      throw new NoSuchMethodException (methodName);
    return m;
  }

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
}
