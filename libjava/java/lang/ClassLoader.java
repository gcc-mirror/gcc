// ClassLoader.java - Define policies for loading Java classes.

/* Copyright (C) 1998, 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.lang;

import java.io.InputStream;
import java.net.URL;
import java.net.URLConnection;
import java.util.Stack;

/**
 * The class <code>ClassLoader</code> is intended to be subclassed by
 * applications in order to describe new ways of loading classes,
 * such as over the network.
 *
 * @author  Kresten Krab Thorup
 */

public abstract class ClassLoader {

  static private ClassLoader system;
  private ClassLoader parent;

  public ClassLoader getParent ()
  {
    /* FIXME: security */
    return parent;
  }
    
  public static native ClassLoader getSystemClassLoader ();

  /**
   * Creates a <code>ClassLoader</code> with no parent.
   * @exception java.lang.SecurityException if not allowed
   */
  protected ClassLoader() 
  {
    this (null);
  }

  /**
   * Creates a <code>ClassLoader</code> with the given parent.   
   * The parent may be <code>null</code>.
   * The only thing this 
   * constructor does, is to call
   * <code>checkCreateClassLoader</code> on the current 
   * security manager. 
   * @exception java.lang.SecurityException if not allowed
   */
  protected ClassLoader(ClassLoader parent) 
  {
    SecurityManager security = System.getSecurityManager ();
    if (security != null)
      security.checkCreateClassLoader ();
    this.parent = parent;
  }

  /** 
   * Loads and link the class by the given name.
   * @param     name the name of the class.
   * @return    the class loaded.
   * @see       ClassLoader#loadClass(String,boolean)
   * @exception java.lang.ClassNotFoundException 
   */ 
  public Class loadClass(String name) 
    throws java.lang.ClassNotFoundException, java.lang.LinkageError
  { 
    return loadClass (name, false);
  }

  /** 
   * Loads the class by the given name.  The default implementation
   * will search for the class in the following order (similar to jdk 1.2)
   * <ul>
   *  <li> First <code>findLoadedClass</code>.
   *  <li> If parent is non-null, <code>parent.loadClass</code>;
   *       otherwise <code>findSystemClass</code>.
   *  <li> <code>findClass</code>.
   * </ul>
   * If <code>link</code> is true, <code>resolveClass</code> is then
   * called.  <p> Normally, this need not be overridden; override
   * <code>findClass</code> instead.
   * @param     name the name of the class.
   * @param     link if the class should be linked.
   * @return    the class loaded.
   * @exception java.lang.ClassNotFoundException 
   * @deprecated 
   */ 
  protected Class loadClass(String name, boolean link)
    throws java.lang.ClassNotFoundException, java.lang.LinkageError
  {
    Class c = findLoadedClass (name);

    if (c == null)
      {
	try {
	  if (parent != null)
	    return parent.loadClass (name, link);
	  else
	    c = findSystemClass (name);
	} catch (ClassNotFoundException ex) {
	  /* ignore, we'll try findClass */;
	}
      }

    if (c == null)
      c = findClass (name);

    if (c == null)
      throw new ClassNotFoundException (name);

    if (link)
      resolveClass (c);

    return c;
  }

  /** Find a class.  This should be overridden by subclasses; the
   *  default implementation throws ClassNotFoundException.
   *
   * @param name Name of the class to find.
   * @return     The class found.
   * @exception  java.lang.ClassNotFoundException
   */
  protected Class findClass (String name)
    throws ClassNotFoundException
  {
    throw new ClassNotFoundException ();
  }

  /** 
   * Defines a class, given the class-data.  According to the JVM, this
   * method should not be used; instead use the variant of this method
   * in which the name of the class being defined is specified
   * explicitly.   
   * <P>
   * If the name of the class, as specified (implicitly) in the class
   * data, denotes a class which has already been loaded by this class
   * loader, an instance of
   * <code>java.lang.ClassNotFoundException</code> will be thrown.
   *
   * @param     data    bytes in class file format.
   * @param     off     offset to start interpreting data.
   * @param     len     length of data in class file.
   * @return    the class defined.
   * @exception java.lang.ClassNotFoundException 
   * @exception java.lang.LinkageError
   * @see ClassLoader#defineClass(String,byte[],int,int) */
  protected final Class defineClass(byte[] data, int off, int len) 
    throws java.lang.ClassNotFoundException, java.lang.LinkageError
  {
    return defineClass (null, data, off, len);
  }

  /** 
   * Defines a class, given the class-data.  This is preferable
   * over <code>defineClass(byte[],off,len)</code> since it is more
   * secure.  If the expected name does not match that of the class
   * file, <code>ClassNotFoundException</code> is thrown.  If
   * <code>name</code> denotes the name of an already loaded class, a
   * <code>LinkageError</code> is thrown.
   * <p>
   * 
   * FIXME: How do we assure that the class-file data is not being
   * modified, simultaneously with the class loader running!?  If this
   * was done in some very clever way, it might break security.  
   * Right now I am thinking that defineclass should make sure never to
   * read an element of this array more than once, and that that would
   * assure the ``immutable'' appearance.  It is still to be determined
   * if this is in fact how defineClass operates.
   *
   * @param     name    the expected name.
   * @param     data    bytes in class file format.
   * @param     off     offset to start interpreting data.
   * @param     len     length of data in class file.
   * @return    the class defined.
   * @exception java.lang.ClassNotFoundException 
   * @exception java.lang.LinkageError
   */
  protected final synchronized Class defineClass(String name,
						 byte[] data,
						 int off,
						 int len)
    throws java.lang.ClassNotFoundException, java.lang.LinkageError
  {
    if (data==null || data.length < off+len || off<0 || len<0)
      throw new ClassFormatError ("arguments to defineClass "
				  + "are meaningless");

    // as per 5.3.5.1
    if (name != null  &&  findLoadedClass (name) != null)
      throw new java.lang.LinkageError ("class " 
					+ name 
					+ " already loaded");

    try {
      // Since we're calling into native code here, 
      // we better make sure that any generated
      // exception is to spec!

      return defineClass0 (name, data, off, len);

    } catch (java.lang.LinkageError x) {
      throw x;		// rethrow

    } catch (java.lang.ClassNotFoundException x) {
      throw x;		// rethrow

    } catch (java.lang.VirtualMachineError x) {
      throw x;		// rethrow

    } catch (java.lang.Throwable x) {
      // This should never happen, or we are beyond spec.  
      
      throw new InternalError ("Unexpected exception "
			       + "while defining class "
			       + name + ": " 
			       + x.toString ());
     }
  }

  /** This is the entry point of defineClass into the native code */
  private native Class defineClass0 (String name,
				     byte[] data,
				     int off,
				     int len)
    throws java.lang.ClassNotFoundException, java.lang.LinkageError;


  /** This is called by defineClass0, once the "raw" and uninitialized
   * class object has been created, and handles exceptions generated
   * while actually defining the class (_Jv_DefineClass).  defineClass0
   * holds the lock on the new class object, so it needs to capture
   * these exceptions.  */

  private static Throwable defineClass1 (Class klass, byte[] data,
					 int offset, int length)
  {
    try {
      defineClass2 (klass, data, offset, length);
    } catch (Throwable x) {
      return x;
    }
    return null;
  }
 
  /** This is just a wrapper for _Jv_DefineClass */
  private static native void defineClass2 (Class klass, byte[] data, 
				    int offset, int length)
    throws Throwable;

  /** 
   * Link the given class.  This will bring the class to a state where
   * the class initializer can be run.  Linking involves the following
   * steps: 
   * <UL>
   * <LI>  Prepare (allocate and internalize) the constant strings that
   *       are used in this class.
   * <LI>  Allocate storage for static fields, and define the layout
   *       of instance fields.
   * <LI>  Perform static initialization of ``static final'' int,
   *       long, float, double and String fields for which there is a
   *       compile-time constant initializer.
   * <LI>  Create the internal representation of the ``vtable''.
   * </UL>
   * For <code>gcj</code>-compiled classes, only the first step is
   * performed.  The compiler will have done the rest already.
   * <P>
   * This is called by the system automatically,
   * as part of class initialization; there is no reason to ever call
   * this method directly.  
   * <P> 
   * For historical reasons, this method has a name which is easily
   * misunderstood.  Java classes are never ``resolved''.  Classes are
   * linked; whereas method and field references are resolved.
   *
   * @param     clazz the class to link.
   * @exception java.lang.LinkageError
   */
  protected final void resolveClass(Class clazz)
    throws java.lang.LinkageError
  {
    resolveClass0(clazz);
  }

  static void resolveClass0(Class clazz)
    throws java.lang.LinkageError
  {
    synchronized (clazz)
      {
	try {
	  linkClass0 (clazz);
	} catch (Throwable x) {
	  markClassErrorState0 (clazz);

	  if (x instanceof Error)
	    throw (Error)x;
	  else    
	    throw new java.lang.InternalError
	      ("unexpected exception during linking: " + x);
	}
      }
  }

  /** Internal method.  Calls _Jv_PrepareClass and
   * _Jv_PrepareCompiledClass.  This is only called from resolveClass.  */ 
  private static native void linkClass0(Class clazz)
    throws java.lang.LinkageError;

  /** Internal method.  Marks the given clazz to be in an erroneous
   * state, and calls notifyAll() on the class object.  This should only
   * be called when the caller has the lock on the class object.  */
  private static native void markClassErrorState0(Class clazz);


  /** 
   * Returns a class found in a system-specific way, typically
   * via the <code>java.class.path</code> system property.  Loads the 
   * class if necessary.
   *
   * @param     name the class to resolve.
   * @return    the class loaded.
   * @exception java.lang.LinkageError 
   * @exception java.lang.ClassNotFoundException 
   */
  protected Class findSystemClass(String name) 
    throws java.lang.ClassNotFoundException, java.lang.LinkageError
  {
    return getSystemClassLoader ().loadClass (name);
  }

  /*
   * Does currently nothing.
   */ 
  protected final void setSigners(Class claz, Object[] signers) {
    /* claz.setSigners (signers); */
  }

  /**
   * If a class named <code>name</code> was previously loaded using
   * this <code>ClassLoader</code>, then it is returned.  Otherwise
   * it returns <code>null</code>.  (Unlike the JDK this is native,
   * since we implement the class table internally.)
   * @param     name  class to find.
   * @return    the class loaded, or null.
   */ 
  protected native Class findLoadedClass(String name);

  public static final InputStream getSystemResourceAsStream(String name) {
    return getSystemClassLoader().getResourceAsStream (name);
  }

  public static final URL getSystemResource(String name) {
    return getSystemClassLoader().getResource (name);
  }

  /**
   *   Return an InputStream representing the resource name.  
   *   This is essentially like 
   *   <code>getResource(name).openStream()</code>, except
   *   it masks out any IOException and returns null on failure.
   * @param   name  resource to load
   * @return  an InputStream, or null
   * @see     java.lang.ClassLoader#getResource(String)
   * @see     java.io.InputStream
   */
  public InputStream getResourceAsStream(String name) 
  {
    try {
      URL res = getResource (name);
      if (res == null) return null;
      return res.openStream ();
    } catch (java.io.IOException x) {
       return null;
     }
  }
 
  /**
   * Return an java.io.URL representing the resouce <code>name</code>.  
   * The default implementation just returns <code>null</code>.
   * @param   name  resource to load
   * @return  a URL, or null if there is no such resource.
   * @see     java.lang.ClassLoader#getResourceAsBytes(String)
   * @see     java.lang.ClassLoader#getResourceAsStream(String)
   * @see     java.io.URL
   */
  public URL getResource(String name) {
    return null;
  }

}

