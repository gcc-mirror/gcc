// ClassLoader.java - Define policies for loading Java classes.

/* Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.lang;

import java.io.InputStream;
import java.io.IOException;
import java.net.URL;
import java.security.AllPermission;
import java.security.CodeSource;
import java.security.Permission;
import java.security.Permissions;
import java.security.Policy;
import java.security.ProtectionDomain;
import java.util.*;

/**
 * The ClassLoader is a way of customizing the way Java gets its classes
 * and loads them into memory.  The verifier and other standard Java things
 * still run, but the ClassLoader is allowed great flexibility in determining
 * where to get the classfiles and when to load and resolve them. For that
 * matter, a custom ClassLoader can perform on-the-fly code generation or
 * modification!
 *
 * <p>Every classloader has a parent classloader that is consulted before
 * the 'child' classloader when classes or resources should be loaded.   
 * This is done to make sure that classes can be loaded from an hierarchy of
 * multiple classloaders and classloaders do not accidentially redefine   
 * already loaded classes by classloaders higher in the hierarchy.
 *   
 * <p>The grandparent of all classloaders is the bootstrap classloader, which
 * loads all the standard system classes as implemented by GNU Classpath. The
 * other special classloader is the system classloader (also called
 * application classloader) that loads all classes from the CLASSPATH
 * (<code>java.class.path</code> system property). The system classloader
 * is responsible for finding the application classes from the classpath,
 * and delegates all requests for the standard library classes to its parent
 * the bootstrap classloader. Most programs will load all their classes
 * through the system classloaders.
 *
 * <p>The bootstrap classloader in GNU Classpath is implemented as a couple of
 * static (native) methods on the package private class
 * <code>java.lang.VMClassLoader</code>, the system classloader is an
 * instance of <code>gnu.java.lang.SystemClassLoader</code>
 * (which is a subclass of <code>java.net.URLClassLoader</code>).
 *
 * <p>Users of a <code>ClassLoader</code> will normally just use the methods
 * <ul>
 *  <li> <code>loadClass()</code> to load a class.</li>
 *  <li> <code>getResource()</code> or <code>getResourceAsStream()</code>
 *       to access a resource.</li>
 *  <li> <code>getResources()</code> to get an Enumeration of URLs to all
 *       the resources provided by the classloader and its parents with the
 *       same name.</li>
 * </ul>
 *
 * <p>Subclasses should implement the methods
 * <ul>
 *  <li> <code>findClass()</code> which is called by <code>loadClass()</code>
 *       when the parent classloader cannot provide a named class.</li>
 *  <li> <code>findResource()</code> which is called by
 *       <code>getResource()</code> when the parent classloader cannot provide
 *       a named resource.</li>
 *  <li> <code>findResources()</code> which is called by
 *       <code>getResource()</code> to combine all the resources with the
 *       same name from the classloader and its parents.</li>
 *  <li> <code>findLibrary()</code> which is called by
 *       <code>Runtime.loadLibrary()</code> when a class defined by the
 *       classloader wants to load a native library.</li>
 * </ul>
 *
 * @author John Keiser
 * @author Mark Wielaard
 * @author Eric Blake
 * @author Kresten Krab Thorup
 * @see Class
 * @since 1.0
 * @status still missing 1.4 functionality
 */
public abstract class ClassLoader
{
  /**
   * All classes loaded by this classloader. VM's may choose to implement
   * this cache natively; but it is here available for use if necessary. It
   * is not private in order to allow native code (and trusted subclasses)
   * access to this field.
   */
  final Map loadedClasses = new HashMap();

  /**
   * The desired assertion status of classes loaded by this loader, if not
   * overridden by package or class instructions.
   */
  // Package visible for use by Class.
  boolean defaultAssertionStatus = VMClassLoader.defaultAssertionStatus();

  /**
   * The command-line state of the package assertion status overrides. This
   * map is never modified, so it does not need to be synchronized.
   */
  // Package visible for use by Class.
  static final Map systemPackageAssertionStatus
    = VMClassLoader.packageAssertionStatus();

  /**
   * The map of package assertion status overrides, or null if no package
   * overrides have been specified yet. The values of the map should be
   * Boolean.TRUE or Boolean.FALSE, and the unnamed package is represented
   * by the null key. This map must be synchronized on this instance.
   */
  // Package visible for use by Class.
  Map packageAssertionStatus;

  /**
   * The command-line state of the class assertion status overrides. This
   * map is never modified, so it does not need to be synchronized.
   */
  // Package visible for use by Class.
  static final Map systemClassAssertionStatus
    = VMClassLoader.classAssertionStatus();

  /**
   * The map of class assertion status overrides, or null if no class
   * overrides have been specified yet. The values of the map should be
   * Boolean.TRUE or Boolean.FALSE. This map must be synchronized on this
   * instance.
   */
  // Package visible for use by Class.
  Map classAssertionStatus;

  /**
   * The classloader that is consulted before this classloader.
   * If null then the parent is the bootstrap classloader.
   */
  private final ClassLoader parent;

  /**
   * All packages defined by this classloader. It is not private in order to
   * allow native code (and trusted subclasses) access to this field.
   */
  private HashMap definedPackages = new HashMap();

  /**
   * Returns the parent of this classloader. If the parent of this
   * classloader is the bootstrap classloader then this method returns
   * <code>null</code>. A security check may be performed on
   * <code>RuntimePermission("getClassLoader")</code>.
   *
   * @throws SecurityException if the security check fails
   * @since 1.2
   */
  public final ClassLoader getParent ()
  {
    // Check if we may return the parent classloader
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      {
	/* FIXME: security, getClassContext() not implemented.
	Class c = VMSecurityManager.getClassContext()[1];
	ClassLoader cl = c.getClassLoader();
	if (cl != null && cl != this)
	  sm.checkPermission(new RuntimePermission("getClassLoader"));
	*/
      }
    return parent;
  }

  /**
   * Returns the system classloader. The system classloader (also called
   * the application classloader) is the classloader that was used to
   * load the application classes on the classpath (given by the system
   * property <code>java.class.path</code>. This is set as the context
   * class loader for a thread. The system property
   * <code>java.system.class.loader</code>, if defined, is taken to be the
   * name of the class to use as the system class loader, which must have
   * a public constructor which takes a ClassLoader as a parent; otherwise this
   * uses gnu.java.lang.SystemClassLoader.
   *
   * <p>Note that this is different from the bootstrap classloader that
   * actually loads all the real "system" classes (the bootstrap classloader
   * is the parent of the returned system classloader).
   *
   * <p>A security check will be performed for
   * <code>RuntimePermission("getClassLoader")</code> if the calling class
   * is not a parent of the system class loader.
   *
   * @return the system class loader
   * @throws SecurityException if the security check fails
   * @throws IllegalStateException if this is called recursively
   * @throws Error if <code>java.system.class.loader</code> fails to load
   * @since 1.2
   */
  public static ClassLoader getSystemClassLoader ()
  {
    return gnu.gcj.runtime.VMClassLoader.instance;
  }

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
   * @since 1.2
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
    throws java.lang.ClassNotFoundException
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
    throws java.lang.ClassNotFoundException
  {
    Class c = findLoadedClass (name);

    if (c == null)
      {
	try
	  {
	    ClassLoader cl = parent;
	    if (parent == null)
	      cl = gnu.gcj.runtime.VMClassLoader.instance;
	    if (cl != this)
	      c = cl.loadClass (name, link);
	  }
	catch (ClassNotFoundException ex)
	  {
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

  /**
   * Called for every class name that is needed but has not yet been
   * defined by this classloader or one of its parents. It is called by
   * <code>loadClass()</code> after both <code>findLoadedClass()</code> and
   * <code>parent.loadClass()</code> couldn't provide the requested class.
   *
   * <p>The default implementation throws a
   * <code>ClassNotFoundException</code>. Subclasses should override this
   * method. An implementation of this method in a subclass should get the
   * class bytes of the class (if it can find them), if the package of the
   * requested class doesn't exist it should define the package and finally
   * it should call define the actual class. It does not have to resolve the
   * class. It should look something like the following:<br>
   *
   * <pre>
   * // Get the bytes that describe the requested class
   * byte[] classBytes = classLoaderSpecificWayToFindClassBytes(name);
   * // Get the package name
   * int lastDot = name.lastIndexOf('.');
   * if (lastDot != -1)
   *   {
   *     String packageName = name.substring(0, lastDot);
   *     // Look if the package already exists
   *     if (getPackage(pkg) == null)
   *       {
   *         // define the package
   *         definePackage(packageName, ...);
   *       }
   *   }
   * // Define and return the class
   *  return defineClass(name, classBytes, 0, classBytes.length);
   * </pre>
   *
   * <p><code>loadClass()</code> makes sure that the <code>Class</code>
   * returned by <code>findClass()</code> will later be returned by
   * <code>findLoadedClass()</code> when the same class name is requested.
   *
   * @param name class name to find (including the package name)
   * @return the requested Class
   * @throws ClassNotFoundException when the class can not be found
   * @since 1.2
   */   
  protected Class findClass (String name)
    throws ClassNotFoundException
  {
    throw new ClassNotFoundException (name);
  }

  // Protection Domain definitions 
  // FIXME: should there be a special protection domain used for native code?
  
  // The permission required to check what a classes protection domain is.
  static final Permission protectionDomainPermission
    = new RuntimePermission("getProtectionDomain");
  // The protection domain returned if we cannot determine it. 
  static ProtectionDomain unknownProtectionDomain;
  // Protection domain to use when a class is defined without one specified.
  static ProtectionDomain defaultProtectionDomain;

  static
  {
    Permissions permissions = new Permissions();
    permissions.add(new AllPermission());
    unknownProtectionDomain = new ProtectionDomain(null, permissions);  

    CodeSource cs = new CodeSource(null, null);
    defaultProtectionDomain =
      new ProtectionDomain(cs, Policy.getPolicy().getPermissions(cs));
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
    throws ClassFormatError
  {
    return defineClass (null, data, off, len, defaultProtectionDomain);
  }

  /**
   * Helper to define a class using a string of bytes without a
   * ProtectionDomain. Subclasses should call this method from their
   * <code>findClass()</code> implementation. The name should use '.'
   * separators, and discard the trailing ".class".  The default protection
   * domain has the permissions of
   * <code>Policy.getPolicy().getPermissions(new CodeSource(null, null))<code>.
   *
   * @param name the name to give the class, or null if unknown
   * @param data the data representing the classfile, in classfile format
   * @param offset the offset into the data where the classfile starts
   * @param len the length of the classfile data in the array
   * @return the class that was defined
   * @throws ClassFormatError if data is not in proper classfile format
   * @throws IndexOutOfBoundsException if offset or len is negative, or
   *         offset + len exceeds data
   * @throws SecurityException if name starts with "java."
   * @since 1.1
   */
  protected final Class defineClass(String name, byte[] data, int off, int len)
    throws ClassFormatError
  {
    return defineClass (name, data, off, len, defaultProtectionDomain);
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
   * @param     protectionDomain security protection domain for the class.
   * @return    the class defined.
   * @exception java.lang.ClassNotFoundException 
   * @exception java.lang.LinkageError
   */
  protected final synchronized Class defineClass(String name,
						 byte[] data,
						 int off,
						 int len,
						 ProtectionDomain protectionDomain)
    throws ClassFormatError
  {
    if (data==null || data.length < off+len || off<0 || len<0)
      throw new ClassFormatError ("arguments to defineClass "
				  + "are meaningless");

    // as per 5.3.5.1
    if (name != null && findLoadedClass (name) != null)
      throw new java.lang.LinkageError ("class " 
					+ name 
					+ " already loaded");

    if (protectionDomain == null)
      protectionDomain = defaultProtectionDomain;

    try
      {
	Class retval = defineClass0 (name, data, off, len, protectionDomain);
	loadedClasses.put(retval.getName(), retval);
	return retval;
      }
    catch (LinkageError x)
      {
	throw x;		// rethrow
      }
    catch (VirtualMachineError x)
      {
	throw x;		// rethrow
      }
    catch (Throwable x)
      {
	// This should never happen, or we are beyond spec.  
      	InternalError r = new InternalError ("Unexpected exception "
					     + "while defining class "
					     + name);
	r.initCause(x);
	throw r;
      }
  }

  /** This is the entry point of defineClass into the native code */
  private native Class defineClass0 (String name,
				     byte[] data,
				     int off,
				     int len,
				     ProtectionDomain protectionDomain)
    throws ClassFormatError;

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
  {
    resolveClass0(clazz);
  }

  static void resolveClass0(Class clazz)
  {
    synchronized (clazz)
      {
	try
	  {
	    linkClass0 (clazz);
	  }
	catch (Throwable x)
	  {
	    markClassErrorState0 (clazz);

	    LinkageError e;
	    if (x instanceof LinkageError)
	      e = (LinkageError)x;
	    else if (x instanceof ClassNotFoundException)
	      {
		e = new NoClassDefFoundError("while resolving class: "
					     + clazz.getName());
		e.initCause (x);
	      }
	    else
	      {
		e = new LinkageError ("unexpected exception during linking: "
				      + clazz.getName());
		e.initCause (x);
	      }
	    throw e;
	  }
      }
  }

  /** Internal method.  Calls _Jv_PrepareClass and
   * _Jv_PrepareCompiledClass.  This is only called from resolveClass.  */ 
  private static native void linkClass0(Class clazz);

  /** Internal method.  Marks the given clazz to be in an erroneous
   * state, and calls notifyAll() on the class object.  This should only
   * be called when the caller has the lock on the class object.  */
  private static native void markClassErrorState0(Class clazz);

  /**
   * Defines a new package and creates a Package object.
   * The package should be defined before any class in the package is
   * defined with <code>defineClass()</code>. The package should not yet
   * be defined before in this classloader or in one of its parents (which
   * means that <code>getPackage()</code> should return <code>null</code>).
   * All parameters except the <code>name</code> of the package may be
   * <code>null</code>.
   * <p>
   * Subclasses should call this method from their <code>findClass()</code>
   * implementation before calling <code>defineClass()</code> on a Class
   * in a not yet defined Package (which can be checked by calling
   * <code>getPackage()</code>).
   *
   * @param name The name of the Package
   * @param specTitle The name of the specification
   * @param specVendor The name of the specification designer
   * @param specVersion The version of this specification
   * @param implTitle The name of the implementation
   * @param implVendor The vendor that wrote this implementation
   * @param implVersion The version of this implementation
   * @param sealed If sealed the origin of the package classes
   * @return the Package object for the specified package
   *
   * @exception IllegalArgumentException if the package name is null or if
   * it was already defined by this classloader or one of its parents.
   *
   * @see Package
   * @since 1.2
   */
  protected Package definePackage(String name,
				  String specTitle, String specVendor,
				  String specVersion, String implTitle,
				  String implVendor, String implVersion,
				  URL sealed)
  {
    if (getPackage(name) != null)
      throw new IllegalArgumentException("Package " + name
					 + " already defined");
    Package p = new Package(name,
			    specTitle, specVendor, specVersion,
			    implTitle, implVendor, implVersion,
			    sealed);
    synchronized (definedPackages)
    {
      definedPackages.put(name, p);
    }
    return p;
  }

  /**
   * Returns the Package object for the requested package name. It returns
   * null when the package is not defined by this classloader or one of its
   * parents.
   *
   * @param name the package name to find
   * @return the package, if defined
   * @since 1.2
   */
  protected Package getPackage(String name)
  {
    Package p;
    if (parent == null)
      // XXX - Should we use the bootstrap classloader?
      p = null;
    else
      p = parent.getPackage(name);

    if (p == null)
      {
        synchronized (definedPackages)
	{
	  p = (Package) definedPackages.get(name);
	}
      }

    return p;
  }

  /**
   * Returns all Package objects defined by this classloader and its parents.
   *
   * @return an array of all defined packages
   * @since 1.2
   */
  protected Package[] getPackages()
  {
    Package[] allPackages;

    // Get all our packages.
    Package[] packages;
    synchronized(definedPackages)
    {
      packages = new Package[definedPackages.size()];
      definedPackages.values().toArray(packages);
    }

    // If we have a parent get all packages defined by our parents.
    if (parent != null)
      {
	Package[] parentPackages = parent.getPackages();
	allPackages = new Package[parentPackages.length + packages.length];
	System.arraycopy(parentPackages, 0, allPackages, 0,
			 parentPackages.length);
	System.arraycopy(packages, 0, allPackages, parentPackages.length,
			 packages.length);
      }
    else
      // XXX - Should we use the bootstrap classloader?
      allPackages = packages;

    return allPackages;
  }

  /**
   * Called by <code>Runtime.loadLibrary()</code> to get an absolute path
   * to a (system specific) library that was requested by a class loaded
   * by this classloader. The default implementation returns
   * <code>null</code>. It should be implemented by subclasses when they
   * have a way to find the absolute path to a library. If this method
   * returns null the library is searched for in the default locations
   * (the directories listed in the <code>java.library.path</code> system
   * property).
   *
   * @param name the (system specific) name of the requested library
   * @return the full pathname to the requested library, or null
   * @see Runtime#loadLibrary()
   * @since 1.2
   */
  protected String findLibrary(String name)
  {
    return null;
  }

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
  protected final Class findSystemClass(String name) 
    throws java.lang.ClassNotFoundException
  {
    return gnu.gcj.runtime.VMClassLoader.instance.loadClass (name);
  }

  /**
   * Helper to set the signers of a class. This should be called after
   * defining the class.
   *
   * @param c the Class to set signers of
   * @param signers the signers to set
   * @since 1.1
   */   
  protected final void setSigners(Class c, Object[] signers)
  {
    /*
     * Does currently nothing. FIXME.
     */ 
  }

  /**
   * If a class named <code>name</code> was previously loaded using
   * this <code>ClassLoader</code>, then it is returned.  Otherwise
   * it returns <code>null</code>.
   * @param     name  class to find.
   * @return    the class loaded, or null.
   */ 
  protected final synchronized Class findLoadedClass(String name)
  {
    return (Class) loadedClasses.get(name);
  }

  /**
   * Get a resource using the system classloader.
   *
   * @param name the name of the resource relative to the system classloader
   * @return an input stream for the resource, or null
   * @since 1.1
   */
  public static InputStream getSystemResourceAsStream(String name) {
    return getSystemClassLoader().getResourceAsStream (name);
  }

  /**
   * Get the URL to a resource using the system classloader.
   *
   * @param name the name of the resource relative to the system classloader
   * @return the URL to the resource
   * @since 1.1
   */
  public static URL getSystemResource(String name) {
    return getSystemClassLoader().getResource (name);
  }

  /**
   * Get an Enumeration of URLs to resources with a given name using the
   * the system classloader. The enumeration firsts lists the resources with
   * the given name that can be found by the bootstrap classloader followed
   * by the resources with the given name that can be found on the classpath.
   *
   * @param name the name of the resource relative to the system classloader
   * @return an Enumeration of URLs to the resources
   * @throws IOException if I/O errors occur in the process
   * @since 1.2
   */
  public static Enumeration getSystemResources(String name) throws IOException
  {
    return getSystemClassLoader().getResources(name);
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
    try
      {
	URL res = getResource (name);
	if (res == null)
          return null;
	return res.openStream ();
      }
    catch (java.io.IOException x)
      {
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
  public URL getResource (String name) 
  {
    // The rules say search the parent class if non-null,
    // otherwise search the built-in class loader (assumed to be
    // the system ClassLoader).  If not found, call
    // findResource().
    URL result = null;

    ClassLoader delegate = parent;

    if (delegate == null)
      delegate = getSystemClassLoader ();
	
    // Protect ourselves from looping.
    if (this != delegate)
      result = delegate.getResource (name);

    if (result != null)
      return result;
    else
      return findResource (name);
  }

  /**
   * Called whenever a resource is needed that could not be provided by
   * one of the parents of this classloader. It is called by
   * <code>getResource()</code> after <code>parent.getResource()</code>
   * couldn't provide the requested resource.
   *
   * <p>The default implementation always returns null. Subclasses should
   * override this method when they can provide a way to return a URL
   * to a named resource.
   *
   * @param name the name of the resource to be found
   * @return a URL to the named resource or null when not found
   * @since 1.2
   */
  protected URL findResource (String name)
  {
    // Default to returning null.  Derived classes implement this.
    return null;
  }

  /**
   * Returns an Enumeration of all resources with a given name that can
   * be found by this classloader and its parents. Certain classloaders
   * (such as the URLClassLoader when given multiple jar files) can have
   * multiple resources with the same name that come from multiple locations.
   * It can also occur that a parent classloader offers a resource with a
   * certain name and the child classloader also offers a resource with that
   * same name. <code>getResource() only offers the first resource (of the
   * parent) with a given name. This method lists all resources with the
   * same name. The name should use '/' as path separators.
   *
   * <p>The Enumeration is created by first calling <code>getResources()</code>
   * on the parent classloader and then calling <code>findResources()</code>
   * on this classloader.
   *
   * @param name the resource name
   * @return an enumaration of all resources found
   * @throws IOException if I/O errors occur in the process
   * @since 1.2
   */
  public final Enumeration getResources(String name) throws IOException
  {
    // The rules say search the parent class if non-null,
    // otherwise search the built-in class loader (assumed to be
    // the system ClassLoader).  If not found, call
    // findResource().
    Enumeration result = null;

    ClassLoader delegate = parent;

    if (delegate == null)
      delegate = getSystemClassLoader ();
	
    // Protect ourselves from looping.
    if (this != delegate)
      result = delegate.getResources (name);

    if (result != null)
      return result;
    else
      return findResources (name);
  }

  /**
   * Called whenever all locations of a named resource are needed.
   * It is called by <code>getResources()</code> after it has called
   * <code>parent.getResources()</code>. The results are combined by
   * the <code>getResources()</code> method.
   *
   * <p>The default implementation always returns an empty Enumeration.
   * Subclasses should override it when they can provide an Enumeration of
   * URLs (possibly just one element) to the named resource.
   * The first URL of the Enumeration should be the same as the one
   * returned by <code>findResource</code>.
   *
   * @param name the name of the resource to be found
   * @return a possibly empty Enumeration of URLs to the named resource
   * @throws IOException if I/O errors occur in the process
   * @since 1.2
   */
  protected Enumeration findResources(String name) throws IOException
  {
    return Collections.enumeration(Collections.EMPTY_LIST);
  }

  /**
   * Set the default assertion status for classes loaded by this classloader,
   * used unless overridden by a package or class request.
   *
   * @param enabled true to set the default to enabled
   * @see #setClassAssertionStatus(String, boolean)
   * @see #setPackageAssertionStatus(String, boolean)
   * @see #clearAssertionStatus()
   * @since 1.4
   */
  public void setDefaultAssertionStatus(boolean enabled)
  {
    defaultAssertionStatus = enabled;
  }

  /**
   * Set the default assertion status for packages, used unless overridden
   * by a class request. This default also covers subpackages, unless they
   * are also specified. The unnamed package should use null for the name.
   *
   * @param name the package (and subpackages) to affect
   * @param enabled true to set the default to enabled
   * @see #setDefaultAssertionStatus(String, boolean)
   * @see #setClassAssertionStatus(String, boolean)
   * @see #clearAssertionStatus()
   * @since 1.4
   */
  public synchronized void setPackageAssertionStatus(String name,
                                                     boolean enabled)
  {
    if (packageAssertionStatus == null)
      packageAssertionStatus
        = new HashMap(systemPackageAssertionStatus);
    packageAssertionStatus.put(name, Boolean.valueOf(enabled));
  }
  
  /**
   * Set the default assertion status for a class. This only affects the
   * status of top-level classes, any other string is harmless.
   *
   * @param name the class to affect
   * @param enabled true to set the default to enabled
   * @throws NullPointerException if name is null
   * @see #setDefaultAssertionStatus(String, boolean)
   * @see #setPackageAssertionStatus(String, boolean)
   * @see #clearAssertionStatus()
   * @since 1.4
   */
  public synchronized void setClassAssertionStatus(String name,
                                                   boolean enabled)
  {
    if (classAssertionStatus == null)
      classAssertionStatus = new HashMap(systemClassAssertionStatus);
    // The toString() hack catches null, as required.
    classAssertionStatus.put(name.toString(), Boolean.valueOf(enabled));
  }
  
  /**
   * Resets the default assertion status of this classloader, its packages
   * and classes, all to false. This allows overriding defaults inherited
   * from the command line.
   *
   * @see #setDefaultAssertionStatus(boolean)
   * @see #setClassAssertionStatus(String, boolean)
   * @see #setPackageAssertionStatus(String, boolean)
   * @since 1.4
   */
  public synchronized void clearAssertionStatus()
  {
    defaultAssertionStatus = false;
    packageAssertionStatus = new HashMap();
    classAssertionStatus = new HashMap();
  }
}
