/* System.java -- useful methods to interface with the system
   Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003 Free Software Foundation, Inc.

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

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.FileDescriptor;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.PrintStream;
import java.util.Properties;
import java.util.PropertyPermission;
import gnu.classpath.Configuration;

/**
 * System represents system-wide resources; things that represent the
 * general environment.  As such, all methods are static.
 *
 * @author John Keiser
 * @author Eric Blake <ebb9@email.byu.edu>
 * @since 1.0
 * @status still missing 1.4 functionality
 */
public final class System
{
  // WARNING: System is a CORE class in the bootstrap cycle. See the comments
  // in vm/reference/java/lang/Runtime for implications of this fact.

  /**
   * Add to the default properties. The field is stored in Runtime, because
   * of the bootstrap sequence; but this adds several useful properties to
   * the defaults. Once the default is stabilized, it should not be modified;
   * instead it is passed as a parent properties for fast setup of the
   * defaults when calling <code>setProperties(null)</code>.
   */
  static
  {
    // Note that this loadLibrary() takes precedence over the one in Object,
    // since Object.<clinit> is waiting for System.<clinit> to complete
    // first; but loading a library twice is harmless.
    if (Configuration.INIT_LOAD_LIBRARY)
      loadLibrary("javalang");

    Properties defaultProperties = Runtime.defaultProperties;

    // Set base URL if not already set.
    if (defaultProperties.get("gnu.classpath.home.url") == null)
      defaultProperties.put("gnu.classpath.home.url",
			    "file://"
			    + defaultProperties.get("gnu.classpath.home")
			    + "/lib");

    // Set short name if not already set.
    if (defaultProperties.get("gnu.classpath.vm.shortname") == null)
      {
	String value = defaultProperties.getProperty("java.vm.name");
	int index = value.lastIndexOf(' ');
	if (index != -1)
	  value = value.substring(index + 1);
	defaultProperties.put("gnu.classpath.vm.shortname", value);
      }

    defaultProperties.put("gnu.cpu.endian",
			  isWordsBigEndian() ? "big" : "little");

    // GCJ LOCAL: Classpath sets common encoding aliases here.
    // Since we don't (yet) have gnu.java.io.EncodingManager, these
    // are a waste of time and just slow down system startup.

    // XXX FIXME - Temp hack for old systems that set the wrong property
    if (defaultProperties.get("java.io.tmpdir") == null)
      defaultProperties.put("java.io.tmpdir",
                            defaultProperties.get("java.tmpdir"));
  }

  /**
   * Stores the current system properties. This can be modified by
   * {@link #setProperties(Properties)}, but will never be null, because
   * setProperties(null) sucks in the default properties.
   */
  // Note that we use clone here and not new.  Some programs assume
  // that the system properties do not have a parent.
  private static Properties properties
    = (Properties) Runtime.defaultProperties.clone();

  /**
   * The standard InputStream. This is assigned at startup and starts its
   * life perfectly valid. Although it is marked final, you can change it
   * using {@link #setIn(InputStream)} through some hefty VM magic.
   *
   * <p>This corresponds to the C stdin and C++ cin variables, which
   * typically input from the keyboard, but may be used to pipe input from
   * other processes or files.  That should all be transparent to you,
   * however.
   */
  public static final InputStream in
    = new BufferedInputStream(new FileInputStream(FileDescriptor.in));
  /**
   * The standard output PrintStream.  This is assigned at startup and
   * starts its life perfectly valid. Although it is marked final, you can
   * change it using {@link #setOut(PrintStream)} through some hefty VM magic.
   *
   * <p>This corresponds to the C stdout and C++ cout variables, which
   * typically output normal messages to the screen, but may be used to pipe
   * output to other processes or files.  That should all be transparent to
   * you, however.
   */
  public static final PrintStream out
    = new PrintStream(new BufferedOutputStream(new FileOutputStream(FileDescriptor.out)), true);
  /**
   * The standard output PrintStream.  This is assigned at startup and
   * starts its life perfectly valid. Although it is marked final, you can
   * change it using {@link #setOut(PrintStream)} through some hefty VM magic.
   *
   * <p>This corresponds to the C stderr and C++ cerr variables, which
   * typically output error messages to the screen, but may be used to pipe
   * output to other processes or files.  That should all be transparent to
   * you, however.
   */
  public static final PrintStream err
    = new PrintStream(new BufferedOutputStream(new FileOutputStream(FileDescriptor.err)), true);

  /**
   * This class is uninstantiable.
   */
  private System()
  {
  }

  /**
   * Set {@link #in} to a new InputStream. This uses some VM magic to change
   * a "final" variable, so naturally there is a security check,
   * <code>RuntimePermission("setIO")</code>.
   *
   * @param in the new InputStream
   * @throws SecurityException if permission is denied
   * @since 1.1
   */
  public static void setIn(InputStream in)
  {
    SecurityManager sm = Runtime.securityManager; // Be thread-safe.
    if (sm != null)
      sm.checkPermission(new RuntimePermission("setIO"));
    setIn0(in);
  }

  /**
   * Set {@link #out} to a new PrintStream. This uses some VM magic to change
   * a "final" variable, so naturally there is a security check,
   * <code>RuntimePermission("setIO")</code>.
   *
   * @param out the new PrintStream
   * @throws SecurityException if permission is denied
   * @since 1.1
   */
  public static void setOut(PrintStream out)
  {
    SecurityManager sm = Runtime.securityManager; // Be thread-safe.
    if (sm != null)
      sm.checkPermission(new RuntimePermission("setIO"));
    
    setOut0(out);
  }

  /**
   * Set {@link #err} to a new PrintStream. This uses some VM magic to change
   * a "final" variable, so naturally there is a security check,
   * <code>RuntimePermission("setIO")</code>.
   *
   * @param err the new PrintStream
   * @throws SecurityException if permission is denied
   * @since 1.1
   */
  public static void setErr(PrintStream err)
  {
    SecurityManager sm = Runtime.securityManager; // Be thread-safe.
    if (sm != null)
      sm.checkPermission(new RuntimePermission("setIO"));
    setErr0(err);
  }

  /**
   * Set the current SecurityManager. If a security manager already exists,
   * then <code>RuntimePermission("setSecurityManager")</code> is checked
   * first. Since this permission is denied by the default security manager,
   * setting the security manager is often an irreversible action.
   *
   * <STRONG>Spec Note:</STRONG> Don't ask me, I didn't write it.  It looks
   * pretty vulnerable; whoever gets to the gate first gets to set the policy.
   * There is probably some way to set the original security manager as a
   * command line argument to the VM, but I don't know it.
   *
   * @param sm the new SecurityManager
   * @throws SecurityException if permission is denied
   */
  public synchronized static void setSecurityManager(SecurityManager sm)
  {
    // Implementation note: the field lives in Runtime because of bootstrap
    // initialization issues. This method is synchronized so that no other
    // thread changes it to null before this thread makes the change.
    if (Runtime.securityManager != null)
      Runtime.securityManager.checkPermission
        (new RuntimePermission("setSecurityManager"));
    Runtime.securityManager = sm;
  }

  /**
   * Get the current SecurityManager. If the SecurityManager has not been
   * set yet, then this method returns null.
   *
   * @return the current SecurityManager, or null
   */
  public static SecurityManager getSecurityManager()
  {
    // Implementation note: the field lives in Runtime because of bootstrap
    // initialization issues.
    return Runtime.securityManager;
  }

  /**
   * Get the current time, measured in the number of milliseconds from the
   * beginning of Jan. 1, 1970. This is gathered from the system clock, with
   * any attendant incorrectness (it may be timezone dependent).
   *
   * @return the current time
   * @see java.util.Date
   */
  public static native long currentTimeMillis();

  /**
   * Copy one array onto another from <code>src[srcStart]</code> ...
   * <code>src[srcStart+len-1]</code> to <code>dest[destStart]</code> ...
   * <code>dest[destStart+len-1]</code>. First, the arguments are validated:
   * neither array may be null, they must be of compatible types, and the
   * start and length must fit within both arrays. Then the copying starts,
   * and proceeds through increasing slots.  If src and dest are the same
   * array, this will appear to copy the data to a temporary location first.
   * An ArrayStoreException in the middle of copying will leave earlier
   * elements copied, but later elements unchanged.
   *
   * @param src the array to copy elements from
   * @param srcStart the starting position in src
   * @param dest the array to copy elements to
   * @param destStart the starting position in dest
   * @param len the number of elements to copy
   * @throws NullPointerException if src or dest is null
   * @throws ArrayStoreException if src or dest is not an array, if they are
   *         not compatible array types, or if an incompatible runtime type
   *         is stored in dest
   * @throws IndexOutOfBoundsException if len is negative, or if the start or
   *         end copy position in either array is out of bounds
   */
  public static native void arraycopy(Object src, int srcStart,
				      Object dest, int destStart, int len);

  /**
   * Get a hash code computed by the VM for the Object. This hash code will
   * be the same as Object's hashCode() method.  It is usually some
   * convolution of the pointer to the Object internal to the VM.  It
   * follows standard hash code rules, in that it will remain the same for a
   * given Object for the lifetime of that Object.
   *
   * @param o the Object to get the hash code for
   * @return the VM-dependent hash code for this Object
   * @since 1.1
   */
  public static native int identityHashCode(Object o);

  /**
   * Get all the system properties at once. A security check may be performed,
   * <code>checkPropertiesAccess</code>. Note that a security manager may
   * allow getting a single property, but not the entire group.
   *
   * <p>The required properties include:
   * <dl>
   * <dt>java.version         <dd>Java version number
   * <dt>java.vendor          <dd>Java vendor specific string
   * <dt>java.vendor.url      <dd>Java vendor URL
   * <dt>java.home            <dd>Java installation directory
   * <dt>java.vm.specification.version <dd>VM Spec version
   * <dt>java.vm.specification.vendor  <dd>VM Spec vendor
   * <dt>java.vm.specification.name    <dd>VM Spec name
   * <dt>java.vm.version      <dd>VM implementation version
   * <dt>java.vm.vendor       <dd>VM implementation vendor
   * <dt>java.vm.name         <dd>VM implementation name
   * <dt>java.specification.version    <dd>Java Runtime Environment version
   * <dt>java.specification.vendor     <dd>Java Runtime Environment vendor
   * <dt>java.specification.name       <dd>Java Runtime Environment name
   * <dt>java.class.version   <dd>Java class version number
   * <dt>java.class.path      <dd>Java classpath
   * <dt>java.library.path    <dd>Path for finding Java libraries
   * <dt>java.io.tmpdir       <dd>Default temp file path
   * <dt>java.compiler        <dd>Name of JIT to use
   * <dt>java.ext.dirs        <dd>Java extension path
   * <dt>os.name              <dd>Operating System Name
   * <dt>os.arch              <dd>Operating System Architecture
   * <dt>os.version           <dd>Operating System Version
   * <dt>file.separator       <dd>File separator ("/" on Unix)
   * <dt>path.separator       <dd>Path separator (":" on Unix)
   * <dt>line.separator       <dd>Line separator ("\n" on Unix)
   * <dt>user.name            <dd>User account name
   * <dt>user.home            <dd>User home directory
   * <dt>user.dir             <dd>User's current working directory
   * </dl>
   *
   * In addition, gnu defines several other properties, where ? stands for
   * each character in '0' through '9':
   * <dl>
   * <dl> gnu.classpath.vm.shortname <dd> Succinct version of the VM name;
   *      used for finding property files in file system
   * <dl> gnu.classpath.home.url <dd> Base URL; used for finding
   *      property files in file system
   * <dt> gnu.cpu.endian      <dd>big or little
   * <dt> gnu.java.io.encoding_scheme_alias.ISO-8859-?   <dd>8859_?
   * <dt> gnu.java.io.encoding_scheme_alias.iso-8859-?   <dd>8859_?
   * <dt> gnu.java.io.encoding_scheme_alias.iso8859_?    <dd>8859_?
   * <dt> gnu.java.io.encoding_scheme_alias.iso-latin-_? <dd>8859_?
   * <dt> gnu.java.io.encoding_scheme_alias.latin?       <dd>8859_?
   * <dt> gnu.java.io.encoding_scheme_alias.UTF-8        <dd>UTF8
   * <dt> gnu.java.io.encoding_scheme_alias.utf-8        <dd>UTF8
   * </dl>
   *
   * @return the system properties, will never be null
   * @throws SecurityException if permission is denied
   */
  public static Properties getProperties()
  {
    SecurityManager sm = Runtime.securityManager; // Be thread-safe.
    if (sm != null)
      sm.checkPropertiesAccess();
    return properties;
  }

  /**
   * Set all the system properties at once. A security check may be performed,
   * <code>checkPropertiesAccess</code>. Note that a security manager may
   * allow setting a single property, but not the entire group. An argument
   * of null resets the properties to the startup default.
   *
   * @param properties the new set of system properties
   * @throws SecurityException if permission is denied
   */
  public static void setProperties(Properties properties)
  {
    SecurityManager sm = Runtime.securityManager; // Be thread-safe.
    if (sm != null)
      sm.checkPropertiesAccess();
    if (properties == null)
      {
	// Note that we use clone here and not new.  Some programs
	// assume that the system properties do not have a parent.
	properties = (Properties) Runtime.defaultProperties.clone();
      }
    System.properties = properties;
  }

  /**
   * Get a single system property by name. A security check may be performed,
   * <code>checkPropertyAccess(key)</code>.
   *
   * @param key the name of the system property to get
   * @return the property, or null if not found
   * @throws SecurityException if permission is denied
   * @throws NullPointerException if key is null
   * @throws IllegalArgumentException if key is ""
   */
  public static String getProperty(String key)
  {
    SecurityManager sm = Runtime.securityManager; // Be thread-safe.
    if (sm != null)
      sm.checkPropertyAccess(key);
    else if (key.length() == 0)
      throw new IllegalArgumentException("key can't be empty");
    return properties.getProperty(key);
  }

  /**
   * Get a single system property by name. A security check may be performed,
   * <code>checkPropertyAccess(key)</code>.
   *
   * @param key the name of the system property to get
   * @param def the default
   * @return the property, or def if not found
   * @throws SecurityException if permission is denied
   * @throws NullPointerException if key is null
   * @throws IllegalArgumentException if key is ""
   */
  public static String getProperty(String key, String def)
  {
    SecurityManager sm = Runtime.securityManager; // Be thread-safe.
    if (sm != null)
      sm.checkPropertyAccess(key);
    return properties.getProperty(key, def);
  }

  /**
   * Set a single system property by name. A security check may be performed,
   * <code>checkPropertyAccess(key, "write")</code>.
   *
   * @param key the name of the system property to set
   * @param value the new value
   * @return the previous value, or null
   * @throws SecurityException if permission is denied
   * @throws NullPointerException if key is null
   * @throws IllegalArgumentException if key is ""
   * @since 1.2
   */
  public static String setProperty(String key, String value)
  {
    SecurityManager sm = Runtime.securityManager; // Be thread-safe.
    if (sm != null)
      sm.checkPermission(new PropertyPermission(key, "write"));
    return (String) properties.setProperty(key, value);
  }

  /**
   * This used to get an environment variable, but following Sun's lead,
   * it now throws an Error. Use <code>getProperty</code> instead.
   *
   * @param name the name of the environment variable
   * @return this does not return
   * @throws Error this is not supported
   * @deprecated use {@link #getProperty(String)}; getenv is not supported
   */
  public static String getenv(String name)
  {
    throw new Error("getenv no longer supported, use properties instead: "
                    + name);
  }

  /**
   * Terminate the Virtual Machine. This just calls
   * <code>Runtime.getRuntime().exit(status)</code>, and never returns.
   * Obviously, a security check is in order, <code>checkExit</code>.
   *
   * @param status the exit status; by convention non-zero is abnormal
   * @throws SecurityException if permission is denied
   * @see Runtime#exit(int)
   */
  public static void exit(int status)
  {
    Runtime.getRuntime().exit(status);
  }

  /**
   * Calls the garbage collector. This is only a hint, and it is up to the
   * implementation what this hint suggests, but it usually causes a
   * best-effort attempt to reclaim unused memory from discarded objects.
   * This calls <code>Runtime.getRuntime().gc()</code>.
   *
   * @see Runtime#gc()
   */
  public static void gc()
  {
    Runtime.getRuntime().gc();
  }

  /**
   * Runs object finalization on pending objects. This is only a hint, and
   * it is up to the implementation what this hint suggests, but it usually
   * causes a best-effort attempt to run finalizers on all objects ready
   * to be reclaimed. This calls
   * <code>Runtime.getRuntime().runFinalization()</code>.
   *
   * @see Runtime#runFinalization()
   */
  public static void runFinalization()
  {
    Runtime.getRuntime().runFinalization();
  }

  /**
   * Tell the Runtime whether to run finalization before exiting the
   * JVM.  This is inherently unsafe in multi-threaded applications,
   * since it can force initialization on objects which are still in use
   * by live threads, leading to deadlock; therefore this is disabled by
   * default. There may be a security check, <code>checkExit(0)</code>. This
   * calls <code>Runtime.getRuntime().runFinalizersOnExit()</code>.
   *
   * @param finalizeOnExit whether to run finalizers on exit
   * @throws SecurityException if permission is denied
   * @see Runtime#runFinalizersOnExit()
   * @since 1.1
   * @deprecated never rely on finalizers to do a clean, thread-safe,
   *             mop-up from your code
   */
  public static void runFinalizersOnExit(boolean finalizeOnExit)
  {
    Runtime.getRuntime().runFinalizersOnExit(finalizeOnExit);
  }

  /**
   * Load a code file using its explicit system-dependent filename. A security
   * check may be performed, <code>checkLink</code>. This just calls
   * <code>Runtime.getRuntime().load(filename)</code>.
   *
   * @param filename the code file to load
   * @throws SecurityException if permission is denied
   * @throws UnsatisfiedLinkError if the file cannot be loaded
   * @see Runtime#load(String)
   */
  public static void load(String filename)
  {
    Runtime.getRuntime().load(filename);
  }

  /**
   * Load a library using its explicit system-dependent filename. A security
   * check may be performed, <code>checkLink</code>. This just calls
   * <code>Runtime.getRuntime().load(filename)</code>.
   *
   * @param libname the library file to load
   * @throws SecurityException if permission is denied
   * @throws UnsatisfiedLinkError if the file cannot be loaded
   * @see Runtime#load(String)
   */
  public static void loadLibrary(String libname)
  {
    Runtime.getRuntime().loadLibrary(libname);
  }

  /**
   * Convert a library name to its platform-specific variant.
   *
   * @param libname the library name, as used in <code>loadLibrary</code>
   * @return the platform-specific mangling of the name
   * @since 1.2
   */
  public static String mapLibraryName(String libname)
  {
    // XXX Fix this!!!!
    return Runtime.nativeGetLibname("", libname);
  }

  /**
   * Detect big-endian systems.
   *
   * @return true if the system is big-endian.
   */
  static native boolean isWordsBigEndian();

  /**
   * Set {@link #in} to a new InputStream.
   *
   * @param in the new InputStream
   * @see #setIn(InputStream)
   */
  private static native void setIn0(InputStream in);

  /**
   * Set {@link #out} to a new PrintStream.
   *
   * @param out the new PrintStream
   * @see #setOut(PrintStream)
   */
  private static native void setOut0(PrintStream out);

  /**
   * Set {@link #err} to a new PrintStream.
   *
   * @param err the new PrintStream
   * @see #setErr(PrintStream)
   */
  private static native void setErr0(PrintStream err);
} // class System
