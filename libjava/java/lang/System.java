/* System.java -- useful methods to interface with the system
   Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007
   Free Software Foundation, Inc.

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

import gnu.classpath.SystemProperties;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.FileDescriptor;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.nio.channels.Channel;
import java.nio.channels.spi.SelectorProvider;
import java.util.AbstractCollection;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Properties;
import java.util.PropertyPermission;

/**
 * System represents system-wide resources; things that represent the
 * general environment.  As such, all methods are static.
 *
 * @author John Keiser
 * @author Eric Blake (ebb9@email.byu.edu)
 * @since 1.0
 * @status still missing 1.4 functionality
 */
public final class System
{
  // WARNING: System is a CORE class in the bootstrap cycle. See the comments
  // in vm/reference/java/lang/Runtime for implications of this fact.

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
   * change it using {@link #setErr(PrintStream)} through some hefty VM magic.
   *
   * <p>This corresponds to the C stderr and C++ cerr variables, which
   * typically output error messages to the screen, but may be used to pipe
   * output to other processes or files.  That should all be transparent to
   * you, however.
   */
  public static final PrintStream err
    = new PrintStream(new BufferedOutputStream(new FileOutputStream(FileDescriptor.err)), true);

  /**
   * A cached copy of the environment variable map.
   */
  private static Map<String,String> environmentMap;

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
    SecurityManager sm = SecurityManager.current; // Be thread-safe.
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
    SecurityManager sm = SecurityManager.current; // Be thread-safe.
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
    SecurityManager sm = SecurityManager.current; // Be thread-safe.
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
   * @param sm the new SecurityManager
   * @throws SecurityException if permission is denied
   */
  public static synchronized void setSecurityManager(SecurityManager sm)
  {
    // Implementation note: the field lives in SecurityManager because of
    // bootstrap initialization issues. This method is synchronized so that
    // no other thread changes it to null before this thread makes the change.
    if (SecurityManager.current != null)
      SecurityManager.current.checkPermission
        (new RuntimePermission("setSecurityManager"));
    SecurityManager.current = sm;
  }

  /**
   * Get the current SecurityManager. If the SecurityManager has not been
   * set yet, then this method returns null.
   *
   * @return the current SecurityManager, or null
   */
  public static SecurityManager getSecurityManager()
  {
    return SecurityManager.current;
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
   * Get the current time, measured in nanoseconds.  The result is as
   * precise as possible, and is measured against a fixed epoch.
   * However, unlike currentTimeMillis(), the epoch chosen is
   * arbitrary and may vary by platform, etc.
   * @since 1.5
   */
  public static native long nanoTime();

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
   * <dt>java.version</dt>         <dd>Java version number</dd>
   * <dt>java.vendor</dt>          <dd>Java vendor specific string</dd>
   * <dt>java.vendor.url</dt>      <dd>Java vendor URL</dd>
   * <dt>java.home</dt>            <dd>Java installation directory</dd>
   * <dt>java.vm.specification.version</dt> <dd>VM Spec version</dd>
   * <dt>java.vm.specification.vendor</dt>  <dd>VM Spec vendor</dd>
   * <dt>java.vm.specification.name</dt>    <dd>VM Spec name</dd>
   * <dt>java.vm.version</dt>      <dd>VM implementation version</dd>
   * <dt>java.vm.vendor</dt>       <dd>VM implementation vendor</dd>
   * <dt>java.vm.name</dt>         <dd>VM implementation name</dd>
   * <dt>java.specification.version</dt>    <dd>Java Runtime Environment version</dd>
   * <dt>java.specification.vendor</dt>     <dd>Java Runtime Environment vendor</dd>
   * <dt>java.specification.name</dt>       <dd>Java Runtime Environment name</dd>
   * <dt>java.class.version</dt>   <dd>Java class version number</dd>
   * <dt>java.class.path</dt>      <dd>Java classpath</dd>
   * <dt>java.library.path</dt>    <dd>Path for finding Java libraries</dd>
   * <dt>java.io.tmpdir</dt>       <dd>Default temp file path</dd>
   * <dt>java.compiler</dt>        <dd>Name of JIT to use</dd>
   * <dt>java.ext.dirs</dt>        <dd>Java extension path</dd>
   * <dt>os.name</dt>              <dd>Operating System Name</dd>
   * <dt>os.arch</dt>              <dd>Operating System Architecture</dd>
   * <dt>os.version</dt>           <dd>Operating System Version</dd>
   * <dt>file.separator</dt>       <dd>File separator ("/" on Unix)</dd>
   * <dt>path.separator</dt>       <dd>Path separator (":" on Unix)</dd>
   * <dt>line.separator</dt>       <dd>Line separator ("\n" on Unix)</dd>
   * <dt>user.name</dt>            <dd>User account name</dd>
   * <dt>user.home</dt>            <dd>User home directory</dd>
   * <dt>user.dir</dt>             <dd>User's current working directory</dd>
   * </dl>
   *
   * In addition, gnu defines several other properties, where ? stands for
   * each character in '0' through '9':
   * <dl>
   * <dt>gnu.classpath.home</dt>         <dd>Path to the classpath libraries.</dd>
   * <dt>gnu.classpath.version</dt>      <dd>Version of the classpath libraries.</dd>
   * <dt>gnu.classpath.vm.shortname</dt> <dd>Succinct version of the VM name;
   *     used for finding property files in file system</dd>
   * <dt>gnu.classpath.home.url</dt>     <dd> Base URL; used for finding
   *     property files in file system</dd>
   * <dt>gnu.cpu.endian</dt>             <dd>big or little</dd>
   * <dt>gnu.java.io.encoding_scheme_alias.ISO-8859-?</dt>   <dd>8859_?</dd>
   * <dt>gnu.java.io.encoding_scheme_alias.iso-8859-?</dt>   <dd>8859_?</dd>
   * <dt>gnu.java.io.encoding_scheme_alias.iso8859_?</dt>    <dd>8859_?</dd>
   * <dt>gnu.java.io.encoding_scheme_alias.iso-latin-_?</dt> <dd>8859_?</dd>
   * <dt>gnu.java.io.encoding_scheme_alias.latin?</dt>       <dd>8859_?</dd>
   * <dt>gnu.java.io.encoding_scheme_alias.UTF-8</dt>        <dd>UTF8</dd>
   * <dt>gnu.java.io.encoding_scheme_alias.utf-8</dt>        <dd>UTF8</dd>
   * <dt>gnu.java.util.zoneinfo.dir</dt>	<dd>Root of zoneinfo tree</dd>
   * </dl>
   *
   * @return the system properties, will never be null
   * @throws SecurityException if permission is denied
   */
  public static Properties getProperties()
  {
    SecurityManager sm = SecurityManager.current; // Be thread-safe.
    if (sm != null)
      sm.checkPropertiesAccess();
    return SystemProperties.getProperties();
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
    SecurityManager sm = SecurityManager.current; // Be thread-safe.
    if (sm != null)
      sm.checkPropertiesAccess();
    SystemProperties.setProperties(properties);
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
    SecurityManager sm = SecurityManager.current; // Be thread-safe.
    if (sm != null)
      sm.checkPropertyAccess(key);
    else if (key.length() == 0)
      throw new IllegalArgumentException("key can't be empty");
    return SystemProperties.getProperty(key);
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
    SecurityManager sm = SecurityManager.current; // Be thread-safe.
    if (sm != null)
      sm.checkPropertyAccess(key);
    return SystemProperties.getProperty(key, def);
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
    SecurityManager sm = SecurityManager.current; // Be thread-safe.
    if (sm != null)
      sm.checkPermission(new PropertyPermission(key, "write"));
    return SystemProperties.setProperty(key, value);
  }

  /**
   * Remove a single system property by name. A security check may be
   * performed, <code>checkPropertyAccess(key, "write")</code>.
   *
   * @param key the name of the system property to remove
   * @return the previous value, or null
   * @throws SecurityException if permission is denied
   * @throws NullPointerException if key is null
   * @throws IllegalArgumentException if key is ""
   * @since 1.5
   */
  public static String clearProperty(String key)
  {
    SecurityManager sm = SecurityManager.current; // Be thread-safe.
    if (sm != null)
      sm.checkPermission(new PropertyPermission(key, "write"));
    // This handles both the null pointer exception and the illegal
    // argument exception.
    if (key.length() == 0)
      throw new IllegalArgumentException("key can't be empty");
    return SystemProperties.remove(key);
  }

  /**
   * Gets the value of an environment variable.
   *
   * @param name the name of the environment variable
   * @return the string value of the variable or null when the
   *         environment variable is not defined.
   * @throws NullPointerException
   * @throws SecurityException if permission is denied
   * @since 1.5
   * @specnote This method was deprecated in some JDK releases, but
   *           was restored in 1.5.
   */
  public static String getenv(String name)
  {
    if (name == null)
      throw new NullPointerException();
    SecurityManager sm = SecurityManager.current; // Be thread-safe.
    if (sm != null)
      sm.checkPermission(new RuntimePermission("getenv." + name));
    return getenv0(name);
  }

  /**
   * <p>
   * Returns an unmodifiable view of the system environment variables.
   * If the underlying system does not support environment variables,
   * an empty map is returned.
   * </p>
   * <p>
   * The returned map is read-only and does not accept queries using
   * null keys or values, or those of a type other than <code>String</code>.
   * Attempts to modify the map will throw an
   * <code>UnsupportedOperationException</code>, while attempts
   * to pass in a null value will throw a
   * <code>NullPointerException</code>.  Types other than <code>String</code>
   * throw a <code>ClassCastException</code>.
   * </p>
   * <p>
   * As the returned map is generated using data from the underlying
   * platform, it may not comply with the <code>equals()</code>
   * and <code>hashCode()</code> contracts.  It is also likely that
   * the keys of this map will be case-sensitive.
   * </p>
   * <p>
   * Use of this method may require a security check for the
   * RuntimePermission "getenv.*".
   * </p>
   *
   * @return a map of the system environment variables.
   * @throws SecurityException if the checkPermission method of
   *         an installed security manager prevents access to
   *         the system environment variables.
   * @since 1.5
   */
  public static Map<String, String> getenv()
  {
    SecurityManager sm = SecurityManager.current; // Be thread-safe.
    if (sm != null)
      sm.checkPermission(new RuntimePermission("getenv.*"));
    if (environmentMap == null)
      {
	// List<String> environ = (List<String>)VMSystem.environ();
	// FIXME
	List<String> environ = new ArrayList<String>();
	Map<String,String> variables = new EnvironmentMap();
	for (String pair : environ)
	  {
	    String[] parts = pair.split("=");
	    variables.put(parts[0], parts[1]);
	  }
	environmentMap = Collections.unmodifiableMap(variables);
      }
    return environmentMap;
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
   * <p>
   * The library is loaded using the class loader associated with the
   * class associated with the invoking method.
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
   * <p>
   * The library is loaded using the class loader associated with the
   * class associated with the invoking method.
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

  /**
   * Gets the value of an environment variable.
   *
   * @see #getenv(String)
   */
  static native String getenv0(String name);

  /**
   * Returns the inherited channel of the VM.
   *
   * This wraps the inheritedChannel() call of the system's default
   * {@link SelectorProvider}.
   *
   * @return the inherited channel of the VM
   *
   * @throws IOException If an I/O error occurs
   * @throws SecurityException If an installed security manager denies access
   *         to RuntimePermission("inheritedChannel")
   *
   * @since 1.5
   */
  public static Channel inheritedChannel()
    throws IOException
  {
    return SelectorProvider.provider().inheritedChannel();
  }

  /**
   * This is a specialised <code>Collection</code>, providing
   * the necessary provisions for the collections used by the
   * environment variable map.  Namely, it prevents
   * querying anything but <code>String</code>s.
   *
   * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
   */
  private static class EnvironmentCollection
    extends AbstractCollection<String>
  {

    /**
     * The wrapped collection.
     */
    protected Collection<String> c;

    /**
     * Constructs a new environment collection, which
     * wraps the elements of the supplied collection.
     *
     * @param coll the collection to use as a base for
     *             this collection.
     */
    public EnvironmentCollection(Collection<String> coll)
    {
      c = coll;
    }
        
    /**
     * Blocks queries containing a null object or an object which
     * isn't of type <code>String</code>.  All other queries
     * are forwarded to the underlying collection.
     *
     * @param obj the object to look for.
     * @return true if the object exists in the collection.
     * @throws NullPointerException if the specified object is null.
     * @throws ClassCastException if the specified object is not a String.
     */
    public boolean contains(Object obj)
    {
      if (obj == null)
	  throw new
	    NullPointerException("This collection does not support " +
				 "null values.");
      if (!(obj instanceof String))
	  throw new
	    ClassCastException("This collection only supports Strings.");
      return c.contains(obj);
    }
    
    /**
     * Blocks queries where the collection contains a null object or
     * an object which isn't of type <code>String</code>.  All other
     * queries are forwarded to the underlying collection.
     *
     * @param coll the collection of objects to look for.
     * @return true if the collection contains all elements in the collection.
     * @throws NullPointerException if the collection is null.
     * @throws NullPointerException if any collection entry is null.
     * @throws ClassCastException if any collection entry is not a String.
     */
    public boolean containsAll(Collection<?> coll)
    {
      for (Object o: coll)
	{
	  if (o == null)
	      throw new
		NullPointerException("This collection does not support " +
				     "null values.");
	  if (!(o instanceof String))
	      throw new
		ClassCastException("This collection only supports Strings.");
	}
      return c.containsAll(coll);
    }

    /**
     * This returns an iterator over the map elements, with the
     * same provisions as for the collection and underlying map.
     *
     * @return an iterator over the map elements.
     */
    public Iterator<String> iterator()
    {
      return c.iterator();
    }
    
    /**
     * Blocks the removal of elements from the collection.
     *
     * @return true if the removal was sucessful.
     * @throws NullPointerException if the collection is null.
     * @throws NullPointerException if any collection entry is null.
     * @throws ClassCastException if any collection entry is not a String.
     */
    public boolean remove(Object key)
    {
      if (key == null)
	  throw new
	    NullPointerException("This collection does not support " +
				 "null values.");
      if (!(key instanceof String))
	  throw new
	    ClassCastException("This collection only supports Strings.");
      return c.contains(key);
    }	
        
    /**
     * Blocks the removal of all elements in the specified
     * collection from the collection.
     *
     * @param coll the collection of elements to remove.
     * @return true if the elements were removed.
     * @throws NullPointerException if the collection is null.
     * @throws NullPointerException if any collection entry is null.
     * @throws ClassCastException if any collection entry is not a String.
     */
    public boolean removeAll(Collection<?> coll)
    {
      for (Object o: coll)
	{
	  if (o == null)
	      throw new
		NullPointerException("This collection does not support " +
				     "null values.");
	  if (!(o instanceof String))
	    throw new
	      ClassCastException("This collection only supports Strings.");
	}
      return c.removeAll(coll);
    }
    
    /**
     * Blocks the retention of all elements in the specified
     * collection from the collection.
     *
     * @param c the collection of elements to retain.
     * @return true if the other elements were removed.
     * @throws NullPointerException if the collection is null.
     * @throws NullPointerException if any collection entry is null.
     * @throws ClassCastException if any collection entry is not a String.
     */
    public boolean retainAll(Collection<?> coll)
    {
      for (Object o: coll)
	{
	  if (o == null)
	      throw new
		NullPointerException("This collection does not support " +
				     "null values.");
	  if (!(o instanceof String))
	    throw new
	      ClassCastException("This collection only supports Strings.");
	}
      return c.containsAll(coll);
    }

    /**
     * This simply calls the same method on the wrapped
     * collection.
     *
     * @return the size of the underlying collection.
     */
    public int size()
    {
      return c.size();
    }

  } // class EnvironmentCollection<String>

  /**
   * This is a specialised <code>HashMap</code>, which
   * prevents the addition or querying of anything other than
   * <code>String</code> objects. 
   *
   * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
   */
  static class EnvironmentMap
    extends HashMap<String,String>
  {
    
    /**
     * Cache the entry set.
     */
    private transient Set<Map.Entry<String,String>> entries;

    /**
     * Cache the key set.
     */
    private transient Set<String> keys;

    /**
     * Cache the value collection.
     */
    private transient Collection<String> values;

    /**
     * Constructs a new empty <code>EnvironmentMap</code>.
     */
    EnvironmentMap()
    {
      super();
    }

    /**
     * Constructs a new <code>EnvironmentMap</code> containing
     * the contents of the specified map.
     *
     * @param m the map to be added to this.
     * @throws NullPointerException if a key or value is null.
     * @throws ClassCastException if a key or value is not a String.
     */    
    EnvironmentMap(Map<String,String> m)
    {
      super(m);
    }

    /**
     * Blocks queries containing a null key or one which is not
     * of type <code>String</code>.  All other queries
     * are forwarded to the superclass.
     *
     * @param key the key to look for in the map.
     * @return true if the key exists in the map.
     * @throws NullPointerException if the specified key is null.
     */
    public boolean containsKey(Object key)
    {
      if (key == null)
	throw new
	  NullPointerException("This map does not support null keys.");
      if (!(key instanceof String))
	throw new
	  ClassCastException("This map only allows queries using Strings.");
      return super.containsKey(key);
    }
    
    /**
     * Blocks queries using a null or non-<code>String</code> value.
     * All other queries are forwarded to the superclass.
     *
     * @param value the value to look for in the map.
     * @return true if the value exists in the map.
     * @throws NullPointerException if the specified value is null.
     */
    public boolean containsValue(Object value)
    {
      if (value == null)
	  throw new
	    NullPointerException("This map does not support null values.");
      if (!(value instanceof String))
	throw new
	  ClassCastException("This map only allows queries using Strings.");
      return super.containsValue(value);
    }

    /**
     * Returns a set view of the map entries, with the same
     * provisions as for the underlying map.
     *
     * @return a set containing the map entries.
     */
    public Set<Map.Entry<String,String>> entrySet()
    {
      if (entries == null)
        entries = super.entrySet();
      return entries;
    }

    /**
     * Blocks queries containing a null or non-<code>String</code> key.
     * All other queries are passed on to the superclass.
     *
     * @param key the key to retrieve the value for.
     * @return the value associated with the given key.
     * @throws NullPointerException if the specified key is null.
     * @throws ClassCastException if the specified key is not a String.
     */
    public String get(Object key)
    {
      if (key == null)
	throw new
	  NullPointerException("This map does not support null keys.");
      if (!(key instanceof String))
	throw new
	  ClassCastException("This map only allows queries using Strings.");
      return super.get(key);
    }
    
    /**
     * Returns a set view of the keys, with the same
     * provisions as for the underlying map.
     *
     * @return a set containing the keys.
     */
    public Set<String> keySet()
    {
      if (keys == null)
        keys = new EnvironmentSet(super.keySet());
      return keys;
    }

    /**
     * Associates the given key to the given value. If the
     * map already contains the key, its value is replaced.
     * The map does not accept null keys or values, or keys
     * and values not of type {@link String}.
     *
     * @param key the key to map.
     * @param value the value to be mapped.
     * @return the previous value of the key, or null if there was no mapping
     * @throws NullPointerException if a key or value is null.
     * @throws ClassCastException if a key or value is not a String.
     */
    public String put(String key, String value)
    {
      if (key == null)
	throw new NullPointerException("A new key is null.");
      if (value == null)
	throw new NullPointerException("A new value is null.");
      if (!(key instanceof String))
	throw new ClassCastException("A new key is not a String.");
      if (!(value instanceof String))
	throw new ClassCastException("A new value is not a String.");
      return super.put(key, value);
    }

    /**
     * Removes a key-value pair from the map.  The queried key may not
     * be null or of a type other than a <code>String</code>.
     *
     * @param key the key of the entry to remove.
     * @return the removed value.
     * @throws NullPointerException if the specified key is null.
     * @throws ClassCastException if the specified key is not a String.
     */
    public String remove(Object key)
    {
      if (key == null)
	throw new
	  NullPointerException("This map does not support null keys.");
      if (!(key instanceof String))
	throw new
	  ClassCastException("This map only allows queries using Strings.");
      return super.remove(key);
    }
    
    /**
     * Returns a collection view of the values, with the same
     * provisions as for the underlying map.
     *
     * @return a collection containing the values.
     */
    public Collection<String> values()
    {
      if (values == null)
        values = new EnvironmentCollection(super.values());
      return values;
    }
    
  }

  /**
   * This is a specialised <code>Set</code>, providing
   * the necessary provisions for the collections used by the
   * environment variable map.  Namely, it prevents
   * modifications and the use of queries with null
   * or non-<code>String</code> values.
   *
   * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
   */
  private static class EnvironmentSet
    extends EnvironmentCollection
    implements Set<String>
  {

    /**
     * Constructs a new environment set, which
     * wraps the elements of the supplied set.
     *
     * @param set the set to use as a base for
     *             this set.
     */
    public EnvironmentSet(Set<String> set)
    {
      super(set);
    }

    /**
     * This simply calls the same method on the wrapped
     * collection.
     *
     * @param obj the object to compare with.
     * @return true if the two objects are equal.
     */
    public boolean equals(Object obj)
    {
      return c.equals(obj);
    }

    /**
     * This simply calls the same method on the wrapped
     * collection.
     *
     * @return the hashcode of the collection.
     */
    public int hashCode()
    {
      return c.hashCode();
    }

  } // class EnvironmentSet<String>

} // class System
