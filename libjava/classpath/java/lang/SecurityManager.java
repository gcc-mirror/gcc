/* SecurityManager.java -- security checks for privileged actions
   Copyright (C) 1998, 1999, 2001, 2002, 2005  Free Software Foundation, Inc.

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

import gnu.classpath.VMStackWalker;

import java.awt.AWTPermission;
import java.io.File;
import java.io.FileDescriptor;
import java.io.FilePermission;
import java.lang.reflect.Member;
import java.net.InetAddress;
import java.net.SocketPermission;
import java.security.AccessControlContext;
import java.security.AccessController;
import java.security.AllPermission;
import java.security.Permission;
import java.security.PrivilegedAction;
import java.security.Security;
import java.security.SecurityPermission;
import java.util.PropertyPermission;
import java.util.StringTokenizer;

/**
 * SecurityManager is a class you can extend to create your own Java
 * security policy.  By default, there is no SecurityManager installed in
 * 1.1, which means that all things are permitted to all people. The security
 * manager, if set, is consulted before doing anything with potentially
 * dangerous results, and throws a <code>SecurityException</code> if the
 * action is forbidden.
 *
 * <p>A typical check is as follows, just before the dangerous operation:<br>
 * <pre>
 * SecurityManager sm = System.getSecurityManager();
 * if (sm != null)
 *   sm.checkABC(<em>argument</em>, ...);
 * </pre>
 * Note that this is thread-safe, by caching the security manager in a local
 * variable rather than risking a NullPointerException if the mangager is
 * changed between the check for null and before the permission check.
 *
 * <p>The special method <code>checkPermission</code> is a catchall, and
 * the default implementation calls
 * <code>AccessController.checkPermission</code>. In fact, all the other
 * methods default to calling checkPermission.
 *
 * <p>Sometimes, the security check needs to happen from a different context,
 * such as when called from a worker thread. In such cases, use
 * <code>getSecurityContext</code> to take a snapshot that can be passed
 * to the worker thread:<br>
 * <pre>
 * Object context = null;
 * SecurityManager sm = System.getSecurityManager();
 * if (sm != null)
 *   context = sm.getSecurityContext(); // defaults to an AccessControlContext
 * // now, in worker thread
 * if (sm != null)
 *   sm.checkPermission(permission, context);
 * </pre>
 *
 * <p>Permissions fall into these categories: File, Socket, Net, Security,
 * Runtime, Property, AWT, Reflect, and Serializable. Each of these
 * permissions have a property naming convention, that follows a hierarchical
 * naming convention, to make it easy to grant or deny several permissions
 * at once. Some permissions also take a list of permitted actions, such
 * as "read" or "write", to fine-tune control even more. The permission
 * <code>java.security.AllPermission</code> grants all permissions.
 *
 * <p>The default methods in this class deny all things to all people. You
 * must explicitly grant permission for anything you want to be legal when
 * subclassing this class.
 *
 * @author John Keiser
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see ClassLoader
 * @see SecurityException
 * @see #checkTopLevelWindow(Object)
 * @see System#getSecurityManager()
 * @see System#setSecurityManager(SecurityManager)
 * @see AccessController
 * @see AccessControlContext
 * @see AccessControlException
 * @see Permission
 * @see BasicPermission
 * @see java.io.FilePermission
 * @see java.net.SocketPermission
 * @see java.util.PropertyPermission
 * @see RuntimePermission
 * @see java.awt.AWTPermission
 * @see Policy
 * @see SecurityPermission
 * @see ProtectionDomain
 * @since 1.0
 * @status still missing 1.4 functionality
 */
public class SecurityManager
{
  /**
   * The current security manager. This is located here instead of in
   * System, to avoid security problems, as well as bootstrap issues.
   * Make sure to access it in a thread-safe manner; it is package visible
   * to avoid overhead in java.lang.
   */
  static volatile SecurityManager current;

  /**
   * Tells whether or not the SecurityManager is currently performing a
   * security check.
   * @deprecated Use {@link #checkPermission(Permission)} instead.
   */
  protected boolean inCheck;

  /**
   * Construct a new security manager. There may be a security check, of
   * <code>RuntimePermission("createSecurityManager")</code>.
   *
   * @throws SecurityException if permission is denied
   */
  public SecurityManager()
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkPermission(new RuntimePermission("createSecurityManager"));
  }

  /**
   * Tells whether or not the SecurityManager is currently performing a
   * security check.
   *
   * @return true if the SecurityManager is in a security check
   * @see #inCheck
   * @deprecated use {@link #checkPermission(Permission)} instead
   */
  public boolean getInCheck()
  {
    return inCheck;
  }

  /**
   * Get a list of all the classes currently executing methods on the Java
   * stack.  getClassContext()[0] is the currently executing method (ie. the
   * class that CALLED getClassContext, not SecurityManager).
   *
   * @return an array of classes on the Java execution stack
   */
  protected Class[] getClassContext()
  {
    Class[] stack1 = VMStackWalker.getClassContext();
    Class[] stack2 = new Class[stack1.length - 1];
    System.arraycopy(stack1, 1, stack2, 0, stack1.length - 1);
    return stack2;
  }

  /**
   * Find the ClassLoader of the first non-system class on the execution
   * stack. A non-system class is one whose ClassLoader is not equal to
   * {@link ClassLoader#getSystemClassLoader()} or its ancestors. This
   * will return null in three cases:
   *
   * <ul>
   * <li>All methods on the stack are from system classes</li>
   * <li>All methods on the stack up to the first "privileged" caller, as
   *  created by {@link AccessController.doPrivileged(PrivilegedAction)},
   *  are from system classes</li>
   * <li>A check of <code>java.security.AllPermission</code> succeeds.</li>
   * </ul>
   * 
   * @return the most recent non-system ClassLoader on the execution stack
   * @deprecated use {@link #checkPermission(Permission)} instead
   */
  protected ClassLoader currentClassLoader()
  {
    Class cl = currentLoadedClass();
    return cl != null ? cl.getClassLoader() : null;
  }

  /**
   * Find the first non-system class on the execution stack. A non-system
   * class is one whose ClassLoader is not equal to
   * {@link ClassLoader#getSystemClassLoader()} or its ancestors. This
   * will return null in three cases:
   *
   * <ul>
   * <li>All methods on the stack are from system classes</li>
   * <li>All methods on the stack up to the first "privileged" caller, as
   *  created by {@link AccessController.doPrivileged(PrivilegedAction)},
   *  are from system classes</li>
   * <li>A check of <code>java.security.AllPermission</code> succeeds.</li>
   * </ul>
   * 
   * @return the most recent non-system Class on the execution stack
   * @deprecated use {@link #checkPermission(Permission)} instead
   */
  protected Class currentLoadedClass()
  {
    int i = classLoaderDepth();
    return i >= 0 ? getClassContext()[i] : null;
  }

  /**
   * Get the depth of a particular class on the execution stack.
   *
   * @param className the fully-qualified name to search for
   * @return the index of the class on the stack, or -1
   * @deprecated use {@link #checkPermission(Permission)} instead
   */
  protected int classDepth(String className)
  {
    Class[] c = getClassContext();
    for (int i = 0; i < c.length; i++)
      if (className.equals(c[i].getName()))
        return i;
    return -1;
  }

  /**
   * Get the depth on the execution stack of the most recent non-system class.
   * A non-system class is one whose ClassLoader is not equal to
   * {@link ClassLoader#getSystemClassLoader()} or its ancestors. This
   * will return -1 in three cases:
   *
   * <ul>
   * <li>All methods on the stack are from system classes</li>
   * <li>All methods on the stack up to the first "privileged" caller, as
   *  created by {@link AccessController.doPrivileged(PrivilegedAction)},
   *  are from system classes</li>
   * <li>A check of <code>java.security.AllPermission</code> succeeds.</li>
   * </ul>
   * 
   * @return the index of the most recent non-system Class on the stack
   * @deprecated use {@link #checkPermission(Permission)} instead
   */
  protected int classLoaderDepth()
  {
    try
      {
        checkPermission(new AllPermission());
      }
    catch (SecurityException e)
      {
        Class[] c = getClassContext();
        for (int i = 0; i < c.length; i++)
          if (c[i].getClassLoader() != null)
            // XXX Check if c[i] is AccessController, or a system class.
            return i;
      }
    return -1;
  }

  /**
   * Tell whether the specified class is on the execution stack.
   *
   * @param className the fully-qualified name of the class to find
   * @return whether the specified class is on the execution stack
   * @deprecated use {@link #checkPermission(Permission)} instead
   */
  protected boolean inClass(String className)
  {
    return classDepth(className) != -1;
  }

  /**
   * Tell whether there is a class loaded with an explicit ClassLoader on
   * the stack.
   *
   * @return whether a class with an explicit ClassLoader is on the stack
   * @deprecated use {@link #checkPermission(Permission)} instead
   */
  protected boolean inClassLoader()
  {
    return classLoaderDepth() != -1;
  }

  /**
   * Get an implementation-dependent Object that contains enough information
   * about the current environment to be able to perform standard security
   * checks later.  This is used by trusted methods that need to verify that
   * their callers have sufficient access to perform certain operations.
   *
   * <p>Currently the only methods that use this are checkRead() and
   * checkConnect(). The default implementation returns an
   * <code>AccessControlContext</code>.
   *
   * @return a security context
   * @see #checkConnect(String, int, Object)
   * @see #checkRead(String, Object)
   * @see AccessControlContext
   * @see AccessController#getContext()
   */
  public Object getSecurityContext()
  {
    return AccessController.getContext();
  }

  /**
   * Check if the current thread is allowed to perform an operation that
   * requires the specified <code>Permission</code>. This defaults to
   * <code>AccessController.checkPermission</code>.
   *
   * @param perm the <code>Permission</code> required
   * @throws SecurityException if permission is denied
   * @throws NullPointerException if perm is null
   * @since 1.2
   */
  public void checkPermission(Permission perm)
  {
    AccessController.checkPermission(perm);
  }

  /**
   * Check if the current thread is allowed to perform an operation that
   * requires the specified <code>Permission</code>. This is done in a
   * context previously returned by <code>getSecurityContext()</code>. The
   * default implementation expects context to be an AccessControlContext,
   * and it calls <code>AccessControlContext.checkPermission(perm)</code>.
   *
   * @param perm the <code>Permission</code> required
   * @param context a security context
   * @throws SecurityException if permission is denied, or if context is
   *         not an AccessControlContext
   * @throws NullPointerException if perm is null
   * @see #getSecurityContext()
   * @see AccessControlContext#checkPermission(Permission)
   * @since 1.2
   */
  public void checkPermission(Permission perm, Object context)
  {
    if (! (context instanceof AccessControlContext))
      throw new SecurityException("Missing context");
    ((AccessControlContext) context).checkPermission(perm);
  }

  /**
   * Check if the current thread is allowed to create a ClassLoader. This
   * method is called from ClassLoader.ClassLoader(), and checks
   * <code>RuntimePermission("createClassLoader")</code>. If you override
   * this, you should call <code>super.checkCreateClassLoader()</code> rather
   * than throwing an exception.
   *
   * @throws SecurityException if permission is denied
   * @see ClassLoader#ClassLoader()
   */
  public void checkCreateClassLoader()
  {
    checkPermission(new RuntimePermission("createClassLoader"));
  }

  /**
   * Check if the current thread is allowed to modify another Thread. This is
   * called by Thread.stop(), suspend(), resume(), interrupt(), destroy(),
   * setPriority(), setName(), and setDaemon(). The default implementation
   * checks <code>RuntimePermission("modifyThread")</code> on system threads
   * (ie. threads in ThreadGroup with a null parent), and returns silently on
   * other threads.
   *
   * <p>If you override this, you must do two things. First, call
   * <code>super.checkAccess(t)</code>, to make sure you are not relaxing
   * requirements. Second, if the calling thread has
   * <code>RuntimePermission("modifyThread")</code>, return silently, so that
   * core classes (the Classpath library!) can modify any thread.
   *
   * @param thread the other Thread to check
   * @throws SecurityException if permission is denied
   * @throws NullPointerException if thread is null
   * @see Thread#stop()
   * @see Thread#suspend()
   * @see Thread#resume()
   * @see Thread#setPriority(int)
   * @see Thread#setName(String)
   * @see Thread#setDaemon(boolean)
   */
  public void checkAccess(Thread thread)
  {
    if (thread.getThreadGroup() != null 
	&& thread.getThreadGroup().getParent() != null)
      checkPermission(new RuntimePermission("modifyThread"));
  }

  /**
   * Check if the current thread is allowed to modify a ThreadGroup. This is
   * called by Thread.Thread() (to add a thread to the ThreadGroup),
   * ThreadGroup.ThreadGroup() (to add this ThreadGroup to a parent),
   * ThreadGroup.stop(), suspend(), resume(), interrupt(), destroy(),
   * setDaemon(), and setMaxPriority(). The default implementation
   * checks <code>RuntimePermission("modifyThread")</code> on the system group
   * (ie. the one with a null parent), and returns silently on other groups.
   *
   * <p>If you override this, you must do two things. First, call
   * <code>super.checkAccess(t)</code>, to make sure you are not relaxing
   * requirements. Second, if the calling thread has
   * <code>RuntimePermission("modifyThreadGroup")</code>, return silently,
   * so that core classes (the Classpath library!) can modify any thread.
   *
   * @param g the ThreadGroup to check
   * @throws SecurityException if permission is denied
   * @throws NullPointerException if g is null
   * @see Thread#Thread()
   * @see ThreadGroup#ThreadGroup()
   * @see ThreadGroup#stop()
   * @see ThreadGroup#suspend()
   * @see ThreadGroup#resume()
   * @see ThreadGroup#interrupt()
   * @see ThreadGroup#setDaemon(boolean)
   * @see ThreadGroup#setMaxPriority(int)
   */
  public void checkAccess(ThreadGroup g)
  {
    if (g.getParent() != null)
      checkPermission(new RuntimePermission("modifyThreadGroup"));
  }

  /**
   * Check if the current thread is allowed to exit the JVM with the given
   * status. This method is called from Runtime.exit() and Runtime.halt().
   * The default implementation checks
   * <code>RuntimePermission("exitVM")</code>. If you override this, call
   * <code>super.checkExit</code> rather than throwing an exception.
   *
   * @param status the status to exit with
   * @throws SecurityException if permission is denied
   * @see Runtime#exit(int)
   * @see Runtime#halt(int)
   */
  public void checkExit(int status)
  {
    checkPermission(new RuntimePermission("exitVM"));
  }

  /**
   * Check if the current thread is allowed to execute the given program. This
   * method is called from Runtime.exec(). If the name is an absolute path,
   * the default implementation checks
   * <code>FilePermission(program, "execute")</code>, otherwise it checks
   * <code>FilePermission("&lt;&lt;ALL FILES&gt;&gt;", "execute")</code>. If
   * you override this, call <code>super.checkExec</code> rather than
   * throwing an exception.
   *
   * @param program the name of the program to exec
   * @throws SecurityException if permission is denied
   * @throws NullPointerException if program is null
   * @see Runtime#exec(String[], String[], File)
   */
  public void checkExec(String program)
  {
    if (! program.equals(new File(program).getAbsolutePath()))
      program = "<<ALL FILES>>";
    checkPermission(new FilePermission(program, "execute"));
  }

  /**
   * Check if the current thread is allowed to link in the given native
   * library. This method is called from Runtime.load() (and hence, by
   * loadLibrary() as well). The default implementation checks
   * <code>RuntimePermission("loadLibrary." + filename)</code>. If you
   * override this, call <code>super.checkLink</code> rather than throwing
   * an exception.
   *
   * @param filename the full name of the library to load
   * @throws SecurityException if permission is denied
   * @throws NullPointerException if filename is null
   * @see Runtime#load(String)
   */
  public void checkLink(String filename)
  {
    // Use the toString() hack to do the null check.
    checkPermission(new RuntimePermission("loadLibrary."
                                          + filename.toString()));
  }

  /**
   * Check if the current thread is allowed to read the given file using the
   * FileDescriptor. This method is called from
   * FileInputStream.FileInputStream(). The default implementation checks
   * <code>RuntimePermission("readFileDescriptor")</code>. If you override
   * this, call <code>super.checkRead</code> rather than throwing an
   * exception.
   *
   * @param desc the FileDescriptor representing the file to access
   * @throws SecurityException if permission is denied
   * @throws NullPointerException if desc is null
   * @see FileInputStream#FileInputStream(FileDescriptor)
   */
  public void checkRead(FileDescriptor desc)
  {
    if (desc == null)
      throw new NullPointerException();
    checkPermission(new RuntimePermission("readFileDescriptor"));
  }

  /**
   * Check if the current thread is allowed to read the given file. This
   * method is called from FileInputStream.FileInputStream(),
   * RandomAccessFile.RandomAccessFile(), File.exists(), canRead(), isFile(),
   * isDirectory(), lastModified(), length() and list(). The default
   * implementation checks <code>FilePermission(filename, "read")</code>. If
   * you override this, call <code>super.checkRead</code> rather than
   * throwing an exception.
   *
   * @param filename the full name of the file to access
   * @throws SecurityException if permission is denied
   * @throws NullPointerException if filename is null
   * @see File
   * @see FileInputStream#FileInputStream(String)
   * @see RandomAccessFile#RandomAccessFile(String)
   */
  public void checkRead(String filename)
  {
    checkPermission(new FilePermission(filename, "read"));
  }

  /**
   * Check if the current thread is allowed to read the given file. using the
   * given security context. The context must be a result of a previous call
   * to <code>getSecurityContext()</code>. The default implementation checks
   * <code>AccessControlContext.checkPermission(new FilePermission(filename,
   * "read"))</code>. If you override this, call <code>super.checkRead</code>
   * rather than throwing an exception.
   *
   * @param filename the full name of the file to access
   * @param context the context to determine access for
   * @throws SecurityException if permission is denied, or if context is
   *         not an AccessControlContext
   * @throws NullPointerException if filename is null
   * @see #getSecurityContext()
   * @see AccessControlContext#checkPermission(Permission)
   */
  public void checkRead(String filename, Object context)
  {
    if (! (context instanceof AccessControlContext))
      throw new SecurityException("Missing context");
    AccessControlContext ac = (AccessControlContext) context;
    ac.checkPermission(new FilePermission(filename, "read"));
  }

  /**
   * Check if the current thread is allowed to write the given file using the
   * FileDescriptor. This method is called from
   * FileOutputStream.FileOutputStream(). The default implementation checks
   * <code>RuntimePermission("writeFileDescriptor")</code>. If you override
   * this, call <code>super.checkWrite</code> rather than throwing an
   * exception.
   *
   * @param desc the FileDescriptor representing the file to access
   * @throws SecurityException if permission is denied
   * @throws NullPointerException if desc is null
   * @see FileOutputStream#FileOutputStream(FileDescriptor)
   */
  public void checkWrite(FileDescriptor desc)
  {
    if (desc == null)
      throw new NullPointerException();
    checkPermission(new RuntimePermission("writeFileDescriptor"));
  }

  /**
   * Check if the current thread is allowed to write the given file. This
   * method is called from FileOutputStream.FileOutputStream(),
   * RandomAccessFile.RandomAccessFile(), File.canWrite(), mkdir(), and
   * renameTo(). The default implementation checks
   * <code>FilePermission(filename, "write")</code>. If you override this,
   * call <code>super.checkWrite</code> rather than throwing an exception.
   *
   * @param filename the full name of the file to access
   * @throws SecurityException if permission is denied
   * @throws NullPointerException if filename is null
   * @see File
   * @see File#canWrite()
   * @see File#mkdir()
   * @see File#renameTo()
   * @see FileOutputStream#FileOutputStream(String)
   * @see RandomAccessFile#RandomAccessFile(String)
   */
  public void checkWrite(String filename)
  {
    checkPermission(new FilePermission(filename, "write"));
  }

  /**
   * Check if the current thread is allowed to delete the given file. This
   * method is called from File.delete(). The default implementation checks
   * <code>FilePermission(filename, "delete")</code>. If you override this,
   * call <code>super.checkDelete</code> rather than throwing an exception.
   *
   * @param filename the full name of the file to delete
   * @throws SecurityException if permission is denied
   * @throws NullPointerException if filename is null
   * @see File#delete()
   */
  public void checkDelete(String filename)
  {
    checkPermission(new FilePermission(filename, "delete"));
  }

  /**
   * Check if the current thread is allowed to connect to a given host on a
   * given port. This method is called from Socket.Socket(). A port number
   * of -1 indicates the caller is attempting to determine an IP address, so
   * the default implementation checks
   * <code>SocketPermission(host, "resolve")</code>. Otherwise, the default
   * implementation checks
   * <code>SocketPermission(host + ":" + port, "connect")</code>. If you
   * override this, call <code>super.checkConnect</code> rather than throwing
   * an exception.
   *
   * @param host the host to connect to
   * @param port the port to connect on
   * @throws SecurityException if permission is denied
   * @throws NullPointerException if host is null
   * @see Socket#Socket()
   */
  public void checkConnect(String host, int port)
  {
    if (port == -1)
      checkPermission(new SocketPermission(host, "resolve"));
    else
      // Use the toString() hack to do the null check.
      checkPermission(new SocketPermission(host.toString() + ":" + port,
                                           "connect"));
  }

  /**
   * Check if the current thread is allowed to connect to a given host on a
   * given port, using the given security context. The context must be a
   * result of a previous call to <code>getSecurityContext</code>. A port
   * number of -1 indicates the caller is attempting to determine an IP
   * address, so the default implementation checks
   * <code>AccessControlContext.checkPermission(new SocketPermission(host,
   * "resolve"))</code>. Otherwise, the default implementation checks
   * <code>AccessControlContext.checkPermission(new SocketPermission(host
   * + ":" + port, "connect"))</code>. If you override this, call
   * <code>super.checkConnect</code> rather than throwing an exception.
   *
   * @param host the host to connect to
   * @param port the port to connect on
   * @param context the context to determine access for
   *
   * @throws SecurityException if permission is denied, or if context is
   *         not an AccessControlContext
   * @throws NullPointerException if host is null
   *
   * @see #getSecurityContext()
   * @see AccessControlContext#checkPermission(Permission)
   */
  public void checkConnect(String host, int port, Object context)
  {
    if (! (context instanceof AccessControlContext))
      throw new SecurityException("Missing context");
    AccessControlContext ac = (AccessControlContext) context;
    if (port == -1)
      ac.checkPermission(new SocketPermission(host, "resolve"));
    else
      // Use the toString() hack to do the null check.
      ac.checkPermission(new SocketPermission(host.toString() + ":" + port,
                                              "connect"));
  }

  /**
   * Check if the current thread is allowed to listen to a specific port for
   * data. This method is called by ServerSocket.ServerSocket(). The default
   * implementation checks
   * <code>SocketPermission("localhost:" + (port == 0 ? "1024-" : "" + port),
   * "listen")</code>. If you override this, call
   * <code>super.checkListen</code> rather than throwing an exception.
   *
   * @param port the port to listen on
   * @throws SecurityException if permission is denied
   * @see ServerSocket#ServerSocket(int)
   */
  public void checkListen(int port)
  {
    checkPermission(new SocketPermission("localhost:"
                                         + (port == 0 ? "1024-" : "" +port),
                                         "listen"));
  }

  /**
   * Check if the current thread is allowed to accept a connection from a
   * particular host on a particular port. This method is called by
   * ServerSocket.implAccept(). The default implementation checks
   * <code>SocketPermission(host + ":" + port, "accept")</code>. If you
   * override this, call <code>super.checkAccept</code> rather than throwing
   * an exception.
   *
   * @param host the host which wishes to connect
   * @param port the port the connection will be on
   * @throws SecurityException if permission is denied
   * @throws NullPointerException if host is null
   * @see ServerSocket#accept()
   */
  public void checkAccept(String host, int port)
  {
    // Use the toString() hack to do the null check.
    checkPermission(new SocketPermission(host.toString() + ":" + port,
                                         "accept"));
  }

  /**
   * Check if the current thread is allowed to read and write multicast to
   * a particular address. The default implementation checks
   * <code>SocketPermission(addr.getHostAddress(), "accept,connect")</code>.
   * If you override this, call <code>super.checkMulticast</code> rather than
   * throwing an exception.
   *
   * @param addr the address to multicast to
   * @throws SecurityException if permission is denied
   * @throws NullPointerException if host is null
   * @since 1.1
   */
  public void checkMulticast(InetAddress addr)
  {
    checkPermission(new SocketPermission(addr.getHostAddress(),
                                         "accept,connect"));
  }

  /**
   *Check if the current thread is allowed to read and write multicast to
   * a particular address with a particular ttl (time-to-live) value. The
   * default implementation ignores ttl, and checks
   * <code>SocketPermission(addr.getHostAddress(), "accept,connect")</code>.
   * If you override this, call <code>super.checkMulticast</code> rather than
   * throwing an exception.
   *
   * @param addr the address to multicast to
   * @param ttl value in use for multicast send
   * @throws SecurityException if permission is denied
   * @throws NullPointerException if host is null
   * @since 1.1
   * @deprecated use {@link #checkPermission(Permission)} instead
   */
  public void checkMulticast(InetAddress addr, byte ttl)
  {
    checkPermission(new SocketPermission(addr.getHostAddress(),
                                         "accept,connect"));
  }

  /**
   * Check if the current thread is allowed to read or write all the system
   * properties at once. This method is called by System.getProperties()
   * and setProperties(). The default implementation checks
   * <code>PropertyPermission("*", "read,write")</code>. If you override
   * this, call <code>super.checkPropertiesAccess</code> rather than
   * throwing an exception.
   *
   * @throws SecurityException if permission is denied
   * @see System#getProperties()
   * @see System#setProperties(Properties)
   */
  public void checkPropertiesAccess()
  {
    checkPermission(new PropertyPermission("*", "read,write"));
  }

  /**
   * Check if the current thread is allowed to read a particular system
   * property (writes are checked directly via checkPermission). This method
   * is called by System.getProperty() and setProperty(). The default
   * implementation checks <code>PropertyPermission(key, "read")</code>. If
   * you override this, call <code>super.checkPropertyAccess</code> rather
   * than throwing an exception.
   *
   * @param key the key of the property to check
   *
   * @throws SecurityException if permission is denied
   * @throws NullPointerException if key is null
   * @throws IllegalArgumentException if key is ""
   *
   * @see System#getProperty(String)
   */
  public void checkPropertyAccess(String key)
  {
    checkPermission(new PropertyPermission(key, "read"));
  }

  /**
   * Check if the current thread is allowed to create a top-level window. If
   * it is not, the operation should still go through, but some sort of
   * nonremovable warning should be placed on the window to show that it
   * is untrusted. This method is called by Window.Window(). The default
   * implementation checks
   * <code>AWTPermission("showWindowWithoutWarningBanner")</code>, and returns
   * true if no exception was thrown. If you override this, use
   * <code>return super.checkTopLevelWindow</code> rather than returning
   * false.
   *
   * @param window the window to create
   * @return true if there is permission to show the window without warning
   * @throws NullPointerException if window is null
   * @see Window#Window(Frame)
   */
  public boolean checkTopLevelWindow(Object window)
  {
    if (window == null)
      throw new NullPointerException();
    try
      {
        checkPermission(new AWTPermission("showWindowWithoutWarningBanner"));
        return true;
      }
    catch (SecurityException e)
      {
        return false;
      }
  }

  /**
   * Check if the current thread is allowed to create a print job. This
   * method is called by Toolkit.getPrintJob(). The default implementation
   * checks <code>RuntimePermission("queuePrintJob")</code>. If you override
   * this, call <code>super.checkPrintJobAccess</code> rather than throwing
   * an exception.
   *
   * @throws SecurityException if permission is denied
   * @see Toolkit#getPrintJob(Frame, String, Properties)
   * @since 1.1
   */
  public void checkPrintJobAccess()
  {
    checkPermission(new RuntimePermission("queuePrintJob"));
  }

  /**
   * Check if the current thread is allowed to use the system clipboard. This
   * method is called by Toolkit.getSystemClipboard(). The default
   * implementation checks <code>AWTPermission("accessClipboard")</code>. If
   * you override this, call <code>super.checkSystemClipboardAccess</code>
   * rather than throwing an exception.
   *
   * @throws SecurityException if permission is denied
   * @see Toolkit#getSystemClipboard()
   * @since 1.1
   */
  public void checkSystemClipboardAccess()
  {
    checkPermission(new AWTPermission("accessClipboard"));
  }

  /**
   * Check if the current thread is allowed to use the AWT event queue. This
   * method is called by Toolkit.getSystemEventQueue(). The default
   * implementation checks <code>AWTPermission("accessEventQueue")</code>.
   * you override this, call <code>super.checkAwtEventQueueAccess</code>
   * rather than throwing an exception.
   *
   * @throws SecurityException if permission is denied
   * @see Toolkit#getSystemEventQueue()
   * @since 1.1
   */
  public void checkAwtEventQueueAccess()
  {
    checkPermission(new AWTPermission("accessEventQueue"));
  }

  /**
   * Check if the current thread is allowed to access the specified package
   * at all. This method is called by ClassLoader.loadClass() in user-created
   * ClassLoaders. The default implementation gets a list of all restricted
   * packages, via <code>Security.getProperty("package.access")</code>. Then,
   * if packageName starts with or equals any restricted package, it checks
   * <code>RuntimePermission("accessClassInPackage." + packageName)</code>.
   * If you override this, you should call
   * <code>super.checkPackageAccess</code> before doing anything else.
   *
   * @param packageName the package name to check access to
   * @throws SecurityException if permission is denied
   * @throws NullPointerException if packageName is null
   * @see ClassLoader#loadClass(String, boolean)
   * @see Security#getProperty(String)
   */
  public void checkPackageAccess(String packageName)
  {
    checkPackageList(packageName, "package.access", "accessClassInPackage.");
  }

  /**
   * Check if the current thread is allowed to define a class into the
   * specified package. This method is called by ClassLoader.loadClass() in
   * user-created ClassLoaders. The default implementation gets a list of all
   * restricted packages, via
   * <code>Security.getProperty("package.definition")</code>. Then, if
   * packageName starts with or equals any restricted package, it checks
   * <code>RuntimePermission("defineClassInPackage." + packageName)</code>.
   * If you override this, you should call
   * <code>super.checkPackageDefinition</code> before doing anything else.
   *
   * @param packageName the package name to check access to
   * @throws SecurityException if permission is denied
   * @throws NullPointerException if packageName is null
   * @see ClassLoader#loadClass(String, boolean)
   * @see Security#getProperty(String)
   */
  public void checkPackageDefinition(String packageName)
  {
    checkPackageList(packageName, "package.definition", "defineClassInPackage.");
  }

  /**
   * Check if the current thread is allowed to set the current socket factory.
   * This method is called by Socket.setSocketImplFactory(),
   * ServerSocket.setSocketFactory(), and URL.setURLStreamHandlerFactory().
   * The default implementation checks
   * <code>RuntimePermission("setFactory")</code>. If you override this, call
   * <code>super.checkSetFactory</code> rather than throwing an exception.
   *
   * @throws SecurityException if permission is denied
   * @see Socket#setSocketImplFactory(SocketImplFactory)
   * @see ServerSocket#setSocketFactory(SocketImplFactory)
   * @see URL#setURLStreamHandlerFactory(URLStreamHandlerFactory)
   */
  public void checkSetFactory()
  {
    checkPermission(new RuntimePermission("setFactory"));
  }

  /**
   * Check if the current thread is allowed to get certain types of Methods,
   * Fields and Constructors from a Class object. This method is called by
   * Class.getMethod[s](), Class.getField[s](), Class.getConstructor[s],
   * Class.getDeclaredMethod[s](), Class.getDeclaredField[s](), and
   * Class.getDeclaredConstructor[s](). The default implementation allows
   * PUBLIC access, and access to classes defined by the same classloader as
   * the code performing the reflection. Otherwise, it checks
   * <code>RuntimePermission("accessDeclaredMembers")</code>. If you override
   * this, do not call <code>super.checkMemberAccess</code>, as this would
   * mess up the stack depth check that determines the ClassLoader requesting
   * the access.
   *
   * @param c the Class to check
   * @param memberType either DECLARED or PUBLIC
   * @throws SecurityException if permission is denied, including when
   *         memberType is not DECLARED or PUBLIC
   * @throws NullPointerException if c is null
   * @see Class
   * @see Member#DECLARED
   * @see Member#PUBLIC
   * @since 1.1
   */
  public void checkMemberAccess(Class c, int memberType)
  {
    if (c == null)
      throw new NullPointerException();
    if (memberType == Member.PUBLIC)
      return;
    // XXX Allow access to classes created by same classloader before next
    // check.
    checkPermission(new RuntimePermission("accessDeclaredMembers"));
  }

  /**
   * Test whether a particular security action may be taken. The default
   * implementation checks <code>SecurityPermission(action)</code>. If you
   * override this, call <code>super.checkSecurityAccess</code> rather than
   * throwing an exception.
   *
   * @param action the desired action to take
   * @throws SecurityException if permission is denied
   * @throws NullPointerException if action is null
   * @throws IllegalArgumentException if action is ""
   * @since 1.1
   */
  public void checkSecurityAccess(String action)
  {
    checkPermission(new SecurityPermission(action));
  }

  /**
   * Get the ThreadGroup that a new Thread should belong to by default. Called
   * by Thread.Thread(). The default implementation returns the current
   * ThreadGroup of the current Thread. <STRONG>Spec Note:</STRONG> it is not
   * clear whether the new Thread is guaranteed to pass the
   * checkAccessThreadGroup() test when using this ThreadGroup, but I presume
   * so.
   *
   * @return the ThreadGroup to put the new Thread into
   * @since 1.1
   */
  public ThreadGroup getThreadGroup()
  {
    return Thread.currentThread().getThreadGroup();
  }

  /**
   * Helper that checks a comma-separated list of restricted packages, from
   * <code>Security.getProperty("package.definition")</code>, for the given
   * package access permission. If packageName starts with or equals any
   * restricted package, it checks
   * <code>RuntimePermission(permission + packageName)</code>.
   *
   * @param packageName the package name to check access to
   * @param restriction "package.access" or "package.definition"
   * @param permission the base permission, including the '.'
   * @throws SecurityException if permission is denied
   * @throws NullPointerException if packageName is null
   * @see #checkPackageAccess(String)
   * @see #checkPackageDefinition(String)
   */
  void checkPackageList(String packageName, final String restriction,
                        String permission)
  {
    if (packageName == null)
      throw new NullPointerException();

    String list = (String)AccessController.doPrivileged(new PrivilegedAction()
      {
	public Object run()
        {
	  return Security.getProperty(restriction);
	}
      });

    if (list == null || list.equals(""))
      return;

    String packageNamePlusDot = packageName + ".";

    StringTokenizer st = new StringTokenizer(list, ",");
    while (st.hasMoreTokens())
      {
	if (packageNamePlusDot.startsWith(st.nextToken()))
	  {
	    Permission p = new RuntimePermission(permission + packageName);
	    checkPermission(p);
	    return;
	  }
      }
  }
}
