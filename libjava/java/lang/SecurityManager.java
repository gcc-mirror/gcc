/* java.lang.SecurityManager
   Copyright (C) 1998, 1999, 2001 Free Software Foundation, Inc.

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

import java.net.*;
import java.util.*;
import java.io.*;

/**
 ** SecurityManager is a class you can extend to create
 ** your own Java security policy.  By default, there is
 ** no SecurityManager installed in 1.1, which means that
 ** all things are permitted to all people.<P>
 **
 ** The default methods in this class deny all
 ** things to all people.
 **
 ** @author  John Keiser
 ** @version 1.1.0, 31 May 1998
 ** @since JDK1.0
 **/
public class SecurityManager {
	/** Tells whether or not the SecurityManager is currently
	 ** performing a security check.
	 **/
	protected boolean inCheck;

	/** Tells whether or not the SecurityManager is currently
	 ** performing a security check.
	 **
	 ** @return whether or not the SecurityManager is
	 **         currently performing a security check.
	 **/
	public boolean getInCheck() {
		return inCheck;
	}

	/** Get a list of all the classes currently executing
	 ** methods on the Java stack.  getClassContext()[0] is
	 ** the currently executing method
	 ** <STRONG>Spec Note:</STRONG> does not say whether
	 ** the stack will include the getClassContext() call or
	 ** the one just before it.
	 **
	 ** @return an array containing all the methods on classes
	 **         on the Java execution stack.
	 **/
	protected Class[] getClassContext() {
		return VMSecurityManager.getClassContext();
	}

	/** Find the ClassLoader for the most recent class on the
	 ** stack that was loaded by an explicit ClassLoader.  If
	 ** everything on the stack was loaded by the system
	 ** classloader, null is returned.
	 **
	 ** @return the most recent ClassLoader on the execution
	 **         stack.
	 **/
	protected ClassLoader currentClassLoader() {
		return VMSecurityManager.currentClassLoader();
	}

	/** Find the most recent class on the stack that was
	 ** loaded by an explicit ClassLoader.  If everything on
	 ** the stack was loaded by the system classloader, null
	 ** is returned.
	 **
	 ** @return the most recent loaded Class on the execution
	 **         stack.
	 **/
	protected Class currentLoadedClass() {
		Class[] c = getClassContext();
		for(int i=0;i<c.length;i++) {
			if(c[i].getClassLoader() != null) {
				return c[i];
			}
		}
		return null;
	}

	/** Get the depth on the execution stack of the most
	 ** recent class that was loaded by an explicit
	 ** ClassLoader.  This can be used as an index into
	 ** getClassContext().
	 **
	 ** @return the index of the most recent loaded Class on
	 **         the execution stack.
	 **/
	protected int classLoaderDepth() {
		Class[] c = getClassContext();
		for(int i=0;i<c.length;i++) {
			if(c[i].getClassLoader() != null) {
				return i;
			}
		}
		return -1;
	}

	/** Tell whether there is a class loaded with an explicit
	 ** ClassLoader on the stack.
	 **
	 ** @return whether there is a class loaded with an
	 **         explicit ClassLoader on the stack.
	 **/
	protected boolean inClassLoader() {
		return classLoaderDepth() != -1;
	}


	/** Get the depth of a particular class on the execution
	 ** stack.
	 **
	 ** @param className the fully-qualified name of the class
	 **        to search for on the stack.
	 ** @return the index of the class on the stack, or -1 if
	 **         the class is not on the stack.
	 **/
	protected int classDepth(String className) {
		Class[] c = getClassContext();
		for(int i=0;i<c.length;i++) {
			if(className.equals(c[i].getName())) {
				return i;
			}
		}
		return -1;
	}

	/** Tell whether the specified class is on the execution
	 ** stack.
	 **
	 ** @param className the fully-qualified name of the class
	 **        to search for on the stack.
	 ** @return whether the specified class is on the
	 **         execution stack.
	 **/
	protected boolean inClass(String className) {
		return classDepth(className) != -1;
	}

	/** Get an implementation-dependent Object that contains
	 ** enough information about the current environment to be
	 ** able to perform standard security checks later.  This
	 ** is used by trusted methods that need to verify that
	 ** their callers have sufficient access to perform
	 ** certain operations.<P>
	 **
	 ** Currently the only methods that use this are checkRead()
	 ** and checkConnect().
	 **
	 ** @see checkConnect(java.lang.String,int,java.lang.Object)
	 ** @see checkRead(java.lang.String,java.lang.Object)
	 **/
	public Object getSecurityContext() {
		return new SecurityContext(getClassContext());
	}

	/** Check if the current thread is allowed to create a
	 ** ClassLoader.<P>
	 **
	 ** This method is called from ClassLoader.ClassLoader(),
	 ** in other words, whenever a ClassLoader is created.<P>
	 **
	 ** SecurityManager's implementation always denies access.
	 **
	 ** @exception SecurityException if the operation is not
	 **            permitted.
	 ** @see java.lang.ClassLoader#ClassLoader()
	 **/
	public void checkCreateClassLoader() {
		throw new SecurityException("Cannot create new ClassLoaders.");
	}

	/** Check if the current thread is allowed to modify this
	 ** other Thread.<P>
	 **
	 ** Called by Thread.stop(), suspend(), resume(), and
	 ** interrupt(), destroy(), setPriority(), setName() and
	 ** setDaemon().<P>
	 **
	 ** SecurityManager's implementation always denies access.
	 **
	 ** @param g the Thread to check against
	 ** @exception SecurityException if the operation is not
	 **            permitted.
	 ** @see java.lang.Thread#stop()
	 ** @see java.lang.Thread#suspend()
	 ** @see java.lang.Thread#resume()
	 ** @see java.lang.Thread#interrupt()
	 ** @see java.lang.Thread#destroy()
	 ** @see java.lang.Thread#setPriority(int)
	 ** @see java.lang.Thread#setName(java.lang.String)
	 ** @see java.lang.Thread#setDaemon(boolean)
	 **/
	public void checkAccess(Thread t) {
		throw new SecurityException("Cannot modify Threads.");
	}

	/** Check if the current thread is allowed to modify this
	 ** ThreadGroup.<P>
	 **
	 ** Called by Thread.Thread() (to add a thread to the
	 ** ThreadGroup), ThreadGroup.ThreadGroup() (to add this
	 ** ThreadGroup to a parent), ThreadGroup.stop(),
	 ** suspend(), resume(), interrupt(), destroy(),
	 ** setDaemon(), and setMaxPriority().<P>
	 **
	 ** SecurityManager's implementation always denies access.
	 **
	 ** @param g the ThreadGroup to check against
	 ** @exception SecurityException if the operation is not
	 **            permitted.
	 ** @see java.lang.Thread#Thread()
	 ** @see java.lang.ThreadGroup#ThreadGroup()
	 ** @see java.lang.ThreadGroup#stop()
	 ** @see java.lang.ThreadGroup#suspend()
	 ** @see java.lang.ThreadGroup#resume()
	 ** @see java.lang.ThreadGroup#interrupt()
	 ** @see java.lang.ThreadGroup#setDaemon(boolean)
	 ** @see java.lang.ThreadGroup#setMaxPriority(int)
	 **/
	public void checkAccess(ThreadGroup g) {
		throw new SecurityException("Cannot modify ThreadGroups.");
	}

	/** Check if the current thread is allowed to exit the
	 ** JVM with the given status.<P>
	 **
	 ** This method is called from Runtime.exit().<P>
	 **
	 ** SecurityManager's implementation always denies access.
	 **
	 ** @param status the status to exit with
	 ** @exception SecurityException if the operation is not
	 **            permitted.
	 ** @see java.lang.Runtime#exit()
	 ** @see java.lang.Runtime#exit(int)
	 **/
	public void checkExit(int status) {
		throw new SecurityException("Cannot exit JVM.");
	}

	/** Check if the current thread is allowed to execute the
	 ** given program.<P>
	 **
	 ** This method is called from Runtime.exec().<P>
	 **
	 ** SecurityManager's implementation always denies access.
	 **
	 ** @param program the name of the program to exec
	 ** @exception SecurityException if the operation is not
	 **            permitted.
	 ** @see java.lang.Runtime#exec(java.lang.String[],java.lang.String[])
	 **/
	public void checkExec(String program) {
		throw new SecurityException("Cannot execute programs.");
	}

	/** Check if the current thread is allowed to link in the
	 ** given native library.<P>
	 **
	 ** This method is called from Runtime.load() (and hence,
	 ** by loadLibrary() as well).<P>
	 **
	 ** SecurityManager's implementation always denies access.
	 **
	 ** @param filename the full name of the library to load
	 ** @exception SecurityException if the operation is not
	 **            permitted.
	 ** @see java.lang.Runtime#load(java.lang.String)
	 **/
	public void checkLink(String filename) {
		throw new SecurityException("Cannot link native libraries.");
	}

	/** Check if the current thread is allowed to read the
	 ** given file using the FileDescriptor.<P>
	 **
	 ** This method is called from
	 ** FileInputStream.FileInputStream().<P>
	 **
	 ** SecurityManager's implementation always denies access.
	 **
	 ** @param desc the FileDescriptor representing the file
	 **        to access
	 ** @exception SecurityException if the operation is not
	 **            permitted.
	 ** @see java.io.FileInputStream#FileInputStream(java.io.FileDescriptor)
	 **/
	public void checkRead(FileDescriptor desc) {
		throw new SecurityException("Cannot read files via file descriptors.");
	}

	/** Check if the current thread is allowed to read the
	 ** given file.<P>
	 **
	 ** This method is called from
	 ** FileInputStream.FileInputStream(),
	 ** RandomAccessFile.RandomAccessFile(), File.exists(),
	 ** canRead(), isFile(), isDirectory(), lastModified(),
	 ** length() and list().<P>
	 **
	 ** SecurityManager's implementation always denies access.
	 **
	 ** @param filename the full name of the file to access
	 ** @exception SecurityException if the operation is not
	 **            permitted.
	 ** @see java.io.File
	 ** @see java.io.FileInputStream#FileInputStream(java.lang.String)
	 ** @see java.io.RandomAccessFile#RandomAccessFile(java.lang.String)
	 **/
	public void checkRead(String filename) {
		throw new SecurityException("Cannot read files via file names.");
	}

	/** Check if the current thread is allowed to read the
	 ** given file. using the given SecurityContext.<P>
	 **
	 ** I know of no core class that calls this method.<P>
	 **
	 ** SecurityManager's implementation always denies access.
	 **
	 ** @param filename the full name of the file to access
	 ** @param securityContext the Security Context to
	 **        determine access for.
	 ** @exception SecurityException if the operation is not
	 **            permitted.
	 **/
	public void checkRead(String filename, Object securityContext) {
		throw new SecurityException("Cannot read files via file names.");
	}

	/** Check if the current thread is allowed to write to the
	 ** given file using the FileDescriptor.<P>
	 **
	 ** This method is called from
	 ** FileOutputStream.FileOutputStream().<P>
	 **
	 ** SecurityManager's implementation always denies access.
	 **
	 ** @param desc the FileDescriptor representing the file
	 **        to access
	 ** @exception SecurityException if the operation is not
	 **            permitted.
	 ** @see java.io.FileOutputStream#FileOutputStream(java.io.FileDescriptor)
	 **/
	public void checkWrite(FileDescriptor desc) {
		throw new SecurityException("Cannot write files via file descriptors.");
	}

	/** Check if the current thread is allowed to write to the
	 ** given file.<P>
	 **
	 ** This method is called from
	 ** FileOutputStream.FileOutputStream(),
	 ** RandomAccessFile.RandomAccessFile(),
	 ** File.canWrite(), mkdir(), and renameTo().<P>
	 **
	 ** SecurityManager's implementation always denies access.
	 **
	 ** @param filename the full name of the file to access
	 ** @exception SecurityException if the operation is not
	 **            permitted.
	 ** @see java.io.File#canWrite()
	 ** @see java.io.File#mkdir()
	 ** @see java.io.File#renameTo()
	 ** @see java.io.FileOutputStream#FileOutputStream(java.lang.String)
	 ** @see java.io.RandomAccessFile#RandomAccessFile(java.lang.String)
	 **/
	public void checkWrite(String filename) {
		throw new SecurityException("Cannot write files via file names.");
	}

	/** Check if the current thread is allowed to delete the
	 ** given file.<P>
	 **
	 ** This method is called from File.delete().<P>
	 **
	 ** SecurityManager's implementation always denies access.
	 **
	 ** @param filename the full name of the file to delete
	 ** @exception SecurityException if th operation is not
	 **            permitted.
	 ** @see java.io.File#delete()
	 **/
	public void checkDelete(String filename) {
		throw new SecurityException("Cannot delete files.");
	}

	/** Check if the current thread is allowed to connect to a
	 ** given host on a given port.<P>
	 **
	 ** This method is called from Socket.Socket().
	 **
	 ** SecurityManager's implementation always denies access.
	 **
	 ** @param host the host to connect to
	 ** @param port the port to connect on
	 ** @exception SecurityException if the operation is not
	 **            permitted
	 ** @see java.net.Socket#Socket()
	 **/
	public void checkConnect(String host, int port) {
		throw new SecurityException("Cannot make network connections.");
	}

	/** Check if the current thread is allowed to connect to a
	 ** given host on a given port using a specific security
	 ** context to determine access.<P>
	 **
	 ** This method is not called in the 1.1 core classes.<P>
	 **
	 ** SecurityManager's implementation always denies access.
	 **
	 ** @param host the host to connect to
	 ** @param port the port to connect on
	 ** @param securityContext the security context to
	 **        determine access with
	 ** @exception SecurityException if the operation is not
	 **            permitted
	 **/
	public void checkConnect(String host, int port, Object securityContext) {
		throw new SecurityException("Cannot make network connections.");
	}

	/** Check if the current thread is allowed to listen to a
	 ** specific port for data.<P>
	 **
	 ** This method is called by ServerSocket.ServerSocket().<P>
	 **
	 ** SecurityManager's implementation always denies access.
	 **
	 ** @param port the port to listen on
	 ** @exception SecurityException if the operation is not
	 **            permitted
	 ** @see java.net.ServerSocket#ServerSocket(int)
	 **/
	public void checkListen(int port) {
		throw new SecurityException("Cannot listen for connections.");
	}

	/** Check if the current thread is allowed to accept a
	 ** connection from a particular host on a particular
	 ** port.<P>
	 **
	 ** This method is called by ServerSocket.implAccept().<P>
	 **
	 ** SecurityManager's implementation always denies access.
	 **
	 ** @param host the host which wishes to connect
	 ** @param port the port the connection will be on
	 ** @exception SecurityException if the operation is not
	 **            permitted
	 ** @see java.net.ServerSocket#accept()
	 **/
	public void checkAccept(String host, int port) {
		throw new SecurityException("Cannot accept connections.");
	}

	/** Check if the current thread is allowed to read and
	 ** write multicast to a particular address.<P>
	 **
	 ** SecurityManager's implementation always denies access.
	 **
	 ** @XXX where is it called?
	 **
	 ** @param addr the address to multicast to.
	 ** @exception SecurityException if the operation is not
	 **            permitted.
	 **/
	public void checkMulticast(InetAddress addr) {
		throw new SecurityException("Cannot read or write multicast.");
	}

	/** Check if the current thread is allowed to read and
	 ** write multicast to a particular address with a
	 ** particular ttl value.<P>
	 **
	 ** SecurityManager's implementation always denies access.<P>
	 **
	 ** @XXX where is it called?
	 **
	 ** @XXX what the hell is ttl?  Expand abbreviation.
	 **
	 ** @param addr the address to multicast to.
	 ** @param ttl the ttl value to use
	 ** @exception SecurityException if the operation is not
	 **            permitted.
	 **/
	public void checkMulticast(InetAddress addr, byte ttl) {
		throw new SecurityException("Cannot read or write multicast.");
	}

        /**
         ** Check if the current thread is allowed to perform an
         ** operation that requires the specified <code>Permission</code>.
         **
         ** @param perm The <code>Permission</code> required.
         ** @exception SecurityException If the operation is not allowed.
         **/
         public void checkPermission(java.security.Permission perm) {
		throw new SecurityException("Operation not allowed");
	}

        /**
         ** Check if the current thread is allowed to perform an
         ** operation that requires the specified <code>Permission</code>.
         **
         ** @param perm The <code>Permission</code> required.
	 ** @param context A security context
         ** @exception SecurityException If the operation is not allowed.
	 ** @since 1.2
         **/
         public void checkPermission(java.security.Permission perm,
				     Object context) {
		throw new SecurityException("Operation not allowed");
	}

	/** Check if the current thread is allowed to read or
	 ** write all the system properties at once.<P>
	 **
	 ** This method is called by System.getProperties()
	 ** and setProperties().<P>
	 **
	 ** SecurityManager's implementation always denies access.
	 **
	 ** @exception SecurityException if the operation is not
	 **            permitted.
	 ** @see java.lang.System#getProperties()
	 ** @see java.lang.System#setProperties(java.util.Properties)
	 **/
	public void checkPropertiesAccess() {
		throw new SecurityException("Cannot access all system properties at once.");
	}

	/** Check if the current thread is allowed to read or
	 ** write a particular system property.<P>
	 **
	 ** This method is called by System.getProperty() and
	 ** setProperty().<P>
	 **
	 ** SecurityManager's implementation always denies access.
	 **
	 ** @exception SecurityException is the operation is not
	 **            permitted.
	 ** @see java.lang.System#getProperty(java.lang.String)
	 ** @see java.lang.System#setProperty(java.lang.String,java.lang.String)
	 **/
	public void checkPropertyAccess(String name) {
		throw new SecurityException("Cannot access individual system properties.");
	}

	/** Check if the current thread is allowed to create a
	 ** top-level window.  If it is not, the operation should
	 ** still go through, but some sort of nonremovable
	 ** warning should be placed on the window to show that it
	 ** is untrusted.<P>
	 **
	 ** This method is called by Window.Window().<P>
	 **
	 ** SecurityManager's implementation always denies access.
	 **
	 ** @param window the window to create
	 ** @see java.awt.Window#Window(java.awt.Frame)
	 **/
	public boolean checkTopLevelWindow(Object window) {
	  return false;
	}

	/** Check if the current thread is allowed to create a
	 ** print job.<P>
	 **
	 ** This method is called by Toolkit.getPrintJob().  (I
	 ** assume so, at least, it just don't say nothing about
	 ** it in the spec.)<P>
	 ** 
	 ** SecurityManager's implementation always denies access.
	 **
	 ** @exception SecurityException if the operation is not
	 **            permitted.
	 ** @see java.awt.Toolkit.getPrintJob(java.awt.Frame,java.lang.String,java.util.Properties)
	 **/
	public void checkPrintJobAccess() {
		throw new SecurityException("Cannot create print jobs.");
	}

	/** Check if the current thread is allowed to use the
	 ** system clipboard.<P>
	 **
	 ** This method is called by Toolkit.getSystemClipboard().
	 ** (I assume.)<P>
	 **
	 ** SecurityManager's implementation always denies access.
	 **
	 ** @exception SecurityException if the operation is not
	 **            permitted.
	 ** @see java.awt.Toolkit#getSystemClipboard()
	 **/
	public void checkSystemClipboardAccess() {
		throw new SecurityException("Cannot access the system clipboard.");
	}

	/** Check if the current thread is allowed to use the AWT
	 ** event queue.<P>
	 **
	 ** This method is called by Toolkit.getSystemEventQueue().<P>
	 **
	 ** SecurityManager's implementation always denies access.
	 **
	 ** @exception SecurityException if the operation is not
	 **            permitted.
	 ** @see java.awt.Toolkit#getSystemEventQueue()
	 **/
	public void checkAwtEventQueueAccess() {
		throw new SecurityException("Cannot access the AWT event queue.");
	}

	/** Check if the current thread is allowed to access the
	 ** specified package at all.<P>
	 **
	 ** This method is called by ClassLoader.loadClass() in
	 ** user-created ClassLoaders.<P>
	 **
	 ** SecurityManager's implementation always denies access.
	 **
	 ** @param packageName the package name to check access to
	 ** @exception SecurityException if the operation is not
	 **            permitted.
	 ** @see java.lang.ClassLoader#loadClass(java.lang.String,boolean)
	 **/
	public void checkPackageAccess(String packageName) {
		throw new SecurityException("Cannot access packages via the ClassLoader.");
	}

	/** Check if the current thread is allowed to define
	 ** classes the specified package.  If the class already
	 ** created, though, ClassLoader.loadClass() can still
	 ** return the Class if checkPackageAccess() checks out.<P>
	 **
	 ** This method is called by ClassLoader.loadClass() in
	 ** user-created ClassLoaders.<P>
	 **
	 ** SecurityManager's implementation always denies access.
	 **
	 ** @param packageName the package name to check access to
	 ** @exception SecurityException if the operation is not
	 **            permitted.
	 ** @see java.lang.ClassLoader#loadClass(java.lang.String,boolean)
	 **/
	public void checkPackageDefinition(String packageName) {
		throw new SecurityException("Cannot load classes into any packages via the ClassLoader.");
	}

	/** Check if the current thread is allowed to set the
	 ** current socket factory.<P>
	 **
	 ** This method is called by Socket.setSocketImplFactory(),
	 ** ServerSocket.setSocketFactory(), and
	 ** URL.setURLStreamHandlerFactory().<P>
	 **
	 ** SecurityManager's implementation always denies access.
	 **
	 ** @exception SecurityException if the operation is not
	 **            permitted.
	 ** @see java.net.Socket#setSocketImplFactory(java.net.SocketImplFactory)
	 ** @see java.net.ServerSocket#setSocketFactory(java.net.SocketImplFactory)
	 ** @see java.net.URL#setURLStreamHandlerFactory(java.net.URLStreamHandlerFactory)
	 **/
	public void checkSetFactory() {
		throw new SecurityException("Cannot set the socket factory.");
	}

	/** Check if the current thread is allowed to get certain
	 ** types of Methods, Fields and Constructors from a Class
	 ** object.<P>
	 **
	 ** This method is called by Class.getMethod[s](),
	 ** Class.getField[s](), Class.getConstructor[s],
	 ** Class.getDeclaredMethod[s](),
	 ** Class.getDeclaredField[s](), and
	 ** Class.getDeclaredConstructor[s]().<P>
	 **
	 ** SecurityManager's implementation always denies access.
	 **
	 ** @param c the Class to check
	 ** @param memberType the type of members to check
	 **        against, either Member.DECLARED or
	 **        Member.PUBLIC.
	 ** @exception SecurityException if the operation is not
	 **            permitted.
	 ** @see java.lang.Class
	 ** @see java.lang.reflect.Member#DECLARED
	 ** @see java.lang.reflect.Member#PUBLIC
	 **/
	public void checkMemberAccess(Class c, int memberType) {
		throw new SecurityException("Cannot access members of classes.");
	}

	/** Test whether a particular security action may be
	 ** taken.
	 ** @param action the desired action to take
	 ** @exception SecurityException if the action is denied.
	 ** @XXX I have no idea what actions must be tested
	 **      or where.
	 **/
	public void checkSecurityAccess(String action) {
		checkPermission (new java.security.SecurityPermission (action));
	}

	/** Get the ThreadGroup that a new Thread should belong
	 ** to by default.<P>
	 **
	 ** Called by Thread.Thread().<P>
	 **
	 ** SecurityManager's implementation just uses the
	 ** ThreadGroup of the current Thread.<P>
	 **
	 ** <STRONG>Spec Note:</STRONG> it is not clear whether
	 ** the new Thread is guaranteed to pass the
	 ** checkAccessThreadGroup() test when using this
	 ** ThreadGroup.  I presume so.
	 **
	 ** @return the ThreadGroup to put the new Thread into.
	 **/
	public ThreadGroup getThreadGroup() {
		return Thread.currentThread().getThreadGroup();
	}

	public SecurityManager () {
		if (System.getSecurityManager () != null)
			throw new SecurityException ();
	}
}

class SecurityContext {
	Class[] classes;
	SecurityContext(Class[] classes) {
		this.classes = classes;
	}
}
