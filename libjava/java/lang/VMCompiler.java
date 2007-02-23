/* VMClassLoader.java -- Reference implementation of compiler interface
   Copyright (C) 2004, 2005, 2006, 2007 Free Software Foundation

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

import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStreamReader;
import java.security.MessageDigest;
import java.security.ProtectionDomain;
import java.security.NoSuchAlgorithmException;
import java.util.WeakHashMap;
import java.util.HashSet;
import java.util.Enumeration;
import java.util.StringTokenizer;
import java.util.Vector;
import gnu.gcj.runtime.SharedLibHelper;
import gnu.gcj.runtime.PersistentByteMap;
import gnu.java.security.hash.MD5;

/**
 * This class is just a per-VM reflection of java.lang.Compiler.
 * All methods are defined identically.
 */
final class VMCompiler
{
  // True if we want to use gcj-jit.
  public static boolean useCompiler = true;

  // True if we're able to use gcj-jit.
  public static final boolean canUseCompiler;

  // Compiler to use.
  public static String gcjJitCompiler;

  // Compiler options.
  public static String gcjJitCompilerOptions;

  // Temporary directory to use.
  public static String gcjJitTmpdir;

  public static boolean precompiles()
  {
    return (canUseCompiler & useCompiler);
  }

  // This maps a ClassLoader to a set of SharedLibHelper objects that
  // it has used.  We do things this way to ensure that a
  // SharedLibHelper is collected if and only if the ClassLoader is.
  private static WeakHashMap sharedHelperMap = new WeakHashMap();

  private static Vector precompiledMapFiles;

  // We create a single MD5 engine and then clone it whenever we want
  // a new one.

  // We don't use 
  //
  // md5Digest = MessageDigest.getInstance("MD5");
  //
  // here because that loads a great deal of security provider code as
  // interpreted bytecode -- before we're able to use this class to
  // load precompiled classes.

  private static final MD5 md5Digest
    = new gnu.java.security.hash.MD5();

  static
  {
    gcjJitCompiler = System.getProperty("gnu.gcj.jit.compiler");
    if (gcjJitCompiler == null)
      canUseCompiler = false;
    else
      {
	gcjJitCompilerOptions = System.getProperty("gnu.gcj.jit.options",
						   "-g");
	gcjJitTmpdir = System.getProperty("gnu.gcj.jit.cachedir");
	// Note that we *don't* choose java.io.tmpdir as a default --
	// that would allow easy attacks against the VM.
	if (gcjJitTmpdir == null)
	  canUseCompiler = false;
	else
	  canUseCompiler = true;
      }

    String prop = System.getProperty ("gnu.gcj.precompiled.db.path");
    if (prop != null)
      {
	precompiledMapFiles = new Vector();
	// Add the 
	StringTokenizer st
	  = new StringTokenizer (prop,
				 System.getProperty ("path.separator", ":"));
	{
	  while (st.hasMoreElements ()) 
	    {  
	      String e = st.nextToken ();
	      try
		{
		  PersistentByteMap map 
		    = new PersistentByteMap
		    (e, PersistentByteMap.AccessMode.READ_ONLY);
		  precompiledMapFiles.add(map);
		}
	      catch (IllegalArgumentException _)
		{
		  // Not a map file	      
		}
	      catch (java.io.IOException _)
		{
		}
	      catch (java.nio.BufferUnderflowException _)
		{
		  // Invalid map file.
		}
	    }
	}
      }
  }

  /**
   * Don't allow new `Compiler's to be made.
   */
  private VMCompiler()
  {
  }

  private static Class loadSharedLibrary(ClassLoader loader,
					 String fileName,
					 ProtectionDomain domain,
					 String className)
  {
    Class c = null;
    SharedLibHelper helper 
	= SharedLibHelper.findHelper (loader, fileName, domain.getCodeSource(), 
				      domain, false);
    c = helper.findClass (className);
    if (c != null)
      {
	HashSet hs = (HashSet) sharedHelperMap.get(loader);
	if (hs == null)
	  {
	    hs = new HashSet();
	    sharedHelperMap.put(loader, hs);
	  }
	hs.add(helper);
      }
    return c;
  }

  /**
   * Compile a class given the bytes for it.  Returns the Class, or
   * null if compilation failed or otherwise could not be done.
   */
  public static Class compileClass(ClassLoader loader,
				   String name, byte[] data,
				   int offset, int len,
				   ProtectionDomain domain)
  {
    if (precompiledMapFiles == null && !precompiles())
      return null;

    byte digest[];

    try
      {
	MD5 md = (MD5) md5Digest.clone();
	md.update(data);
	digest = md.digest();
      }
    catch (NullPointerException _)
      {
	// If md5Digest==null -- but really this should never happen
	// either, since the MD5 digest is in libgcj.
	return null;
      }

    // We use lookaside cache files to determine whether these bytes
    // correspond to a class file that is part of a precompiled DSO.
    if (precompiledMapFiles != null)
      {
	try
	  {
	    Enumeration elements = precompiledMapFiles.elements();
	    while (elements.hasMoreElements())
	      {
		PersistentByteMap map = (PersistentByteMap)elements.nextElement();
		byte[] soName = map.get(digest);
		if (soName != null)
		  return loadSharedLibrary(loader, 
					   new String(soName), 
					   domain, name);
	      }
	  }
	catch (Exception _)
	  {
	  }
	catch (UnknownError _)
	  {
	    // SharedLibHelper will throw UnknownError if the dlopen
	    // fails for some reason.  We ignore it and continue on.
	  }
      }
 
    if (!precompiles())
      return null;

    try
      {
	// FIXME: Make sure that the class represented by the
	// bytes in DATA really is the class named in NAME.  Make
	// sure it's not "java.*".
	StringBuffer hexBytes = new StringBuffer(gcjJitTmpdir);
	hexBytes.append(File.separatorChar);
	int digestLength = digest.length;
	for (int i = 0; i < digestLength; ++i)
	  {
	    int v = digest[i] & 0xff;
	    if (v < 16)
	      hexBytes.append('0');	    
	    hexBytes.append(Integer.toHexString(v));
	  }

	// FIXME: use System.mapLibraryName?
	// I'm thinking we should use that, plus a class specified
	// via a property that determines lookup policy.
	File soFile = new File(hexBytes + ".so");
	if (soFile.isFile())
          return loadSharedLibrary (loader, soFile.toString(), domain,
				    name);

	File classFile = new File(hexBytes + ".class");
	classFile.delete();
	if (classFile.createNewFile() != true)	  
	  return null;

	FileOutputStream f = new FileOutputStream (classFile);
	// FIXME: race condition if bytes change... ?
	f.write(data, offset, len);

	// Invoke the compiler.
	StringBuffer command = new StringBuffer(gcjJitCompiler);
	command.append(" ");
	command.append(classFile);
	command.append(" ");
	command.append(gcjJitCompilerOptions);
	// These options are required.
	command.append(" -findirect-dispatch -fjni -shared -fPIC -o ");
	command.append(soFile);
	Process p = Runtime.getRuntime().exec(command.toString());

	// Read the process' stderr into a string.
	StringBuffer err = new StringBuffer();
	InputStreamReader stderr = new InputStreamReader (p.getErrorStream());
	char[] inBuf = new char[500];
	int bytesRead;
	while ((bytesRead = stderr.read (inBuf)) != -1)
	  err.append(inBuf, 0, bytesRead);

	if (p.waitFor() != 0)
	  {
	    // FIXME: we could log err.toString() somewhere...
	    return null;
	  }

	return loadSharedLibrary(loader, soFile.toString(), domain, name);
      }
    catch (Exception _)
      {
	return null;
      }
  }

  /**
   * Compile the class named by <code>oneClass</code>.
   *
   * @param oneClass the class to compile
   * @return <code>false</code> if no compiler is available or
   *         compilation failed, <code>true</code> if compilation succeeded
   * @throws NullPointerException if oneClass is null
   */
  public static boolean compileClass(Class oneClass)
  {
    // Never succeed.
    return false;
  }

  /**
   * Compile the classes whose name matches <code>classNames</code>.
   *
   * @param classNames the name of classes to compile
   * @return <code>false</code> if no compiler is available or
   *         compilation failed, <code>true</code> if compilation succeeded
   * @throws NullPointerException if classNames is null
   */
  public static boolean compileClasses(String classNames)
  {
    // Note the incredibly lame interface.  Always fail.
    return false;
  }

  /**
   * This method examines the argument and performs an operation
   * according to the compilers documentation.  No specific operation
   * is required.
   *
   * @param arg a compiler-specific argument
   * @return a compiler-specific value, including null
   * @throws NullPointerException if the compiler doesn't like a null arg
   */
  public static Object command(Object arg)
  {
    // Our implementation defines this to a no-op.
    return null;
  }

  /**
   * Calling <code>Compiler.enable()</code> will cause the compiler
   * to resume operation if it was previously disabled; provided that a
   * compiler even exists.
   */
  public static void enable()
  {
    useCompiler = true;
  }

  /**
   * Calling <code>Compiler.disable()</code> will cause the compiler
   * to be suspended; provided that a compiler even exists.
   */
  public static void disable()
  {
    useCompiler = false;
  }
}
