// File.java - File name

/* Copyright (C) 1998, 1999, 2000, 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.io;

import java.util.*;
import java.net.*;
import gnu.gcj.runtime.FileDeleter;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date September 24, 1998 
 */

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * Status:  Complete to version 1.3.
 */

public class File implements Serializable, Comparable
{
  public boolean canRead ()
  {
    checkRead();
    return _access (READ);
  }

  public boolean canWrite ()
  {
    checkWrite();
    return _access (WRITE);
  }
  
  private native boolean performCreate() throws IOException;

  /** @since 1.2 */
  public boolean createNewFile() throws IOException
  {
    checkWrite();
    return performCreate();
  }
  
  private native boolean performDelete ();
  public boolean delete ()
  {
    SecurityManager s = System.getSecurityManager();
    String name = path;
    if (s != null)
      s.checkDelete(path);
    return performDelete ();
  }

  public boolean equals (Object obj)
  {
    if (! (obj instanceof File))
      return false;
    File other = (File) obj;
    if (caseSensitive)
      return (path.equals(other.path));
    else
      return (path.equalsIgnoreCase(other.path));      
  }

  public boolean exists ()
  {
    checkRead();
    return _access (EXISTS);
  }

  public File (String p)
  {
    path = normalizePath(p);
  }

  // Remove duplicate and redundant separator characters.
  private String normalizePath(String p)
  {
    // On Windows, convert any '/' to '\'.  This appears to be the same logic
    // that Sun's Win32 Java performs.
    if (separatorChar == '\\')
      p = p.replace ('/', '\\');

    int dupIndex = p.indexOf(dupSeparator);
    int plen = p.length();

    // Special case: permit Windows UNC path prefix.
    if (dupSeparator.equals("\\\\") && dupIndex == 0)
      dupIndex = p.indexOf(dupSeparator, 1);

    if (dupIndex == -1)
      {
        // Ignore trailing separator.
        if (plen > 1 && p.charAt(plen - 1) == separatorChar)
	  return p.substring(0, plen - 1);
	else
	  return p;
      }
    
    StringBuffer newpath = new StringBuffer(plen);
    int last = 0;
    while (dupIndex != -1)
      {
        newpath.append(p.substring(last, dupIndex));
	// Ignore the duplicate path characters.
	while (p.charAt(dupIndex) == separatorChar)
	  {
	    dupIndex++;
	    if (dupIndex == plen)
	      return newpath.toString();
	  }
	newpath.append(separatorChar);
	last = dupIndex;
	dupIndex = p.indexOf(dupSeparator, last);
      }
    
    // Again, ignore possible trailing separator.
    int end;
    if (plen > 1 && p.charAt(plen - 1) == separatorChar)
      end = plen - 1;
    else
      end = plen;
    newpath.append(p.substring(last, end));
    
    return newpath.toString();
  }
  
  public File (String dirPath, String name)
  {
    if (name == null)
      throw new NullPointerException ();
    if (dirPath != null && dirPath.length() > 0)
      {
	// Try to be smart about the number of separator characters.
	if (dirPath.charAt(dirPath.length() - 1) == separatorChar
	    || name.length() == 0)
	  path = normalizePath(dirPath + name);
	else
	  path = normalizePath(dirPath + separatorChar + name);
      }
    else
      path = normalizePath(name);
  }

  public File (File dir, String name)
  {
    this (dir == null ? null : dir.path, name);
  }

  // FIXME  ???
  public String getAbsolutePath ()
  {
    if (isAbsolute ())
      return path;
    return System.getProperty("user.dir") + separatorChar + path;
  }

  /** @since 1.2 */
  public File getAbsoluteFile ()
  {
    return new File (getAbsolutePath());
  }

  public native String getCanonicalPath () throws IOException;

  /** @since 1.2 */
  public File getCanonicalFile () throws IOException
  {
    return new File (getCanonicalPath());
  }

  public String getName ()
  {
    int last = path.lastIndexOf(separatorChar);
    return path.substring(last + 1);
  }

  public String getParent ()
  {
    int last = path.lastIndexOf(separatorChar);
    if (last == -1)
      return null;
    // FIXME: POSIX assumption.
    if (last == 0 && path.charAt (0) == '/')
      ++last;
    return path.substring(0, last);
  }

  /** @since 1.2 */
  public File getParentFile ()
  {
    String parent = getParent ();
    return (parent == null ? null : new File (parent));
  }

  public String getPath ()
  {
    return path;
  }

  public int hashCode ()
  {
    if (caseSensitive)
      return (path.hashCode() ^ 1234321);
    else
      return (path.toLowerCase().hashCode() ^ 1234321);
  }

  public native boolean isAbsolute ();

  public boolean isDirectory ()
  {
    checkRead();
    return _stat (DIRECTORY);
  }

  public boolean isFile ()
  {
    checkRead();
    return _stat (ISFILE);
  }

  /** @since 1.2 */
  public boolean isHidden()
  {
    checkRead();
    return _stat (ISHIDDEN);
  }

  public long lastModified ()
  {
    checkRead();
    return attr (MODIFIED);
  }

  public long length ()
  {
    checkRead();
    return attr (LENGTH);
  }
    
  private final native Object[] performList (FilenameFilter filter,
					     FileFilter fileFilter,
					     Class result_type);

  public String[] list (FilenameFilter filter)
  {
    checkRead();
    return (String[]) performList (filter, null, String.class);
  }

  public String[] list ()
  {
    checkRead();
    return (String[]) performList (null, null, String.class);
  }

  /** @since 1.2 */
  public File[] listFiles()
  {
    checkRead();
    return (File[]) performList (null, null, File.class);
  }
  
  /** @since 1.2 */
  public File[] listFiles(FilenameFilter filter)
  {
    checkRead();
    return (File[]) performList (filter, null, File.class);
  }
  
  /** @since 1.2 */
  public File[] listFiles(FileFilter filter)
  {
    checkRead();
    return (File[]) performList (null, filter, File.class);
  }

  public String toString ()
  {
    return path;
  }

  public URL toURL () throws MalformedURLException
  {
    return new URL ("file://" + getAbsolutePath ()
		    + (isDirectory() ? "/" : ""));
  }

  private final native boolean performMkdir ();

  public boolean mkdir ()
  {
    checkWrite();
    return performMkdir ();
  }

  private static boolean mkdirs (File x)
  {
    if (x.isDirectory())
      return true;
    String p = x.getPath();
    String parent = x.getParent();
    if (parent != null)
      {
	x.path = parent;
	if (! mkdirs (x))
	  return false;
	x.path = p;
      }
    return x.mkdir();
  }

  public boolean mkdirs ()
  {
    checkWrite();
    if (isDirectory ())
      return false;
    return mkdirs (new File (path));
  }

  private static synchronized String nextValue ()
  {
    return Long.toString(counter++, Character.MAX_RADIX);
  }

  /** @since 1.2 */
  public static File createTempFile (String prefix, String suffix,
				     File directory)
    throws IOException
  {
    // Grab the system temp directory if necessary
    if (directory == null)
      {
	String dirname = tmpdir;
	if (dirname == null)
	  throw 
	    new IOException("Cannot determine system temporary directory"); 
	
	directory = new File(dirname);
	if (!directory.exists())
	  throw new IOException("System temporary directory " 
				+ directory.getName() + " does not exist.");
	if (!directory.isDirectory())
	  throw new IOException("System temporary directory " 
				+ directory.getName() 
				+ " is not really a directory.");
      }

    if (prefix.length () < 3)
      throw new IllegalArgumentException ("Prefix too short: " + prefix);
    if (suffix == null)
      suffix = ".tmp";

    // Truncation rules.
    // `6' is the number of characters we generate.
    if (prefix.length () + 6 + suffix.length () > maxPathLen)
      {
	int suf_len = 0;
	if (suffix.charAt(0) == '.')
	  suf_len = 4;
	suffix = suffix.substring(0, suf_len);
	if (prefix.length () + 6 + suf_len > maxPathLen)
	  prefix = prefix.substring(0, maxPathLen - 6 - suf_len);
      }

    File f;

    // How many times should we try?  We choose 100.
    for (int i = 0; i < 100; ++i)
      {
	// This is ugly.
	String t = "ZZZZZZ" + nextValue ();
	String l = prefix + t.substring(t.length() - 6) + suffix;
	try
	  {
	    f = new File(directory, l);
	    if (f.createNewFile())
	      return f;
	  }
	catch (IOException ignored)
	  {
	  }
      }

    throw new IOException ("cannot create temporary file");
  }

  private native boolean performSetReadOnly();

  /** @since 1.2 */
  public boolean setReadOnly()
  {
    checkWrite();
    return performSetReadOnly();
  }

  private static native File[] performListRoots();

  /** @since 1.2 */
  public static File[] listRoots()
  {
    File[] roots = performListRoots();
    
    SecurityManager s = System.getSecurityManager();
    if (s != null)
      {
	// Only return roots to which the security manager permits read access.
	int count = roots.length;
	for (int i = 0; i < roots.length; i++)
	  {
	    try
	      {
        	s.checkRead(roots[i].path);		
	      }
	    catch (SecurityException sx)
	      {
	        roots[i] = null;
		count--;
	      }
	  }
	if (count != roots.length)
	  {
	    File[] newRoots = new File[count];
	    int k = 0;
	    for (int i=0; i < roots.length; i++)
	      {
	        if (roots[i] != null)
		  newRoots[k++] = roots[i];
	      }
	    roots = newRoots;
	  }
      }
    return roots;
  }

  public static File createTempFile (String prefix, String suffix)
    throws IOException
  {
    return createTempFile (prefix, suffix, null);
  }

  /** @since 1.2 */
  public int compareTo(File other)
  {
    if (caseSensitive)
      return path.compareTo (other.path);
    else
      return path.compareToIgnoreCase (other.path);
  }

  /** @since 1.2 */
  public int compareTo(Object o)
  {
    File other = (File) o;
    return compareTo (other);
  }

  private native boolean performRenameTo (File dest);
  public boolean renameTo (File dest)
  {
    SecurityManager s = System.getSecurityManager();
    String sname = getName();
    String dname = dest.getName();
    if (s != null)
      {
	s.checkWrite(sname);
	s.checkWrite(dname);
      }
    return performRenameTo (dest);
  }

  private native boolean performSetLastModified(long time);
  
  /** @since 1.2 */
  public boolean setLastModified(long time)
  {
    checkWrite();
    return performSetLastModified(time);
  }

  public static final String pathSeparator
    = System.getProperty("path.separator");
  public static final char pathSeparatorChar = pathSeparator.charAt(0);
  public static final String separator = System.getProperty("file.separator");
  public static final char separatorChar = separator.charAt(0);

  static final String tmpdir = System.getProperty("java.io.tmpdir");
  static int maxPathLen;
  static boolean caseSensitive;
  static String dupSeparator = separator + separator;
  
  static
  {
    init_native();
  }
  
  // Native function called at class initialization. This should should
  // set the maxPathLen and caseSensitive variables.
  private static native void init_native();

  // The path.
  private String path;

  // We keep a counter for use by createTempFile.  We choose the first
  // value randomly to try to avoid clashes with other VMs.
  private static long counter = Double.doubleToLongBits (Math.random ());

  private void checkWrite ()
  {
    SecurityManager s = System.getSecurityManager();
    if (s != null)
      s.checkWrite(path);
  }

  private void checkRead ()
  {
    SecurityManager s = System.getSecurityManager();
    if (s != null)
      s.checkRead(path);
  }

  /** 
    * Add this File to the set of files to be deleted upon normal
    * termination.
    *
    * @since 1.2 
    */
  // FIXME: This should use the ShutdownHook API once we implement that.
  public void deleteOnExit ()
  {
    SecurityManager sm = System.getSecurityManager ();
    if (sm != null)
      sm.checkDelete (getName ());

    FileDeleter.add (this);
  }

  private void writeObject (ObjectOutputStream oos) throws IOException
  {
    oos.defaultWriteObject ();
    oos.writeChar (separatorChar);
  }

  private void readObject (ObjectInputStream ois)
    throws ClassNotFoundException, IOException
  {
    ois.defaultReadObject ();

    // If the file was from an OS with a different dir separator,
    // fixup the path to use the separator on this OS.
    char oldSeparatorChar = ois.readChar ();
    if (oldSeparatorChar != separatorChar)
      path = path.replace (oldSeparatorChar, separatorChar);
  }

  // QUERY arguments to access function.
  private final static int READ = 0;
  private final static int WRITE = 1;
  private final static int EXISTS = 2;

  // QUERY arguments to stat function.
  private final static int DIRECTORY = 0;
  private final static int ISFILE = 1;
  private final static int ISHIDDEN = 2;

  // QUERY arguments to attr function.
  private final static int MODIFIED = 0;
  private final static int LENGTH = 1;
  
  private final native long attr (int query);
  // On OSF1 V5.0, `stat' is a macro.  It is easiest to use the name
  // `_stat' instead.  We do the same thing for `_access' just in
  // case.
  private final native boolean _access (int query);
  private final native boolean _stat (int query);

  private static final long serialVersionUID = 301077366599181567L;
}
