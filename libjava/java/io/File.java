// File.java - File name

/* Copyright (C) 1998, 1999, 2000  Free Software Foundation

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
 * Status:  Complete to version 1.1; 1.2 functionality missing.
 * A known bug: most calls to the security manager can generate
 * IOException since we use the canonical path.
 */

public class File implements Serializable
{
  public boolean canRead ()
  {
    return access (checkRead (), READ);
  }

  public boolean canWrite ()
  {
    SecurityManager s = System.getSecurityManager();
    String p = safeCanonicalPath ();
    // FIXME: it isn't entirely clear what to do if we can't find the
    // canonical path.
    if (p == null)
      return false;
    if (s != null)
      s.checkWrite(p);
    return access (p, WRITE);
  }

  private final native static boolean performDelete (String canon);
  public boolean delete ()
  {
    SecurityManager s = System.getSecurityManager();
    String p = safeCanonicalPath ();
    // FIXME: what is right?
    if (p == null)
      return false;
    if (s != null)
      s.checkDelete(p);
    return performDelete (p);
  }

  public boolean equals (Object obj)
  {
    if (! (obj instanceof File))
      return false;
    File other = (File) obj;
    return path.compareTo(other.path) == 0;
  }

  public boolean exists ()
  {
    return access (checkRead (), EXISTS);
  }

  public File (String p)
  {
    if (p == null)
      throw new NullPointerException ();
    path = p;
  }

  public File (String dirPath, String name)
  {
    if (name == null)
      throw new NullPointerException ();
    if (dirPath != null && dirPath.length() > 0)
      {
	// Try to be smart about the number of separator characters.
	if (dirPath.charAt(dirPath.length() - 1) == separatorChar)
	  path = dirPath + name;
	else
	  path = dirPath + separatorChar + name;
      }
    else
      path = name;
  }

  public File (File dir, String name)
  {
    this (dir == null ? null : dir.path, name);
  }

  public String getAbsolutePath ()
  {
    if (isAbsolute ())
      return path;
    return System.getProperty("user.dir") + separatorChar + path;
  }

  public File getAbsoluteFile () throws IOException
  {
    return new File (getAbsolutePath());
  }

  public native String getCanonicalPath () throws IOException;

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
    return path.substring(0, last);
  }

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
    // FIXME: test.
    return path.hashCode();
  }

  public native boolean isAbsolute ();

  public boolean isDirectory ()
  {
    return stat (checkRead (), DIRECTORY);
  }

  public boolean isFile ()
  {
    return stat (checkRead (), ISFILE);
  }

  public long lastModified ()
  {
    return attr (checkRead (), MODIFIED);
  }

  public long length ()
  {
    return attr (checkRead (), LENGTH);
  }

  private final native String[] performList (String canon,
					     FilenameFilter filter);
  public String[] list (FilenameFilter filter)
  {
    return performList (checkRead (), filter);
  }

  public String[] list ()
  {
    return performList (checkRead (), null);
  }

  public String toString ()
  {
    return path;
  }

  public URL toURL () throws MalformedURLException
  {
    return new URL ("file:" + path + (isDirectory() ? "/" : ""));
  }

  private final native boolean performMkdir ();
  public boolean mkdir ()
  {
    SecurityManager s = System.getSecurityManager();
    if (s != null)
      {
	// NOTE: in theory we should use the canonical path.  In
	// practice, we can't compute the canonical path until we've
	// made this completely.  Lame.
	s.checkWrite(path);
      }
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
	x.setPath(parent);
	if (! mkdirs (x))
	  return false;
	x.setPath(p);
      }
    return x.mkdir();
  }

  public boolean mkdirs ()
  {
    SecurityManager s = System.getSecurityManager();
    if (s != null)
      {
	// NOTE: in theory we should use the canonical path.  In
	// practice, we can't compute the canonical path until we've
	// made this completely.  Lame.
	s.checkWrite(path);
      }

    if (isDirectory ())
      return false;
    return mkdirs (new File (path));
  }

  private static synchronized String nextValue ()
  {
    return Long.toString(counter++, Character.MAX_RADIX);
  }

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

    // FIXME: filename length varies by architecture and filesystem.
    int max_length = 255;

    // Truncation rules.
    // `6' is the number of characters we generate.
    if (prefix.length () + 6 + suffix.length () > max_length)
      {
	int suf_len = 0;
	if (suffix.charAt(0) == '.')
	  suf_len = 4;
	suffix = suffix.substring(0, suf_len);
	if (prefix.length () + 6 + suf_len > max_length)
	  prefix = prefix.substring(0, max_length - 6 - suf_len);
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
	    if (f.exists())
	      continue;
	    else
	      {
		String af = f.getAbsolutePath ();
		
		// Check to see if we're allowed to write to it.
		SecurityManager s = System.getSecurityManager();
		if (s != null)
		  s.checkWrite (af);
		
		// Now create the file.
		FileDescriptor fd = 
		  new FileDescriptor (af, 
				      FileDescriptor.WRITE
				      | FileDescriptor.EXCL);
		fd.close ();
		return f;
	      }
	  }
	catch (IOException _)
	  {
	  }
      }

    throw new IOException ("cannot create temporary file");
  }

  public static File createTempFile (String prefix, String suffix)
    throws IOException
  {
    return createTempFile (prefix, suffix, null);
  }

  private final native boolean performRenameTo (File dest);
  public boolean renameTo (File dest)
  {
    SecurityManager s = System.getSecurityManager();
    if (s != null)
      {
	// FIXME: JCL doesn't specify which path to check.  We check the
	// source since we can canonicalize it.
	s.checkWrite(safeCanonicalPath());
      }
    return performRenameTo (dest);
  }

  public static final String pathSeparator
    = System.getProperty("path.separator");
  public static final char pathSeparatorChar = pathSeparator.charAt(0);
  public static final String separator = System.getProperty("file.separator");
  public static final char separatorChar = separator.charAt(0);

  private static final String tmpdir = System.getProperty("java.io.tmpdir");

  // The path.
  private String path;

  // We keep a counter for use by createTempFile.  We choose the first
  // value randomly to try to avoid clashes with other VMs.
  private static long counter = Double.doubleToLongBits (Math.random ());

  // mkdirs() uses this to avoid repeated allocations.
  private final void setPath (String n)
  {
    path = n;
  }


  private final String checkRead ()
  {
    SecurityManager s = System.getSecurityManager();
    String p = safeCanonicalPath ();
    if (p == null)
      return null;
    if (s != null)
      s.checkRead(p);
    return p;
  }

  // Return canonical path, or null.
  private final String safeCanonicalPath ()
  {
    String p = null;
    try
      {
	p = getCanonicalPath ();
      }
    catch (IOException x)
      {
	// Nothing.
      }
    return p;
  }

  // Add this File to the set of files to be deleted upon normal
  // termination.
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

  // QUERY arguments to attr function.
  private final static int MODIFIED = 0;
  private final static int LENGTH = 1;

  private final native long attr (String p, int query);
  private final native boolean access (String p, int query);
  private final native boolean stat (String p, int query);

  private static final long serialVersionUID = 301077366599181567L;
}
