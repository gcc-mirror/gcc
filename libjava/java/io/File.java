// File.java - File name

/* Copyright (C) 1998, 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.io;

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

  private final native boolean performDelete (String canon);
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
    if (dirPath != null)
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

  public native String getCanonicalPath () throws IOException;

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


  // The path.
  private String path;

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
}
