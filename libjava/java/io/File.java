/* File.java -- Class representing a file on disk
   Copyright (C) 1998, 1999, 2000, 2001, 2003 Free Software Foundation, Inc.

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


package java.io;

import java.net.MalformedURLException;
import java.net.URL;
import gnu.classpath.Configuration;
import gnu.gcj.runtime.FileDeleter;

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * Status:  Complete to version 1.3.
 */

/**
 * This class represents a file or directory on a local disk.  It provides
 * facilities for dealing with a variety of systems that use various
 * types of path separators ("/" versus "\", for example).  It also
 * contains method useful for creating and deleting files and directories.
 *
 * @author Aaron M. Renn <arenn@urbanophile.com>
 * @author Tom Tromey <tromey@cygnus.com>
 */
public class File implements Serializable, Comparable
{
  private static final long serialVersionUID = 301077366599181567L;
	
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

  /**
   * This is the path separator string for the current host. This field
   * contains the value of the <code>file.separator</code> system property.
   * An example separator string would be "/" on the GNU system.
   */
  public static final String separator = System.getProperty("file.separator");

  /**
   * This is the first character of the file separator string.  On many
   * hosts (for example, on the GNU system), this represents the entire 
   * separator string.  The complete separator string is obtained from the
   * <code>file.separator</code>system property.
   */
  public static final char separatorChar = separator.charAt(0);
  
  /**
   * This is the string that is used to separate the host name from the
   * path name in paths than include the host name.  It is the value of
   * the <code>path.separator</code> system property.
   */
  public static final String pathSeparator
    = System.getProperty("path.separator");
  
  /**
   * This is the first character of the string used to separate the host name
   * from the path name in paths that include a host.  The separator string
   * is taken from the <code>path.separator</code> system property.
   */
  public static final char pathSeparatorChar = pathSeparator.charAt(0);

  static final String tmpdir = System.getProperty("java.io.tmpdir");
  static int maxPathLen;
  static boolean caseSensitive;
  static String dupSeparator = separator + separator;
  
  static
  {
    if (Configuration.INIT_LOAD_LIBRARY)
      {
        System.loadLibrary ("javaio");
      }
    
    init_native();
  }
  
  // Native function called at class initialization. This should should
  // set the maxPathLen and caseSensitive variables.
  private static native void init_native();

  /**
   * This is the path to the file set when the object is created.  It
   * may be an absolute or relative path name.
   */
  private String path;

  // We keep a counter for use by createTempFile.  We choose the first
  // value randomly to try to avoid clashes with other VMs.
  private static long counter = Double.doubleToLongBits (Math.random());

  /**
   * This method tests whether or not the current thread is allowed to
   * to read the file pointed to by this object.  This will be true if and
   * and only if 1) the file exists and 2) the <code>SecurityManager</code>
   * (if any) allows access to the file via it's <code>checkRead</code>
   * method 3) the file is readable.
   *
   * @return <code>true</code> if reading is allowed, 
   * <code>false</code> otherwise
   *
   * @exception SecurityException If the <code>SecurityManager</code> 
   * does not allow access to the file
   */
  public boolean canRead()
  {
    checkRead();
    return _access (READ);
  }

  /**
   * This method test whether or not the current thread is allowed to
   * write to this object.  This will be true if and only if 1) The
   * <code>SecurityManager</code> (if any) allows write access to the
   * file and 2) The file exists and 3) The file is writable.  To determine
   * whether or not a non-existent file can be created, check the parent
   * directory for write access.
   *
   * @return <code>true</code> if writing is allowed, <code>false</code> 
   * otherwise
   *
   * @exception SecurityException If the <code>SecurityManager</code> 
   * does not allow access to the file
   */
  public boolean canWrite()
  {
    checkWrite();
    return _access (WRITE);
  }
  
  private native boolean performCreate() throws IOException;

  /**
   * This method creates a new file of zero length with the same name as
   * the path of this <code>File</code> object if an only if that file
   * does not already exist.
   * <p>
   * A <code>SecurityManager</code>checkWrite</code> check is done prior
   * to performing this action.
   *
   * @return <code>true</code> if the file was created, <code>false</code> if
   * the file alread existed.
   *
   * @exception IOException If an I/O error occurs
   * @exception SecurityException If the <code>SecurityManager</code> will
   * not allow this operation to be performed.
   *
   * @since 1.2
   */
  public boolean createNewFile() throws IOException
  {
    checkWrite();
    return performCreate();
  }
 
  /*
   * This native method handles the actual deleting of the file
   */
  private native boolean performDelete();

  /**
   * This method deletes the file represented by this object.  If this file
   * is a directory, it must be empty in order for the delete to succeed.
   *
   * @return <code>true</code> if the file was deleted, <code>false</code> 
   * otherwise
   *
   * @exception SecurityException If deleting of the file is not allowed
   */
  public synchronized boolean delete()
  {
    SecurityManager s = System.getSecurityManager();
    
    if (s != null)
      s.checkDelete (path);
    
    return performDelete();
  }

  /**
   * This method tests two <code>File</code> objects for equality by 
   * comparing the path of the specified <code>File</code> against the path
   * of this object.  The two objects are equal if an only if 1) The
   * argument is not null 2) The argument is a <code>File</code> object and
   * 3) The path of the <code>File</code>argument is equal to the path
   * of this object.
   * <p>
   * The paths of the files are determined by calling the 
   * <code>getPath()</code>
   * method on each object.
   *
   * @return <code>true</code> if the two objects are equal, 
   * <code>false</code> otherwise.
   */
  public boolean equals (Object obj)
  {
    if (! (obj instanceof File))
      return false;
    
    File other = (File) obj;

    if (caseSensitive)
      return path.equals (other.path);
    else
      return path.equalsIgnoreCase (other.path);
  }

  /**
   * This method tests whether or not the file represented by the object
   * actually exists on the filesystem.
   *
   * @return <code>true</code> if the file exists, <code>false</code>otherwise.
   *
   * @exception SecurityException If reading of the file is not permitted
   */
  public boolean exists()
  {
    checkRead();
    return _access (EXISTS);
  }

  /**
   * This method initializes a new <code>File</code> object to represent
   * a file with the specified path.
   *
   * @param name The path name of the file
   */
  public File (String name)
  {
    path = normalizePath (name);
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
        // Ignore trailing separator (though on Windows "a:\", for
        // example, is a valid and minimal path).
        if (plen > 1 && p.charAt (plen - 1) == separatorChar)
	  {
	    if (! (separatorChar == '\\' && plen == 3 && p.charAt (1) == ':'))
	      return p.substring (0, plen - 1);
	  }
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
    
    // Again, ignore possible trailing separator (except special cases
    // like "a:\" on Windows).
    int end;
    if (plen > 1 && p.charAt (plen - 1) == separatorChar)
    {
      if (separatorChar == '\\' && plen == 3 && p.charAt (1) == ':')
        end = plen;
      else
        end = plen - 1;
    }
    else
      end = plen;
    newpath.append(p.substring(last, end));
    
    return newpath.toString();
  }
 
  /**
   * This method initializes a new <code>File</code> object to represent
   * a file in the specified named directory.  The path name to the file
   * will be the directory name plus the separator string plus the file
   * name.  If the directory path name ends in the separator string, another
   * separator string will still be appended.
   *
   * @param dirPath The path to the directory the file resides in
   * @param name The name of the file
   */
  public File (String dirPath, String name)
  {
    if (name == null)
      throw new NullPointerException();
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

  /**
   * This method initializes a new <code>File</code> object to represent
   * a file in the specified directory.  If the <code>directory</code>
   * argument is <code>null</code>, the file is assumed to be in the
   * current directory as specified by the <code>user.dir</code> system
   * property
   *
   * @param directory The directory this file resides in
   * @param name The name of the file
   */
  public File (File directory, String name)
  {
    this (directory == null ? null : directory.path, name);
  }

  /**
   * This method returns the path of this file as an absolute path name.
   * If the path name is already absolute, then it is returned.  Otherwise
   * the value returned is the current directory plus the separatory
   * string plus the path of the file.  The current directory is determined
   * from the <code>user.dir</code> system property.
   *
   * @return The absolute path of this file
   */
  public String getAbsolutePath()
  {
    if (isAbsolute())
      return path;
    else if (separatorChar == '\\' 
             && path.length() > 0 && path.charAt (0) == '\\')
      {
        // On Windows, even if the path starts with a '\\' it is not
        // really absolute until we prefix the drive specifier from
        // the current working directory to it.
        return System.getProperty ("user.dir").substring (0, 2) + path;
      }
    else if (separatorChar == '\\' 
             && path.length() > 1 && path.charAt (1) == ':'
             && ((path.charAt (0) >= 'a' && path.charAt (0) <= 'z')
                 || (path.charAt (0) >= 'A' && path.charAt (0) <= 'Z')))
      {
        // On Windows, a process has a current working directory for
        // each drive and a path like "G:foo\bar" would mean the 
        // absolute path "G:\wombat\foo\bar" if "\wombat" is the 
        // working directory on the G drive.
        String drvDir = null;
        try
          {
            drvDir = new File (path.substring (0, 2)).getCanonicalPath();
          }
        catch (IOException e)
          {
            drvDir = path.substring (0, 2) + "\\";
          }
        
        // Note: this would return "C:\\." for the path "C:.", if "\"
        // is the working folder on the C drive, but this is 
        // consistent with what Sun's JRE 1.4.1.01 actually returns!
        if (path.length() > 2)
          return drvDir + '\\' + path.substring (2, path.length());
        else
          return drvDir;
      }
    else
      return System.getProperty ("user.dir") + separatorChar + path;
  }

  /**
   * This method returns a <code>File</code> object representing the
   * absolute path of this object.
   *
   * @return A <code>File</code> with the absolute path of the object.
   *
   * @since 1.2
   */
  public File getAbsoluteFile()
  {
    return new File (getAbsolutePath());
  }

  /**
   * This method returns a canonical representation of the pathname of
   * this file.  The actual form of the canonical representation is
   * different.  On the GNU system, the canonical form differs from the
   * absolute form in that all relative file references to "." and ".."
   * are resolved and removed.
   * <p>
   * Note that this method, unlike the other methods which return path
   * names, can throw an IOException.  This is because native method 
   * might be required in order to resolve the canonical path
   *
   * @exception IOException If an error occurs
   */
  public native String getCanonicalPath() throws IOException;

  /**
   * This method returns a <code>File</code> object representing the
   * canonical path of this object.
   *
   * @return A <code>File</code> instance representing the canonical path of
   * this object.
   *
   * @exception IOException If an error occurs.
   *
   * @since 1.2
   */
  public File getCanonicalFile() throws IOException
  {
    return new File (getCanonicalPath());
  }

  /**
   * This method returns the name of the file.  This is everything in the
   * complete path of the file after the last instance of the separator
   * string.
   *
   * @return The file name
   */
  public String getName()
  {
    int nameSeqIndex = 0;

    if (separatorChar == '\\' && path.length() > 1)
      {
        // On Windows, ignore the drive specifier or the leading '\\'
        // of a UNC network path, if any (a.k.a. the "prefix").
        if ((path.charAt (0) == '\\' && path.charAt (1) == '\\')
            || (((path.charAt (0) >= 'a' && path.charAt (0) <= 'z')
		 || (path.charAt (0) >= 'A' && path.charAt (0) <= 'Z'))
		&& path.charAt (1) == ':'))
	  {
	    if (path.length() > 2)
	      nameSeqIndex = 2;
	    else
	      return "";
	  }
      }

    String nameSeq 
      = (nameSeqIndex > 0 ? path.substring (nameSeqIndex) : path);

    int last = nameSeq.lastIndexOf (separatorChar);

    return nameSeq.substring (last + 1);
  }

  /**
   * This method returns a <code>String</code> the represents this file's
   * parent.  <code>null</code> is returned if the file has no parent.  The
   * parent is determined via a simple operation which removes the
   *
   * @return The parent directory of this file
   */
  public String getParent()
  {
    String prefix = null;
    int nameSeqIndex = 0;

    // The "prefix", if present, is the leading "/" on UNIX and 
    // either the drive specifier (e.g. "C:") or the leading "\\"
    // of a UNC network path on Windows.
    if (separatorChar == '/' && path.charAt (0) == '/')
      {
        prefix = "/";
        nameSeqIndex = 1;
      }
    else if (separatorChar == '\\' && path.length() > 1)
      {
        if ((path.charAt (0) == '\\' && path.charAt (1) == '\\')
            || (((path.charAt (0) >= 'a' && path.charAt (0) <= 'z')
                 || (path.charAt (0) >= 'A' && path.charAt (0) <= 'Z'))
                && path.charAt (1) == ':'))
          {
            prefix = path.substring (0, 2);
            nameSeqIndex = 2;
          }
      }

    // According to the JDK docs, the returned parent path is the 
    // portion of the name sequence before the last separator
    // character, if found, prefixed by the prefix, otherwise null.
    if (nameSeqIndex < path.length())
      {
        String nameSeq = path.substring (nameSeqIndex, path.length());
        int last = nameSeq.lastIndexOf (separatorChar);
        if (last == -1)
          return prefix;
        else if (last == (nameSeq.length() - 1))
          // Note: The path would not have a trailing separator
          // except for cases like "C:\" on Windows (see 
          // normalizePath( )), where Sun's JRE 1.4 returns null.
          return null;
        else if (last == 0)
          last++;

        if (prefix != null)
          return prefix + nameSeq.substring (0, last);
        else
          return nameSeq.substring (0, last);
      }
    else
      // Sun's JRE 1.4 returns null if the prefix is the only 
      // component of the path - so "/" gives null on UNIX and 
      // "C:", "\\", etc. return null on Windows.
      return null;
  }

  /**
   * This method returns a <code>File</code> object representing the parent
   * file of this one.
   *
   * @param A <code>File</code> for the parent of this object.  
   * <code>null</code>
   * will be returned if this object does not have a parent.
   *
   * @since 1.2
   */
  public File getParentFile()
  {
    String parent = getParent();
    return parent != null ? new File (parent) : null;
  }

  /**
   * Returns the path name that represents this file.  May be a relative
   * or an absolute path name
   *
   * @return The pathname of this file
   */
  public String getPath()
  {
    return path;
  }

  /**
   * This method returns a hash code representing this file.  It is the
   * hash code of the path of this file (as returned by <code>getPath()</code>)
   * exclusived or-ed with the value 1234321.
   *
   * @return The hash code for this object
   */
  public int hashCode()
  {
    if (caseSensitive)
      return path.hashCode() ^ 1234321;
    else
      return path.toLowerCase().hashCode() ^ 1234321;
  }

  /**
   * This method returns true if this object represents an absolute file
   * path and false if it does not.  The definition of an absolute path varies
   * by system.  As an example, on GNU systems, a path is absolute if it starts
   * with a "/".
   *
   * @return <code>true</code> if this object represents an absolute 
   * file name, <code>false</code> otherwise.
   */
  public native boolean isAbsolute();

  /**
   * This method tests whether or not the file represented by this object
   * is a directory.  In order for this method to return <code>true</code>,
   * the file represented by this object must exist and be a directory.
   * 
   * @return <code>true</code> if this file is a directory, <code>false</code>
   * otherwise
   *
   * @exception SecurityException If reading of the file is not permitted
   */
  public boolean isDirectory()
  {
    checkRead();
    return _stat (DIRECTORY);
  }

  /**
   * This method tests whether or not the file represented by this object
   * is a "plain" file.  A file is a plain file if and only if it 1) Exists,
   * 2) Is not a directory or other type of special file.
   *
   * @return <code>true</code> if this is a plain file, <code>false</code> 
   * otherwise
   *
   * @exception SecurityException If reading of the file is not permitted
   */
  public boolean isFile()
  {
    checkRead();
    return _stat (ISFILE);
  }

  /**
   * This method tests whether or not this file represents a "hidden" file.
   * On GNU systems, a file is hidden if its name begins with a "."
   * character.  Files with these names are traditionally not shown with
   * directory listing tools.
   *
   * @return <code>true</code> if the file is hidden, <code>false</code>
   * otherwise.
   *
   * @since 1.2
   */
  public boolean isHidden()
  {
    checkRead();
    return _stat (ISHIDDEN);
  }

  /**
   * This method returns the last modification time of this file.  The
   * time value returned is an abstract value that should not be interpreted
   * as a specified time value.  It is only useful for comparing to other
   * such time values returned on the same system.  In that case, the larger
   * value indicates a more recent modification time. 
   * <p>
   * If the file does not exist, then a value of 0 is returned.
   *
   * @return The last modification time of the file
   *
   * @exception SecurityException If reading of the file is not permitted
   */
  public long lastModified()
  {
    checkRead();
    return attr (MODIFIED);
  }

  /**
   * This method returns the length of the file represented by this object,
   * or 0 if the specified file does not exist.
   *
   * @return The length of the file
   *
   * @exception SecurityException If reading of the file is not permitted
   */
  public long length()
  {
    checkRead();
    return attr (LENGTH);
  }

  /*
   * This native function actually produces the list of file in this
   * directory
   */
  private final native Object[] performList (FilenameFilter filter,
					     FileFilter fileFilter,
					     Class result_type);

  /**
   * This method returns a array of <code>String</code>'s representing the
   * list of files is then directory represented by this object.  If this
   * object represents a non-directory file or a non-existent file, then
   * <code>null</code> is returned.  The list of files will not contain
   * any names such as "." or ".." which indicate the current or parent
   * directory.  Also, the names are not guaranteed to be sorted.
   * <p>
   * In this form of the <code>list()</code> method, a filter is specified
   * that allows the caller to control which files are returned in the
   * list.  The <code>FilenameFilter</code> specified is called for each
   * file returned to determine whether or not that file should be included
   * in the list.
   * <p>
   * A <code>SecurityManager</code> check is made prior to reading the
   * directory.  If read access to the directory is denied, an exception
   * will be thrown.
   *
   * @param filter An object which will identify files to exclude from 
   * the directory listing.
   *
   * @return An array of files in the directory, or <code>null</code> 
   * if this object does not represent a valid directory.
   * 
   * @exception SecurityException If read access is not allowed to the 
   * directory by the <code>SecurityManager</code>
   */
  public String[] list (FilenameFilter filter)
  {
    checkRead();
    return (String[]) performList (filter, null, String.class);
  }

  /**
   * This method returns a array of <code>String</code>'s representing the
   * list of files is then directory represented by this object.  If this
   * object represents a non-directory file or a non-existent file, then
   * <code>null</code> is returned.  The list of files will not contain
   * any names such as "." or ".." which indicate the current or parent
   * directory.  Also, the names are not guaranteed to be sorted.
   * <p>
   * A <code>SecurityManager</code> check is made prior to reading the
   * directory.  If read access to the directory is denied, an exception
   * will be thrown.
   *
   * @return An array of files in the directory, or <code>null</code> if 
   * this object does not represent a valid directory.
   * 
   * @exception SecurityException If read access is not allowed to the 
   * directory by the <code>SecurityManager</code>
   */
  public String[] list()
  {
    checkRead();
    return (String[]) performList (null, null, String.class);
  }

  /**
   * This method returns an array of <code>File</code> objects representing
   * all the files in the directory represented by this object. If this
   * object does not represent a directory, <code>null</code> is returned.
   * Each of the returned <code>File</code> object is constructed with this
   * object as its parent.
   * <p>
   * A <code>SecurityManager</code> check is made prior to reading the
   * directory.  If read access to the directory is denied, an exception
   * will be thrown.
   *
   * @return An array of <code>File</code> objects for this directory.
   *
   * @exception SecurityException If the <code>SecurityManager</code> denies
   * access to this directory.
   *
   * @since 1.2
   */
  public File[] listFiles()
  {
    checkRead();
    return (File[]) performList (null, null, File.class);
  }
  
  /**
   * This method returns an array of <code>File</code> objects representing
   * all the files in the directory represented by this object. If this
   * object does not represent a directory, <code>null</code> is returned.
   * Each of the returned <code>File</code> object is constructed with this
   * object as its parent.
   * <p> 
   * In this form of the <code>listFiles()</code> method, a filter is specified
   * that allows the caller to control which files are returned in the
   * list.  The <code>FilenameFilter</code> specified is called for each
   * file returned to determine whether or not that file should be included
   * in the list.
   * <p>
   * A <code>SecurityManager</code> check is made prior to reading the
   * directory.  If read access to the directory is denied, an exception
   * will be thrown.
   *
   * @return An array of <code>File</code> objects for this directory.
   *
   * @exception SecurityException If the <code>SecurityManager</code> denies
   * access to this directory.
   *
   * @since 1.2
   */
  public File[] listFiles (FilenameFilter filter)
  {
    checkRead();
    return (File[]) performList (filter, null, File.class);
  }

  /**
   * This method returns an array of <code>File</code> objects representing
   * all the files in the directory represented by this object. If this
   * object does not represent a directory, <code>null</code> is returned.
   * Each of the returned <code>File</code> object is constructed with this
   * object as its parent.
   * <p> 
   * In this form of the <code>listFiles()</code> method, a filter is specified
   * that allows the caller to control which files are returned in the
   * list.  The <code>FileFilter</code> specified is called for each
   * file returned to determine whether or not that file should be included
   * in the list.
   * <p>
   * A <code>SecurityManager</code> check is made prior to reading the
   * directory.  If read access to the directory is denied, an exception
   * will be thrown.
   *
   * @return An array of <code>File</code> objects for this directory.
   *
   * @exception SecurityException If the <code>SecurityManager</code> denies
   * access to this directory.
   *
   * @since 1.2
   */
  public File[] listFiles (FileFilter filter)
  {
    checkRead();
    return (File[]) performList (null, filter, File.class);
  }

  /**
   * This method returns a <code>String</code> that is the path name of the
   * file as returned by <code>getPath</code>.
   *
   * @return A <code>String</code> representation of this file
   */
  public String toString()
  {
    return path;
  }

  /**
   * This method returns a <code>URL</code> with the <code>file:</code>
   * protocol that represents this file.  The exact form of this URL is
   * system dependent.
   *
   * @return A <code>URL</code> for this object.
   *
   * @exception MalformedURLException If the URL cannot be created 
   * successfully.
   */
  public URL toURL() throws MalformedURLException
  {
    // On Win32, Sun's JDK returns URLs of the form "file:/c:/foo/bar.txt",
    // while on UNIX, it returns URLs of the form "file:/foo/bar.txt". 
    if (separatorChar == '\\')
      return new URL ("file:/" + getAbsolutePath().replace ('\\', '/')
		      + (isDirectory() ? "/" : ""));
    else
      return new URL ("file:" + getAbsolutePath()
		      + (isDirectory() ? "/" : ""));
  }

  /*
   * This native method actually creates the directory
   */
  private final native boolean performMkdir();

  /**
   * This method creates a directory for the path represented by this object.
   *
   * @return <code>true</code> if the directory was created, 
   * <code>false</code> otherwise
   *
   * @exception SecurityException If write access is not allowed to this file
   */
  public boolean mkdir()
  {
    checkWrite();
    return performMkdir();
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

  /**
   * This method creates a directory for the path represented by this file.
   * It will also create any intervening parent directories if necessary.
   *
   * @return <code>true</code> if the directory was created, 
   * <code>false</code> otherwise
   *
   * @exception SecurityException If write access is not allowed to this file
   */
  public boolean mkdirs()
  {
    checkWrite();
    if (isDirectory())
      return false;
    return mkdirs (new File (path));
  }

  private static synchronized String nextValue()
  {
    return Long.toString(counter++, Character.MAX_RADIX);
  }

  /**
   * This method creates a temporary file in the specified directory.  If 
   * the directory name is null, then this method uses the system temporary 
   * directory. The files created are guaranteed not to currently exist and 
   * the same file name will never be used twice in the same virtual 
   * machine instance.  
   * The system temporary directory is determined by examinging the 
   * <code>java.io.tmpdir</code> system property.
   * <p>
   * The <code>prefix</code> parameter is a sequence of at least three
   * characters that are used as the start of the generated filename.  The
   * <code>suffix</code> parameter is a sequence of characters that is used
   * to terminate the file name.  This parameter may be <code>null</code>
   * and if it is, the suffix defaults to ".tmp".
   * <p>
   * If a <code>SecurityManager</code> exists, then its <code>checkWrite</code>
   * method is used to verify that this operation is permitted.
   *
   * @param prefix The character prefix to use in generating the path name.
   * @param suffix The character suffix to use in generating the path name.
   * @param directory The directory to create the file in, or 
   * <code>null</code> for the default temporary directory
   *
   * @exception IllegalArgumentException If the patterns is not valid
   * @exception SecurityException If there is no permission to perform 
   * this operation
   * @exception IOException If an error occurs
   *
   * @since 1.2
   */
  public static File createTempFile (String prefix, String suffix,
				     File directory)
    throws IOException
  {
    // Grab the system temp directory if necessary
    if (directory == null)
      {
        String dirname = tmpdir;
        if (dirname == null)
          throw new IOException ("Cannot determine system temporary directory"); 
	
        directory = new File (dirname);
        if (!directory.exists())
          throw new IOException ("System temporary directory "
                                 + directory.getName() + " does not exist.");
        if (!directory.isDirectory())
          throw new IOException ("System temporary directory "
                                 + directory.getName()
                                 + " is not really a directory.");
      }

    // Check if prefix is at least 3 characters long
    if (prefix.length() < 3)
      throw new IllegalArgumentException ("Prefix too short: " + prefix);

    // Set default value of suffix
    if (suffix == null)
      suffix = ".tmp";

    // Truncation rules.
    // `6' is the number of characters we generate.
    if (prefix.length() + 6 + suffix.length() > maxPathLen)
      {
	int suf_len = 0;
	if (suffix.charAt(0) == '.')
	  suf_len = 4;
	suffix = suffix.substring(0, suf_len);
	if (prefix.length() + 6 + suf_len > maxPathLen)
	  prefix = prefix.substring(0, maxPathLen - 6 - suf_len);
      }

    File f;

    // How many times should we try?  We choose 100.
    for (int i = 0; i < 100; ++i)
      {
	// This is ugly.
	String t = "ZZZZZZ" + nextValue();
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

  /*
   * This native method sets the permissions to make the file read only.
   */
  private native boolean performSetReadOnly();

  /**
   * This method sets the file represented by this object to be read only.
   * A read only file or directory cannot be modified.  Please note that 
   * GNU systems allow read only files to be deleted if the directory it
   * is contained in is writable.
   *
   * @return <code>true</code> if the operation succeeded, <code>false</code>
   * otherwise.
   *
   * @exception SecurityException If the <code>SecurityManager</code> does
   * not allow this operation.
   *
   * @since 1.2
   */
  public boolean setReadOnly()
  {
    checkWrite();
    return performSetReadOnly();
  }

  private static native File[] performListRoots();

  /**
   * This method returns an array of filesystem roots.  Some operating systems
   * have volume oriented filesystem.  This method provides a mechanism for
   * determining which volumes exist.  GNU systems use a single hierarchical
   * filesystem, so will have only one "/" filesystem root.
   *
   * @return An array of <code>File</code> objects for each filesystem root
   * available.
   *
   * @since 1.2
   */
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
        	s.checkRead (roots[i].path);		
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

  /**
   * This method creates a temporary file in the system temporary directory. 
   * The files created are guaranteed not to currently exist and the same file
   * name will never be used twice in the same virtual machine instance.  The
   * system temporary directory is determined by examinging the 
   * <code>java.io.tmpdir</code> system property.
   * <p>
   * The <code>prefix</code> parameter is a sequence of at least three
   * characters that are used as the start of the generated filename.  The
   * <code>suffix</code> parameter is a sequence of characters that is used
   * to terminate the file name.  This parameter may be <code>null</code>
   * and if it is, the suffix defaults to ".tmp".
   * <p>
   * If a <code>SecurityManager</code> exists, then its <code>checkWrite</code>
   * method is used to verify that this operation is permitted.
   * <p>
   * This method is identical to calling 
   * <code>createTempFile(prefix, suffix, null)</code>.
   *
   * @param prefix The character prefix to use in generating the path name.
   * @param suffix The character suffix to use in generating the path name.
   *
   * @exception IllegalArgumentException If the prefix or suffix are not valid.
   * @exception SecurityException If there is no permission to perform 
   * this operation
   * @exception IOException If an error occurs
   */
  public static File createTempFile (String prefix, String suffix)
    throws IOException
  {
    return createTempFile (prefix, suffix, null);
  }

  /**
   * This method compares the specified <code>File</code> to this one
   * to test for equality.  It does this by comparing the canonical path names
   * of the files. 
   * <p>
   * The canonical paths of the files are determined by calling the
   * <code>getCanonicalPath</code> method on each object.
   * <p>
   * This method returns a 0 if the specified <code>Object</code> is equal
   * to this one, a negative value if it is less than this one 
   * a positive value if it is greater than this one.
   *
   * @return An integer as described above
   *
   * @since 1.2
   */
  public int compareTo (File other)
  {
    if (caseSensitive)
      return path.compareTo (other.path);
    else
      return path.compareToIgnoreCase (other.path);
  }

  /**
   * This method compares the specified <code>Object</code> to this one
   * to test for equality.  It does this by comparing the canonical path names
   * of the files.  This method is identical to <code>compareTo(File)</code>
   * except that if the <code>Object</code> passed to it is not a 
   * <code>File</code>, it throws a <code>ClassCastException</code>
   * <p>
   * The canonical paths of the files are determined by calling the
   * <code>getCanonicalPath</code> method on each object.
   * <p>
   * This method returns a 0 if the specified <code>Object</code> is equal
   * to this one, a negative value if it is less than this one 
   * a positive value if it is greater than this one.
   *
   * @return An integer as described above
   *
   * @exception ClassCastException If the passed <code>Object</code> is 
   * not a <code>File</code>
   *
   * @since 1.2
   */
  public int compareTo (Object obj)
  {
    return compareTo ((File) obj);
  }

  /*
   * This native method actually performs the rename.
   */
  private native boolean performRenameTo (File dest);

  /**
   * This method renames the file represented by this object to the path
   * of the file represented by the argument <code>File</code>.
   *
   * @param dest The <code>File</code> object representing the target name
   *
   * @return <code>true</code> if the rename succeeds, <code>false</code> 
   * otherwise.
   *
   * @exception SecurityException If write access is not allowed to the 
   * file by the <code>SecurityMananger</code>.
   */
  public synchronized boolean renameTo (File dest)
  {
    SecurityManager s = System.getSecurityManager();
    String sname = getName();
    String dname = dest.getName();
    if (s != null)
      {
	s.checkWrite (sname);
	s.checkWrite (dname);
      }
    return performRenameTo (dest);
  }

  /*
   * This method does the actual setting of the modification time.
   */
  private native boolean performSetLastModified(long time);
 
  /**
   * This method sets the modification time on the file to the specified
   * value.  This is specified as the number of seconds since midnight
   * on January 1, 1970 GMT.
   *
   * @param time The desired modification time.
   *
   * @return <code>true</code> if the operation succeeded, <code>false</code>
   * otherwise.
   *
   * @exception IllegalArgumentException If the specified time is negative.
   * @exception SecurityException If the <code>SecurityManager</code> will
   * not allow this operation.
   *
   * @since 1.2
   */
  public boolean setLastModified (long time) 
  {
    if (time < 0)
      throw new IllegalArgumentException("Negative modification time: " + time);

    checkWrite();
    return performSetLastModified(time);
  }

  private void checkWrite()
  {
    // Check the SecurityManager
    SecurityManager s = System.getSecurityManager();
    
    if (s != null)
      s.checkWrite (path);
  }

  private void checkRead()
  {
    // Check the SecurityManager
    SecurityManager s = System.getSecurityManager();
    
    if (s != null)
      s.checkRead (path);
  }

  /** 
   * Add this File to the set of files to be deleted upon normal
   * termination.
   *
   * @exception SecurityException If deleting of the file is not allowed
   *
   * @since 1.2 
   */
  // FIXME: This should use the ShutdownHook API once we implement that.
  public void deleteOnExit()
  {
    // Check the SecurityManager
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkDelete (getName());

    FileDeleter.add (this);
  }

  private void writeObject (ObjectOutputStream oos) throws IOException
  {
    oos.defaultWriteObject();
    oos.writeChar (separatorChar);
  }

  private void readObject (ObjectInputStream ois)
    throws ClassNotFoundException, IOException
  {
    ois.defaultReadObject();

    // If the file was from an OS with a different dir separator,
    // fixup the path to use the separator on this OS.
    char oldSeparatorChar = ois.readChar();
    
    if (oldSeparatorChar != separatorChar)
      path = path.replace (oldSeparatorChar, separatorChar);
  }
  
} // class File

