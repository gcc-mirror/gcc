/* VMFile.java -- Class for methods natively accessing files
   Copyright (C) 2004, 2006  Free Software Foundation, Inc.

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


package java.io;

import java.net.MalformedURLException;
import java.net.URL;

import gnu.classpath.Configuration;
import gnu.java.io.PlatformHelper;


/**
 * @author Michael Koch (konqueror@gmx.de)
 */
final class VMFile
{
  // FIXME: We support only case sensitive filesystems currently.
  static final boolean IS_CASE_SENSITIVE = true;
  static final boolean IS_DOS_8_3 = false;

  static
  {
    if (Configuration.INIT_LOAD_LIBRARY)
      {
	System.loadLibrary("javaio");
      }
  }

  /*
   * This native method does the actual work of getting the last file
   * modification time.  It also does the existence check to avoid the
   * overhead of a call to exists()
   */
  static native long lastModified(String path);

  /*
   * This native method sets the permissions to make the file read only.
   */
  static native boolean setReadOnly(String path);

  /**
   * This method is used to create a temporary file
   */
  static native boolean create(String path) throws IOException;

  /*
   * This native function actually produces the list of file in this
   * directory
   */
  static native String[] list(String dirpath);

  /*
   * This native method actually performs the rename.
   */
  static native boolean renameTo(String targetpath, String destpath);

  /*
   * This native method actually determines the length of the file and
   * handles the existence check
   */
  static native long length(String path);

  /*
   * This native method does the actual checking of file existence.
   */
  static native boolean exists(String path);

  /*
   * This native method handles the actual deleting of the file
   */
  static native boolean delete(String path);

  /*
   * This method does the actual setting of the modification time.
   */
  static native boolean setLastModified(String path, long time);

  /*
   * This native method actually creates the directory
   */
  static native boolean mkdir(String dirpath);

  /**
   * Set the read permission of the file.
   */
  public static synchronized native boolean setReadable(String path,
                                           		boolean readable,
                                           		boolean ownerOnly);
  
  /**
   * Set the write permission of the file.
   */
  public static synchronized native boolean setWritable(String path,
                                                        boolean writable,
                                                        boolean ownerOnly);
  
  /**
   * Set the execute permission of the file.
   */
  public static synchronized native boolean setExecutable(String path,
                                                          boolean executable,
                                                          boolean ownerOnly);
  
  /*
   * This native method does the actual check of whether or not a file
   * is a plain file or not.  It also handles the existence check to
   * eliminate the overhead of a call to exists()
   */
  static native boolean isFile(String path);

  /**
   * This native method checks file permissions for writing
   */
  static synchronized native boolean canWrite(String path);
  
  /**
   * This methods checks if a directory can be written to.
   */
  static boolean canWriteDirectory(File dir)
  {
    try
      {
        String filename = IS_DOS_8_3 ? "tst" : "test-dir-write";
        File test = File.createTempFile(filename, null, dir);
        return (test != null && test.delete());
      }
    catch (IOException ioe)
      {
        return false;
      }
  }

  /**
   * This native method checks file permissions for reading
   */
  static synchronized native boolean canRead(String path);

  /**
   * This native method checks file permissions for execution
   */
  static synchronized native boolean canExecute(String path);
  
  /*
   * This method does the actual check of whether or not a file is a
   * directory or not.  It also handle the existence check to eliminate
   * the overhead of a call to exists()
   */
  static native boolean isDirectory(String dirpath);

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
  static File[] listRoots()
  {
	File[] roots = new File[1];
	roots[0] = new File("/");
	return roots;
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
  static boolean isHidden(String path)
  {
	// FIXME: this only works on UNIX
	return getName(path).startsWith(".");
  }

  /**
   * This method returns the name of the file.  This is everything in the
   * complete path of the file after the last instance of the separator
   * string.
   *
   * @return The file name
   */
  static String getName(String path)
  {
	int pos = PlatformHelper.lastIndexOfSeparator(path);
	if (pos == -1)
	  return path;
	
	if (PlatformHelper.endWithSeparator(path))
	  return "";
	
	return path.substring(pos + File.separator.length());
  }

  /**
   * Returns the path as an absolute path name. The value returned is the
   * current directory plus the separatory string plus the path of the file.
   * The current directory is determined from the <code>user.dir</code> system
   * property.
   *
   * @param path the path to convert to absolute path
   *
   * @return the absolute path that corresponds to <code>path</code>
   */
  static String getAbsolutePath(String path)
  {
    if (File.separatorChar == '\\' 
      && path.length() > 0 && path.charAt (0) == '\\')
      {
        // On Windows, even if the path starts with a '\\' it is not
        // really absolute until we prefix the drive specifier from
        // the current working directory to it.
        return System.getProperty ("user.dir").substring (0, 2) + path;
      }
    else if (File.separatorChar == '\\'
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
    else if (path.equals(""))
      return System.getProperty ("user.dir");
    else
      return System.getProperty ("user.dir") + File.separatorChar + path;
  }

  /**
   * This method returns true if the path represents an absolute file
   * path and false if it does not.  The definition of an absolute path varies
   * by system.  As an example, on GNU systems, a path is absolute if it starts
   * with a "/".
   *
   * @param path the path to check
   *
   * @return <code>true</code> if path represents an absolute file name,
   *         <code>false</code> otherwise.
   */
  static boolean isAbsolute(String path)
  {
    if (File.separatorChar == '\\')
        return path.startsWith(File.separator + File.separator)
               || (path.length() > 2
                   && ((path.charAt(0) >= 'a' && path.charAt(0) <= 'z')
                       || (path.charAt(0) >= 'A' && path.charAt(0) <= 'Z'))
                       && path.charAt(1) == ':'
                       && path.charAt(2) == '\\');
    else
      return path.startsWith(File.separator);
  }

  /**
   * Returns a <code>URL</code> with the <code>file:</code>
   * protocol that represents this file.  The exact form of this URL is
   * system dependent.
   *
   * @param file the file to convert to URL
   *
   * @return a <code>URL</code> for this object.
   *
   * @throws MalformedURLException if the URL cannot be created
   *         successfully.
   */
  static URL toURL(File file)
    throws MalformedURLException
  {
    // On Win32, Sun's JDK returns URLs of the form "file:/c:/foo/bar.txt",
    // while on UNIX, it returns URLs of the form "file:/foo/bar.txt". 
    if (File.separatorChar == '\\')
      return new URL ("file:/" + file.getAbsolutePath().replace ('\\', '/')
                      + (file.isDirectory() ? "/" : ""));
    else
      return new URL ("file:" + file.getAbsolutePath()
                      + (file.isDirectory() ? "/" : ""));
  }

   /**
   * This method returns a canonical representation of the pathname of
   * this file.  The actual form of the canonical representation is
   * system-dependent.  On the GNU system, conversion to canonical
   * form involves the removal of redundant separators, references to
   * "." and "..", and symbolic links.
   * <p>
   * Note that this method, unlike the other methods which return path
   * names, can throw an IOException.  This is because native method 
   * might be required in order to resolve the canonical path
   *
   * @exception IOException If an error occurs
   */
  public static native String toCanonicalForm(String path) throws IOException;
}
