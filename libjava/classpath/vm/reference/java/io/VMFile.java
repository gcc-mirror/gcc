/* VMFile.java -- Class for methods natively accessing files
   Copyright (C) 2004  Free Software Foundation, Inc.

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
   * This method returns a canonical representation of the pathname of
   * the given path.  The actual form of the canonical representation is
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
  public static String toCanonicalForm(String path) throws IOException
  {
	// FIXME: this only works on UNIX
	return PlatformHelper.toCanonicalForm(path);
  }
}
