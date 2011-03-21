/* UnixFileSystemView.java --
   Copyright (C) 2005  Free Software Foundation, Inc.

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

package javax.swing.filechooser;

import gnu.classpath.NotImplementedException;

import java.io.File;
import java.io.IOException;

import javax.swing.Icon;


/**
 * A concrete implementation of {@link FileSystemView} that is appropriate for
 * Unix-like systems.
 *
 * @see FileSystemView#getFileSystemView()
 */
class UnixFileSystemView extends FileSystemView
{
  /** The default name for new folders. */
  private static final String NEW_FOLDER_NAME = "NewFolder";

  /**
   * Creates a new folder with a unique name in the specified directory and
   * returns a {@link File} object representing the new directory.  The name
   * of the new folder is <code>NewFolder</code> or, if a directory or file
   * with that name already exists, <code>NewFolder.n</code> where
   * <code>n</code> is the lowest integer greater than zero that results in
   * a unique directory name.
   *
   * @param containingDir  the directory to contain the new folder
   *                       (<code>null</code> not permitted).
   *
   * @return A {@link File} object representing the new directory.
   *
   * @throws IOException if an exception occurs while creating the new
   *                     directory.
   */
  public File createNewFolder(File containingDir) throws IOException
  {
    int count = 0;
    File f = null;
    String filename = containingDir.getAbsolutePath() + File.separator
                      + NEW_FOLDER_NAME;
    while (f == null)
      {
        String full = filename;
        if (count > 0)
          full += "." + (count++);
        f = new File(full);
        if (f.isDirectory() || f.isFile())
          {
            count++;
            f = null;
          }
      }
    f.mkdir();
    return f;
  }

  /**
   * Returns an array containing the file system root.
   *
   * @return An array containing the file system root.
   */
  public File[] getRoots()
  {
    return File.listRoots();
  }

  /**
   * Returns the name of a file as it would be displayed by the underlying
   * system.
   *
   * @param f  the file.
   *
   * @return the name of a file as it would be displayed by the underlying
   *         system
   */
  public String getSystemDisplayName(File f)
  {
    String name = null;
    if (f != null)
      {
        if (isRoot(f))
          name = f.getAbsolutePath();
        else
          {
            try
              {
                String path = f.getCanonicalPath();
                name = path.substring(path.lastIndexOf(File.separator) + 1);
              }
            catch (IOException e)
              {
                name = f.getName();
              }
          }
      }
    return name;
  }

  /**
   * Returns the icon that would be displayed for the given file by the
   * underlying system.  This method is NOT YET IMPLEMENTED.
   *
   * @param f  the file.
   *
   * @return <code>null</code>.
   */
  public Icon getSystemIcon(File f)
    throws NotImplementedException
  {
    // FIXME: Implement;
    return null;
  }

  /**
   * Returns the description of a file that would be displayed by the
   * underlying system.  This method is NOT YET IMPLEMENTED.
   *
   * @param f  the file.
   *
   * @return <code>null</code>.
   */
  public String getSystemTypeDescription(File f)
    throws NotImplementedException
  {
    // FIXME: Implement.
    return null;
  }

  /**
   * DOCUMENT ME!
   *
   * @param f DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public boolean isRoot(File f)
  {
    return isFileSystemRoot(f);
  }
}
