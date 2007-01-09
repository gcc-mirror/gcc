/* FileSystemView.java --
   Copyright (C) 2002, 2004, 2005  Free Software Foundation, Inc.

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

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

import javax.swing.Icon;
import javax.swing.JFileChooser;


/**
 * The base class providing a view of the file system for use by the 
 * {@link JFileChooser} component.
 */
public abstract class FileSystemView
{
  /** The instance returned by {@link #getFileSystemView()}. */
  private static FileSystemView defaultFileSystemView;
  
  /**
   * Creates a new file object with the given name in the specified directory.
   *
   * @param dir  the directory (<code>null</code> permitted).
   * @param filename  the file name.
   *
   * @return A new file object.
   */
  public File createFileObject(File dir, String filename)
  {
    return new File(dir, filename);
  }

  /**
   * Creates a new file object from the specified path.
   *
   * @param path  the path.
   *
   * @return A new file object.
   */
  public File createFileObject(String path)
  {
    File f = new File(path);
    if (isFileSystemRoot(f))
      f = this.createFileSystemRoot(f);
    return f;
  }

  /**
   * DOCUMENT ME!
   *
   * @param f DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  protected File createFileSystemRoot(File f)
  {
    File[] roots = File.listRoots();
    if (roots == null)
      return null;
    return roots[0];
  }

  /**
   * Creates a new folder with a unique name in the specified directory and
   * returns a {@link File} object representing the new directory.
   *
   * @param containingDir  the directory to contain the new folder 
   *                       (<code>null</code> not permitted).
   *
   * @return A {@link File} object representing the new directory.
   *
   * @throws IOException if an exception occurs while creating the new 
   *                     directory.
   */
  public abstract File createNewFolder(File containingDir)
                                throws IOException;

  /**
   * DOCUMENT ME!
   *
   * @param parent DOCUMENT ME!
   * @param fileName DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public File getChild(File parent, String fileName)
  {
    // FIXME: Handle the case when parent and child are special folders.
    return new File(parent, fileName);
  }

  /**
   * Returns the default directory.
   *
   * @return The default directory.
   */
  public File getDefaultDirectory()
  {
    return getHomeDirectory();
  }

  /**
   * Returns an array containing the files in the given directory.  The 
   * <code>useFileHiding</code> controls whether or not hidden files are 
   * included in the result.
   *
   * @param dir  the directory (if <code>null</code>
   * @param useFileHiding  a flag that controls whether or not hidden files are
   *                       included in the result (pass in <code>true</code> to
   *                       exclude hidden files).
   *
   * @return The files in the given directory (possibly <code>null</code>).
   */
  public File[] getFiles(File dir, boolean useFileHiding)
  {
    if (dir == null || dir.listFiles() == null)
      return null;
    File[] files = dir.listFiles();
    if (! useFileHiding)
      return files;
    ArrayList trim = new ArrayList();
    for (int i = 0; i < files.length; i++)
      if (! files[i].isHidden())
	trim.add(files[i]);
    File[] value = (File[]) trim.toArray(new File[trim.size()]);
    return value;
  }

  /**
   * Returns a default {@link FileSystemView} appropriate for the platform.
   *
   * @return A default {@link FileSystemView} appropriate for the platform.
   */
  public static FileSystemView getFileSystemView()
  {
    if (defaultFileSystemView == null)
      {
        // FIXME: We need to support other file systems too.
        defaultFileSystemView = new UnixFileSystemView();
      }
    return defaultFileSystemView;
  }

  /**
   * Returns the home directory for the current user.
   *
   * @return The home directory for the current user.
   */
  public File getHomeDirectory()
  {
    return createFileObject(System.getProperty("user.home"));
  }

  /**
   * Returns the parent directory for the given file/directory.
   *
   * @param f  the file/directory.
   *
   * @return The parent directory (or <code>null</code> if there is no parent
   *         directory).
   */
  public File getParentDirectory(File f)
  {
    if (f == null)
      return null;
    return f.getParentFile();
  }

  /**
   * Returns an array containing the file system roots.  On Unix-like platforms,
   * this array will contain just a single item ("/"), while other platforms
   * may return multiple roots.
   * <p>
   * This method is implemented to return <code>null</code>, subclasses must
   * override this method.
   *
   * @return An array containing the file system roots.
   */
  public File[] getRoots()
  {
    // subclass
    return null;
  }

  /**
   * Returns the name of a file as it would be displayed by the underlying 
   * system.
   *
   * @param f  the file.
   *
   * @return the name of a file as it would be displayed by the underlying 
   *         system
   *
   * @specnote The specification suggests that the information here is
   *           fetched from a ShellFolder class. This seems to be a non public
   *           private file handling class. We simply return File.getName()
   *           here and leave special handling to subclasses.
   */
  public String getSystemDisplayName(File f)
  {
    String name = null;
    if (f != null)
      name = f.getName();
    return name;
  }

  /**
   * Returns the icon that would be displayed for the given file by the 
   * underlying system.  This implementation returns <code>null</code>, 
   * subclasses must override.
   *
   * @param f  the file.
   *
   * @return <code>null</code>.
   */
  public Icon getSystemIcon(File f)
  {
    return null;
  }

  /**
   * Returns the type description of a file that would be displayed by the 
   * underlying system.  This implementation returns <code>null</code>, 
   * subclasses must override.
   *
   * @param f  the file.
   *
   * @return <code>null</code>.
   */
  public String getSystemTypeDescription(File f)
  {
    return null;
  }

  /**
   * DOCUMENT ME!
   *
   * @param dir DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public boolean isComputerNode(File dir)
  {
    return false;
  }

  /**
   * Returns <code>true</code> if the given directory represents a disk 
   * drive, and <code>false</code> otherwise.  This default implementation
   * always returns <code>false</code>.
   *
   * @param dir  the directory.
   *
   * @return <code>false</code>.
   */
  public boolean isDrive(File dir)
  {
    return false;
  }

  /**
   * Returns <code>true</code> if <code>f</code> is a file or directory, and
   * <code>false</code> otherwise.
   *
   * @param f  the file/directory.
   *
   * @return <code>true</code> if <code>f</code> is a file or directory, and
   * <code>false</code> otherwise.
   */
  public boolean isFileSystem(File f)
  {
    return (f.isFile() || f.isDirectory());
  }

  /**
   * Returns <code>true</code> if the given directory is a file system root,
   * and <code>false</code> otherwise.
   *
   * @param dir  the directory.
   *
   * @return <code>true</code> if the given directory is a file system root,
   *          and <code>false</code> otherwise.
   */
  public boolean isFileSystemRoot(File dir)
  {
    File[] roots = File.listRoots();
    if (roots == null || dir == null)
      return false;
    String filename = dir.getAbsolutePath();
    for (int i = 0; i < roots.length; i++)
      if (roots[i].getAbsolutePath().equals(filename))
	return true;
    return false;
  }

  /**
   * Returns <code>true</code> if the given directory represents a floppy 
   * drive, and <code>false</code> otherwise.  This default implementation
   * always returns <code>false</code>.
   *
   * @param dir  the directory.
   *
   * @return <code>false</code>.
   */
  public boolean isFloppyDrive(File dir)
  {
    return false;
  }

  /**
   * Returns <code>true</code> if the given file is hidden, and 
   * <code>false</code> otherwise.
   *
   * @param f  the file.
   *
   * @return <code>true</code> if the given file is hidden, and 
   *         <code>false</code> otherwise.
   */
  public boolean isHiddenFile(File f)
  {
    return f.isHidden();
  }

  /**
   * Returns <code>true</code> if <code>folder</code> is the parent of 
   * <code>file</code>, and <code>false</code> otherwise.
   *
   * @param folder  the folder (<code>null</code> not permitted).
   * @param file  the file (<code>null</code> not permitted).
   *
   * @return <code>true</code> if <code>folder</code> is the parent of 
   *         <code>file</code>, and <code>false</code> otherwise.
   */
  public boolean isParent(File folder, File file)
  {
    File parent = file.getParentFile();
    if (parent == null)
      return false;
    return folder.equals(parent);
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
    // These are not file system roots.
    return false;
  }

  /**
   * Returns <code>true</code> if the file is traversable, and 
   * <code>false</code> otherwise.  Here, all directories are considered
   * traversable, and files are considered non-traversable. 
   *
   * @param f  the file or directory (<code>null</code> not permitted).
   *
   * @return <code>true</code> if the file is traversable, and 
   *         <code>false</code> otherwise.
   */
  public Boolean isTraversable(File f)
  {
    // Tested. A directory where the user has no permission to rwx is still
    // traversable. (No files are listed when you traverse the directory)
    // My best guess is that as long as it's a directory, the file is
    // traversable.
    return Boolean.valueOf(f.isDirectory());
  }
}
