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


/**
 * DOCUMENT ME!
 */
public abstract class FileSystemView
{
  /**
   * DOCUMENT ME!
   *
   * @param dir DOCUMENT ME!
   * @param filename DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public File createFileObject(File dir, String filename)
  {
    return new File(dir, filename);
  }

  /**
   * DOCUMENT ME!
   *
   * @param path DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public File createFileObject(String path)
  {
    return new File(path);
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
   * DOCUMENT ME!
   *
   * @param containingDir DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   *
   * @throws IOException DOCUMENT ME!
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
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public File getDefaultDirectory()
  {
    return getHomeDirectory();
  }

  /**
   * DOCUMENT ME!
   *
   * @param dir DOCUMENT ME!
   * @param useFileHiding DOCUMENT ME!
   *
   * @return DOCUMENT ME!
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
    File[] value = (File[]) trim.toArray(new File[0]);
    return value;
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public static FileSystemView getFileSystemView()
  {
    if (File.separator.equals("/"))
      return new UnixFileSystemView();

    // else if (File.Separator.equals("\"))
    //	return new Win32FileSystemView();
    // else 
    //	return new GenericFileSystemView();
    return null;
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public File getHomeDirectory()
  {
    return createFileObject(System.getProperty("user.home"));
  }

  /**
   * DOCUMENT ME!
   *
   * @param f DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public File getParentDirectory(File f)
  {
    if (f == null)
      return null;
    return f.getParentFile();
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public File[] getRoots()
  {
    // subclass
    return null;
  }

  /**
   * DOCUMENT ME!
   *
   * @param f DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public String getSystemDisplayName(File f)
  {
    return null;
  }

  /**
   * DOCUMENT ME!
   *
   * @param f DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public Icon getSystemIcon(File f)
  {
    return null;
  }

  /**
   * DOCUMENT ME!
   *
   * @param f DOCUMENT ME!
   *
   * @return DOCUMENT ME!
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
   * DOCUMENT ME!
   *
   * @param dir DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public boolean isDrive(File dir)
  {
    return false;
  }

  /**
   * DOCUMENT ME!
   *
   * @param f DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public boolean isFileSystem(File f)
  {
    return (f.isFile() || f.isDirectory());
  }

  /**
   * DOCUMENT ME!
   *
   * @param dir DOCUMENT ME!
   *
   * @return DOCUMENT ME!
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
   * DOCUMENT ME!
   *
   * @param dir DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public boolean isFloppyDrive(File dir)
  {
    return false;
  }

  /**
   * DOCUMENT ME!
   *
   * @param f DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public boolean isHiddenFile(File f)
  {
    return f.isHidden();
  }

  /**
   * DOCUMENT ME!
   *
   * @param folder DOCUMENT ME!
   * @param file DOCUMENT ME!
   *
   * @return DOCUMENT ME!
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
   * DOCUMENT ME!
   *
   * @param f DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public Boolean isTraversable(File f)
  {
    // Tested. A directory where the user has no permission to rwx is still
    // traversable. (No files are listed when you traverse the directory)
    // My best guess is that as long as it's a directory, the file is
    // traversable.
    return new Boolean(f.isDirectory());
  }
}
