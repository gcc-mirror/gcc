/* BasicDirectoryModel.java --
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

package javax.swing.plaf.basic;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.util.Collections;
import java.util.Comparator;
import java.util.Enumeration;
import java.util.Vector;
import javax.swing.AbstractListModel;
import javax.swing.JFileChooser;
import javax.swing.event.ListDataEvent;
import javax.swing.filechooser.FileSystemView;


/**
 * DOCUMENT ME!
 */
public class BasicDirectoryModel extends AbstractListModel
  implements PropertyChangeListener
{
  /** DOCUMENT ME! */
  private Vector contents;

  /** DOCUMENT ME! */
  private int directories;

  /** DOCUMENT ME! */
  private int listingMode;

  /** DOCUMENT ME! */
  private JFileChooser filechooser;

  /** DOCUMENT ME! */
  private Comparator comparator = new Comparator()
    {
      public int compare(Object o1, Object o2)
      {
	if (lt((File) o1, (File) o2))
	  return -1;
	else
	  return 1;
      }
    };

  /**
   * Creates a new BasicDirectoryModel object.
   *
   * @param filechooser DOCUMENT ME!
   */
  public BasicDirectoryModel(JFileChooser filechooser)
  {
    this.filechooser = filechooser;
    filechooser.addPropertyChangeListener(this);
    listingMode = filechooser.getFileSelectionMode();
    contents = new Vector();
  }

  /**
   * DOCUMENT ME!
   *
   * @param o DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public boolean contains(Object o)
  {
    return contents.contains(o);
  }

  /**
   * DOCUMENT ME!
   */
  public void fireContentsChanged()
  {
    fireContentsChanged(this, 0, getSize() - 1);
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public Vector getDirectories()
  {
    Vector tmp = new Vector();
    for (int i = 0; i < directories; i++)
      tmp.add(contents.get(i));
    return tmp;
  }

  /**
   * DOCUMENT ME!
   *
   * @param index DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public Object getElementAt(int index)
  {
    if (index > getSize() - 1)
      return null;
    if (listingMode == JFileChooser.FILES_ONLY)
      return contents.get(directories + index);
    else
      return contents.elementAt(index);
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public Vector getFiles()
  {
    Vector tmp = new Vector();
    for (int i = directories; i < getSize(); i++)
      tmp.add(contents.get(i));
    return tmp;
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public int getSize()
  {
    if (listingMode == JFileChooser.DIRECTORIES_ONLY)
      return directories;
    else if (listingMode == JFileChooser.FILES_ONLY)
      return contents.size() - directories;
    return contents.size();
  }

  /**
   * DOCUMENT ME!
   *
   * @param o DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public int indexOf(Object o)
  {
    if (listingMode == JFileChooser.FILES_ONLY)
      return contents.indexOf(o) - directories;
    return contents.indexOf(o);
  }

  /**
   * DOCUMENT ME!
   *
   * @param e DOCUMENT ME!
   */
  public void intervalAdded(ListDataEvent e)
  {
    // obsoleted
  }

  /**
   * DOCUMENT ME!
   *
   * @param e DOCUMENT ME!
   */
  public void intervalRemoved(ListDataEvent e)
  {
    // obsoleted
  }

  /**
   * DOCUMENT ME!
   */
  public void invalidateFileCache()
  {
    // obsoleted
  }

  /**
   * DOCUMENT ME!
   *
   * @param a DOCUMENT ME!
   * @param b DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  protected boolean lt(File a, File b)
  {
    boolean aTrav = filechooser.isTraversable(a);
    boolean bTrav = filechooser.isTraversable(b);

    if (aTrav == bTrav)
      {
	String aname = a.getName().toLowerCase();
	String bname = b.getName().toLowerCase();
	return ((aname.compareTo(bname) < 0) ? true : false);
      }
    else
      {
	if (aTrav)
	  return true;
	else
	  return false;
      }
  }

  /**
   * DOCUMENT ME!
   *
   * @param e DOCUMENT ME!
   */
  public void propertyChange(PropertyChangeEvent e)
  {
    if (e.getPropertyName().equals(JFileChooser.FILE_SELECTION_MODE_CHANGED_PROPERTY))
      listingMode = filechooser.getFileSelectionMode();
  }

  /**
   * DOCUMENT ME!
   *
   * @param oldFile DOCUMENT ME!
   * @param newFile DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public boolean renameFile(File oldFile, File newFile)
  {
    // FIXME: implement
    return false;
  }

  /**
   * DOCUMENT ME!
   *
   * @param v DOCUMENT ME!
   */
  protected void sort(Vector v)
  {
    Collections.sort(v, comparator);
    Enumeration e = Collections.enumeration(v);
    Vector tmp = new Vector();
    for (; e.hasMoreElements();)
      tmp.add(e.nextElement());

    contents = tmp;
  }

  /**
   * DOCUMENT ME!
   */
  public void validateFileCache()
  {
    contents.clear();
    directories = 0;
    FileSystemView fsv = filechooser.getFileSystemView();
    File[] list = fsv.getFiles(filechooser.getCurrentDirectory(),
                               filechooser.isFileHidingEnabled());

    if (list == null)
      return;

    for (int i = 0; i < list.length; i++)
      {
	if (list[i] == null)
	  continue;
	if (filechooser.accept(list[i]))
	  {
	    contents.add(list[i]);
	    if (filechooser.isTraversable(list[i]))
	      directories++;
	  }
      }
    sort(contents);
    filechooser.revalidate();
    filechooser.repaint();
  }
}
