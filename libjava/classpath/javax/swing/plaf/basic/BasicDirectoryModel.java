/* BasicDirectoryModel.java --
   Copyright (C) 2005, 2006  Free Software Foundation, Inc.

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
import java.util.Iterator;
import java.util.List;
import java.util.Vector;
import javax.swing.AbstractListModel;
import javax.swing.JFileChooser;
import javax.swing.SwingUtilities;
import javax.swing.event.ListDataEvent;
import javax.swing.filechooser.FileSystemView;


/**
 * Implements an AbstractListModel for directories where the source
 * of the files is a JFileChooser object. 
 *
 * This class is used for sorting and ordering the file list in
 * a JFileChooser L&F object.
 */
public class BasicDirectoryModel extends AbstractListModel
  implements PropertyChangeListener
{
  /** The list of files itself */
  private Vector contents;

  /**
   * The directories in the list.
   */
  private Vector directories;

  /**
   * The files in the list.
   */
  private Vector files;

  /** The listing mode of the associated JFileChooser,
      either FILES_ONLY, DIRECTORIES_ONLY or FILES_AND_DIRECTORIES */
  private int listingMode;

  /** The JFileCooser associated with this model */
  private JFileChooser filechooser;

  /**
   * The thread that loads the file view.
   */
  private DirectoryLoadThread loadThread;

  /**
   * This thread is responsible for loading file lists from the
   * current directory and updating the model.
   */
  private class DirectoryLoadThread extends Thread
  {

    /**
     * Updates the Swing list model.
     */
    private class UpdateSwingRequest
      implements Runnable
    {

      private List added;
      private int addIndex;
      private List removed;
      private int removeIndex;
      private boolean cancel;

      UpdateSwingRequest(List add, int ai, List rem, int ri)
      {
        added = add;
        addIndex = ai;
        removed = rem;
        removeIndex = ri;
        cancel = false;
      }

      public void run()
      {
        if (! cancel)
          {
            int numRemoved = removed == null ? 0 : removed.size();
            int numAdded = added == null ? 0 : added.size();
            synchronized (contents)
              {
                if (numRemoved > 0)
                  contents.removeAll(removed);
                if (numAdded > 0)
                  contents.addAll(added);

                files = null;
                directories = null;
              }
            if (numRemoved > 0 && numAdded == 0)
              fireIntervalRemoved(BasicDirectoryModel.this, removeIndex,
                                  removeIndex + numRemoved - 1);
            else if (numRemoved == 0 && numAdded > 0)
              fireIntervalAdded(BasicDirectoryModel.this, addIndex,
                                addIndex + numAdded - 1);
            else
              fireContentsChanged();
          }
      }

      void cancel()
      {
        cancel = true;
      }
    }

    /**
     * The directory beeing loaded.
     */
    File directory;

    /**
     * Stores all UpdateSwingRequests that are sent to the event queue.
     */
    private UpdateSwingRequest pending;

    /**
     * Creates a new DirectoryLoadThread that loads the specified
     * directory.
     *
     * @param dir the directory to load
     */
    DirectoryLoadThread(File dir)
    {
      super("Basic L&F directory loader");
      directory = dir;
    }

    public void run()
    {
      FileSystemView fsv = filechooser.getFileSystemView();
      File[] files = fsv.getFiles(directory,
                                  filechooser.isFileHidingEnabled());

      // Occasional check if we have been interrupted.
      if (isInterrupted())
        return;

      // Check list for accepted files.
      Vector accepted = new Vector();
      for (int i = 0; i < files.length; i++)
        {
          if (filechooser.accept(files[i]))
            accepted.add(files[i]);
        }
      
      // Occasional check if we have been interrupted.
      if (isInterrupted())
        return;

      // Sort list.
      sort(accepted);

      // Now split up directories from files so that we get the directories
      // listed before the files.
      Vector newFiles = new Vector();
      Vector newDirectories = new Vector();
      for (Iterator i = accepted.iterator(); i.hasNext();)
        {
          File f = (File) i.next();
          boolean traversable = filechooser.isTraversable(f);
          if (traversable)
            newDirectories.add(f);
          else if (! traversable && filechooser.isFileSelectionEnabled())
            newFiles.add(f);

          // Occasional check if we have been interrupted.
          if (isInterrupted())
            return;

        }

      // Build up new file cache. Try to update only the changed elements.
      // This will be important for actions like adding new files or
      // directories inside a large file list.
      Vector newCache = new Vector(newDirectories);
      newCache.addAll(newFiles);

      int newSize = newCache.size();
      int oldSize = contents.size();
      if (newSize < oldSize)
        {
          // Check for removed interval.
          int start = -1;
          int end = -1;
          boolean found = false;
          for (int i = 0; i < newSize && !found; i++)
            {
              if (! newCache.get(i).equals(contents.get(i)))
                {
                  start = i;
                  end = i + oldSize - newSize;
                  found = true;
                }
            }
          if (start >= 0 && end > start
              && contents.subList(end, oldSize)
                                    .equals(newCache.subList(start, newSize)))
            {
              // Occasional check if we have been interrupted.
              if (isInterrupted())
                return;

              Vector removed = new Vector(contents.subList(start, end));
              UpdateSwingRequest r = new UpdateSwingRequest(null, 0,
                                                            removed, start);
              invokeLater(r);
              newCache = null;
            }
        }
      else if (newSize > oldSize)
        {
          // Check for inserted interval.
          int start = oldSize;
          int end = newSize;
          boolean found = false;
          for (int i = 0; i < oldSize && ! found; i++)
            {
              if (! newCache.get(i).equals(contents.get(i)))
                {
                  start = i;
                  boolean foundEnd = false;
                  for (int j = i; j < newSize && ! foundEnd; j++)
                    {
                      if (newCache.get(j).equals(contents.get(i)))
                        {
                          end = j;
                          foundEnd = true;
                        }
                    }
                  end = i + oldSize - newSize;
                }
            }
          if (start >= 0 && end > start
              && newCache.subList(end, newSize)
                                    .equals(contents.subList(start, oldSize)))
            {
              // Occasional check if we have been interrupted.
              if (isInterrupted())
                return;

              List added = newCache.subList(start, end);
              UpdateSwingRequest r = new UpdateSwingRequest(added, start,
                                                            null, 0); 
              invokeLater(r);
              newCache = null;
            }
        }

      // Handle complete list changes (newCache != null).
      if (newCache != null && ! contents.equals(newCache))
        {
          // Occasional check if we have been interrupted.
          if (isInterrupted())
            return;
          UpdateSwingRequest r = new UpdateSwingRequest(newCache, 0,
                                                        contents, 0);
          invokeLater(r);
        }
    }

    /**
     * Wraps SwingUtilities.invokeLater() and stores the request in
     * a Vector so that we can still cancel it later.
     *
     * @param update the request to invoke
     */
    private void invokeLater(UpdateSwingRequest update)
    {
      pending = update;
      SwingUtilities.invokeLater(update);
    }

    /**
     * Cancels all pending update requests that might be in the AWT
     * event queue.
     */
    void cancelPending()
    {
      if (pending != null)
        pending.cancel();
    }
  }

  /** A Comparator class/object for sorting the file list. */
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
    validateFileCache();
  }

  /**
   * Returns whether a given (File) object is included in the list.
   *
   * @param o - The file object to test.
   *
   * @return <code>true</code> if the list contains the given object.
   */
  public boolean contains(Object o)
  {
    return contents.contains(o);
  }

  /**
   * Fires a content change event. 
   */
  public void fireContentsChanged()
  {
    fireContentsChanged(this, 0, getSize() - 1);
  }

  /**
   * Returns a Vector of (java.io.File) objects containing
   * the directories in this list.
   *
   * @return a Vector
   */
  public Vector<File> getDirectories()
  {
    // Synchronize this with the UpdateSwingRequest for the case when
    // contents is modified.
    synchronized (contents)
      {
        Vector dirs = directories;
        if (dirs == null)
          {
            // Initializes this in getFiles().
            getFiles();
            dirs = directories;
          }
        return dirs;
      }
  }

  /**
   * Returns the (java.io.File) object at 
   * an index in the list.
   *
   * @param index The list index
   * @return a File object
   */
  public Object getElementAt(int index)
  {
    if (index > getSize() - 1)
      return null;
    return contents.elementAt(index);
  }

  /**
   * Returns a Vector of (java.io.File) objects containing
   * the files in this list.
   *
   * @return a Vector
   */
  public Vector<File>  getFiles()
  {
    synchronized (contents)
      {
        Vector f = files;
        if (f == null)
          {
            f = new Vector();
            Vector d = new Vector(); // Directories;
            for (Iterator i = contents.iterator(); i.hasNext();)
              {
                File file = (File) i.next();
                if (filechooser.isTraversable(file))
                  d.add(file);
                else
                  f.add(file);
              }
            files = f;
            directories = d;
          }
        return f;
      }
  }

  /**
   * Returns the size of the list, which only includes directories 
   * if the JFileChooser is set to DIRECTORIES_ONLY.
   *
   * Otherwise, both directories and files are included in the count.
   *
   * @return The size of the list.
   */
  public int getSize()
  {
    return contents.size();
  }

  /**
   * Returns the index of an (java.io.File) object in the list.
   *
   * @param o The object - normally a File.
   *
   * @return the index of that object, or -1 if it is not in the list.
   */
  public int indexOf(Object o)
  {
    return contents.indexOf(o);
  }

  /**
   * Obsoleted method which does nothing.
   */
  public void intervalAdded(ListDataEvent e)
  {
    // obsoleted
  }

  /**
   * Obsoleted method which does nothing.
   */
  public void intervalRemoved(ListDataEvent e)
  {
    // obsoleted
  }

  /**
   * Obsoleted method which does nothing.
   */
  public void invalidateFileCache()
  {
    // obsoleted
  }

  /**
   * Less than, determine the relative order in the list of two files
   * for sorting purposes.
   *
   * The order is: directories < files, and thereafter alphabetically,
   * using the default locale collation.
   *
   * @param a the first file
   * @param b the second file
   *
   * @return <code>true</code> if a > b, <code>false</code> if a < b.
   */
  protected boolean lt(File a, File b)
  {
    boolean aTrav = filechooser.isTraversable(a);
    boolean bTrav = filechooser.isTraversable(b);

    if (aTrav == bTrav)
      {
        String aname = a.getName().toLowerCase();
        String bname = b.getName().toLowerCase();
        return (aname.compareTo(bname) < 0) ? true : false;
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
   * Listens for a property change; the change in file selection mode of the
   * associated JFileChooser. Reloads the file cache on that event.
   *
   * @param e - A PropertyChangeEvent.
   */
  public void propertyChange(PropertyChangeEvent e)
  {
    String property = e.getPropertyName();
    if (property.equals(JFileChooser.DIRECTORY_CHANGED_PROPERTY)
        || property.equals(JFileChooser.FILE_FILTER_CHANGED_PROPERTY)
        || property.equals(JFileChooser.FILE_HIDING_CHANGED_PROPERTY)
        || property.equals(JFileChooser.FILE_SELECTION_MODE_CHANGED_PROPERTY)
        || property.equals(JFileChooser.FILE_VIEW_CHANGED_PROPERTY)
        )
      {
	validateFileCache();
      }
  }

  /**
   * Renames a file - However, does <I>not</I> re-sort the list 
   * or replace the old file with the new one in the list.
   *
   * @param oldFile The old file
   * @param newFile The new file name
   *
   * @return <code>true</code> if the rename succeeded
   */
  public boolean renameFile(File oldFile, File newFile)
  {
    return oldFile.renameTo( newFile );
  }

  /**
   * Sorts a Vector of File objects.
   *
   * @param v The Vector to sort.
   */
  protected void sort(Vector<? extends File> v)
  {
    Collections.sort(v, comparator);
  }

  /**
   * Re-loads the list of files
   */
  public void validateFileCache()
  {
    File dir = filechooser.getCurrentDirectory();
    if (dir != null)
      {
        // Cancel all pending requests.
        if (loadThread != null)
          {
            loadThread.interrupt();
            loadThread.cancelPending();
          }
        loadThread = new DirectoryLoadThread(dir);
        loadThread.start();
      }
  }
}

