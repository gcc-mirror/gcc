/* GtkFileDialogPeer.java -- Implements FileDialogPeer with GTK
   Copyright (C) 1998, 1999, 2002, 2004, 2005  Free Software Foundation, Inc.

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


package gnu.java.awt.peer.gtk;

import java.awt.Dialog;
import java.awt.FileDialog;
import java.awt.event.PaintEvent;
import java.awt.peer.FileDialogPeer;
import java.io.File;
import java.io.FilenameFilter;

public class GtkFileDialogPeer extends GtkDialogPeer implements FileDialogPeer
{
  static final String FS = System.getProperty("file.separator");

  private String currentFile = null;
  private String currentDirectory = null;
  private FilenameFilter filter;

  native void create (GtkContainerPeer parent, int mode);
  native void connectSignals ();
  native void nativeSetFile (String file);
  public native String nativeGetDirectory();
  public native void nativeSetDirectory(String directory);
  native void nativeSetFilenameFilter (FilenameFilter filter);

  public void create()
  {
    create((GtkContainerPeer) awtComponent.getParent().getPeer(),
           ((FileDialog) awtComponent).getMode());

    FileDialog fd = (FileDialog) awtComponent;

    nativeSetDirectory(System.getProperty("user.dir"));
    setDirectory(fd.getDirectory());
    setFile(fd.getFile());

    FilenameFilter filter = fd.getFilenameFilter();
    if (filter != null)
      setFilenameFilter(filter);
  }

  public GtkFileDialogPeer (FileDialog fd)
  {
    super (fd);
  }

  void setComponentBounds ()
  {
    if (awtComponent.getHeight () == 0
        && awtComponent.getWidth () == 0)
      {
        int[] dims = new int[2];
        gtkWidgetGetPreferredDimensions (dims);

        if (dims[0] != awtComponent.getWidth()
            || dims[1] != awtComponent.getHeight())
          awtComponent.setSize(dims[0], dims[1]);
      }
    super.setComponentBounds ();
  }

  public void setFile (String fileName)
  {
    /* If nothing changed do nothing.  This usually happens because
       the only way we have to set the file name in FileDialog is by
       calling its SetFile which will call us back. */
    if ((fileName == null && currentFile == null)
        || (fileName != null && fileName.equals (currentFile)))
      return;

    if (fileName == null || fileName.equals (""))
      {
        currentFile = "";
        nativeSetFile ("");
        return;
      }

    // GtkFileChooser requires absolute filenames. If the given filename
    // is not absolute, let's construct it based on current directory.
    currentFile = fileName;
    if (fileName.indexOf(FS) == 0)
      nativeSetFile(fileName);
    else
      nativeSetFile(nativeGetDirectory() + FS + fileName);
  }

  public void setDirectory (String directory)
  {
    /* If nothing changed so nothing.  This usually happens because
       the only way we have to set the directory in FileDialog is by
       calling its setDirectory which will call us back. */
    if ((directory == null && currentDirectory == null)
        || (directory != null && directory.equals(currentDirectory)))
      return;

    if (directory == null || directory.equals(""))
      {
        currentDirectory = FS;
        nativeSetDirectory(FS);
        return;
      }

    // GtkFileChooser requires absolute directory names. If the given directory
    // name is not absolute, construct it based on current directory if it is not
    // null. Otherwise, use FS.
    currentDirectory = directory;
    if (directory.indexOf(FS) == 0)
      nativeSetDirectory(directory);
    else
      nativeSetDirectory(nativeGetDirectory() + FS + directory);
  }

  public void setFilenameFilter (FilenameFilter filter)
  {
    this.filter = filter;
    nativeSetFilenameFilter(filter);
  }

  /* This method interacts with the native callback function of the
     same name.  The native function will extract the filename from the
     GtkFileFilterInfo object and send it to this method, which will
     in turn call the filter's accept() method and give back the return
     value. */
  // called back by native side: filename_filter_cb
  boolean filenameFilterCallback (String fullname)
  {
    String filename = fullname.substring(fullname.lastIndexOf(FS) + 1);
    String dirname = fullname.substring(0, fullname.lastIndexOf(FS));
    File dir = new File(dirname);
    return filter.accept(dir, filename);
  }

  // Sun does not call FileDialog.update.
  protected void updateComponent (PaintEvent event)
  {
    // Override GtkComponetPeer.updateComponent to do nothing.
  }

  // called back by native side: handle_response_cb
  // only called from the GTK thread
  void gtkHideFileDialog ()
  {
    // hide calls back the peer's setVisible method, so locking is a
    // problem.
    ((Dialog) awtComponent).hide();
  }

  // called back by native side: handle_response_cb
  void gtkDisposeFileDialog ()
  {
    ((Dialog) awtComponent).dispose();
  }

  // Callback to set the file and directory values when the user is finished
  // with the dialog.
  // called back by native side: handle_response_cb
  void gtkSetFilename (String fileName)
  {
    FileDialog fd = (FileDialog) awtWidget;
    if (fileName == null)
      {
        currentFile = null;
        fd.setFile(null);
        return;
      }

    int sepIndex = fileName.lastIndexOf (FS);
    if (sepIndex < 0)
      {
        /* This should never happen on Unix (all paths start with '/') */
        currentFile = fileName;
      }
    else
      {
        if (fileName.length() > (sepIndex + 1))
          {
            String fn = fileName.substring (sepIndex + 1);
            currentFile = fn;
          }
        else
          {
            currentFile = null;
          }

        String dn = fileName.substring (0, sepIndex + 1);
        currentDirectory = dn;
        fd.setDirectory(dn);
      }

    fd.setFile (currentFile);
  }
}
