/* GtkFileDialogPeer.java -- Implements FileDialogPeer with GTK
   Copyright (C) 1998, 1999, 2002 Free Software Foundation, Inc.

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


package gnu.java.awt.peer.gtk;

import java.awt.AWTEvent;
import java.awt.Dialog;
import java.awt.FileDialog;
import java.awt.Graphics;
import java.awt.event.WindowEvent;
import java.awt.peer.FileDialogPeer;
import java.io.FilenameFilter;

public class GtkFileDialogPeer extends GtkDialogPeer implements FileDialogPeer
{
  static final String FS = System.getProperty("file.separator");
  
  private String currentFile = null;
  private String currentDirectory = null;

  native void create ();

  public GtkFileDialogPeer (FileDialog fd)
  {
    super (fd);
  }

  native void connectJObject ();
  native void connectSignals ();
  native void nativeSetFile (String file);

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

    // Remove any directory path from the filename
    int sepIndex = fileName.lastIndexOf (FS);
    if (sepIndex < 0)
      {
        currentFile = fileName;
        nativeSetFile (fileName);
      }
    else
      {
        if (fileName.length() > (sepIndex + 1))
	  {
	    String fn = fileName.substring (sepIndex + 1);
            currentFile = fn;
            nativeSetFile (fn);
	  }
	else
	  {
            currentFile = "";
            nativeSetFile ("");
	  }
      }
  }

  public void setDirectory (String directory)
  {
    /* If nothing changed so nothing.  This usually happens because
       the only way we have to set the directory in FileDialog is by
       calling its setDirectory which will call us back. */
    if ((directory == null && currentDirectory == null)
        || (directory != null && directory.equals (currentDirectory)))
      return;

    if (directory == null || directory.equals (""))
      {
        currentDirectory = FS;
        nativeSetFile (FS);
	return;
      }
      
    currentDirectory = directory;

    // Gtk expects the directory to end with a file separator
    if (directory.substring (directory.length () - 1).equals (FS))
      nativeSetFile (directory);
    else
      nativeSetFile (directory + FS);
  }

  public void setFilenameFilter (FilenameFilter filter)
  {
    /* GTK has no filter callbacks yet.  It works by setting a pattern
     * (see gtk_file_selection_complete), which we can't convert
     * to the callback paradigm. With GTK-2.4 there will be a
     * gtk_file_filter_add_custom function that we can use. */
  }

  public Graphics getGraphics ()
  {
    // GtkFileDialog will repaint by itself
    return null;
  }
  
  void gtkHideFileDialog () 
  {
    ((Dialog) awtComponent).hide();
  }
  
  void gtkDisposeFileDialog () 
  {
    ((Dialog) awtComponent).dispose();
  }

  /* Callback to set the file and directory values when the user is finished
   * with the dialog.
   */
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
