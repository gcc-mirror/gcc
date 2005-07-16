/* FileDialog.java -- A filename selection dialog box
   Copyright (C) 1999, 2000, 2001, 2002, 2004, 2005  Free Software Foundation, Inc.

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


package java.awt;

import java.awt.peer.FileDialogPeer;
import java.io.FilenameFilter;
import java.io.Serializable;

/**
  * This class implements a file selection dialog box widget.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  * @author Tom Tromey (tromey@redhat.com)
  */
public class FileDialog extends Dialog implements Serializable
{

/*
 * Static Variables
 */

/**
  * Indicates that the purpose of the dialog is for opening a file.
  */
public static final int LOAD = 0;

/**
  * Indicates that the purpose of the dialog is for saving a file.
  */
public static final int SAVE = 1;

// Serialization constant
private static final long serialVersionUID = 5035145889651310422L;

/*************************************************************************/

/*
 * Instance Variables
 */

/**
  * @serial The directory for this file dialog.
  */
private String dir;

/**
  * @serial The filename for this file dialog
  */
private String file;

/**
  * @serial The filter for selecting filenames to display
  */
private FilenameFilter filter;

/**
  * @serial The mode of this dialog, either <code>LOAD</code> or 
  * <code>SAVE</code>.
  */
private int mode;

/*************************************************************************/

/*
 * Constructors
 */

/**
  * Initializes a new instance of <code>FileDialog</code> with the 
  * specified parent.  This dialog will have no title and will be for
  * loading a file.
  *
  * @param parent The parent frame for this dialog.
  */
public
FileDialog(Frame parent)
{
  this(parent, "", LOAD);
}

/*************************************************************************/

/**
  * Initialized a new instance of <code>FileDialog</code> with the
  * specified parent and title.  This dialog will be for opening a file.
  *
  * @param parent The parent frame for this dialog.
  * @param title The title for this dialog.
  */
public
FileDialog(Frame parent, String title)
{
  this(parent, title, LOAD);
}

/*************************************************************************/

/**
  * Initialized a new instance of <code>FileDialog</code> with the
  * specified parent, title, and mode.
  *
  * @param parent The parent frame for this dialog.
  * @param title The title for this dialog.
  * @param mode The mode of the dialog, either <code>LOAD</code> or
  * <code>SAVE</code>.
  *
  * @exception IllegalArgumentException If an illegal file dialog mode
  * is supplied.
  */
public
FileDialog(Frame parent, String title, int mode)
{
  super(parent, title, true);
  
  if ((mode != LOAD) && (mode != SAVE))
    throw new IllegalArgumentException (
      "Mode argument must be either LOAD or SAVE");

  setMode (mode);
}

/*************************************************************************/

/*
 * Instance Methods
 */

/**
  * Returns the mode of this dialog, either <code>LOAD</code> or
  * <code>SAVE</code>.
  *
  * @return The mode of this dialog.
  */
public int
getMode()
{
  return(mode);
}

/*************************************************************************/

/**
  * Sets the mode of this dialog to either <code>LOAD</code> or
  * <code>SAVE</code>.  This method is only effective before the native
  * peer is created.
  *
  * @param mode The new mode of this file dialog.
  *
  * @exception IllegalArgumentException If an illegal file dialog mode
  * is supplied.
  */
public void
setMode(int mode)
{
  if ((mode != LOAD) && (mode != SAVE))
    throw new IllegalArgumentException("Bad mode: " + mode);

  this.mode = mode;
}

/*************************************************************************/

/**
  * Returns the directory for this file dialog.
  *
  * @return The directory for this file dialog.
  */
public String
getDirectory()
{
  return(dir);
}

/*************************************************************************/

/**
  * Sets the directory for this file dialog.
  *
  * @param dir The new directory for this file dialog.
  */
public synchronized void
setDirectory(String dir)
{
  this.dir = dir;
  if (peer != null)
    {
      FileDialogPeer f = (FileDialogPeer) peer;
      f.setDirectory (dir);
    }
}

/*************************************************************************/

/**
  * Returns the file that is selected in this dialog.
  *
  * @return The file that is selected in this dialog.
  */
public String
getFile()
{
  return(file);
}

/*************************************************************************/

/**
  * Sets the selected file for this dialog.
  *
  * @param file The selected file for this dialog.
  */
public synchronized void
setFile(String file)
{
  this.file = file;
  if (peer != null)
    {
      FileDialogPeer f = (FileDialogPeer) peer;
      f.setFile (file);
    }
}

/*************************************************************************/

/**
  * Returns the filename filter being used by this dialog.
  *
  * @return The filename filter being used by this dialog.
  */
public FilenameFilter
getFilenameFilter()
{
  return(filter);
}

/*************************************************************************/

/**
  * Sets the filename filter used by this dialog.
  *
  * @param filter The new filename filter for this file dialog box.
  */
public synchronized void
setFilenameFilter(FilenameFilter filter)
{
  this.filter = filter;
  if (peer != null)
    {
      FileDialogPeer f = (FileDialogPeer) peer;
      f.setFilenameFilter (filter);
    }
}

/*************************************************************************/

/**
  * Creates the native peer for this file dialog box.
  */
public void
addNotify()
{
  if (peer == null)
    peer = getToolkit ().createFileDialog (this);
  super.addNotify ();
}

/*************************************************************************/

/**
  * Returns a debugging string for this object.
  *
  * @return A debugging string for this object.
  */
protected String
paramString()
{
  return ("dir=" + dir + ",file=" + file +
	  ",mode=" + mode + "," + super.paramString());
}

} // class FileDialog 

