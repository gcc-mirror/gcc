/* Copyright (C) 2000, 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.awt;

import java.awt.peer.FileDialogPeer;
import java.io.FilenameFilter;

/**
 * @author Tom Tromey <tromey@redhat.com>
 * @date April 20, 2001
 */

public class FileDialog extends Dialog
{
  public static int LOAD = 0;
  public static int SAVE = 1;

  public FileDialog (Frame parent)
  {
    this (parent, "", LOAD);
  }

  public FileDialog (Frame parent, String title)
  {
    this (parent, title, LOAD);
  }

  public FileDialog (Frame parent, String title, int mode)
  {
    super (parent, title, true);
    if (mode != LOAD && mode != SAVE)
      throw new IllegalArgumentException ("unknown mode: " + mode);
    this.mode = mode;
  }

  public void addNotify ()
  {
    if (peer == null)
      peer = getToolkit ().createFileDialog (this);
    super.addNotify ();
  }

  public String getDirectory ()
  {
    return dir;
  }

  public String getFile ()
  {
    return file;
  }

  public FilenameFilter getFilenameFilter ()
  {
    return filter;
  }

  public int getMode ()
  {
    return mode;
  }

  protected String paramString ()
  {
    return ("FileDialog[mode=" + mode
	    + ",dir=" + dir
	    + ",file=" + file + "]");
  }

  public void setDirectory (String dir)
  {
    this.dir = dir;
    if (peer != null)
      {
	FileDialogPeer f = (FileDialogPeer) peer;
	f.setDirectory (dir);
      }
  }

  public void setFile (String file)
  {
    this.file = file;
    if (peer != null)
      {
	FileDialogPeer f = (FileDialogPeer) peer;
	f.setFile (file);
      }
  }

  public void setFilenameFilter (FilenameFilter filter)
  {
    this.filter = filter;
    if (peer != null)
      {
	FileDialogPeer f = (FileDialogPeer) peer;
	f.setFilenameFilter (filter);
      }
  }

  public void setMode (int mode)
  {
    if (mode != LOAD && mode != SAVE)
      throw new IllegalArgumentException ("unknown mode: " + mode);
    this.mode = mode;
    // FIXME: update peer?
  }

  // Names here from serialization spec.
  private int mode;
  private String dir;
  private String file;
  private FilenameFilter filter;
}
