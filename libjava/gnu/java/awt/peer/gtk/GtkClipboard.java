/* GtkClipboard.java
   Copyright (C) 1999 Free Software Foundation, Inc.

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

import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.ClipboardOwner;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.StringSelection;
import java.awt.datatransfer.Transferable;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.IOException;

public class GtkClipboard extends Clipboard
{
  /* the number of milliseconds that we'll wait around for the
     owner of the GDK_SELECTION_PRIMARY selection to convert 
     the requested data */
  final static int SELECTION_RECEIVED_TIMEOUT = 5000;

  /* We currently only support transferring of text between applications */
  static String selection;
  static Object selectionLock = new Object ();

  static boolean hasSelection = false;

  protected 
  GtkClipboard ()
  {
    super ("System Clipboard");
    initNativeState ();
  }

  public Transferable 
  getContents (Object requestor)
  {
    synchronized (this)
      {
	if (hasSelection)
	  return contents;
      }

    /* Java doesn't own the selection, so we need to ask X11 */
    synchronized (selectionLock)
      {
	requestStringConversion ();
	try 
	  {
	    selectionLock.wait (SELECTION_RECEIVED_TIMEOUT);
	  } 
	catch (InterruptedException e)
	  {
	    return null;
	  }
	
	return (selection == null) ? null : new StringSelection (selection);
      }
  }

  void 
  stringSelectionReceived (String newSelection)
  {
    synchronized (selectionLock)
      {
	selection = newSelection;
	selectionLock.notify ();
      }
  }

  /* convert Java clipboard data into a String suitable for sending
     to another application */
  synchronized String
  stringSelectionHandler () throws IOException
  {
    String selection = null;

    try {
      if (contents.isDataFlavorSupported (DataFlavor.stringFlavor))
	selection = (String)contents.getTransferData (DataFlavor.stringFlavor);
      else if (contents.isDataFlavorSupported (DataFlavor.plainTextFlavor))
	{
	  StringBuffer sbuf = new StringBuffer ();
	  InputStreamReader reader;
	  char readBuf[] = new char[512];
	  int numChars;
	  
	  reader = new InputStreamReader 
	    ((InputStream) 
	     contents.getTransferData (DataFlavor.plainTextFlavor), "UNICODE");
	  
	  while (true)
	    {
	      numChars = reader.read (readBuf);
	      if (numChars == -1)
		break;
	      sbuf.append (readBuf, 0, numChars);
	    }
	  
	  selection = new String (sbuf);
	}
    } catch (Exception e) { }
    
    return selection;
  }

  public synchronized void
  setContents (Transferable contents, ClipboardOwner owner)
  {
    selectionGet ();

    this.contents = contents;
    this.owner = owner;

    hasSelection = true;
  }

  synchronized
  void selectionClear ()
  {
    hasSelection = false;

    if (owner != null)
      {
	owner.lostOwnership (this, contents);
	owner = null;
	contents = null;
      }
  }

  native void initNativeState ();
  native static void requestStringConversion ();
  native static void selectionGet ();
}
