/* TransferHandler.java --
   Copyright (C) 2004 Free Software Foundation, Inc.

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


package javax.swing;

import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.event.ActionEvent;
import java.awt.event.InputEvent;
import java.io.Serializable;

public class TransferHandler implements Serializable
{
  static class TransferAction extends AbstractAction
  {
    private String command;

    public TransferAction(String command)
    {
      this.command = command;
    }
    
    public void actionPerformed(ActionEvent event)
    {
      JComponent component = (JComponent) event.getSource();
      TransferHandler transferHandler = component.getTransferHandler();
      Clipboard clipboard = getClipboard(component);

      if (command.equals(COMMAND_COPY))
	transferHandler.exportToClipboard(component, clipboard, COPY);
      else if (command.equals(COMMAND_CUT))
	transferHandler.exportToClipboard(component, clipboard, MOVE);
      else if (command.equals(COMMAND_PASTE))
	{
	  Transferable transferable = clipboard.getContents(null);

	  if (transferable != null)
	    transferHandler.importData(component, transferable);
	}
    }
  
    private static Clipboard getClipboard(JComponent component)
    {
      SecurityManager sm = System.getSecurityManager();
    
      if (sm != null)
	{
	  try
	    {
	      sm.checkSystemClipboardAccess();

	      // We may access system clipboard.
	      return component.getToolkit().getSystemClipboard();
	    }
	  catch (SecurityException e)
	    {
	      // We may not access system clipboard.
	    }
	}
    
      // Create VM-local clipboard if non exists yet.
      if (clipboard == null)
        clipboard = new Clipboard("Clipboard");

      return clipboard;
    }
  }
  
  private static final long serialVersionUID = -967749805571669910L;

  private static final String COMMAND_COPY = "copy";
  private static final String COMMAND_CUT = "cut";
  private static final String COMMAND_PASTE = "paste";
  
  public static final int NONE = 0;
  public static final int COPY = 1;
  public static final int MOVE = 2;
  public static final int COPY_OR_MOVE = 3;

  private static Action copyAction = new TransferAction(COMMAND_COPY);
  private static Action cutAction = new TransferAction(COMMAND_CUT);
  private static Action pasteAction = new TransferAction(COMMAND_PASTE);
  
  /**
   * Clipboard if system clipboard may not be used.
   * Package-private to avoid an accessor method.
   */
  static Clipboard clipboard;
  
  private int sourceActions;
  private Icon visualRepresentation;
  
  public static Action getCopyAction()
  {
    return copyAction;
  }

  public static Action getCutAction()
  {
    return cutAction;
  }

  public static Action getPasteAction()
  {
    return pasteAction;
  }

  protected TransferHandler()
  {
    this.sourceActions = NONE;
  }

  public TransferHandler(String property)
  {
    this.sourceActions = property != null ? COPY : NONE;
  }

  public boolean canImport (JComponent c, DataFlavor[] flavors)
  {
    return false;
  }

  protected Transferable createTransferable(JComponent c) 
  {
    return null;
  }

  public void exportAsDrag (JComponent c, InputEvent e, int action) 
  {    
  }

  protected void exportDone (JComponent c, Transferable data, int action) 
  {
  }

  public void exportToClipboard(JComponent c, Clipboard clip, int action) 
  {
  } 

  public int getSourceActions (JComponent c)
  {
    return sourceActions;
  }

  public Icon getVisualRepresentation (Transferable t)
  {
    return visualRepresentation;
  }

  public boolean importData (JComponent c, Transferable t) 
  {
    return false;
  }
}
