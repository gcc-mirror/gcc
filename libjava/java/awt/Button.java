/* Copyright (C) 2000  Free Software Foundation

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt;
import java.awt.peer.ButtonPeer;
import java.awt.peer.ComponentPeer;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date July 30, 2000
 */

public class Button extends Component
{
  public Button ()
  {
    this (null);
  }

  public Button (String label)
  {
    this.label = label;
  }

  public void addActionListener (ActionListener l)
  {
    listeners = AWTEventMulticaster.add (listeners, l);
  }

  public void addNotify ()
  {
    if (peer == null)
      peer = (ComponentPeer) getToolkit ().createButton (this);
  }

  public String getActionCommand ()
  {
    return command;
  }

  public String getLabel ()
  {
    return label;
  }

  protected String paramString ()
  {
    return "Button[" + label + "]";
  }

  protected void processActionEvent (ActionEvent e)
  {
    if (listeners != null)
      listeners.actionPerformed (e);
  }

  protected void processEvent (AWTEvent e)
  {
    if (e instanceof ActionEvent)
      processActionEvent ((ActionEvent) e);
    else
      super.processEvent (e);
  }

  public void removeActionListener (ActionListener l)
  {
    listeners = AWTEventMulticaster.remove (listeners, l);
  }

  public void setActionCommand (String command)
  {
    this.command = (command == null) ? label : command;
  }

  public void setLabel (String label)
  {
    this.label = label;
    if (peer != null)
      {
	ButtonPeer bp = (ButtonPeer) peer;
	bp.setLabel (label);
      }
  }

  private String label;
  private String command;
  private ActionListener listeners;
}
