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
import java.util.EventListener;

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
    actionListener = AWTEventMulticaster.add (actionListener, l);
  }

  public void addNotify ()
  {
    if (peer == null)
      peer = getToolkit ().createButton (this);
    super.addNotify();
  }

  public String getActionCommand ()
  {
    return actionCommand;
  }

  public String getLabel ()
  {
    return label;
  }

  protected String paramString ()
  {
    return "Button[" + label + "]";
  }

  void dispatchEventImpl(AWTEvent e)
  {
      super.dispatchEventImpl(e);
      
      if (e.id <= ActionEvent.ACTION_LAST 
	  && e.id >= ActionEvent.ACTION_FIRST
	  && (actionListener != null 
	      || (eventMask & AWTEvent.ACTION_EVENT_MASK) != 0))
	  processEvent(e);
  }

  protected void processActionEvent (ActionEvent e)
  {
    if (actionListener != null)
      actionListener.actionPerformed (e);
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
    actionListener = AWTEventMulticaster.remove (actionListener, l);
  }

  public EventListener[] getListeners(Class listenerType)
  {
    if (listenerType == ActionListener.class)
      return getListenersImpl(listenerType, actionListener);
    return super.getListeners(listenerType);
  }

  public void setActionCommand (String command)
  {
    this.actionCommand = (command == null) ? label : command;
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

  String label;
  String actionCommand;

  transient ActionListener actionListener;
}
