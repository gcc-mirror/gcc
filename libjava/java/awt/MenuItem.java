/* Copyright (C) 1999, 2000, 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.awt;
import java.awt.event.*;
import java.util.EventListener;
import java.awt.peer.MenuItemPeer;

public class MenuItem extends MenuComponent
{
  // Fields from the serialization spec. Decalare others "transient".
  boolean enabled;
  String label;
  String actionCommand;
  long eventMask;
  MenuShortcut shortcut;
  int menuItemSerializedDataVersion;

  transient ActionListener actionListener;

  public MenuItem ()
  {
    this.label = "";
  }

  public MenuItem (String label)
  {
    this.label = label;
  }

  public MenuItem (String label, MenuShortcut shortcut)
  {
    this.label = label;
    this.shortcut = shortcut;
  }

  public String getLabel()
  {
    return label;
  }

  public synchronized void setLabel(String label)
  {
    this.label = label;
    if (peer != null)
      {
	MenuItemPeer mp = (MenuItemPeer) peer;
	mp.setLabel (label);
      }
  }

  public boolean isEnabled()
  {
    return enabled;
  }

  public synchronized void setEnabled(boolean b)
  {
    // The JCL says this method is ignored if the enabled state does
    // not change.  I take that to mean that the peer is not notified
    // in this case.
    if (this.enabled != b)
      {
	this.enabled = b;
	if (peer != null)
	  {
	    MenuItemPeer mp = (MenuItemPeer) peer;
	    mp.setEnabled (b);
	  }
      }
  }

  /** @deprecated Use setEnabled() instead. */
  public void enable()
  {
    setEnabled(true);
  }

  /** @deprecated Use setEnabled() instead. */
  public void enable(boolean b)
  {
    setEnabled(b);
  }

  /** @deprecated Use setEnabled() instead. */
  public void disable()
  {
    setEnabled(false);
  }

  public MenuShortcut getShortcut()
  {
    return shortcut;
  }

  public void setShortcut(MenuShortcut s)
  {
    this.shortcut = s;
  }

  public void deleteShortcut()
  {
    setShortcut(null);
  }

  protected final void enableEvents(long eventsToEnable)
  {
    eventMask |= eventsToEnable;
    // TODO: see comment in Component.enableEvents().    
  }

  protected final void disableEvents(long eventsToDisable)
  {
    eventMask &= ~eventsToDisable;    
  }

  public void setActionCommand(String command)
  {
    this.actionCommand = command;
  }

  public String getActionCommand()
  {
    return actionCommand;
  }

  public synchronized void addActionListener(ActionListener l)
  {
    actionListener = AWTEventMulticaster.add(actionListener, l);
    if (actionListener != null)
      enableEvents(AWTEvent.ACTION_EVENT_MASK);
  }

  public synchronized void removeActionListener(ActionListener l)
  {
    actionListener = AWTEventMulticaster.remove(actionListener, l);
  }

  public void addNotify ()
  {
    if (peer != null)
      {
	// This choice of toolkit seems unsatisfying, but I'm not sure
	// what else to do.
	peer = Toolkit.getDefaultToolkit ().createMenuItem (this);
      }
  }

  /** Returns all registered EventListers of the given listenerType. 
    * listenerType must be a subclass of EventListener, or a 
    * ClassClassException is thrown.
    * @since 1.3 
    */
  public EventListener[] getListeners(Class listenerType)
  {
    if (listenerType == ActionListener.class)
      return Component.getListenersImpl(listenerType, actionListener);
    else
      return Component.getListenersImpl(listenerType, null);
  }

  void dispatchEventImpl(AWTEvent e)
  {
    if (e.id <= ActionEvent.ACTION_LAST 
	&& e.id >= ActionEvent.ACTION_FIRST
	&& (actionListener != null
	    || (eventMask & AWTEvent.ACTION_EVENT_MASK) != 0))
      processEvent(e);
  }

  protected void processEvent(AWTEvent e)
  {
    if (e instanceof ActionEvent)
      processActionEvent((ActionEvent) e);
  }

  protected void processActionEvent(ActionEvent e)
  {
    if (actionListener != null)
      actionListener.actionPerformed(e);
  }

  public String paramString()
  {
    return name + ",label=" + label;
  }

  // Accessibility API not yet implemented.
  // public AccessibleContext getAccessibleContext()
}
