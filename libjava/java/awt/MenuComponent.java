/* Copyright (C) 1999, 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.awt;

/* Status: partially complete, untested. */

public abstract class MenuComponent implements java.io.Serializable
{
  // Fields from the serialization spec. Decalare others "transient".
  Font font;
  String name;
  boolean nameExplicitlySet;
  boolean newEventsOnly;
  //AccessibleContext accessibleContext;
  
  transient MenuContainer parent;
  transient java.awt.peer.MenuComponentPeer peer;

  public MenuComponent()
  {
  }

  public String getName()
  {
    if (name == null && !nameExplicitlySet)
      name = generateName();
    return name;
  }
  
  /** Subclasses should override this to generate unique names like 
    * "menuitem0".
    */
  String generateName()
  {
    // MenuComponent is abstract.
    return null;
  }

  public void setName(String name)
  {
    nameExplicitlySet = true;
    this.name = name;
  }

  public MenuContainer getParent()
  {
    return parent;
  }

  /** @deprecated Don't use this. */
  public java.awt.peer.MenuComponentPeer getPeer()
  {
    return peer;
  }

  public Font getFont()
  {
    return font;
  }

  public void setFont(Font f)
  {
    this.font = f;
  }

  public void removeNotify()
  {
    if (peer != null)
      peer.dispose ();
    peer = null;
  }

  /** @deprecated Replaced by dispatchEvent(AWTEvent) */
  public boolean postEvent(Event evt)
  {
    return false;
  }

  public final void dispatchEvent(AWTEvent e)
  {
    // FIXME
    dispatchEventImpl(e);
  }
  
  void dispatchEventImpl(AWTEvent e)
  {
    // This is overridden by subclasses that support events.
  }

  protected void processEvent(AWTEvent e)
  {
    // Nothing to do here? This is be overridden by subclasses that 
    // support events.
  }

  protected String paramString()
  {
    return name;
  }

  public String toString()
  {
    return this.getClass().getName() + "[" + paramString() + "]";
  }

  protected final Object getTreeLock()
  {
    // FIXME: figure out how the tree lock works.
    return null;
  }

  // Accessibility API not yet implemented.
  // public AccessibleContext getAccessibleContext()
}
