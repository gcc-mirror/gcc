/* Copyright (C) 1999, 2000  Free Software Foundation

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt;

import java.awt.event.*;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.util.EventListener;
import java.awt.peer.ComponentPeer;
import java.awt.peer.ContainerPeer;

/* A very incomplete placeholder. */

public abstract class Container extends Component
{
  /* Serialized fields from the serialization spec. */
  int ncomponents;
  Component[] component;
  LayoutManager layoutMgr;
  /* LightweightDispatcher dispatcher; */ // wtf?
  Dimension maxSize;
  int containerSerializedDataVersion;

  /* Anything else is non-serializable, and should be declared "transient". */
  transient ContainerListener containerListener;  
  
  public Container()
  {
  }

  public int getComponentCount()
  {
    return ncomponents;
  }

  /** @deprecated Use getComponentCount() instead. */
  public int countComponents()
  {
    return ncomponents;
  }

  public Component getComponent (int n)
  {
    if (n < 0 || n >= ncomponents)
      throw new ArrayIndexOutOfBoundsException("no such component");
    return component[n];
  }

  public Component[] getComponents()
  {
    Component[] result = new Component[ncomponents];
    if (ncomponents > 0)
      System.arraycopy(component, 0, result, 0, ncomponents);
    return result;
  }

  public Insets getInsets()
  {
    // FIXME
    return null;
  }
  
  /** @deprecated Use getInsets() instead. */
  public Insets insets()
  {
    return getInsets();
  }
  
  public Component add (Component comp)
  {
    return add (comp, -1);
  }
  
  public Component add(String name, Component comp)
  {
    // FIXME
    return null;
  }

  public Component add(Component comp, int index)
  {
    // This isn't the most efficient implementation.  We could do less
    // copying when growing the array.  It probably doesn't matter.
    if (ncomponents >= component.length)
      {
	int nl = component.length * 2;
	Component[] c = new Component[nl];
	System.arraycopy (component, 0, c, 0, ncomponents);
	component = c;
      }

    if (index == -1)
      component[ncomponents++] = comp;
    else
      {
	System.arraycopy (component, index, component, index + 1,
			  ncomponents - index);
	component[index] = comp;
	++ncomponents;
      }

    return comp;
  }

  public void add(Component comp, Object constraints)
  {
    // FIXME
  }

  public void add(Component comp, Object constraints, int index)
  {
    // FIXME
  }

  protected void addImpl(Component comp, Object constraints, int index)
  {
    // FIXME
  }

  public void remove (int index)
  {
    System.arraycopy (component, index + 1, component, index,
		      ncomponents - index - 1);
    component[--ncomponents] = null;
  }

  public void remove (Component comp)
  {
    for (int i = 0; i < ncomponents; ++i)
      if (component[i] == comp)
	{
	  remove (i);
	  break;
	}
  }

  public void removeAll()
  {
    while (ncomponents >= 0)
      component[--ncomponents] = null;
  }

  public LayoutManager getLayout()
  {
    return layoutMgr;
  }
  
  public void setLayout(LayoutManager mgr)
  {
    layoutMgr = mgr;
    // FIXME
  }
  
  public void doLayout()
  {
    // FIXME
  }

  /** @deprecated Use doLayout() instead. */
  public void layout()
  {
    doLayout();
  }

  public void invalidate()
  {
    // FIXME
  }

  public void validate()
  {
    // FIXME
  }

  protected void validateTree()
  {
    // FIXME
  }

  public void setFont(Font f)
  {
    // FIXME
  }

  public Dimension getPreferredSize()
  {
    // FIXME
    return null;
  }
  
  /** @deprecated Use getPreferredSize() instead */
  public Dimension preferredSize()
  {
    return getPreferredSize();
  }
  
  public Dimension getMinimumSize()
  {
    // FIXME
    return null;
  }
  
  /** @deprecated Use getMinimumSize() instead */
  public Dimension minimumSize()
  {
    return getMinimumSize();
  }
  
  public Dimension getMaximumSize()
  {
    // FIXME
    return null;    
  }
  
  public float getAlignmentX()
  {
    // FIXME
    return 0;
  }

  public float getAlignmentY()
  {
    // FIXME
    return 0;
  }

  public void paint(Graphics g)
  {
    // FIXME
  }

  public void update(Graphics g)
  {
    // FIXME
  }

  public void print(Graphics g)
  {
    // FIXME
  }

  public void paintComponents(Graphics g)
  {
    // FIXME
  }

  public void printComponents(Graphics g)
  {
    // FIXME
  }
  
  void dispatchEventImpl(AWTEvent e)
  {
    if ((e.id <= ContainerEvent.CONTAINER_LAST
             && e.id >= ContainerEvent.CONTAINER_FIRST)
	&& (containerListener != null
	    || (eventMask & AWTEvent.CONTAINER_EVENT_MASK) != 0))
      processEvent(e); 
    else super.dispatchEventImpl(e);
  }  

  public void addContainerListener(ContainerListener l)
  {
    containerListener = (ContainerListener) 
                          AWTEventMulticaster.add(containerListener, l);
  }

  public void removeContainerListener(ContainerListener l)
  {
    containerListener = (ContainerListener)
			  AWTEventMulticaster.remove(containerListener, l);
  }

  /** @since 1.3 */
  public EventListener[] getListeners(Class listenerType)
  {
    if (listenerType == ContainerListener.class)
      return getListenersImpl(listenerType, containerListener);
    else return super.getListeners(listenerType);
  }
  
  protected void processEvent(AWTEvent e)
  {
    if (e instanceof ContainerEvent)
      processContainerEvent((ContainerEvent) e);
    else super.processEvent(e);
  }
  
  protected void processContainerEvent(ContainerEvent e)
  {
    if (componentListener == null)
      return;
    switch (e.id)
      {
	case ContainerEvent.COMPONENT_ADDED:
	  containerListener.componentAdded(e);
	break;

	case ContainerEvent.COMPONENT_REMOVED:
	  containerListener.componentRemoved(e);
	break;    
      }
  }

  /** @deprecated */
  public void deliverEvent(Event e)
  {
  }
  
  public Component getComponentAt(int x, int y)
  {
    // FIXME
    return null;
  }

  /** @deprecated Use getComponentAt() instead */
  public Component locate(int x, int y)
  {
    return getComponentAt(x, y);
  }

  public Component getComponentAt(Point p)
  {
    return getComponentAt(p.x, p.y);
  }

  public Component findComponentAt(int x, int y)
  {
    // FIXME
    return null;
  }

  public Component findComponentAt(Point p)
  {
    return findComponentAt(p.x, p.y);
  }

  public void addNotify ()
  {
    for (int i = ncomponents;  --i >= 0; )
      component[i].addNotify();
    peer = (ComponentPeer) getToolkit ().createContainer (this);
  }

  public void removeNotify()
  {
    // FIXME
  }

  public boolean isAncestorOf (Component comp)
  {
    for (;;)
      {
	if (comp == null)
	  return false;
	if (comp == this)
	  return true;
	comp = comp.getParent();
      }
  }

  protected String paramString()
  {
    return "FIXME";
  }
  
  public void list(PrintStream out, int indent)
  {
    // FIXME  
  }
  
  public void list(PrintWriter out, int indent)
  {
    // FIXME  
  }
}
