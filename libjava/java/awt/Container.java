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

  // Insets.
  private transient Insets myInsets;

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
    return myInsets;
  }

  /** @deprecated Use getInsets() instead. */
  public Insets insets()
  {
    return getInsets();
  }
  
  public Component add (Component comp)
  {
    addImpl (comp, null, -1);
    return comp;
  }

  public Component add (String name, Component comp)
  {
    addImpl (comp, name, -1);
    return comp;
  }

  public Component add (Component comp, int index)
  {
    addImpl (comp, null, index);
    return comp;
  }

  public void add (Component comp, Object constraints)
  {
    addImpl (comp, constraints, -1);
  }

  public void add (Component comp, Object constraints, int index)
  {
    addImpl (comp, constraints, index);
  }

  protected void addImpl (Component comp, Object constraints, int index)
  {
    if (index > ncomponents
	|| comp instanceof Window
	|| (comp instanceof Container
	    && ((Container) comp).isAncestorOf (this)))
      throw new IllegalArgumentException ();

    // Reparent component, and make sure component is instantiated if
    // we are.
    if (comp.parent != this)
      comp.parent.remove (comp);
    comp.parent = this;
    if (peer != null)
      comp.addNotify ();

    invalidate ();

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

    // Notify the layout manager.
    if (layoutMgr != null)
      {
	if (constraints != null && layoutMgr instanceof LayoutManager2)
	  {
	    LayoutManager2 lm2 = (LayoutManager2) layoutMgr;
	    lm2.addLayoutComponent (comp, constraints);
	  }
	else
	  layoutMgr.addLayoutComponent ((String) constraints, comp);
      }

    ContainerEvent ce = new ContainerEvent (this,
					    ContainerEvent.COMPONENT_ADDED,
					    comp);
    dispatchEvent (ce);
  }

  public void remove (int index)
  {
    Component r = component[index];

    r.removeNotify ();

    System.arraycopy (component, index + 1, component, index,
		      ncomponents - index - 1);
    component[--ncomponents] = null;

    invalidate ();

    if (layoutMgr != null)
      layoutMgr.removeLayoutComponent (r);

    ContainerEvent ce = new ContainerEvent (this,
					    ContainerEvent.COMPONENT_REMOVED,
					    r);
    dispatchEvent (ce);
  }

  public void remove (Component comp)
  {
    for (int i = 0; i < ncomponents; ++i)
      {
	if (component[i] == comp)
	  {
	    remove (i);
	    break;
	  }
      }
  }

  public void removeAll()
  {
    while (ncomponents > 0)
      remove (0);
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
    if (layoutMgr != null)
      layoutMgr.layoutContainer (this);
  }

  /** @deprecated Use doLayout() instead. */
  public void layout()
  {
    doLayout();
  }

  public void invalidate()
  {
    super.invalidate ();
  }

  public void validate()
  {
    if (! isValid ())
      {
	doLayout ();
	validateTree ();
      }
  }

  protected void validateTree()
  {
    for (int i = 0; i < ncomponents; ++i)
      component[i].validate ();
  }

  public void setFont(Font f)
  {
    // FIXME
  }

  public Dimension getPreferredSize()
  {
    if (layoutMgr != null)
      return layoutMgr.preferredLayoutSize (this);
    else
      return super.getPreferredSize ();
  }
  
  /** @deprecated Use getPreferredSize() instead */
  public Dimension preferredSize()
  {
    return getPreferredSize();
  }
  
  public Dimension getMinimumSize()
  {
    if (layoutMgr != null)
      return layoutMgr.minimumLayoutSize (this);
    else
      return super.getMinimumSize ();
  }
  
  /** @deprecated Use getMinimumSize() instead */
  public Dimension minimumSize()
  {
    return getMinimumSize();
  }
  
  public Dimension getMaximumSize()
  {
    if (layoutMgr != null && layoutMgr instanceof LayoutManager2)
      {
	LayoutManager2 lm2 = (LayoutManager2) layoutMgr;
	return lm2.maximumLayoutSize (this);
      }
    else
      return super.getMaximumSize ();
  }

  public float getAlignmentX()
  {
    if (layoutMgr instanceof LayoutManager2)
      {
	LayoutManager2 lm2 = (LayoutManager2) layoutMgr;
	return lm2.getLayoutAlignmentX (this);
      }
    else
      return CENTER_ALIGNMENT;
  }

  public float getAlignmentY()
  {
    if (layoutMgr instanceof LayoutManager2)
      {
	LayoutManager2 lm2 = (LayoutManager2) layoutMgr;
	return lm2.getLayoutAlignmentY (this);
      }
    else
      return CENTER_ALIGNMENT;
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
    for (int i = 0; i < ncomponents; ++i)
      component[i].printAll (g);
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
    containerListener = AWTEventMulticaster.add (containerListener, l);
  }

  public void removeContainerListener(ContainerListener l)
  {
    containerListener = AWTEventMulticaster.remove(containerListener, l);
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
    if (containerListener == null)
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
  
  public Component getComponentAt (int x, int y)
  {
    if (! contains (x, y))
      return null;
    for (int i = 0; i < ncomponents; ++i)
      {
	int x2 = x - component[i].x;
	int y2 = y - component[i].y;
	if (component[i].contains (x2, y2))
	  return component[i];
      }
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
  }

  public void removeNotify()
  {
    for (int i = 0; i < ncomponents; ++i)
      component[i].removeNotify ();
    // FIXME: remove our peer.
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
  
  public void list (PrintStream out, int indent)
  {
    for (int i = 0; i < indent; ++i)
      out.print (' ');
    out.println (toString ());
    for (int i = 0; i < ncomponents; ++i)
      component[i].list (out, indent + 2);
  }

  public void list(PrintWriter out, int indent)
  {
    for (int i = 0; i < indent; ++i)
      out.print (' ');
    out.println (toString ());
    for (int i = 0; i < ncomponents; ++i)
      component[i].list (out, indent + 2);
  }
}
