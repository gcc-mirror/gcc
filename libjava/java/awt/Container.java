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
import java.awt.peer.LightweightPeer;

/* A somewhat incomplete class. */

public class Container extends Component
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
    if (peer == null)
	return new Insets(0, 0, 0, 0);
	
    return ((ContainerPeer) peer).getInsets();
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
    if (comp.parent != null)
      comp.parent.remove (comp);
    comp.parent = this;
    if (peer != null)
      {
	comp.addNotify ();

	if (comp.isLightweight())
	  enableEvents(comp.eventMask);
      }

    invalidate ();

    if (component == null)
      component = new Component[4]; // FIXME, better initial size?

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
    invalidate ();
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
	validateTree ();
      }
  }

  protected void validateTree()
  {
    if (valid) return; 

    ContainerPeer cPeer = null;
    if ((peer != null) && !(peer instanceof LightweightPeer))
      {
	cPeer = (ContainerPeer) peer;
	cPeer.beginValidate();
      }

    doLayout ();
    for (int i = 0; i < ncomponents; ++i)
      {
	Component comp = component[i];
	if (comp instanceof Container)
	  {
	    ((Container) comp).validateTree();
	  }
	else
	  {
	    component[i].validate();
	  }
      }
    
    /* children will call invalidate() when they are layed out. It
       is therefore imporant that valid is not set to true
       before after the children has been layed out. */
    valid = true;

    if (cPeer != null)
      cPeer.endValidate();
  }

  public void setFont(Font f)
  {
    super.setFont(f);
    // FIXME, should invalidate all children with font == null
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
    if (!isShowing())
      return;
    super.paint(g);
    visitChildren(g, GfxPaintVisitor.INSTANCE, true);
  }

  /** 
   * Perform a graphics operation on the children of this container.
   * For each applicable child, the visitChild() method will be called
   * to perform the graphics operation.
   *
   * @param gfx The graphics object that will be used to derive new
   * graphics objects for the children.
   *
   * @param visitor Object encapsulating the graphics operation that
   * should be performed.
   *
   * @param lightweightOnly If true, only lightweight components will
   * be visited.
   */
  private void visitChildren(Graphics gfx, GfxVisitor visitor,
		     boolean lightweightOnly)
  {
    // FIXME: do locking

    for (int i = 0; i < ncomponents; ++i)
      {
	Component comp = component[i];
	boolean applicable = comp.isVisible()
	  && (comp.isLightweight() || !lightweightOnly);

	if (applicable)
	  visitChild(gfx, visitor, comp);
      }
  }

  /**
   * Perform a graphics operation on a child. A translated and clipped
   * graphics object will be created, and the visit() method of the
   * visitor will be called to perform the operation.
   *
   * @param gfx The graphics object that will be used to derive new
   * graphics objects for the child.
   *
   * @param visitor Object encapsulating the graphics operation that
   * should be performed.
   *
   * @param comp The child component that should be visited.
   */
  private void visitChild(Graphics gfx, GfxVisitor visitor,
			  Component comp)
  {
    Rectangle bounds = comp.getBounds();
    Rectangle clip = gfx.getClipBounds().intersection(bounds);
    
    if (clip.isEmpty()) return;

    Graphics gfx2 = gfx.create();
    gfx2.setClip(clip.x, clip.y, clip.width, clip.height);
    gfx2.translate(bounds.x, bounds.y);
    
    visitor.visit(comp, gfx2);
  }

  public void update(Graphics g)
  {
    super.update(g);
  }

  public void print(Graphics g)
  {
    super.print(g);
    visitChildren(g, GfxPrintVisitor.INSTANCE, true);
  }

  public void paintComponents(Graphics g)
  {
    super.paint(g);
    visitChildren(g, GfxPaintAllVisitor.INSTANCE, true);
  }

  public void printComponents(Graphics g)
  {
    super.paint(g);
    visitChildren(g, GfxPrintAllVisitor.INSTANCE, true);
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
	// Ignore invisible children...
	if (!component[i].isVisible())
	  continue;

	int x2 = x - component[i].x;
	int y2 = y - component[i].y;
	if (component[i].contains (x2, y2))
	  return component[i];
      }
    return this;
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

  public Component findComponentAt (int x, int y)
  {
    if (! contains (x, y))
      return null;

    for (int i = 0; i < ncomponents; ++i)
      {
	// Ignore invisible children...
	if (!component[i].isVisible())
	  continue;

	int x2 = x - component[i].x;
	int y2 = y - component[i].y;
	// We don't do the contains() check right away because
	// findComponentAt would redundantly do it first thing.
	if (component[i] instanceof Container)
	  {
	    Container k = (Container) component[i];
	    Component r = k.findComponentAt (x2, y2);
	    if (r != null)
	      return r;
	  }
	else if (component[i].contains (x2, y2))
	  return component[i];
      }

    return this;
  }

  public Component findComponentAt(Point p)
  {
    return findComponentAt(p.x, p.y);
  }

  public void addNotify ()
  {
    super.addNotify();
  }

  void addNotifyContainerChildren()
  {
    for (int i = ncomponents;  --i >= 0; )
      {
	component[i].addNotify();
	if (component[i].isLightweight())
	  enableEvents(component[i].eventMask);
      }
  }

  public void removeNotify()
  {
    for (int i = 0; i < ncomponents; ++i)
      component[i].removeNotify ();
    super.removeNotify();
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
    String param = super.paramString();
    if (layoutMgr != null)
      param = param + "," + layoutMgr.getClass().getName();

    return param;
  }
  
  public void list (PrintStream out, int indent)
  {
    super.list (out, indent);
    for (int i = 0; i < ncomponents; ++i)
      component[i].list (out, indent + 2);
  }

  public void list(PrintWriter out, int indent)
  {
    super.list (out, indent);
    for (int i = 0; i < ncomponents; ++i)
      component[i].list (out, indent + 2);
  }


  /* The following classes are used in concert with the
     visitChildren() method to implement all the graphics operations
     that requires traversal of the containment hierarchy. */

  abstract static class GfxVisitor
  {
    public abstract void visit(Component c, Graphics gfx);
  }

  static class GfxPaintVisitor extends GfxVisitor
  {
    public void visit(Component c, Graphics gfx) { c.paint(gfx); }
    public static final GfxVisitor INSTANCE = new GfxPaintVisitor();
  }

  static class GfxPrintVisitor extends GfxVisitor
  {
    public void visit(Component c, Graphics gfx) { c.print(gfx); }
    public static final GfxVisitor INSTANCE = new GfxPrintVisitor();
  }

  static class GfxPaintAllVisitor extends GfxVisitor
  {
    public void visit(Component c, Graphics gfx) { c.paintAll(gfx); }
    public static final GfxVisitor INSTANCE = new GfxPaintAllVisitor();
  }

  static class GfxPrintAllVisitor extends GfxVisitor
  {
    public void visit(Component c, Graphics gfx) { c.printAll(gfx); }
    public static final GfxVisitor INSTANCE = new GfxPrintAllVisitor();
  }

  // This is used to implement Component.transferFocus.
  Component findNextFocusComponent (Component child)
  {
    int start, end;
    if (child != null)
      {
	for (start = 0; start < ncomponents; ++start)
	  {
	    if (component[start] == child)
	      break;
	  }
	end = start;
	// This special case lets us be sure to terminate.
	if (end == 0)
	  end = ncomponents;
	++start;
      }
    else
      {
	start = 0;
	end = ncomponents;
      }

    for (int j = start; j != end; ++j)
      {
	if (j >= ncomponents)
	  {
	    // The JCL says that we should wrap here.  However, that
	    // seems wrong.  To me it seems that focus order should be
	    // global within in given window.  So instead if we reach
	    // the end we try to look in our parent, if we have one.
	    if (parent != null)
	      return parent.findNextFocusComponent (this);
	    j -= ncomponents;
	  }
	if (component[j] instanceof Container)
	  {
	    Component c = component[j];
	    c = c.findNextFocusComponent (null);
	    if (c != null)
	      return c;
	  }
	else if (component[j].isFocusTraversable ())
	  return component[j];
      }

    return null;
  }
}
