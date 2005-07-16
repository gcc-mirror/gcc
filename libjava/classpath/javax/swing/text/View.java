/* View.java -- 
   Copyright (C) 2002, 2004, 2005  Free Software Foundation, Inc.

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


package javax.swing.text;

import java.awt.Container;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;

import javax.swing.JComponent;
import javax.swing.SwingConstants;
import javax.swing.event.DocumentEvent;

public abstract class View implements SwingConstants
{
  public static final int BadBreakWeight = 0;
  public static final int ExcellentBreakWeight = 2000;
  public static final int ForcedBreakWeight = 3000;
  public static final int GoodBreakWeight = 1000;

  public static final int X_AXIS = 0;
  public static final int Y_AXIS = 1;
    
  private float width, height;
  private Element elt;
  private View parent;

  /**
   * The child views.
   */
  View[] children;

  /**
   * Creates a new <code>View</code> instance.
   *
   * @param elem an <code>Element</code> value
   */
  public View(Element elem)
  {
    elt = elem;
    children = new View[0];
  }

  public abstract void paint(Graphics g, Shape s);

  public void setParent(View parent)
  {
    this.parent = parent;
  }
    
  public View getParent()
  {
    return parent;
  }
    
  public Container getContainer()
  {
    View parent = getParent();
    return parent != null ? parent.getContainer() : null;
  }
  
  public Document getDocument()
  {
    return getElement().getDocument();
  }
    
  public Element getElement()
  {
    return elt;
  }

  public abstract float getPreferredSpan(int axis);

  public int getResizeWeight(int axis)
  {
    return 0;
  }

  public float getMaximumSpan(int axis)
  {
    if (getResizeWeight(axis) <= 0)
      return getPreferredSpan(axis);

    return Integer.MAX_VALUE;
  }

  public float getMinimumSpan(int axis)
  {
    if (getResizeWeight(axis) <= 0)
      return getPreferredSpan(axis);

    return Integer.MAX_VALUE;
  }
  
  public void setSize(float width, float height)
  {
    // The default implementation does nothing.
  }
  
  public float getAlignment(int axis)
  {
    return 0.5f;
  }

  public AttributeSet getAttributes()
  {
    return getElement().getAttributes();
  }
  
  public boolean isVisible()
  {
    return true;
  }

  public int getViewCount()
  {
    return 0;
  }
  
  public View getView(int index)
  {
    return null;
  }

  public ViewFactory getViewFactory()
  {
    View parent = getParent();
    return parent != null ? parent.getViewFactory() : null;
  }

  public void replace(int offset, int length, View[] views)
  {
    // Default implementation does nothing.
  }

  public void insert(int offset, View view)
  {
    View[] array = { view };
    replace(offset, 1, array);
  }

  public void append(View view)
  {
    View[] array = { view };
    replace(getViewCount(), 1, array);
  }

  public void removeAll()
  {
    replace(0, getViewCount(), null); 
  }

  public void remove(int index)
  {
    replace(index, 1, null); 
  }

  public View createFragment(int p0, int p1)
  {
    // The default implementation doesn't support fragmentation.
    return this;
  }

  public int getStartOffset()
  {
    return getElement().getStartOffset();
  }

  public int getEndOffset()
  {
    return getElement().getEndOffset();
  }

  public Shape getChildAllocation(int index, Shape a)
  {
    return null;
  }
  
  /**
   * @since 1.4
   */
  public int getViewIndex(float x, float y, Shape allocation)
  {
    return -1;
  }
  
  /**
   * @since 1.4
   */
  public String getToolTipText(float x, float y, Shape allocation)
  {
    int index = getViewIndex(x, y, allocation);

    if (index < -1)
      return null;

    Shape childAllocation = getChildAllocation(index, allocation);

    if (childAllocation.getBounds().contains(x, y))
      return getView(index).getToolTipText(x, y, childAllocation);

    return null;
  }

  /**
   * @since 1.3
   */
  public Graphics getGraphics()
  {
    return getContainer().getGraphics();
  }

  public void preferenceChanged(View child, boolean width, boolean height)
  {
    if (parent != null)
      parent.preferenceChanged(this, width, height);
    else
      ((JComponent) getContainer()).revalidate();
  }

  public int getBreakWeight(int axis, float pos, float len)
  {
    return BadBreakWeight;
  }

  public View breakView(int axis, int offset, float pos, float len)
  {
    return this;
  }

  /**
   * @since 1.3
   */
  public int getViewIndex(int pos, Position.Bias b)
  {
    return -1;
  }

  /**
   * Receive notification about an insert update to the text model.
   *
   * The default implementation of this method does the following:
   * <ul>
   * <li>Call {@link #updateChildren} if the element that this view is
   * responsible for has changed. This makes sure that the children can
   * correctly represent the model.<li>
   * <li>Call {@link #forwardUpdate}. This forwards the DocumentEvent to
   * the child views.<li>
   * <li>Call {@link #updateLayout}. Gives the view a chance to either
   * repair its layout, reschedule layout or do nothing at all.</li>
   * </ul>
   *
   * @param ev the DocumentEvent that describes the change
   * @param shape the shape of the view
   * @param vf the ViewFactory for creating child views
   */
  public void insertUpdate(DocumentEvent ev, Shape shape, ViewFactory vf)
  {
    Element el = getElement();
    DocumentEvent.ElementChange ec = ev.getChange(el);
    if (ec != null)
      updateChildren(ec, ev, vf);
    forwardUpdate(ec, ev, shape, vf);
    updateLayout(ec, ev, shape);
  }

  /**
   * Receive notification about a remove update to the text model.
   *
   * The default implementation of this method does the following:
   * <ul>
   * <li>Call {@link #updateChildren} if the element that this view is
   * responsible for has changed. This makes sure that the children can
   * correctly represent the model.<li>
   * <li>Call {@link #forwardUpdate}. This forwards the DocumentEvent to
   * the child views.<li>
   * <li>Call {@link #updateLayout}. Gives the view a chance to either
   * repair its layout, reschedule layout or do nothing at all.</li>
   * </ul>
   *
   * @param ev the DocumentEvent that describes the change
   * @param shape the shape of the view
   * @param vf the ViewFactory for creating child views
   */
  public void removeUpdate(DocumentEvent ev, Shape shape, ViewFactory vf)
  {
    Element el = getElement();
    DocumentEvent.ElementChange ec = ev.getChange(el);
    if (ec != null)
        updateChildren(ec, ev, vf);
    forwardUpdate(ec, ev, shape, vf);
    updateLayout(ec, ev, shape);
  }

  /**
   * Receive notification about a change update to the text model.
   *
   * The default implementation of this method does the following:
   * <ul>
   * <li>Call {@link #updateChildren} if the element that this view is
   * responsible for has changed. This makes sure that the children can
   * correctly represent the model.<li>
   * <li>Call {@link #forwardUpdate}. This forwards the DocumentEvent to
   * the child views.<li>
   * <li>Call {@link #updateLayout}. Gives the view a chance to either
   * repair its layout, reschedule layout or do nothing at all.</li>
   * </ul>
   *
   * @param ev the DocumentEvent that describes the change
   * @param shape the shape of the view
   * @param vf the ViewFactory for creating child views
   */
  public void changedUpdate(DocumentEvent ev, Shape shape, ViewFactory vf)
  {
    Element el = getElement();
    DocumentEvent.ElementChange ec = ev.getChange(el);
    if (ec != null)
        updateChildren(ec, ev, vf);
    forwardUpdate(ec, ev, shape, vf);
    updateLayout(ec, ev, shape);
  }

  /**
   * Updates the list of children that is returned by {@link #getView}
   * and {@link #getViewCount}.
   *
   * Element that are specified as beeing added in the ElementChange record are
   * assigned a view for using the ViewFactory. Views of Elements that
   * are specified as beeing removed are removed from the list.
   *
   * @param ec the ElementChange record that describes the change of the
   *           element
   * @param ev the DocumentEvent describing the change of the document model
   * @param vf the ViewFactory to use for creating new views
   *
   * @return whether or not the child views represent the child elements of
   *         the element that this view is responsible for. Some views may
   *         create views that are responsible only for parts of the element
   *         that they are responsible for and should then return false.
   *
   * @since 1.3
   */
  protected boolean updateChildren(DocumentEvent.ElementChange ec,
                                   DocumentEvent ev,
                                   ViewFactory vf)
  {
    Element[] added = ec.getChildrenAdded();
    Element[] removed = ec.getChildrenRemoved();
    View[] newChildren = new View[children.length + added.length
                                  - removed.length];
    int index = ec.getIndex();
    System.arraycopy(children, 0, newChildren, 0, index);
    System.arraycopy(children, index, added, 0, added.length);
    int index2 = index + removed.length;
    int len2 = children.length - index2;
    System.arraycopy(children, index2, newChildren, index + added.length,
                     len2);
    children = newChildren;

    return true;
  }

  /**
   * Forwards the DocumentEvent to child views that need to get notified
   * of the change to the model. This calles {@link #forwardUpdateToView}
   * for each View that must be forwarded to.
   *
   * @param ec the ElementChange describing the element changes (may be
   *           <code>null</code> if there were no changes)
   * @param ev the DocumentEvent describing the changes to the model
   * @param shape the current allocation of the view
   * @param vf the ViewFactory used to create new Views
   *
   * @since 1.3
   */
  protected void forwardUpdate(DocumentEvent.ElementChange ec,
                               DocumentEvent ev, Shape shape, ViewFactory vf)
  {
    for (int i = 0; i < children.length; i++)
      {
        View child = children[i];
        forwardUpdateToView(child, ev, shape, vf);
      }
  }

  /**
   * Forwards an update event to the given child view. This calls
   * {@link #insertUpdate}, {@link #removeUpdate} or {@link #changedUpdate},
   * depending on the type of document event.
   *
   * @param view the View to forward the event to
   * @param ev the DocumentEvent to forward
   * @param shape the current allocation of the View
   * @param vf the ViewFactory used to create new Views
   *
   * @since 1.3
   */
  protected void forwardUpdateToView(View view, DocumentEvent ev, Shape shape,
                                     ViewFactory vf)
  {
    DocumentEvent.EventType type = ev.getType();
    if (type == DocumentEvent.EventType.INSERT)
      view.insertUpdate(ev, shape, vf);
    else if (type == DocumentEvent.EventType.REMOVE)
      view.removeUpdate(ev, shape, vf);
    else if (type == DocumentEvent.EventType.CHANGE)
      view.changedUpdate(ev, shape, vf);
  }

  /**
   * Updates the layout.
   *
   * @param ec the ElementChange that describes the changes to the element
   * @param ev the DocumentEvent that describes the changes to the model
   * @param shape the current allocation for this view
   *
   * @since 1.3
   */
  protected void updateLayout(DocumentEvent.ElementChange ec,
                              DocumentEvent ev, Shape shape)
  {
    Rectangle b = shape.getBounds();
    if (ec != null)
      preferenceChanged(this, true, true);
  }
}

