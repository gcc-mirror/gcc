/* View.java -- 
   Copyright (C) 2002, 2004, 2005, 2006  Free Software Foundation, Inc.

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

import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.event.DocumentEvent;

public abstract class View implements SwingConstants
{
  public static final int BadBreakWeight = 0;
  public static final int ExcellentBreakWeight = 2000;
  public static final int ForcedBreakWeight = 3000;
  public static final int GoodBreakWeight = 1000;

  public static final int X_AXIS = 0;
  public static final int Y_AXIS = 1;
    
  private Element elt;
  private View parent;

  /**
   * Creates a new <code>View</code> instance.
   *
   * @param elem an <code>Element</code> value
   */
  public View(Element elem)
  {
    elt = elem;
  }

  public abstract void paint(Graphics g, Shape s);

  /**
   * Sets the parent for this view. This is the first method that is beeing
   * called on a view to setup the view hierarchy. This is also the last method
   * beeing called when the view is disconnected from the view hierarchy, in
   * this case <code>parent</code> is null.
   *
   * If <code>parent</code> is <code>null</code>, a call to this method also
   * calls <code>setParent</code> on the children, thus disconnecting them from
   * the view hierarchy. That means that super must be called when this method
   * is overridden.
   *
   * @param parent the parent to set, <code>null</code> when this view is
   *        beeing disconnected from the view hierarchy
   */
  public void setParent(View parent)
  {
    if (parent == null)
      {
        int numChildren = getViewCount();
        for (int i = 0; i < numChildren; i++)
          {
            View child = getView(i);
            // It is important that we only reset the parent on views that
            // actually belong to us. In FlowView the child may already be
            // reparented.
            if (child.getParent() == this)
              child.setParent(null);
          }
      }

    this.parent = parent;
  }
    
  public View getParent()
  {
    return parent;
  }
    
  public Container getContainer()
  {
    View parent = getParent();
    if (parent == null)
      return null;
    else
      return parent.getContainer();
  }
  
  public Document getDocument()
  {
    return getElement().getDocument();
  }
    
  public Element getElement()
  {
    return elt;
  }

  /**
   * Returns the preferred span along the specified axis. Normally the view is
   * rendered with the span returned here if that is possible.
   *
   * @param axis the axis
   *
   * @return the preferred span along the specified axis
   */
  public abstract float getPreferredSpan(int axis);

  /**
   * Returns the resize weight of this view. A value of <code>0</code> or less
   * means this view is not resizeable. Positive values make the view
   * resizeable. The default implementation returns <code>0</code>
   * unconditionally.
   *
   * @param axis the axis
   *
   * @return the resizability of this view along the specified axis
   */
  public int getResizeWeight(int axis)
  {
    return 0;
  }

  /**
   * Returns the maximum span along the specified axis. The default
   * implementation will forward to
   * {@link #getPreferredSpan(int)} unless {@link #getResizeWeight(int)}
   * returns a value > 0, in which case this returns {@link Integer#MIN_VALUE}.
   *
   * @param axis the axis
   *
   * @return the maximum span along the specified axis
   */
  public float getMaximumSpan(int axis)
  {
    float max = Integer.MAX_VALUE;
    if (getResizeWeight(axis) <= 0)
      max = getPreferredSpan(axis);
    return max;
  }

  /**
   * Returns the minimum span along the specified axis. The default
   * implementation will forward to
   * {@link #getPreferredSpan(int)} unless {@link #getResizeWeight(int)}
   * returns a value > 0, in which case this returns <code>0</code>.
   *
   * @param axis the axis
   *
   * @return the minimum span along the specified axis
   */
  public float getMinimumSpan(int axis)
  {
    float min = 0;
    if (getResizeWeight(axis) <= 0)
      min = getPreferredSpan(axis);
    return min;
  }
  
  public void setSize(float width, float height)
  {
    // The default implementation does nothing.
  }
  
  /**
   * Returns the alignment of this view along the baseline of the parent view.
   * An alignment of <code>0.0</code> will align this view with the left edge
   * along the baseline, an alignment of <code>0.5</code> will align it
   * centered to the baseline, an alignment of <code>1.0</code> will align
   * the right edge along the baseline.
   *
   * The default implementation returns 0.5 unconditionally.
   *
   * @param axis the axis
   *
   * @return the alignment of this view along the parents baseline for the
   *         specified axis
   */
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

  /**
   * Replaces a couple of child views with new child views. If
   * <code>length == 0</code> then this is a simple insertion, if
   * <code>views == null</code> this only removes some child views.
   *
   * @param offset the offset at which to replace
   * @param length the number of child views to be removed
   * @param views the new views to be inserted, may be <code>null</code>
   */
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
    int offset = getViewCount();
    replace(offset, 0, array);
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

    String text = null;
    if (index >= 0)
      {
        allocation = getChildAllocation(index, allocation);
        Rectangle r = allocation instanceof Rectangle ? (Rectangle) allocation
                                                      : allocation.getBounds();
        if (r.contains(x, y))
          text = getView(index).getToolTipText(x, y, allocation);
      }
    return text;
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
    View p = getParent();
    if (p != null)
      p.preferenceChanged(this, width, height);
  }

  public int getBreakWeight(int axis, float pos, float len)
  {
    int weight = BadBreakWeight;
    if (len > getPreferredSpan(axis))
      weight = GoodBreakWeight;
    return weight;
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
    if (getViewCount() > 0)
      {
        Element el = getElement();
        DocumentEvent.ElementChange ec = ev.getChange(el);
        if (ec != null)
          {
            if (! updateChildren(ec, ev, vf))
              ec = null;
          }
        forwardUpdate(ec, ev, shape, vf);
        updateLayout(ec, ev, shape);
      }
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
      {
        if (! updateChildren(ec, ev, vf))
          ec = null;
      }
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
    if (getViewCount() > 0)
      {
        Element el = getElement();
        DocumentEvent.ElementChange ec = ev.getChange(el);
        if (ec != null)
          {
            if (! updateChildren(ec, ev, vf))
              ec = null;
          }
        forwardUpdate(ec, ev, shape, vf);
        updateLayout(ec, ev, shape);
      }
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
    int index = ec.getIndex();

    View[] newChildren = null;
    if (added != null)
      {
        newChildren = new View[added.length];
        for (int i = 0; i < added.length; ++i)
          newChildren[i] = vf.create(added[i]);
      }
    int numRemoved = removed != null ? removed.length : 0;
    replace(index, numRemoved, newChildren);

    return true;
  }

  /**
   * Forwards the DocumentEvent to child views that need to get notified
   * of the change to the model. This calles {@link #forwardUpdateToView}
   * for each View that must be forwarded to.
   *
   * If <code>ec</code> is not <code>null</code> (this means there have been
   * structural changes to the element that this view is responsible for) this
   * method should recognize this and don't notify newly added child views.
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
    int count = getViewCount();
    if (count > 0)
      {
        // Determine start index.
        int startOffset = ev.getOffset();
        int startIndex = getViewIndex(startOffset, Position.Bias.Backward);

        // For REMOVE events we have to forward the event to the last element,
        // for the case that an Element has been removed that represente
        // the offset.
        if (startIndex == -1 && ev.getType() == DocumentEvent.EventType.REMOVE
            && startOffset >= getEndOffset())
          {
            startIndex = getViewCount() - 1;
          }

        // When startIndex is on a view boundary, forward event to the
        // previous view too.
        if (startIndex >= 0)
          {
            View v = getView(startIndex);
            if (v != null)
              {
                if (v.getStartOffset() == startOffset && startOffset > 0)
                  startIndex = Math.max(0, startIndex - 1);
              }
          }
        startIndex = Math.max(0, startIndex);

        // Determine end index.
        int endIndex = startIndex;
        if (ev.getType() != DocumentEvent.EventType.REMOVE)
          {
            endIndex = getViewIndex(startOffset + ev.getLength(),
                                    Position.Bias.Forward);
            if (endIndex < 0)
              endIndex = getViewCount() - 1;
          }

        // Determine hole that comes from added elements (we don't forward
        // the event to newly added views.
        int startAdded = endIndex + 1;
        int endAdded = startAdded;
        Element[] added = (ec != null) ? ec.getChildrenAdded() : null;
        if (added != null && added.length > 0)
          {
            startAdded = ec.getIndex();
            endAdded = startAdded + added.length - 1;
          }

        // Forward event to all views between startIndex and endIndex,
        // and leave out all views in the hole.
        for (int i = startIndex; i <= endIndex; i++)
          {
            // Skip newly added child views.
            if (! (i >= startAdded && i <= endAdded))
              {
                View child = getView(i);
                if (child != null)
                  {
                    Shape childAlloc = getChildAllocation(i, shape);
                    forwardUpdateToView(child, ev, childAlloc, vf);
                  }
              }
          }
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
    if (ec != null && shape != null)
      {
        preferenceChanged(null, true, true);
        Container c = getContainer();
        if (c != null)
          c.repaint();
      }
  }

  /**
   * Maps a position in the document into the coordinate space of the View.
   * The output rectangle usually reflects the font height but has a width
   * of zero.
   *
   * @param pos the position of the character in the model
   * @param a the area that is occupied by the view
   * @param b either {@link Position.Bias#Forward} or
   *        {@link Position.Bias#Backward} depending on the preferred
   *        direction bias. If <code>null</code> this defaults to
   *        <code>Position.Bias.Forward</code>
   *
   * @return a rectangle that gives the location of the document position
   *         inside the view coordinate space
   *
   * @throws BadLocationException if <code>pos</code> is invalid
   * @throws IllegalArgumentException if b is not one of the above listed
   *         valid values
   */
  public abstract Shape modelToView(int pos, Shape a, Position.Bias b)
    throws BadLocationException;

  /**
   * Maps a region in the document into the coordinate space of the View.
   *
   * @param p1 the beginning position inside the document
   * @param b1 the direction bias for the beginning position
   * @param p2 the end position inside the document
   * @param b2 the direction bias for the end position
   * @param a the area that is occupied by the view
   *
   * @return a rectangle that gives the span of the document region
   *         inside the view coordinate space
   *
   * @throws BadLocationException if <code>p1</code> or <code>p2</code> are
   *         invalid
   * @throws IllegalArgumentException if b1 or b2 is not one of the above
   *         listed valid values
   */
  public Shape modelToView(int p1, Position.Bias b1,
			   int p2, Position.Bias b2, Shape a)
    throws BadLocationException
  {
    if (b1 != Position.Bias.Forward && b1 != Position.Bias.Backward)
      throw new IllegalArgumentException
	("b1 must be either Position.Bias.Forward or Position.Bias.Backward");
    if (b2 != Position.Bias.Forward && b2 != Position.Bias.Backward)
      throw new IllegalArgumentException
	("b2 must be either Position.Bias.Forward or Position.Bias.Backward");

    Shape s1 = modelToView(p1, a, b1);
    // Special case for p2 == end index.
    Shape s2;
    if (p2 != getEndOffset())
      {
        s2 = modelToView(p2, a, b2);
      }
    else
      {
        try
          {
            s2 = modelToView(p2, a, b2);
          }
        catch (BadLocationException ex)
          {
            // Assume the end rectangle to be at the right edge of the
            // view.
            Rectangle aRect = a instanceof Rectangle ? (Rectangle) a
                                                     : a.getBounds();
            s2 = new Rectangle(aRect.x + aRect.width - 1, aRect.y, 1,
                               aRect.height);
          }
      }

    // Need to modify the rectangle, so we create a copy in all cases.
    Rectangle r1 = s1.getBounds();
    Rectangle r2 = s2 instanceof Rectangle ? (Rectangle) s2
                                           : s2.getBounds();

    // For multiline view, let the resulting rectangle span the whole view.
    if (r1.y != r2.y)
      {
        Rectangle aRect = a instanceof Rectangle ? (Rectangle) a
                                                 : a.getBounds();
        r1.x = aRect.x;
        r1.width = aRect.width;
      }

    return SwingUtilities.computeUnion(r2.x, r2.y, r2.width, r2.height, r1);
  }

  /**
   * Maps a position in the document into the coordinate space of the View.
   * The output rectangle usually reflects the font height but has a width
   * of zero.
   *
   * This method is deprecated and calls
   * {@link #modelToView(int, Position.Bias, int, Position.Bias, Shape)} with
   * a bias of {@link Position.Bias#Forward}.
   *
   * @param pos the position of the character in the model
   * @param a the area that is occupied by the view
   *
   * @return a rectangle that gives the location of the document position
   *         inside the view coordinate space
   *
   * @throws BadLocationException if <code>pos</code> is invalid
   *
   * @deprecated Use {@link #modelToView(int, Shape, Position.Bias)} instead.
   */
  public Shape modelToView(int pos, Shape a) throws BadLocationException
  {
    return modelToView(pos, a, Position.Bias.Forward);
  }

  /**
   * Maps coordinates from the <code>View</code>'s space into a position
   * in the document model.
   *
   * @param x the x coordinate in the view space
   * @param y the y coordinate in the view space
   * @param a the allocation of this <code>View</code>
   * @param b the bias to use
   *
   * @return the position in the document that corresponds to the screen
   *         coordinates <code>x, y</code>
   */
  public abstract int viewToModel(float x, float y, Shape a, Position.Bias[] b);

  /**
   * Maps coordinates from the <code>View</code>'s space into a position
   * in the document model. This method is deprecated and only there for
   * compatibility.
   *
   * @param x the x coordinate in the view space
   * @param y the y coordinate in the view space
   * @param a the allocation of this <code>View</code>
   *
   * @return the position in the document that corresponds to the screen
   *         coordinates <code>x, y</code>
   *
   * @deprecated Use {@link #viewToModel(float, float, Shape, Position.Bias[])}
   *             instead.
   */
  public int viewToModel(float x, float y, Shape a)
  {
    Position.Bias[] biasRet = new Position.Bias[1];
    biasRet[0] = Position.Bias.Forward;
    return viewToModel(x, y, a, biasRet);
  }

  /**
   * Dumps the complete View hierarchy. This method can be used for debugging
   * purposes.
   */
  protected void dump()
  {
    // Climb up the hierarchy to the parent.
    View parent = getParent();
    if (parent != null)
      parent.dump();
    else
      dump(0);
  }

  /**
   * Dumps the view hierarchy below this View with the specified indentation
   * level.
   *
   * @param indent the indentation level to be used for this view
   */
  void dump(int indent)
  {
    for (int i = 0; i < indent; ++i)
      System.out.print('.');
    System.out.println(this + "(" + getStartOffset() + "," + getEndOffset() + ": " + getElement());

    int count = getViewCount();
    for (int i = 0; i < count; ++i)
      getView(i).dump(indent + 1);
  }

  /**
   * Returns the document position that is (visually) nearest to the given
   * document position <code>pos</code> in the given direction <code>d</code>.
   *
   * @param pos the document position
   * @param b the bias for <code>pos</code>
   * @param a the allocation for this view
   * @param d the direction, must be either {@link SwingConstants#NORTH},
   *        {@link SwingConstants#SOUTH}, {@link SwingConstants#WEST} or
   *        {@link SwingConstants#EAST}
   * @param biasRet an array of {@link Position.Bias} that can hold at least
   *        one element, which is filled with the bias of the return position
   *        on method exit
   *
   * @return the document position that is (visually) nearest to the given
   *         document position <code>pos</code> in the given direction
   *         <code>d</code>
   *
   * @throws BadLocationException if <code>pos</code> is not a valid offset in
   *         the document model
   * @throws IllegalArgumentException if <code>d</code> is not a valid direction
   */
  public int getNextVisualPositionFrom(int pos, Position.Bias b,
                                       Shape a, int d,
                                       Position.Bias[] biasRet)
    throws BadLocationException
  {
    int ret = pos;
    Rectangle r;
    View parent;

    switch (d)
    {
      case EAST:
        // TODO: take component orientation into account?
        // Note: If pos is below zero the implementation will return
        // pos + 1 regardless of whether that value is a correct offset
        // in the document model. However this is what the RI does.
        ret = Math.min(pos + 1, getEndOffset());
        break;
      case WEST:
        // TODO: take component orientation into account?
        ret = Math.max(pos - 1, getStartOffset());
        break;
      case NORTH:
        // Try to find a suitable offset by examining the area above.
        parent = getParent();
        r =  parent.modelToView(pos, a, b).getBounds();
        ret = parent.viewToModel(r.x, r.y - 1, a, biasRet);
        break;
      case SOUTH:
        // Try to find a suitable offset by examining the area below. 
        parent = getParent();
        r =  parent.modelToView(pos, a, b).getBounds();
        ret = parent.viewToModel(r.x + r.width, r.y + r.height, a, biasRet);
        break;
      default:
        throw new IllegalArgumentException("Illegal value for d");
    }
    
    return ret;
  }
}
