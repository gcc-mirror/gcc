/* ScrollPane.java -- Scrolling window
   Copyright (C) 1999, 2002 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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


package java.awt;

import java.awt.peer.ScrollPanePeer;
import java.awt.peer.ContainerPeer;
import java.awt.peer.ComponentPeer;
import java.io.Serializable;
import javax.accessibility.Accessible;

/**
  * This widget provides a scrollable region that allows a single 
  * subcomponent to be viewed through a smaller window.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public class ScrollPane extends Container implements Accessible, Serializable
{

/*
 * Static Variables
 */

/**
  * Constant indicating that scrollbars are created as needed in this
  * windows.
  */
public static final int SCROLLBARS_AS_NEEDED = 0;

/**
  * Constant indicating that scrollbars are always displayed in this
  * window.
  */
public static final int SCROLLBARS_ALWAYS = 1;

/**
  * Constant indicating that scrollbars are never displayed in this window.
  */
public static final int SCROLLBARS_NEVER = 2;

// Serialization constant
private static final long serialVersionUID = 7956609840827222915L;

/*************************************************************************/

/*
 * Instance Variables
 */

/**
  * @serial The horizontal scrollbar for this window.  The methods
  * <code>setMinimum()</code>, <code>setMaximum</code>, and
  * <code>setVisibleAmount</code> must not be called on this scrollbar.
  */
private ScrollPaneAdjustable hAdjustable;

/**
  * @serial The vertical scrollbar for this window.  The methods
  * <code>setMinimum()</code>, <code>setMaximum</code>, and
  * <code>setVisibleAmount</code> must not be called on this scrollbar.
  */
private ScrollPaneAdjustable vAdjustable;

/**
  * @serial Indicates when scrollbars are displayed in this window, will
  * be one of the constants from this class.
  */
private int scrollbarDisplayPolicy;

// Current scroll position
private Point scrollPosition = new Point(0, 0);

/*************************************************************************/

/*
 * Constructors
 */

/**
  * Initializes a new instance of <code>ScrollPane</code> with a default
  * scrollbar policy of <code>SCROLLBARS_AS_NEEDED</code>.
  *
  * @exception HeadlessException If GraphicsEnvironment.isHeadless() is true.
  */
public
ScrollPane()
{
  this(SCROLLBARS_AS_NEEDED);
}

/*************************************************************************/

/**
  * Initializes a new instance of <code>ScrollPane</code> with the
  * specified scrollbar policy.
  *
  * @param scrollbarDisplayPolicy When to display scrollbars, which must
  * be one of the constants defined in this class.
  *
  * @exception HeadlessException If GraphicsEnvironment.isHeadless() is true.
  */
public
ScrollPane(int scrollbarDisplayPolicy)
{
  if (GraphicsEnvironment.isHeadless ())
    throw new HeadlessException ();

  this.scrollbarDisplayPolicy = scrollbarDisplayPolicy;

  if (scrollbarDisplayPolicy != SCROLLBARS_ALWAYS
      && scrollbarDisplayPolicy != SCROLLBARS_AS_NEEDED
      && scrollbarDisplayPolicy != SCROLLBARS_NEVER)
    throw new IllegalArgumentException("Bad scrollbarDisplayPolicy: " +
                                       scrollbarDisplayPolicy);

  if (scrollbarDisplayPolicy != SCROLLBARS_NEVER)
    {
      hAdjustable = new ScrollPaneAdjustable(Scrollbar.HORIZONTAL);
      vAdjustable = new ScrollPaneAdjustable(Scrollbar.VERTICAL);
    }
}

/*************************************************************************/

/*
 * Instance Variables
 */

/**
  * Returns the current scrollbar display policy.
  *
  * @return The current scrollbar display policy.
  */
public int
getScrollbarDisplayPolicy()
{
  return(scrollbarDisplayPolicy);
}

/*************************************************************************/

/**
  * Returns the horizontal scrollbar for this object.  If the scrollbar
  * display policy is set to <code>SCROLLBARS_NEVER</code> then this
  * will be <code>null</code>.
  *
  * @return The horizontal scrollbar for this window.
  */
public Adjustable
getHAdjustable()
{
  return(hAdjustable);
}

/*************************************************************************/

/**
  * Returns the vertical scrollbar for this object.  If the scrollbar
  * display policy is set to <code>SCROLLBARS_NEVER</code> then this
  * will be <code>null</code>.
  *
  * @return The horizontal scrollbar for this window.
  */
public Adjustable
getVAdjustable()
{
  return(vAdjustable);
}

/*************************************************************************/

/**
  * Returns the current viewport size.  The viewport is the region of
  * this object's window where the child is actually displayed.
  *
  * @return The viewport size.
  */
public Dimension
getViewportSize()
{
  Dimension viewsize = getSize();
  Insets insets = getInsets();
  viewsize.width = viewsize.width - (insets.left + insets.right);
  viewsize.height = viewsize.height - (insets.top + insets.bottom);

  ScrollPaneAdjustable v = (ScrollPaneAdjustable)getVAdjustable();
  ScrollPaneAdjustable h = (ScrollPaneAdjustable)getHAdjustable();

  if ((v != null) && v.isVisible())
    viewsize.width = viewsize.width - v.getSize().width;
  if ((h != null) && h.isVisible())
    viewsize.height = viewsize.height - v.getSize().height;

  return(viewsize);
}

/*************************************************************************/

/**
  * Returns the height of a horizontal scrollbar.
  *
  * @return The height of a horizontal scrollbar.
  */
public int
getHScrollbarHeight()
{
  ScrollPanePeer spp = (ScrollPanePeer)getPeer();
  if (spp != null)
    return(spp.getHScrollbarHeight());
  else
    return(0); // FIXME: What to do here?
}

/*************************************************************************/

/**
  * Returns the width of a vertical scrollbar.
  *
  * @return The width of a vertical scrollbar.
  */
public int
getVScrollbarWidth()
{
  ScrollPanePeer spp = (ScrollPanePeer)getPeer();
  if (spp != null)
    return(spp.getVScrollbarWidth());
  else
    return(0); // FIXME: What to do here?
}

/*************************************************************************/

/**
  * Returns the current scroll position of the viewport.
  *
  * @return The current scroll position of the viewport.
  */
public Point
getScrollPosition()
{
  int x = 0;
  int y = 0;

  Adjustable v = getVAdjustable();
  Adjustable h = getHAdjustable();

  if (v != null)
    y = v.getValue();
  if (h != null)
    x = h.getValue();

  return(new Point(x, y));
}

/*************************************************************************/

/**
  * Sets the scroll position to the specified value.
  *
  * @param scrollPosition The new scrollPosition.
  *
  * @exception IllegalArgumentException If the specified value is outside
  * the legal scrolling range.
  */
public void
setScrollPosition(Point scrollPosition) throws IllegalArgumentException
{
  setScrollPosition(scrollPosition.x, scrollPosition.y);
}

/*************************************************************************/

/**
  * Sets the scroll position to the specified value.
  *
  * @param x The new X coordinate of the scroll position.
  * @param y The new Y coordinate of the scroll position.
  *
  * @exception IllegalArgumentException If the specified value is outside
  * the legal scrolling range.
  */
public void
setScrollPosition(int x, int y)
{
  Adjustable h = getHAdjustable();
  Adjustable v = getVAdjustable();

  if (h != null)
    h.setValue(x);
  if (v != null)
    v.setValue(y);

  ScrollPanePeer spp = (ScrollPanePeer)getPeer();
  if (spp != null)
    spp.setScrollPosition(x, y);
}

/*************************************************************************/

/**
  * Notifies this object that it should create its native peer.
  */
public void
addNotify()
{
  if (getPeer() == null)
    return;

  setPeer((ComponentPeer)getToolkit().createScrollPane(this));

  if (hAdjustable != null)
    hAdjustable.addNotify();
  if (vAdjustable != null)
    vAdjustable.removeNotify();
}

/*************************************************************************/

/**
  * Notifies this object that it should destroy its native peers.
  */
public void
removeNotify()
{
  if (hAdjustable != null)
    hAdjustable.removeNotify();
  if (vAdjustable != null)
    vAdjustable.removeNotify();

  super.removeNotify();
}

/*************************************************************************/

/**
  * Adds the specified child component to this container.  A 
  * <code>ScrollPane</code> can have at most one child, so if a second
  * one is added, then first one is removed.
  *
  * @param component The component to add to this container.
  * @param constraints A list of layout constraints for this object.
  * @param index The index at which to add the child, which is ignored
  * in this implementation.
  */
public final void
addImpl(Component component, Object constraints, int index)
{
  Component[] list = getComponents();
  if ((list != null) && (list.length > 0))
    remove(list[0]);

  super.addImpl(component, constraints, -1);

  doLayout();
}

/*************************************************************************/

/**
  * Lays out this component.  This consists of resizing the sole child
  * component to its perferred size.
  */
public void
doLayout()
{
  Component[] list = getComponents();
  if ((list != null) && (list.length > 0))
    {
      Dimension dim = list[0].getPreferredSize();
      list[0].resize(dim);

      Point p = getScrollPosition();
      if (p.x > dim.width)
        p.x = dim.width;
      if (p.y > dim.height)
        p.y = dim.height;

      setScrollPosition(p);
    }
}

/*************************************************************************/

/**
  * Lays out this component.  This consists of resizing the sole child
  * component to its perferred size.
  *
  * @deprecated This method is deprecated in favor of
  * <code>doLayout()</code>.
  */
public void
layout()
{
  doLayout();
}

/*************************************************************************/

/**
  * This method overrides its superclass method to ensure no layout
  * manager is set for this container.  <code>ScrollPane</code>'s do
  * not have layout managers.
  *
  * @param layoutManager Ignored
  */
public final void
setLayout(LayoutManager layoutManager)
{
  return;
}

/*************************************************************************/

/**
  * Prints all of the components in this container.
  *
  * @param graphics The desired graphics context for printing.
  */
public void
printComponents(Graphics graphics)
{
  super.printComponents(graphics);
}

/*************************************************************************/

/**
  * Returns a debug string for this object.
  *
  * @return A debug string for this object.
  */
public String
paramString()
{
  return(getClass().getName());
}

} // class ScrollPane 

