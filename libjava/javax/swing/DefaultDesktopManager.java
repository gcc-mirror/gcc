/* DefaultDesktopManager.java --
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


package javax.swing;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Insets;
import java.awt.Rectangle;
import java.beans.PropertyVetoException;
import java.io.Serializable;

import javax.swing.JInternalFrame.JDesktopIcon;

/**
 * DefaultDesktopManager is the default implementation of DesktopManager for
 * swing. It implements the basic beaviours for JInternalFrames in arbitrary
 * parents. The methods provided by the class are not meant to be called by
 * the user, instead, the JInternalFrame methods will call these methods.
 */
public class DefaultDesktopManager implements DesktopManager, Serializable
{
  /** DOCUMENT ME! */
  private static final long serialVersionUID = 4657624909838017887L;

  /** The property change event fired when the wasIcon property changes. */
  static final String WAS_ICON_ONCE_PROPERTY = "wasIconOnce";

  /**
   * The method of dragging used by the JDesktopPane that parents the
   * JInternalFrame that is being dragged.
   */
  private int currentDragMode = 0;

  /**
   * The cache of the bounds used to draw the outline rectangle when
   * OUTLINE_DRAG_MODE is used.
   */
  private transient Rectangle dragCache = new Rectangle();

  /**
   * A cached JDesktopPane that is stored when the JInternalFrame is initially
   * dragged.
   */
  private transient Container pane;

  /**
   * An array of Rectangles that holds the bounds of the JDesktopIcons in the
   * JDesktopPane when looking for where to place a new icon.
   */
  private transient Rectangle[] iconRects;

  /**
   * This creates a new DefaultDesktopManager object.
   */
  public DefaultDesktopManager()
  {
  }

  /**
   * This method is not normally called since the user will typically add the
   * JInternalFrame to a Container. If this is called, it will try to
   * determine the parent of the JInternalFrame and remove any icon that
   * represents this JInternalFrame and add this JInternalFrame.
   *
   * @param frame The JInternalFrame to open.
   */
  public void openFrame(JInternalFrame frame)
  {
    Container c = frame.getParent();
    if (c == null)
      c = frame.getDesktopIcon().getParent();
    if (c == null)
      return;

    c.remove(frame.getDesktopIcon());
    c.add(frame);
    frame.setVisible(true);
  }

  /**
   * This method removes the JInternalFrame and JDesktopIcon (if one is
   * present) from their parents.
   *
   * @param frame The JInternalFrame to close.
   */
  public void closeFrame(JInternalFrame frame)
  {
    Container c = frame.getParent();
    frame.doDefaultCloseAction();

    if (c != null)
      {
	if (frame.isIcon())
	  c.remove(frame.getDesktopIcon());
	else
	  c.remove(frame);
	c.repaint();
      }
  }

  /**
   * This method resizes the JInternalFrame to match its parent's bounds.
   *
   * @param frame The JInternalFrame to maximize.
   */
  public void maximizeFrame(JInternalFrame frame)
  {
    // Can't maximize from iconified state.
    // It can only return to maximized state, but that would fall under
    // deiconify.
    if (frame.isIcon())
      return;
    frame.setNormalBounds(frame.getBounds());

    Container p = frame.getParent();
    if (p != null)
      {
	Rectangle pBounds = p.getBounds();
	Insets insets = p.getInsets();
	pBounds.width -= insets.left + insets.right;
	pBounds.height -= insets.top + insets.bottom;

	setBoundsForFrame(frame, 0, 0, pBounds.width, pBounds.height);
      }
    if (p instanceof JDesktopPane)
      ((JDesktopPane) p).setSelectedFrame(frame);
    else
      {
	try
	  {
	    frame.setSelected(true);
	  }
	catch (PropertyVetoException e)
	  {
	    // Do nothing.
	  }
      }
  }

  /**
   * This method restores the JInternalFrame's bounds to what they were
   * previous to the setMaximize call.
   *
   * @param frame The JInternalFrame to minimize.
   */
  public void minimizeFrame(JInternalFrame frame)
  {
    Rectangle normalBounds = frame.getNormalBounds();

    JDesktopPane p = frame.getDesktopPane();
    if (p != null)
      p.setSelectedFrame(frame);
    else
      {
	try
	  {
	    frame.setSelected(true);
	  }
	catch (PropertyVetoException e)
	  {
	    // Do nothing.
	  }
      }

    setBoundsForFrame(frame, normalBounds.x, normalBounds.y,
                      normalBounds.width, normalBounds.height);
  }

  /**
   * This method removes the JInternalFrame from its parent and adds its
   * JDesktopIcon representation.
   *
   * @param frame The JInternalFrame to iconify.
   */
  public void iconifyFrame(JInternalFrame frame)
  {
    JDesktopPane p = frame.getDesktopPane();
    JDesktopIcon icon = frame.getDesktopIcon();
    if (p != null && p.getSelectedFrame() == frame)
      p.setSelectedFrame(null);
    else
      {
	try
	  {
	    frame.setSelected(false);
	  }
	catch (PropertyVetoException e)
	  {
	  }
      }

    Container c = frame.getParent();

    if (! wasIcon(frame))
      {
	Rectangle r = getBoundsForIconOf(frame);
	icon.setBounds(r);
	setWasIcon(frame, Boolean.TRUE);
      }

    if (c != null)
      {
	if (icon != null)
	  {
	    c.add(icon);
	    icon.setVisible(true);
	  }
	c.remove(frame);
      }
  }

  /**
   * This method removes the JInternalFrame's JDesktopIcon representation and
   * adds the JInternalFrame back to its parent.
   *
   * @param frame The JInternalFrame to deiconify.
   */
  public void deiconifyFrame(JInternalFrame frame)
  {
    JDesktopIcon icon = frame.getDesktopIcon();
    Container c = icon.getParent();

    removeIconFor(frame);
    c.add(frame);
    frame.setVisible(true);

    if (! frame.isSelected())
      {
	JDesktopPane p = frame.getDesktopPane();
	if (p != null)
	  p.setSelectedFrame(frame);
	else
	  {
	    try
	      {
		frame.setSelected(true);
	      }
	    catch (PropertyVetoException e)
	      {
		// Do nothing.
	      }
	  }
      }

    c.invalidate();
  }

  /**
   * This method activates the JInternalFrame by moving it to the front and
   * selecting it.
   *
   * @param frame The JInternalFrame to activate.
   */
  public void activateFrame(JInternalFrame frame)
  {
    JDesktopPane p = frame.getDesktopPane();

    if (p != null)
      p.setSelectedFrame(frame);
    else
      {
	try
	  {
	    frame.setSelected(true);
	  }
	catch (PropertyVetoException e)
	  {
	  }
      }

    frame.toFront();
  }

  /**
   * This method is called when the JInternalFrame loses focus.
   *
   * @param frame The JInternalFram to deactivate.
   */
  public void deactivateFrame(JInternalFrame frame)
  {
    JDesktopPane p = frame.getDesktopPane();
    if (p != null)
      {
	if (p.getSelectedFrame() == frame)
	  p.setSelectedFrame(null);
      }
    else
      {
	try
	  {
	    frame.setSelected(false);
	  }
	catch (PropertyVetoException e)
	  {
	  }
      }
  }

  /**
   * This method is called to indicate that the DesktopManager should prepare
   * to drag the JInternalFrame. Any state information needed to drag the
   * frame will be prepared now.
   *
   * @param component The JComponent to drag, usually a JInternalFrame.
   */
  public void beginDraggingFrame(JComponent component)
  {
    if (component instanceof JDesktopIcon)
      pane = ((JDesktopIcon) component).getInternalFrame().getDesktopPane();
    else
      pane = ((JInternalFrame) component).getDesktopPane();
    if (pane == null)
      return;

    dragCache = component.getBounds();

    if (! (pane instanceof JDesktopPane))
      currentDragMode = JDesktopPane.LIVE_DRAG_MODE;
    else
      currentDragMode = ((JDesktopPane) pane).getDragMode();
  }

  /**
   * This method is called to drag the JInternalFrame to a new location.
   *
   * @param component The JComponent to drag, usually a JInternalFrame.
   * @param newX The new x coordinate.
   * @param newY The new y coordinate.
   */
  public void dragFrame(JComponent component, int newX, int newY)
  {
    if (currentDragMode == JDesktopPane.OUTLINE_DRAG_MODE)
      {
	// FIXME: Do outline drag mode painting.
      }
    else
      {
	Rectangle b = component.getBounds();
	if (component instanceof JDesktopIcon)
	  component.setBounds(newX, newY, b.width, b.height);
	else
	  setBoundsForFrame((JInternalFrame) component, newX, newY, b.width,
	                    b.height);
      }
  }

  /**
   * This method indicates that the dragging is done. Any state information
   * stored by the DesktopManager can be cleared.
   *
   * @param component The JComponent that has finished dragging.
   */
  public void endDraggingFrame(JComponent component)
  {
    if (currentDragMode == JDesktopPane.OUTLINE_DRAG_MODE)
      {
	setBoundsForFrame((JInternalFrame) component, dragCache.x,
	                  dragCache.y, dragCache.width, dragCache.height);
	pane = null;
	dragCache = null;
      }
    component.repaint();
  }

  /**
   * This method is called to indicate that the given JComponent will be
   * resized. Any state information necessary to resize the JComponent will
   * be prepared now.
   *
   * @param component The JComponent to resize, usually a JInternalFrame.
   * @param direction The direction to drag in (a SwingConstant).
   */
  public void beginResizingFrame(JComponent component, int direction)
  {
    pane = ((JInternalFrame) component).getDesktopPane();
    if (pane == null)
      return;

    dragCache = component.getBounds();
    if (! (pane instanceof JDesktopPane))
      currentDragMode = JDesktopPane.LIVE_DRAG_MODE;
    else
      currentDragMode = ((JDesktopPane) pane).getDragMode();
  }

  /**
   * This method resizes the give JComponent.
   *
   * @param component The JComponent to resize.
   * @param newX The new x coordinate.
   * @param newY The new y coordinate.
   * @param newWidth The new width.
   * @param newHeight The new height.
   */
  public void resizeFrame(JComponent component, int newX, int newY,
                          int newWidth, int newHeight)
  {
    dragCache.setBounds(newX, newY, newWidth, newHeight);

    if (currentDragMode == JDesktopPane.OUTLINE_DRAG_MODE)
      {
	// FIXME: Do outline drag painting.
      }
    else
      setBoundsForFrame(component, dragCache.x, dragCache.y, dragCache.width,
                        dragCache.height);
  }

  /**
   * This method is called to indicate that the given JComponent has finished
   * dragging. Any state information stored by the DesktopManager can be
   * cleared.
   *
   * @param component The JComponent that finished resizing.
   */
  public void endResizingFrame(JComponent component)
  {
    if (currentDragMode == JDesktopPane.OUTLINE_DRAG_MODE)
      {
	setBoundsForFrame((JInternalFrame) component, dragCache.x,
	                  dragCache.y, dragCache.width, dragCache.height);
	pane = null;
	dragCache = null;
      }
    component.repaint();
  }

  /**
   * This method calls setBounds with the given parameters and repaints the
   * JComponent.
   *
   * @param component The JComponent to set bounds for.
   * @param newX The new x coordinate.
   * @param newY The new y coordinate.
   * @param newWidth The new width.
   * @param newHeight The new height.
   */
  public void setBoundsForFrame(JComponent component, int newX, int newY,
                                int newWidth, int newHeight)
  {
    component.setBounds(newX, newY, newWidth, newHeight);
    component.revalidate();

    // If not null, I'd rather repaint the parent
    if (component.getParent() != null)
      component.getParent().repaint();
    else
      component.repaint();
  }

  /**
   * This is a helper method that removes the JDesktopIcon of the given
   * JInternalFrame from the parent.
   *
   * @param frame The JInternalFrame to remove an icon for.
   */
  protected void removeIconFor(JInternalFrame frame)
  {
    JDesktopIcon icon = frame.getDesktopIcon();
    Container c = icon.getParent();
    if (c != null && icon != null)
      c.remove(icon);
  }

  /**
   * This method is called by iconifyFrame to determine the bounds of the
   * JDesktopIcon for the given JInternalFrame.
   *
   * @param frame The JInternalFrame to find the bounds of its JDesktopIcon
   *        for.
   *
   * @return The bounds of the JDesktopIcon.
   */
  protected Rectangle getBoundsForIconOf(JInternalFrame frame)
  {
    // IconRects has no order to it.
    // The icon _must_ be placed in the first free slot (working from 
    // the bottom left corner)
    // The icon also must not be placed where another icon is placed 
    // (regardless whether that frame is an icon currently or not)
    JDesktopPane desktopPane = frame.getDesktopPane();
    Rectangle paneBounds = desktopPane.getBounds();
    Insets insets = desktopPane.getInsets();
    Dimension pref = frame.getDesktopIcon().getPreferredSize();

    if (desktopPane == null)
      return frame.getDesktopIcon().getBounds();

    Component[] frames = desktopPane.getComponents();

    int count = 0;
    for (int i = 0, j = 0; i < frames.length; i++)
      if (frames[i] instanceof JDesktopIcon
          || frames[i] instanceof JInternalFrame
          && ((JInternalFrame) frames[i]).getWasIcon() && frames[i] != frame)
	count++;
    iconRects = new Rectangle[count];
    for (int i = 0, j = 0; i < frames.length; i++)
      if (frames[i] instanceof JDesktopIcon)
	iconRects[--count] = frames[i].getBounds();
      else if (frames[i] instanceof JInternalFrame
               && ((JInternalFrame) frames[i]).getWasIcon()
               && frames[i] != frame)
	iconRects[--count] = ((JInternalFrame) frames[i]).getDesktopIcon()
	                      .getBounds();

    int startingX = insets.left;
    int startingY = paneBounds.height - insets.bottom - pref.height;
    Rectangle ideal = new Rectangle(startingX, startingY, pref.width,
                                    pref.height);
    boolean clear = true;

    while (iconRects.length > 0)
      {
	clear = true;
	for (int i = 0; i < iconRects.length; i++)
	  {
	    if (iconRects[i] != null && iconRects[i].intersects(ideal))
	      {
		clear = false;
		break;
	      }
	  }
	if (clear)
	  return ideal;

	startingX += pref.width;
	if (startingX + pref.width > paneBounds.width - insets.right)
	  {
	    startingX = insets.left;
	    startingY -= pref.height;
	  }
	ideal.setBounds(startingX, startingY, pref.width, pref.height);
      }

    return ideal;
  }

  /**
   * This method sets the bounds of the JInternalFrame right before the
   * maximizeFrame call.
   *
   * @param frame The JInternalFrame being maximized.
   * @param rect The normal bounds.
   */
  protected void setPreviousBounds(JInternalFrame frame, Rectangle rect)
  {
    frame.setNormalBounds(rect);
  }

  /**
   * This method returns the normal bounds of the JInternalFrame from before
   * the maximize call.
   *
   * @param frame The JInternalFrame that is being restored.
   *
   * @return The previous bounds of the JInternalFrame.
   */
  protected Rectangle getPreviousBounds(JInternalFrame frame)
  {
    return frame.getNormalBounds();
  }

  /**
   * This method sets the value to true if the given JInternalFrame has been
   * iconized and the bounds of its DesktopIcon are valid.
   *
   * @param frame The JInternalFrame for the JDesktopIcon.
   * @param value True if the JInternalFrame has been iconized and the bounds
   *        of the JDesktopIcon are valid.
   */
  protected void setWasIcon(JInternalFrame frame, Boolean value)
  {
    frame.setWasIcon(value.booleanValue(), WAS_ICON_ONCE_PROPERTY);
  }

  /**
   * This method returns true if the given JInternalFrame has been iconized
   * and the bounds of its DesktopIcon are valid.
   *
   * @param frame The JInternalFrame for the JDesktopIcon.
   *
   * @return True if the given JInternalFrame has been iconized and the bounds
   *         of its DesktopIcon are valid.
   */
  protected boolean wasIcon(JInternalFrame frame)
  {
    return frame.getWasIcon();
  }
}
