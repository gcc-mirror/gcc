/* AccessibleComponent.java -- aids in accessibly rendering Java components
   Copyright (C) 2000, 2001, 2002, 2005  Free Software Foundation, Inc.

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

package javax.accessibility;

import java.awt.Color;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.FocusListener;

/**
 * Objects which are to be rendered to a screen as part of a graphical
 * user interface should implement this interface.  Accessibility
 * software can use the implementations of this interface to determine
 * and set the screen representation for an object.
 *
 * <p>The <code>AccessibleContext.getAccessibleComponent()</code> method
 * should return <code>null</code> if an object does not implement this
 * interface.
 *
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see Accessible
 * @see AccessibleContext
 * @see AccessibleContext#getAccessibleComponent()
 * @since 1.2
 * @status updated to 1.4
 */
public interface AccessibleComponent
{
  /**
   * Get the background color of this component.
   *
   * @return the background color of this component, or null if not supported
   * @see #setBackground(Color)
   */
  Color getBackground();

  /**
   * Set the background color of this component to the specified color.
   *
   * @param color the color to set the background to
   * @see #getBackground()
   */
  void setBackground(Color color);

  /**
   * Get the foreground color of this component.
   *
   * @return the foreground color of this component, or null if not supported
   * @see #setForeground(Color)
   */
  Color getForeground();

  /**
   * Set the foreground color of this component.
   *
   * @param color the color to set the foreground to
   * @see #getForeground()
   */
  void setForeground(Color color);

  /**
   * Get the cursor of this component.
   *
   * @return the Cursor of this component, or null if not supported
   * @see #setCursor(Cursor)
   */
  Cursor getCursor();

  /**
   * Set the cursor of the component.
   *
   * @param cursor the graphical representation of the cursor to use
   * @see #getCursor()
   */
  void setCursor(Cursor cursor);

  /**
   * Get the font of this component
   *
   * @return the font of the component, or null if not supported
   * @see setFont(Font)
   */
  Font getFont();

  /**
   * Set the font of this component.
   *
   * @param font the font to use
   * @see #getFont()
   */
  void setFont(Font font);

  /**
   * Get the <code>FontMetrics</code> of the specified font in this component.
   *
   * @param font the specified font
   * @return the metrics for the specified font, or null if not supported
   * @throws NullPointerException if font is null
   * @see #getFont()
   */
  FontMetrics getFontMetrics(Font font);

  /**
   * Indicates whether or not this component is enabled. An object which is
   * enabled also has AccessibleState.ENABLED in its StateSet.
   *
   * @return true if the component is enabled
   * @see #setEnabled(boolean)
   * @see AccessibleContext#getAccessibleStateSet()
   * @see AccessibleState#ENABLED
   */
  boolean isEnabled();

  /**
   * Set this component to an enabled or disabled state.
   *
   * @param b true to enable the component, else disable it
   * @see #isEnabled()
   */
  void setEnabled(boolean b);

  /**
   * Indicates whether or not this component is visible or intends to be
   * visible although one of its ancestors may not be. An object which is
   * visible also has AccessibleState.VISIBLE in its StateSet. Check
   * <code>isShowing()</code> to see if the object is on screen.
   *
   * @return true if the component is visible
   * @see #setVisible(boolean)
   * @see AccessibleContext#getAccessibleStateSet()
   * @see AccessibleState#VISIBLE
   */
  boolean isVisible();

  /**
   * Set the visible state of this component.
   *
   * @param b true to make the component visible, else hide it
   * @see #isVisible()
   */
  void setVisible(boolean b);

  /**
   * Indicates whether or not this component is visible by checking
   * the visibility of this component and its ancestors. The component may
   * be hidden on screen by another component like pop-up help. An object
   * which is showing on screen also has AccessibleState.SHOWING in its
   * StateSet.
   *
   * @return true if component and ancestors are visible
   * @see #isVisible()
   * @see #setVisible(boolean)
   * @see AccessibleContext#getAccessibleStateSet()
   * @see AccessibleState#SHOWING
   */
  boolean isShowing();

  /**
   * Tests whether or not the specified point is contained within
   * this component.  The coordinates are specified relative to this
   * component's coordinate system.
   *
   * @param point the Point to locate
   * @return true if the point is within this component
   * @throws NullPointerException if point is null
   * @see #getBounds()
   */
  boolean contains(Point point);

  /**
   * Get the location of this component in the screen's coordinate space.
   * The point specified is the top-left corner of this component.
   *
   * @return the location on screen, or null if off-screen
   * @see #getBounds()
   * @see #getLocation()
   */
  Point getLocationOnScreen();

  /**
   * Get the location of this component in the parent's coordinate system.
   * The point specified is the top-left corner of this component.
   *
   * @return the location in the parent on screen, or null if off-screen
   * @see #getBounds()
   * @see #getLocationOnScreen()
   * @see #setLocation(Point)
   */
  Point getLocation();

  /**
   * Set the location of this component relative to its parent.  The point
   * specified represents the top-left corner of this component.
   *
   * @param point the top-left corner of this component relative to the parent
   * @throws NullPointerException if point is null
   * @see #getLocation()
   */
  void setLocation(Point point);

  /**
   * Get the bounds of this component relative to its parent - it's width,
   * height, and relative location to its parent.
   *
   * @return the bounds of this component, or null if not on screen
   * @see #contains(Point)
   */
  Rectangle getBounds();

  /**
   * Set the bounds of this component to the specified height and width, and
   * relative location to its parent.
   *
   * @param rectangle the new height, width, and relative location
   * @throws NullPointerException if rectangle is null
   */
  void setBounds(Rectangle rectangle);

  /**
   * Get the size of this component - it's width and height.
   *
   * @return the dimensions of this component, or null if not on screen
   * @see #setSize(Dimension)
   */
  Dimension getSize();

  /**
   * Set the size of this component to the given dimensions.
   *
   * @param dimension the new size of the component
   * @throws NullPointerException if dimension is null
   * @see #getSize()
   */
  void setSize(Dimension dimension);

  /**
   * If an object exists at the specified point which is a child of this
   * parent component, and it is accessible, then it is returned.
   *
   * @param point the location within this component's coordinate system
   * @return the accessible child object at that point, or null
   */
  Accessible getAccessibleAt(Point point);

  /**
   * Indicates whether or not this component can accept focus. An object
   * which can accept focus also has AccessibleState.FOCUSABLE in its
   * StateSet.
   *
   * @return true if the component can accept focus
   * @see AccessibleContext#getAccessibleStateSet()
   * @see AccessibleState#FOCUSABLE
   * @see AccessibleState#FOCUSED
   */
  boolean isFocusTraversable();

  /**
   * If this method is called this component will attempt to gain focus,
   * but if it cannot accept focus nothing happens. On success, the StateSet
   * will contain AccessibleState.FOCUSED
   *
   * @see #isFocusTraversable()
   * @see AccessibleState#FOCUSED
   */
  void requestFocus();

  /**
   * Adds the specified listener to this component.
   *
   * @param listener the listener to add to this component
   * @see #removeFocusListener(FocusListener)
   */
  void addFocusListener(FocusListener listener);

  /**
   * Removes the specified listener from this component.
   *
   * @param listener the listener to remove
   * @see #addFocusListener(FocusListener)
   */
  void removeFocusListener(FocusListener listener);
} // interface AccessibleComponent
