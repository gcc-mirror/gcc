/* Copyright (C) 1999, 2000, 2001, 2002  Free Software Foundation

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
import java.awt.event.*;
import java.awt.image.*;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.lang.reflect.*;
import java.util.EventListener;
import java.util.Locale;
import java.util.ResourceBundle;
import java.util.Vector;
import java.awt.peer.ComponentPeer;
import java.awt.peer.LightweightPeer;
import java.beans.PropertyChangeSupport;
import java.beans.PropertyChangeListener;
// import javax.accessibility.AccessibleContext;

/**
  * The root of all evil.
  *
  * Status: Incomplete. The event dispatch mechanism is implemented. All 
  * other methods defined in the J2SE 1.3 API javadoc exist, but are mostly 
  * incomplete or only stubs; except for methods relating to the Drag and Drop, 
  * Input Method, and Accessibility frameworks: These methods are present but 
  * commented out.
  */
public abstract class Component implements ImageObserver, MenuContainer, 
					   java.io.Serializable
{
  /**
   * Constant returned by the <code>getAlignmentY</code> method to indicate
   * that the component wishes to be aligned to the bottom relative to
   * other components.
   */
  public static final float BOTTOM_ALIGNMENT = (float)1.0;

  /**
   * Constant returned by the <code>getAlignmentY</code> and 
   * <code>getAlignmentX</code> methods to indicate
   * that the component wishes to be aligned to the center relative to
   * other components.
   */
  public static final float CENTER_ALIGNMENT = (float)0.5;

  /**
   * Constant returned by the <code>getAlignmentY</code> method to indicate
   * that the component wishes to be aligned to the top relative to
   * other components.
   */
  public static final float TOP_ALIGNMENT = (float)0.0;

  /**
   * Constant returned by the <code>getAlignmentX</code> method to indicate
   * that the component wishes to be aligned to the right relative to
   * other components.
   */
  public static final float RIGHT_ALIGNMENT = (float)1.0;

  /**
   * Constant returned by the <code>getAlignmentX</code> method to indicate
   * that the component wishes to be aligned to the left relative to
   * other components.
   */
  public static final float LEFT_ALIGNMENT = (float)0.0;

  /* Make the treelock a String so that it can easily be identified
     in debug dumps. We clone the String in order to avoid a conflict in 
     the unlikely event that some other package uses exactly the same string
     as a lock object. */
  static Object treeLock = new String("AWT_TREE_LOCK");

  /* Serialized fields from the serialization spec. */
  // FIXME: Default values?
  int x;
  int y;
  int width;
  int height;
  Color foreground;
  Color background;
  Font font;
  Font peerFont;
  Cursor cursor;
  Locale locale;
  boolean visible = true; // default (except for Window)
  boolean enabled = true;
  boolean valid;
  boolean hasFocus;
  //DropTarget dropTarget;
  Vector popups;
  String name;
  boolean nameExplicitlySet;
  Dimension minSize;
  Dimension prefSize;
  boolean newEventsOnly;  
  long eventMask = AWTEvent.PAINT_EVENT_MASK;
  PropertyChangeSupport changeSupport;
  boolean isPacked;
  int componentSerializedDataVersion;
  /* AccessibleContext accessibleContext; */

  /* Anything else is non-serializable, and should be declared "transient". */
  transient Container parent;
  transient java.awt.peer.ComponentPeer peer;

  transient ComponentListener componentListener;
  transient FocusListener focusListener;
  transient KeyListener keyListener;
  transient MouseListener mouseListener;
  transient MouseMotionListener mouseMotionListener;
  transient InputMethodListener inputMethodListener;
  transient HierarchyListener hierarchyListener;
  transient HierarchyBoundsListener hierarchyBoundsListener;

  transient ComponentOrientation orientation = ComponentOrientation.UNKNOWN;

  /**
   * Default constructor for subclasses.
   */
  protected Component()
  {
  }

  /**
   * Returns the name of this component.
   *
   * @return The name of this component.
   */
  public String getName()
  {
    if (name == null && !nameExplicitlySet)
      name = generateName();
    return name;
  }

  /**
   * Sets the name of this component to the specified name.
   *
   * @param name The new name of this component.
   */
  public void setName(String name)
  {
    nameExplicitlySet = true;
    this.name = name;
  }
  
  /** Subclasses should override this to return unique component names like 
    * "menuitem0".
    */
  String generateName()
  {
    // Component is abstract.
    return null;
  }

  /**
   * Returns the parent of this component.
   * 
   * @return The parent of this component.
   */
  public Container getParent()
  {
    return parent;  
  }

  // Sets the peer for this component.
  final void setPeer (ComponentPeer peer)
  {
    this.peer = peer;
  }

  /**
   * Returns the native windowing system peer for this component.
   *
   * @return The peer for this component.
   * @deprecated
   */
  // Classpath's Gtk peers rely on this.
  public java.awt.peer.ComponentPeer getPeer()
  {
    return peer;
  }

  // FIXME: java.awt.dnd classes not yet implemented
  /*
  public void setDropTarget(DropTarget dt)
  {
    this.dropTarget = dt;
  }
  
  public DropTarget getDropTarget()
  {
    return dropTarget;
  }
  */
  
  /** @since 1.3 */
  public GraphicsConfiguration getGraphicsConfiguration()
  {
    return getGraphicsConfigurationImpl();
  }

  /** Implementation method that allows classes such as Canvas and
      Window to override the graphics configuration without violating
      the published API. */
  GraphicsConfiguration getGraphicsConfigurationImpl()
  {
    if (peer != null)
      {
	GraphicsConfiguration config = peer.getGraphicsConfiguration();
	if (config != null)
	  return config;
      }

    if (parent != null)
      return parent.getGraphicsConfiguration();

    return null;
  }

  /**
   * Returns the object used for synchronization locks on this component
   * when performing tree and layout functions.
   *
   * @return The synchronization lock for this component.
   */
  public final Object getTreeLock()
  {
    return treeLock;
  }

  // The sync lock object for this component.
  final void setTreeLock(Object tree_lock)
  {
    this.treeLock = tree_lock;
  }

  /**
   * Returns the toolkit in use for this component.
   *
   * @return The toolkit for this component.
   */
  public Toolkit getToolkit()
  {
    if (peer != null)
      {
	Toolkit tk = peer.getToolkit();
	if (tk != null)
	  return tk;
      }
    if (parent != null)
      return parent.getToolkit ();
    return Toolkit.getDefaultToolkit ();
  }

  /**
   * Tests whether or not this component is valid.  A invalid component needs
   * to have its layout redone.
   *
   * @return <code>true</code> if this component is valid, <code>false</code>
   * otherwise.
   */
  public boolean isValid()
  {
    return valid;
  }
  
  /** @since 1.2 */
  public boolean isDisplayable()
  {
    if (parent != null)
      return parent.isDisplayable();
    return false;
  }

  /**
   * Tests whether or not this component is visible.
   *
   * @return <code>true</code> if the component is visible,
   * <code>false</code> otherwise.
   */
  public boolean isVisible()
  {
    return visible;
  }

  /**
   * Tests whether or not this component is actually being shown on
   * the screen.  This will be true if and only if it this component is
   * visible and its parent components are all visible.
   *
   * @return <code>true</code> if the component is showing on the screen,
   * <code>false</code> otherwise.
   */
  public boolean isShowing()
  {
    if (! visible || peer == null)
      return false;

    return parent == null ? true : parent.isShowing ();
  }

  /**
   * Tests whether or not this component is enabled.
   *
   * @return <code>true</code> if the component is enabled,
   * <code>false</code> otherwise.
   */
  public boolean isEnabled()
  {
    return enabled;
  }

  /**
   * Enables or disables this component.
   *
   * @param enabled <code>true</code> to enable this component, 
   * <code>false</code> to disable it.
   *
   * @deprecated Deprecated in favor of <code>setEnabled()</code>.
   */
  public void setEnabled(boolean b)
  {
    this.enabled = b;
    if (peer != null)
      peer.setEnabled(b);
  }

  /**
   * Enables this component.
   *
   * @deprecated Deprecated in favor of <code>setEnabled()</code>.
   */
  public void enable()
  {
    setEnabled(true);
  }

  /**
   * Enables or disables this component.
   *
   * @param enabled <code>true</code> to enable this component, 
   * <code>false</code> to disable it.
   *
   * @deprecated Deprecated in favor of <code>setEnabled()</code>.
   */
  public void enable(boolean b)
  {
    setEnabled(b);
  }

  /**
   * Disables this component.
   *
   * @deprecated Deprecated in favor of <code>setEnabled()</code>.
   */
  public void disable()
  {
    setEnabled(false);
  }

  public boolean isDoubleBuffered()
  {
    return false;
  }

  /** @since 1.2 */
  public void enableInputMethods(boolean enable)
  {
    // FIXME
  }

  /**
   * Makes this component visible or invisible.
   *
   * @param visible <code>true</code> to make this component visible,
   * </code>false</code> to make it invisible.
   * @specnote  Inspection by subclassing shows that Sun's implementation
   * calls show(boolean) which then calls show() or hide(). It is
   * the show() method that is overriden in subclasses like Window.
   * We do the same to preserve compatibility for subclasses.
   */
  public void setVisible(boolean b)
  {
    if (peer != null)
      peer.setVisible (b);
    this.visible = b;
  }

  /**
   * Makes this component visible on the screen.
   *
   * @deprecated Deprecated in favor of <code>setVisible()</code>.
   */
  public void show()
  {
    setVisible (true);
  }

  /**
   * Makes this component visible or invisible.
   *
   * @param visible <code>true</code> to make this component visible,
   * </code>false</code> to make it invisible.
   *
   * @deprecated Deprecated in favor of <code>setVisible()</code>.
   */
  public void show(boolean b)
  {
    setVisible (b);
  }

  /**
   * Hides this component so that it is no longer shown on the screen.
   *
   * @deprecated Deprecated in favor of <code>setVisible()</code>.
   */
  public void hide()
  {
    setVisible (false);
  }

  /**
   * Returns this component's foreground color.
   *
   * @return This component's foreground color.
   */
  public Color getForeground()
  {
    if (foreground != null)
      return foreground;
    if (parent != null)
      return parent.getForeground();
    return null;
  }

  /**
   * Sets this component's foreground color to the specified color.
   *
   * @param foreground_color The new foreground color.
   */
  public void setForeground(Color c)
  {
    if (peer != null)
      peer.setForeground(c);
    this.foreground = c;
  }

  /**
   * Returns this component's background color.
   *
   * @return the background color of the component. null may be
   * returned instead of the actual background color, if this
   * method is called before the component is added to the
   * component hierarchy.
   */
  public Color getBackground()
  {
    if (background != null)
      return background;
    if (parent != null)
      return parent.getBackground();
    return null;
  }

  /**
   * Sets this component's background color to the specified color.
   *
   * @param background_color The new background color
   */
  public void setBackground(Color c)
  {
    if (peer != null)
      peer.setBackground(c);
    this.background = c;
  }

  /**
   * Returns the font in use for this component.
   *
   * @return The font for this component.
   */
  public Font getFont()
  {
    if (font != null)
      return font;
    if (parent != null)
      return parent.getFont();
    return null;
  }

  /**
   * Sets the font for this component to the specified font.
   *
   * @param font The new font for this component.
   */
  public void setFont(Font f)
  {
    if (peer != null)
      peer.setFont(f);
    this.font = f;
  }

  /**
   * Returns the locale for this component.  If this component does not
   * have a locale, the locale of the parent component is returned.  If the
   * component has no parent, the system default locale is returned.
   *
   * @return The locale for this component.
   */
  public Locale getLocale() throws IllegalComponentStateException
  {
    if (locale != null)
      return locale;
    if (parent == null)
      throw new IllegalComponentStateException
        ("Component has no parent: Can not determine Locale");
    return parent.getLocale();
  }

  /**
   * Sets the locale for this component to the specified locale.
   *
   * @param locale The new locale for this component.
   */
  public void setLocale(Locale l)  
  {
    this.locale = l;

    /* new writing/layout direction perhaps, or make more/less
       room for localized text labels */
    invalidate();
  }

  /**
   * Returns the color model of the device this componet is displayed on.
   *
   * @return This object's color model.
   */
  public ColorModel getColorModel()
  {
    GraphicsConfiguration config = getGraphicsConfiguration();

    if (config != null)
      return config.getColorModel();

    return getToolkit().getColorModel();    
  }

  /**
   * Returns the location of this component's top left corner relative to
   * its parent component.
   *
   * @return The location of this component.
   */
  public Point getLocation()
  {
    return new Point(x, y);
  }

  /**
   * Returns the location of this component's top left corner in screen
   * coordinates.
   *
   * @return The location of this component in screen coordinates.
   */
  public Point getLocationOnScreen()
  {
    if (! isShowing ())
      throw new IllegalComponentStateException ("component not showing");

    // We know peer != null here.
    return peer.getLocationOnScreen ();
  }

  /**
   * Returns the location of this component's top left corner relative to
   * its parent component.
   *
   * @return The location of this component.
   *
   * @deprecated This method is deprecated in favor of 
   * <code>getLocation()</code>.
   */
  public Point location()
  {
    return getLocation();
  }

  /**
   * Moves this component to the specified location.  The coordinates are
   * the new upper left corner of this component.
   *
   * @param x The new X coordinate of this component.
   * @param y The new Y coordinate of this component.
   */
  public void setLocation (int x, int y)
  {
    if ((this.x == x) && (this.y == y))
      return;

    invalidate();

    this.x = x;
    this.y = y;
    if (peer != null)
      peer.setBounds(x, y, width, height);
  }

  /**
   * Moves this component to the specified location.  The coordinates are
   * the new upper left corner of this component.
   *
   * @param x The new X coordinate of this component.
   * @param y The new Y coordinate of this component.
   *
   * @deprecated Deprecated in favor for <code>setLocation</code>.
   */
  public void move(int x, int y)
  {
    setLocation(x,y);
  }

  /**
   * Moves this component to the specified location.  The coordinates are
   * the new upper left corner of this component.
   *
   * @param p New coordinates for this component.
   */
  public void setLocation(Point p)
  {
    setLocation(p.x, p.y);
  }

  /**
   * Returns the size of this object.
   *
   * @return The size of this object.
   */
  public Dimension getSize()
  {
    return new Dimension(width, height);
  }

  /**
   * Returns the size of this object.
   *
   * @return The size of this object.
   *
   * @deprecated This method is deprecated in favor of <code>getSize</code>.
   */
  public Dimension size()
  {
    return getSize();
  }

  /**
   * Sets the size of this component to the specified width and height.
   * 
   * @param width The new width of this component.
   * @param height The new height of this component.
   */
  public void setSize(int width, int height)
  {
    if ((this.width == width) && (this.height == height))
      return;

    invalidate();

    this.width = width;
    this.height = height;
    if (peer != null)
      peer.setBounds(x, y, width, height);
  }

  /**
   * Sets the size of this component to the specified value.
   * 
   * @param width The new width of the component.
   * @param height The new height of the component.
   *
   * @deprecated This method is deprecated in favor of <code>setSize</code>.
   */
  public void resize(int width, int height)
  {
    setSize(width, height);
  }

  /**
   * Sets the size of this component to the specified value.
   * 
   * @param dim The new size of this component.
   */
  public void setSize(Dimension d)
  {
    setSize(d.width, d.height);
  }

  /**
   * Sets the size of this component to the specified value.
   * 
   * @param dim The new size of this component.
   *
   * @deprecated This method is deprecated in favor of <code>setSize</code>.
   */
  public void resize(Dimension d)
  {
    setSize(d.width, d.height);
  }

  /**
   * Returns a bounding rectangle for this component.  Note that the
   * returned rectange is relative to this component's parent, not to
   * the screen.
   *
   * @return The bounding rectangle for this component.
   */
  public Rectangle getBounds()
  {
    return new Rectangle (x, y, width, height);
  }

  /**
   * Returns a bounding rectangle for this component.  Note that the
   * returned rectange is relative to this component's parent, not to
   * the screen.
   *
   * @return The bounding rectangle for this component.
   *
   * @deprecated Deprecated in favor of <code>getBounds()</code>.
   */
  public Rectangle bounds()
  {
    return getBounds();
  }

  /**
   * Sets the bounding rectangle for this component to the specified
   * values.  Note that these coordinates are relative to the parent,
   * not to the screen.
   *
   * @param x The X coordinate of the upper left corner of the rectangle.
   * @param y The Y coordinate of the upper left corner of the rectangle.
   * @param width The width of the rectangle.
   * @param height The height of the rectangle.
   */
  public void setBounds(int x, int y, int w, int h)
  {
    if (this.x == x
	&& this.y == y
	&& this.width == w
	&& this.height == h)
      return;

    invalidate();

    this.x = x;
    this.y = y;
    this.width = w;
    this.height = h;

    if (peer != null)
      peer.setBounds(x, y, w, h);
  }

  /**
   * Sets the bounding rectangle for this component to the specified
   * values.  Note that these coordinates are relative to the parent,
   * not to the screen.
   *
   * @param x The X coordinate of the upper left corner of the rectangle.
   * @param y The Y coordinate of the upper left corner of the rectangle.
   * @param width The width of the rectangle.
   * @param height The height of the rectangle.
   *
   * @deprecated This method is deprecated in favor of
   * <code>setBounds(int, int, int, int)</code>.
   */
  public void reshape(int x, int y, int width, int height)
  {
    setBounds(x, y, width, height);
  }

  /**
   * Sets the bounding rectangle for this component to the specified
   * rectangle.  Note that these coordinates are relative to the parent,
   * not to the screen.
   *
   * @param bounding_rectangle The new bounding rectangle.
   */
  public void setBounds(Rectangle r)
  { 
    setBounds(r.x, r.y, r.width, r.height);
  }
  
  /** @since 1.2 */
  public int getX()
  {
    return x;
  }
  
  /** @since 1.2 */
  public int getY()
  {
    return y;
  }
  
  /** @since 1.2 */
  public int getWidth()
  {
    return width;
  }
  
  /** @since 1.2 */
  public int getHeight()
  {
    return height;
  }
  
  public Rectangle getBounds(Rectangle r)
  {
    r.x = this.x;
    r.y = this.y;
    r.width = this.width;
    r.height = this.height;
    return r;
  }
  
  public Dimension getSize(Dimension d)
  {
    d.width = this.width;
    d.height = this.height;
    return d;
  }
  
  public Point getLocation(Point p)
  {
    p.x = x;
    p.y = y;
    return p;
  }
  
  /** @since 1.2 */
  public boolean isOpaque()
  {
    return !isLightweight();
  }
  
  /** 
   * Return whether the component is lightweight.
   *
   * @return true if component has a peer and and the peer is lightweight.
   *
   * @since 1.2
   */  
  public boolean isLightweight()
  {
    return (peer != null) && (peer instanceof LightweightPeer);
  }

  /**
   * Returns the component's preferred size.
   *
   * @return The component's preferred size.
   */
  public Dimension getPreferredSize()
  {
    if (peer == null)
      return new Dimension(width, height);
    else
      return peer.getPreferredSize();
  }

  /**
   * Returns the component's preferred size.
   *
   * @return The component's preferred size.
   *
   * @deprecated Deprecated in favor of <code>getPreferredSize()</code>.
   */
  public Dimension preferredSize()
  {
    return getPreferredSize();
  }

  /**
   * Returns the component's minimum size.
   *
   * @return The component's minimum size.
   */
  public Dimension getMinimumSize()
  {
    if (peer == null)
      return new Dimension(width, height);
    else
      return peer.getMinimumSize();
  }

  /**
   * Returns the component's minimum size.
   *
   * @return The component's minimum size.
   *
   * @deprecated Deprecated in favor of <code>getMinimumSize()</code>
   */
  public Dimension minimumSize()
  {
    return getMinimumSize();
  }

  /**
   * Returns the component's maximum size.
   *
   * @return The component's maximum size.
   */
  public Dimension getMaximumSize()
  {
    return new Dimension(Integer.MAX_VALUE, Integer.MAX_VALUE);
  }

  /**
   * Returns the preferred horizontal alignment of this component.  The
   * value returned will be one of the constants defined in this class.
   *
   * @return The preferred horizontal alignment of this component.
   */
  public float getAlignmentX()
  {
    return CENTER_ALIGNMENT;
  }

  /**
   * Returns the preferred vertical alignment of this component.  The
   * value returned will be one of the constants defined in this class.
   *
   * @return The preferred vertical alignment of this component.
   */
  public float getAlignmentY()
  {
    return CENTER_ALIGNMENT;
  }

  /**
   * Calls the layout manager to re-layout the component.  This is called
   * during validation of a container in most cases.
   */
  public void doLayout()
  {
    // nothing to do unless we're a container
  }

  /**
   * Calls the layout manager to re-layout the component.  This is called
   * during validation of a container in most cases.
   *
   * @deprecated This method is deprecated in favor of <code>doLayout()</code>.
   */
  public void layout()
  {
    doLayout();
  }

  /**
   * Called to ensure that the layout for this component is valid.
   */
  public void validate()
  {
    valid = true;
  }

  /**
   * Invalidates this component and all of its parent components.  This will
   * cause them to have their layout redone.
   */
  public void invalidate()
  {
    valid = false;

    if ((parent != null) && parent.valid)
      parent.invalidate ();
  }

  /**
   * Returns a graphics object for this component.  Returns <code>null</code>
   * if this component is not currently displayed on the screen.
   *
   * @return A graphics object for this component.
   */
  public Graphics getGraphics()
  {
    if (peer != null)
      {
	Graphics gfx = peer.getGraphics();
	if (gfx != null)
	  return gfx;
      
	// create graphics for lightweight:
	Container parent = getParent();
	if (parent != null)
	  {
	    gfx = parent.getGraphics();
	    Rectangle bounds = getBounds();
	    gfx.setClip(bounds);
	    gfx.translate(bounds.x, bounds.y);
	    return gfx;
	  }
      }
    return null;
  }

  /**
   * Returns the font metrics for the specified font in this component.
   *
   * @param font The font to retrieve metrics for.
   *
   * @return The font metrics for the specified font.
   */
  public FontMetrics getFontMetrics(Font font)
  {
    if (peer == null)
      return getToolkit().getFontMetrics(font);
    return peer.getFontMetrics (font);
  }

  /**
   * Sets the cursor for this component to the specified cursor.
   *
   * @param cursor The new cursor for this component.
   */
  public void setCursor(Cursor cursor)
  {
    this.cursor = cursor;
    if (peer != null)
      peer.setCursor (cursor);
  }

  /**
   * Returns the cursor for this component.
   *
   * @return The cursor for this component.
   */
  public Cursor getCursor()
  {
    return this.cursor;
  }

  /**
   * Paints this component on the screen.  The clipping region in the
   * graphics context will indicate the region that requires painting.
   *
   * @param graphics The graphics context for this paint job.
   */
  public void paint(Graphics g)
  {
  }

  /**
   * Updates this component.  This method fills the component
   * with the background color, then sets the foreground color of the
   * specified graphics context to the foreground color of this component
   * and calls the <code>paint()</code> method.
   * // FIXME: What are the coords relative to?
   *
   * @param graphics The graphics context for this update.
   */
  public void update(Graphics g)
  {
    paint(g);
  }

  /**
   * Paints this entire component, including any sub-components.
   *
   * @param graphics The graphics context for this paint job.
   */
  public void paintAll(Graphics g)
  {    
    if (!visible)
      return;
	
    if (peer != null)
      peer.paint(g);
    paint(g);
  }

  /**
   * Repaint this entire component.  The <code>update()</code> method
   * on this component will be called as soon as possible.
   * // FIXME: What are the coords relative to?
   */
  public void repaint()
  {
    repaint(0, 0, 0, getWidth(), getHeight());
  }

  /**
   * Repaint this entire component.  The <code>update()</code> method
   * on this component will be called in approximate the specified number
   * of milliseconds.
   * // FIXME: What are the coords relative to?
   *
   * @param tm The number of milliseconds before this component should
   * be repainted.
   */
  public void repaint(long tm)
  {
    repaint(tm, 0, 0, getWidth(), getHeight());
  }

  /**
   * Repaints the specified rectangular region within this component.
   * This <code>update</code> method on this component will be called as
   * soon as possible.
   * // FIXME: What are the coords relative to?
   *
   * @param x The X coordinate of the upper left of the region to repaint
   * @param y The Y coordinate of the upper left of the region to repaint
   * @param width The width of the region to repaint.
   * @param height The height of the region to repaint.
   */
  public void repaint(int x, int y, int width, int height)
  {
    repaint(0, x, y, width, height);
  }

  /**
   * Repaints the specified rectangular region within this component.
   * This <code>update</code> method on this component will be called in
   * approximately the specified number of milliseconds.
   * // FIXME: What are the coords relative to?
   *
   * @param tm The number of milliseconds before this component should
   * be repainted.
   * @param x The X coordinate of the upper left of the region to repaint
   * @param y The Y coordinate of the upper left of the region to repaint
   * @param width The width of the region to repaint.
   * @param height The height of the region to repaint.
   */
  public void repaint(long tm, int x, int y, int width, int height)
  {    
    // Handle lightweight repainting by forwarding to native parent
    if (isLightweight() && (parent != null))
      {
	if (parent != null)
	  parent.repaint(tm, x+getX(), y+getY(), width, height);
	return;
      }

    if (peer != null)
      peer.repaint(tm, x, y, width, height);
  }

  /**
   * Prints this component.  This method is
   * provided so that printing can be done in a different manner from
   * painting.  However, the implementation in this class simply calls
   * the <code>paint()</code> method.
   *
   * @param graphics The graphics context of the print device.
   */
  public void print(Graphics g)
  {
    paint(g);
  }

  /**
   * Prints this component, including all sub-components.  This method is
   * provided so that printing can be done in a different manner from
   * painting.  However, the implementation in this class simply calls
   * the <code>paintAll()</code> method.
   *
   * @param graphics The graphics context of the print device.
   */
  public void printAll(Graphics g)
  {
    paintAll(g);
  }

  /**
   * Called when an image has changed so that this component is
   * repainted.
   *
   * @param image The image that has been updated.
   * @param flags Flags as specified in <code>ImageObserver</code>.
   * @param x The X coordinate 
   * @param y The Y coordinate
   * @param width The width
   * @param height The height
   *
   * @return <code>true</code> if the image has been fully loaded,
   * <code>false</code> otherwise.
   */
  public boolean imageUpdate (Image img, int infoflags, int x, int y,
			      int w, int h)
  {
    // FIXME
    return false;
  }

  /**
   * Creates an image from the specified producer.
   *
   * @param producer The image procedure to create the image from.
   *
   * @return The resulting image.
   */
  public Image createImage(ImageProducer producer)
  {
    return peer.createImage(producer);
  }

  /**
   * Creates an image with the specified width and height for use in
   * double buffering.
   *
   * @param width The width of the image.
   * @param height The height of the image.
   *
   * @return The requested image.
   */
  public Image createImage(int width, int height)
  {
    return getGraphicsConfiguration().createCompatibleImage(width, height);
  }

  /**
   * Prepares the specified image for rendering on this component.
   *
   * @param image The image to prepare for rendering.
   * @param observer The image observer to notify of the status of the
   * image preparation.
   *
   * @return <code>true</code> if the image is already fully prepared
   * for rendering, <code>false</code> otherwise.
   */
  public boolean prepareImage(Image image, ImageObserver observer)
  {
    return prepareImage(image, image.getWidth(observer), 
			image.getHeight(observer), observer);
  }

  /**
   * Prepares the specified image for rendering on this component at the
   * specified scaled width and height
   *
   * @param image The image to prepare for rendering.
   * @param width The scaled width of the image.
   * @param height The scaled height of the image.
   * @param observer The image observer to notify of the status of the
   * image preparation.
   *
   * @return <code>true</code> if the image is already fully prepared
   * for rendering, <code>false</code> otherwise.
   */
  public boolean prepareImage(Image image, int width, int height,
			      ImageObserver observer)
  {
    return peer.prepareImage(image, width, height, observer);
  }

  /**
   * Returns the status of the loading of the specified image. The value
   * returned will be those flags defined in <code>ImageObserver</code>.
   *
   * @param image The image to check on.
   * @param observer The observer to be notified as the image loading
   * progresses.
   *
   * @return The image observer flags indicating the status of the load.
   */
  public int checkImage(Image image, ImageObserver observer)
  {
    return checkImage(image, image.getWidth(observer), 
		      image.getHeight(observer), observer);
  }

  /**
   * Returns the status of the loading of the specified image. The value
   * returned will be those flags defined in <code>ImageObserver</code>.
   *
   * @param image The image to check on.
   * @param width The scaled image width.
   * @param height The scaled image height.
   * @param observer The observer to be notified as the image loading
   * progresses.
   *
   * @return The image observer flags indicating the status of the load.
   */
  public int checkImage (Image image, int width, int height,
			 ImageObserver observer)
  {
    if (peer != null)
      return peer.checkImage (image, width, height, observer);
    return getToolkit ().checkImage (image, width, height, observer);
  }

  /**
   * Tests whether or not the specified point is contained within this
   * component.  Coordinates are relative to this component.
   *
   * @param x The X coordinate of the point to test.
   * @param y The Y coordinate of the point to test.
   *
   * @return <code>true</code> if the point is within this component,
   * <code>false</code> otherwise.
   */
  public boolean contains (int x, int y)
  {
    return (x >= 0) && (y >= 0) && (x < width) && (y < height);
  }

  /**
   * Tests whether or not the specified point is contained within this
   * component.  Coordinates are relative to this component.
   *
   * @param x The X coordinate of the point to test.
   * @param y The Y coordinate of the point to test.
   *
   * @return <code>true</code> if the point is within this component,
   * <code>false</code> otherwise.
   *
   * @deprecated Deprecated in favor of <code>contains(int, int)</code>.
   */
  public boolean inside(int x, int y)
  {
    return contains(x,y);
  }

  /**
   * Tests whether or not the specified point is contained within this
   * component.  Coordinates are relative to this component.
   *
   * @param point The point to test.
   *
   * @return <code>true</code> if the point is within this component,
   * <code>false</code> otherwise.
   */
  public boolean contains(Point p)
  {
    return contains(p.x, p.y);
  }

  /**
   * Returns the component occupying the position (x,y).  This will either
   * be this component, an immediate child component, or <code>null</code>
   * if neither of the first two occupies the specified location.
   *
   * @param x The X coordinate to search for components at.
   * @param y The Y coordinate to search for components at.
   *
   * @return The component at the specified location, for <code>null</code>
   * if there is none.
   */
  public Component getComponentAt(int x, int y)
  {
    if (contains(x,y))
      return this;
    return null;
  }

  /**
   * Returns the component occupying the position (x,y).  This will either
   * be this component, an immediate child component, or <code>null</code>
   * if neither of the first two occupies the specified location.
   *
   * @param x The X coordinate to search for components at.
   * @param y The Y coordinate to search for components at.
   *
   * @return The component at the specified location, for <code>null</code>
   * if there is none.
   *
   * @deprecated The method is deprecated in favor of 
   * <code>getComponentAt()</code>.
   */
  public Component locate(int x, int y)
  {
    return getComponentAt(x, y);
  }

  /**
   * Returns the component occupying the specified point  This will either
   * be this component, an immediate child component, or <code>null</code>
   * if neither of the first two occupies the specified location.
   *
   * @param point The point to search for components at.
   *
   * @return The component at the specified location, for <code>null</code>
   * if there is none.
   */
  public Component getComponentAt(Point p)
  {
    return getComponentAt(p.x, p.y);
  }

  /**
   * AWT 1.0 event dispatcher.
   *
   * @deprecated Deprecated in favor of <code>dispatchEvent()</code>.
   */
  public void deliverEvent(Event e)
  {
  }

  /** Forward AWT events to processEvent() if:
    *     - Events have been enabled for this type of event via enableEvents(),
    *   OR:
    *	 - There is at least one registered listener for this type of event
    * 
    * @param event The event to dispatch
    *
    * @specnote This method is final, but we need to be able to 
    *           override it in order to handle other event types in our 
    *	        subclasses. The solution is to define a second, non-final
    *           method - dispatchEventImpl() - to actually do the work. 
    *           Investigations with Thread.dumpStack() on the dispatch thread 
    *           in JDK 1.3 show Sun's implementation is doing the same 
    *           thing.
    */
  public final void dispatchEvent(AWTEvent e)
  {
    dispatchEventImpl(e);

    /* Give the peer a chance to handle the event. */
    if (peer != null)
      peer.handleEvent(e);
  }

  void dispatchEventImpl(AWTEvent e)
  {
    // Make use of event id's in order to avoid multiple instanceof tests.
    if (e.id <= ComponentEvent.COMPONENT_LAST 
        && e.id >= ComponentEvent.COMPONENT_FIRST
        && (componentListener != null 
	    || (eventMask & AWTEvent.COMPONENT_EVENT_MASK) != 0))
      processEvent(e);
    else if (e.id <= KeyEvent.KEY_LAST
             && e.id >= KeyEvent.KEY_FIRST
	     && (keyListener != null
		 || (eventMask & AWTEvent.KEY_EVENT_MASK) != 0))
      processEvent(e);
    else if (e.id <= MouseEvent.MOUSE_LAST
             && e.id >= MouseEvent.MOUSE_FIRST
	     && (mouseListener != null
		 || mouseMotionListener != null
		 || (eventMask & AWTEvent.MOUSE_EVENT_MASK) != 0))
      processEvent(e);
    else if (e.id <= FocusEvent.FOCUS_LAST
             && e.id >= FocusEvent.FOCUS_FIRST
	     && (focusListener != null
		 || (eventMask & AWTEvent.FOCUS_EVENT_MASK) != 0))
      processEvent(e);
    else if (e.id <= InputMethodEvent.INPUT_METHOD_LAST
             && e.id >= InputMethodEvent.INPUT_METHOD_FIRST
	     && (inputMethodListener != null
		 || (eventMask & AWTEvent.INPUT_METHOD_EVENT_MASK) != 0))
      processEvent(e);
    else if (e.id <= HierarchyEvent.HIERARCHY_LAST
             && e.id >= HierarchyEvent.HIERARCHY_FIRST
	     && (hierarchyListener != null
		 || hierarchyBoundsListener != null
		 || (eventMask & AWTEvent.HIERARCHY_EVENT_MASK) != 0))
      processEvent(e);
    else if (e.id <= PaintEvent.PAINT_LAST
	     && e.id >= PaintEvent.PAINT_FIRST
	     && (eventMask & AWTEvent.PAINT_EVENT_MASK) != 0)      
      processEvent(e);
  }
  
  /**
   * AWT 1.0 event dispatcher.
   *
   * @deprecated Deprecated in favor of <code>dispatchEvent()</code>.
   */
  public boolean postEvent(Event e)
  {
    return false;
  }

  /**
   * Adds the specified listener to this component.
   *
   * @param listener The new listener to add.
   */
  public synchronized void addComponentListener(ComponentListener l)
  {
    componentListener = AWTEventMulticaster.add(componentListener, l);
    if (componentListener != null)
      enableEvents(AWTEvent.COMPONENT_EVENT_MASK);
  }

  /**
   * Removes the specified listener from the component.
   *
   * @param listener The listener to remove.
   */
  public synchronized void removeComponentListener(ComponentListener l)
  {
    componentListener = AWTEventMulticaster.remove(componentListener, l);
  }

  /**
   * Adds the specified listener to this component.
   *
   * @param listener The new listener to add.
   */
  public synchronized void addFocusListener(FocusListener l)
  {
    focusListener = AWTEventMulticaster.add(focusListener, l);
    if (focusListener != null)
      enableEvents(AWTEvent.FOCUS_EVENT_MASK);    
  }

  /**
   * Removes the specified listener from the component.
   *
   * @param listener The listener to remove.
   */
  public synchronized void removeFocusListener(FocusListener l)
  {
    focusListener = AWTEventMulticaster.remove(focusListener, l);
  }
  
  /** @since 1.3 */
  public synchronized void addHierarchyListener(HierarchyListener l)
  {
    hierarchyListener = AWTEventMulticaster.add(hierarchyListener, l);
    if (hierarchyListener != null)
      enableEvents(AWTEvent.HIERARCHY_EVENT_MASK);    
  }
  
  /** @since 1.3 */
  public synchronized void removeHierarchyListener(HierarchyListener l)
  {
    hierarchyListener = AWTEventMulticaster.remove(hierarchyListener, l);
  }

  /** @since 1.3 */
  public synchronized void addHierarchyBoundsListener(HierarchyBoundsListener l)
  {
    hierarchyBoundsListener = 
      AWTEventMulticaster.add(hierarchyBoundsListener, l);
    if (hierarchyBoundsListener != null)
      enableEvents(AWTEvent.HIERARCHY_EVENT_MASK);    
  }

  /** @since 1.3 */
  public synchronized void 
    removeHierarchyBoundsListener(HierarchyBoundsListener l)
  {
    hierarchyBoundsListener = 
      AWTEventMulticaster.remove(hierarchyBoundsListener, l);
  }

  /**
   * Adds the specified listener to this component.
   *
   * @param listener The new listener to add.
   */
  public synchronized void addKeyListener(KeyListener l)
  {
    keyListener = AWTEventMulticaster.add(keyListener, l);
    if (keyListener != null)
      enableEvents(AWTEvent.KEY_EVENT_MASK);    
  }

  /**
   * Removes the specified listener from the component.
   *
   * @param listener The listener to remove.
   */
  public synchronized void removeKeyListener(KeyListener l)
  {
    keyListener = AWTEventMulticaster.remove(keyListener, l);
  }

  /**
   * Adds the specified listener to this component.
   *
   * @param listener The new listener to add.
   */
  public synchronized void addMouseListener(MouseListener l)
  {
    mouseListener = AWTEventMulticaster.add(mouseListener, l);
    if (mouseListener != null)
      enableEvents(AWTEvent.MOUSE_EVENT_MASK);    
  }

  /**
   * Removes the specified listener from the component.
   *
   * @param listener The listener to remove.
   */
  public synchronized void removeMouseListener(MouseListener l)
  {
    mouseListener = AWTEventMulticaster.remove(mouseListener, l);    
  }

  /**
   * Adds the specified listener to this component.
   *
   * @param listener The new listener to add.
   */
  public synchronized void addMouseMotionListener(MouseMotionListener l)
  {
    mouseMotionListener = AWTEventMulticaster.add(mouseMotionListener, l);
    if (mouseMotionListener != null)
      enableEvents(AWTEvent.MOUSE_EVENT_MASK);    
  }

  /**
   * Removes the specified listener from the component.
   *
   * @param listener The listener to remove.
   */
  public synchronized void removeMouseMotionListener(MouseMotionListener l)
  {
    mouseMotionListener = AWTEventMulticaster.remove(mouseMotionListener, l);
  }

  /** @since 1.2 */
  public synchronized void addInputMethodListener(InputMethodListener l)
  {
    inputMethodListener = AWTEventMulticaster.add(inputMethodListener, l);
    if (inputMethodListener != null)
      enableEvents(AWTEvent.INPUT_METHOD_EVENT_MASK);    
  }

  /** @since 1.2 */
  public synchronized void removeInputMethodListener(InputMethodListener l)
  {
    inputMethodListener = AWTEventMulticaster.remove(inputMethodListener, l);
  }

  /** Returns all registered EventListers of the given listenerType. 
    * listenerType must be a subclass of EventListener, or a 
    * ClassClassException is thrown.
    * @since 1.3 
    */
  public EventListener[] getListeners(Class listenerType)
  {
    if (listenerType == ComponentListener.class)
      return getListenersImpl(listenerType, componentListener);
    else if (listenerType == FocusListener.class)
      return getListenersImpl(listenerType, focusListener);
    else if (listenerType == KeyListener.class)
      return getListenersImpl(listenerType, keyListener);
    else if (listenerType == MouseListener.class)
      return getListenersImpl(listenerType, mouseListener);
    else if (listenerType == MouseMotionListener.class)
      return getListenersImpl(listenerType, mouseMotionListener);
    else if (listenerType == InputMethodListener.class)
      return getListenersImpl(listenerType, inputMethodListener);
    else if (listenerType == HierarchyListener.class)
      return getListenersImpl(listenerType, hierarchyListener);
    else if (listenerType == HierarchyBoundsListener.class)
      return getListenersImpl(listenerType, hierarchyBoundsListener);
    else
      return getListenersImpl(listenerType, null);
  }
  
  static EventListener[] getListenersImpl(Class listenerType, EventListener el)
  {
    if (! EventListener.class.isAssignableFrom(listenerType))
      throw new ClassCastException();
    
    Vector v = new Vector();
    if (el != null)
      getListenerList (el, v);    
    EventListener[] el_a = (EventListener[]) Array.newInstance(listenerType, 
							       v.size());
    v.copyInto(el_a);
    return el_a;
  }

  static void getListenerList(EventListener el, Vector v)
  {
    if (el instanceof AWTEventMulticaster)
      {
        AWTEventMulticaster mc = (AWTEventMulticaster) el;
        getListenerList(mc.a, v);
	getListenerList(mc.b, v);
      }
    else
      v.addElement(el);      
  }

  // The input method framework is currently unimplemented.  
  // /** @since 1.2 */
  //
  //public InputMethodRequests getInputMethodRequests()
  //
  // /** @since 1.2 */
  //
  // public InputContext getInputContext()

  /**
   * Enables the specified events.  The events to enable are specified
   * by OR-ing together the desired masks from <code>AWTEvent</code>.
   * <p>
   * Events are enabled by default when a listener is attached to the
   * component for that event type.  This method can be used by subclasses
   * to ensure the delivery of a specified event regardless of whether
   * or not a listener is attached.
   *
   * @param enable_events The desired events to enable.
   */
  protected final void enableEvents(long eventsToEnable)
  {
    eventMask |= eventsToEnable;
    // TODO: Unlike Sun's implementation, I think we should try and 
    // enable/disable events at the peer (gtk/X) level. This will avoid 
    // clogging the event pipeline with useless mousemove events that 
    // we arn't interested in, etc. This will involve extending the peer 
    // interface, but thats okay because the peer interfaces have been
    // deprecated for a long time, and no longer feature in the 
    // API specification at all.

    if (isLightweight() && (parent != null))
      parent.enableEvents(eventsToEnable);
    else if (peer != null)
      peer.setEventMask (eventMask);
  }

  /**
   * Disables the specified events.  The events to disable are specified
   * by OR-ing together the desired masks from <code>AWTEvent</code>.
   *
   * @param disable_events The desired events to disable.
   */
  protected final void disableEvents(long eventsToDisable)
  {
    eventMask &= ~eventsToDisable;
    // forward new event mask to peer?
  }

  /** coalesceEvents is called by the EventQueue if two events with the same 
    * event id are queued. Returns a new combined event, or null if no 
    * combining is done. 
    */
  protected AWTEvent coalesceEvents(AWTEvent existingEvent, AWTEvent newEvent)
  {
    switch (existingEvent.id)
      {
      case MouseEvent.MOUSE_MOVED:
      case MouseEvent.MOUSE_DRAGGED:
	// Just drop the old (intermediate) event and return the new one.
	return newEvent;
      case PaintEvent.PAINT:
      case PaintEvent.UPDATE:
	return coalescePaintEvents((PaintEvent) existingEvent,
				   (PaintEvent) newEvent);
      }
    return null;
  }
  
  /**
   * Coalesce paint events. Current heuristic is: Merge if the union of
   * areas is less than twice that of the sum of the areas. The X server
   * tend to create a lot of paint events that are adjacent but not
   * overlapping.
   *
   * <pre>
   * +------+
   * |      +-----+  ...will be merged
   * |      |     |
   * |      |     |
   * +------+     |
   *        +-----+
   * 
   * +---------------+--+
   * |               |  |  ...will not be merged
   * +---------------+  |
   *                 |  |
   *                 |  |
   *                 |  |
   *                 |  |
   *                 |  |
   *                 +--+
   * </pre>
   */
  private PaintEvent coalescePaintEvents(PaintEvent queuedEvent,
					 PaintEvent newEvent)
  {
    Rectangle r1 = queuedEvent.getUpdateRect();
    Rectangle r2 = newEvent.getUpdateRect();
    Rectangle union = r1.union(r2);
    
    int r1a = r1.width * r1.height;
    int r2a = r2.width * r2.height;
    int ua  = union.width * union.height;
    
    if (ua > (r1a+r2a)*2)
      return null;
    /* The 2 factor should maybe be reconsidered. Perhaps 3/2
       would be better? */

    newEvent.setUpdateRect(union);
    return newEvent;
  }

  /**
   * Processes the specified event.  In this class, this method simply
   * calls one of the more specific event handlers.
   * 
   * @param event The event to process.
   */
  protected void processEvent(AWTEvent e)
  {

    /* Note: the order of these if statements are
       important. Subclasses must be checked first. Eg. MouseEvent
       must be checked before ComponentEvent, since a MouseEvent
       object is also an instance of a ComponentEvent. */

    if (e instanceof FocusEvent)
      processFocusEvent((FocusEvent) e);
    else if (e instanceof PaintEvent)
      processPaintEvent((PaintEvent) e);
    else if (e instanceof MouseEvent)
      {
        if (e.id == MouseEvent.MOUSE_MOVED 
	    || e.id == MouseEvent.MOUSE_DRAGGED)
	  processMouseMotionEvent((MouseEvent) e);
	else
	  processMouseEvent((MouseEvent) e);
      }
    else if (e instanceof ComponentEvent)
      processComponentEvent((ComponentEvent) e);
    else if (e instanceof KeyEvent)
      processKeyEvent((KeyEvent) e);
    else if (e instanceof InputMethodEvent)
      processInputMethodEvent((InputMethodEvent) e);
    else if (e instanceof HierarchyEvent)
      {
        if (e.id == HierarchyEvent.HIERARCHY_CHANGED)
	  processHierarchyEvent((HierarchyEvent) e);
	else
	  processHierarchyBoundsEvent((HierarchyEvent) e);
      }
  }

  /**
   * Called when a component event is dispatched and component events are
   * enabled.  This method passes the event along to any listeners
   * that are attached.
   *
   * @param event The <code>ComponentEvent</code> to process.
   */
  protected void processComponentEvent(ComponentEvent e)
  {
    if (componentListener == null)
      return;
    switch (e.id)
      {
        case ComponentEvent.COMPONENT_HIDDEN:
	  componentListener.componentHidden(e);
	break;
		
        case ComponentEvent.COMPONENT_MOVED:
	  componentListener.componentMoved(e);
	break;
	
	case ComponentEvent.COMPONENT_RESIZED:
	  componentListener.componentResized(e);
	break;
	
	case ComponentEvent.COMPONENT_SHOWN:
	  componentListener.componentShown(e);
	break;
      }
  }

  /**
   * Called when a focus event is dispatched and component events are
   * enabled.  This method passes the event along to any listeners
   * that are attached.
   *
   * @param event The <code>FocusEvent</code> to process.
   */
  protected void processFocusEvent(FocusEvent e)
  {
    if (focusListener == null)
      return;
    switch (e.id)
      {
        case FocusEvent.FOCUS_GAINED:
	  focusListener.focusGained(e);
	break;
        case FocusEvent.FOCUS_LOST:
	  focusListener.focusLost(e);
	break;
      }    
  }

  /**
   * Called when a key event is dispatched and component events are
   * enabled.  This method passes the event along to any listeners
   * that are attached.
   *
   * @param event The <code>KeyEvent</code> to process.
   */
  protected void processKeyEvent(KeyEvent e)
  {
    if (keyListener == null)
      return;
    switch (e.id)
      {
	case KeyEvent.KEY_PRESSED:
	  keyListener.keyPressed(e);
	break;
	case KeyEvent.KEY_RELEASED:
	  keyListener.keyReleased(e);
	break;
	case KeyEvent.KEY_TYPED:
	  keyListener.keyTyped(e);
	break;
      }
  }

  /**
   * Called when a regular mouse event is dispatched and component events are
   * enabled.  This method passes the event along to any listeners
   * that are attached.
   *
   * @param event The <code>MouseEvent</code> to process.
   */
  protected void processMouseEvent(MouseEvent e)
  {
    if (mouseListener == null)
      return;
    switch (e.id)
      {
	case MouseEvent.MOUSE_CLICKED:
	  mouseListener.mouseClicked(e);
	break;
        case MouseEvent.MOUSE_ENTERED:
	  mouseListener.mouseEntered(e);
	break;
	case MouseEvent.MOUSE_EXITED:
	  mouseListener.mouseExited(e);
	break;
	case MouseEvent.MOUSE_PRESSED:
	  mouseListener.mousePressed(e);
	break;
	case MouseEvent.MOUSE_RELEASED:
	  mouseListener.mouseReleased(e);
	break;
      }
  }

  /**
   * Called when a mouse motion event is dispatched and component events are
   * enabled.  This method passes the event along to any listeners
   * that are attached.
   *
   * @param event The <code>MouseMotionEvent</code> to process.
   */
  protected void processMouseMotionEvent(MouseEvent e)
  {
    if (mouseMotionListener == null)
      return;
    switch (e.id)
      {
	case MouseEvent.MOUSE_DRAGGED:
	  mouseMotionListener.mouseDragged(e);
	break;
        case MouseEvent.MOUSE_MOVED:
	  mouseMotionListener.mouseMoved(e);
	break;
      }	
  }

  /** @since 1.2 */
  protected void processInputMethodEvent(InputMethodEvent e)
  {
    if (inputMethodListener == null)
      return;
    switch (e.id)
      {
	case InputMethodEvent.CARET_POSITION_CHANGED:
          inputMethodListener.caretPositionChanged(e);
	break;
	case InputMethodEvent.INPUT_METHOD_TEXT_CHANGED:
          inputMethodListener.inputMethodTextChanged(e);
	break;
      }	
  }
  
  /** @since 1.3 */
  protected void processHierarchyEvent(HierarchyEvent e)
  {
    if (hierarchyListener == null)
      return;
    if (e.id == HierarchyEvent.HIERARCHY_CHANGED)
      hierarchyListener.hierarchyChanged(e);
  }
  
  /** @since 1.3 */
  protected void processHierarchyBoundsEvent(HierarchyEvent e)
  {
    if (hierarchyBoundsListener == null)
      return;
    switch (e.id)
      {
        case HierarchyEvent.ANCESTOR_MOVED:
	  hierarchyBoundsListener.ancestorMoved(e);
	break;
	case HierarchyEvent.ANCESTOR_RESIZED:
	  hierarchyBoundsListener.ancestorResized(e);
	break;
      }
  }

  private void processPaintEvent(PaintEvent event)
  {
    // Can't do graphics without peer
    if (peer == null)
      return;

    Graphics gfx = getGraphics();
    Shape clip = event.getUpdateRect();
    gfx.setClip(clip);

    switch (event.id)
      {
      case PaintEvent.PAINT:
	paint(gfx);
	break;
      case PaintEvent.UPDATE:
	update(gfx);
	break;
      default:
	throw new IllegalArgumentException("unknown paint event");
      }
  }

  /**
   * AWT 1.0 event processor.
   *
   * @deprecated Deprecated in favor of <code>processEvent</code>.
   */
  public boolean handleEvent(Event evt)
  {
    return false;
  }

  /**
   * AWT 1.0 mouse event.
   *
   * @deprecated Deprecated in favor of <code>processMouseEvent()</code>.
   */
  public boolean mouseDown(Event evt, int x, int y)
  {
    return false;
  }
  
  /**
   * AWT 1.0 mouse event.
   *
   * @deprecated Deprecated in favor of <code>processMouseMotionEvent()</code>.
   */
  public boolean mouseDrag(Event evt, int x, int y)
  {
    return false;
  }

  /**
   * AWT 1.0 mouse event.
   *
   * @deprecated Deprecated in favor of <code>processMouseEvent()</code>.
   */
  public boolean mouseUp(Event evt, int x, int y)
  {
    return false;
  }

  /**
   * AWT 1.0 mouse event.
   *
   * @deprecated Deprecated in favor of <code>processMouseMotionEvent()</code>.
   */
  public boolean mouseMove(Event evt, int x, int y)
  {
    return false;
  }

  /**
   * AWT 1.0 mouse event.
   *
   * @deprecated Deprecated in favor of <code>processMouseEvent()</code>.
   */
  public boolean mouseEnter(Event evt, int x, int y)
  {
    return false;
  }

  /**
   * AWT 1.0 mouse event.
   *
   * @deprecated Deprecated in favor of <code>processMouseEvent()</code>.
   */
  public boolean mouseExit(Event evt, int x, int y)
  {
    return false;
  }

  /**
   * AWT 1.0 key press event.
   *
   * @deprecated Deprecated in favor of <code>processKeyEvent</code>.
   */
  public boolean keyDown(Event evt, int key)
  {
    return false;
  }

  /**
   * AWT 1.0 key press event.
   *
   * @deprecated Deprecated in favor of <code>processKeyEvent</code>.
   */
  public boolean keyUp(Event evt, int key)
  {
    return false;
  }

  /**
   * AWT 1.0 action event processor.
   *
   * @deprecated Deprecated in favor of the <code>ActionListener</code>
   * interface.
   */
  public boolean action(Event evt, Object what)
  {
    return false;
  }

  /**
   * Called to inform this component it has been added to a container.
   * A native peer - if any - is created at this time.  This method is
   * called automatically by the AWT system and should not be called by
   * user level code.
   */
  public void addNotify()
  {
    if (peer == null)
      peer = getToolkit().createComponent(this);

    /* Now that all the children has gotten their peers, we should
       have the event mask needed for this component and its
       lightweight subcomponents. */

    peer.setEventMask(eventMask);

    /* We do not invalidate here, but rather leave that job up to
       the peer. For efficiency, the peer can choose not to
       invalidate if it is happy with the current dimensions,
       etc. */
  }

  /**
   * Called to inform this component is has been removed from its
   * container.  Its native peer - if any - is destroyed at this time.
   * This method is called automatically by the AWT system and should
   * not be called by user level code.
   */
  public void removeNotify()
  {    
    if (peer != null)
      peer.dispose();
    peer = null;
  }
  
  /** @deprecated */
  public boolean gotFocus(Event evt, Object what)
  {
    return false;
  }
  
  /** @deprecated */
  public boolean lostFocus(Event evt, Object what)
  {
    return false;
  }

  /**
   * Tests whether or not this component is in the group that can
   * be traversed using the keyboard traversal mechanism (such as the TAB
   * key).
   *
   * @return <code>true</code> if the component is traversed via the TAB
   * key, <code>false</code> otherwise.
   */
  public boolean isFocusTraversable()
  {
    return enabled && visible && (peer == null || peer.isFocusTraversable ());
  }

  /**
   * Requests that this component be given focus.  The <code>gotFocus()</code>
   * method on this event will be called when and if this request was
   * successful.
   */
  public void requestFocus()
  {
    // If there's no peer then this component can't get the focus.  We
    // treat it as a silent rejection of the request.
    if (peer != null)
      peer.requestFocus ();
  }

  // This method is used to implement transferFocus().
  // CHILD is the child making the request.
  // This is overridden by Container; when called for an ordinary
  // component there is no child and so we always return null.
  Component findNextFocusComponent (Component child)
  {
    return null;
  }

  /**
   * Transfers focus to the next component in the focus traversal order.
   */
  public void transferFocus()
  {
    Component next;
    if (parent == null)
      next = findNextFocusComponent (null);
    else
      next = parent.findNextFocusComponent (this);
    if (next != null && next != this)
      next.requestFocus ();
  }

  /**
   * AWT 1.0 focus event processor.
   *
   * @deprecated Deprecated in favor of <code>transferFocus()</code>.
   */
  public void nextFocus()
  {
    transferFocus();
  }

  /** @since 1.2 */
  public boolean hasFocus()
  {
    return hasFocus;
  }

  /**
   * Adds the specified popup menu to this component.
   *
   * @param menu The popup menu to be added.
   */
  public synchronized void add(PopupMenu popup)
  {
    if (popups == null)
      popups = new Vector();
    popups.addElement(popup);    
  }

  /**
   * Removes the specified popup menu from this component.
   *
   * @param menu The popup menu to remove.
   */
  public synchronized void remove(MenuComponent popup)
  {
    popups.removeElement(popup);
  }

  /**
   * Returns a debugging string representing this component.
   *
   * @return A string representing this component.
   */
  protected String paramString()
  {
    StringBuffer param = new StringBuffer();
    String name = getName();
    if (name != null)
      {
	param.append(name);
	param.append(",");
      }
    param.append(width);
    param.append("x");
    param.append(height);
    param.append("+");
    param.append(x);
    param.append("+");
    param.append(y);
    
    if (!isValid())
      param.append(",invalid");
    if (!isVisible())
      param.append(",invisible");
    if (!isEnabled())
      param.append(",disabled");
    if (!isOpaque())
      param.append(",translucent");
    if (isDoubleBuffered())
      param.append(",doublebuffered");
    
    return param.toString();
  }

  /**
   * Returns a string representation of this component.
   *
   * @return A string representation of this component
   */
  public String toString()
  {
    return this.getClass().getName() + "[" + paramString() + "]";
  }

  /**
   * Prints a listing of this component to the standard output.
   */
  public void list ()
  {
    list (System.out, 0);
  }

  /**
   * Prints a listing of this component to the specified print stream.
   *
   * @param stream The <code>PrintStream</code> to print to.
   */
  public void list (PrintStream out)
  {
    list (out, 0);
  }

  /**
   * Prints a listing of this component to the specified print stream,
   * starting at the specified indentation point.
   *
   * @param stream The <code>PrintStream</code> to print to.
   * @param indent The indentation point.
   */
  public void list (PrintStream out, int indent)
  {
    for (int i = 0; i < indent; ++i)
      out.print (' ');
    out.println (toString ());
  }

  /**
   * Prints a listing of this component to the specified print writer.
   *
   * @param writer The <code>PrintWrinter</code> to print to.
   */
  public void list (PrintWriter out)
  {
    list (out, 0);
  }

  /**
   * Prints a listing of this component to the specified print writer,
   * starting at the specified indentation point.
   *
   * @param writer The <code>PrintWriter</code> to print to.
   * @param indent The indentation point.
   */
  public void list (PrintWriter out, int indent)
  {
    for (int i = 0; i < indent; ++i)
      out.print (' ');
    out.println (toString ());
  }

  public void addPropertyChangeListener(PropertyChangeListener listener)
  {
    if (changeSupport == null)
      changeSupport = new PropertyChangeSupport(this);
    changeSupport.addPropertyChangeListener(listener);
  }

  public void removePropertyChangeListener(PropertyChangeListener listener)
  {
    if (changeSupport != null)
      changeSupport.removePropertyChangeListener(listener);         
  }

  public void addPropertyChangeListener(String propertyName,
                                	PropertyChangeListener listener)
  {
    if (changeSupport == null)
      changeSupport = new PropertyChangeSupport(this);
    changeSupport.addPropertyChangeListener(propertyName, listener);  
  }

  public void removePropertyChangeListener(String propertyName,
                                           PropertyChangeListener listener)
  {
    if (changeSupport != null)
      changeSupport.removePropertyChangeListener(propertyName, listener);
  }

  protected void firePropertyChange(String propertyName, Object oldValue, 
                                    Object newValue)
  {
    if (changeSupport != null)
      changeSupport.firePropertyChange(propertyName, oldValue, newValue);    
  }

  public void setComponentOrientation(ComponentOrientation o)
  {
    orientation = o;
  }

  public ComponentOrientation getComponentOrientation()
  {
    return orientation;
  }

  /*
  public AccessibleContext getAccessibleContext()
  {
    return accessibleContext;
  }
  */

/**
  * AWT 1.0 focus event processor.
  *
  * @deprecated Deprecated in favor of <code>processFocusEvent</code>.
  
public boolean
gotFocus(Event event, Object what)
{
  return(true);
}
*/

/**
  * AWT 1.0 focus event processor.
  *
  * @deprecated Deprecated in favor of <code>processFocusEvent</code>.
  
public boolean
lostFocus(Event event, Object what)
{
  return(true);
}
*/

}
