/* Component.java -- a graphics component
   Copyright (C) 1999, 2000, 2001, 2002, 2003 Free Software Foundation

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

import java.awt.dnd.DropTarget;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.HierarchyBoundsListener;
import java.awt.event.HierarchyEvent;
import java.awt.event.HierarchyListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.InputMethodEvent;
import java.awt.event.InputMethodListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.event.MouseWheelListener;
import java.awt.event.MouseWheelEvent;
import java.awt.event.PaintEvent;
import java.awt.im.InputContext;
import java.awt.im.InputMethodRequests;
import java.awt.image.BufferStrategy;
import java.awt.image.ColorModel;
import java.awt.image.ImageObserver;
import java.awt.image.ImageProducer;
import java.awt.image.VolatileImage;
import java.awt.peer.ComponentPeer;
import java.awt.peer.LightweightPeer;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.io.ObjectInputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.Serializable;
import java.lang.reflect.Array;
import java.util.Collections;
import java.util.EventListener;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Locale;
import java.util.Set;
import java.util.Vector;
import javax.accessibility.Accessible;
import javax.accessibility.AccessibleComponent;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;
import javax.accessibility.AccessibleState;
import javax.accessibility.AccessibleStateSet;

/**
 * The root of all evil. All graphical representations are subclasses of this
 * giant class, which is designed for screen display and user interaction.
 * This class can be extended directly to build a lightweight component (one
 * not associated with a native window); lightweight components must reside
 * inside a heavyweight window.
 *
 * <p>This class is Serializable, which has some big implications. A user can
 * save the state of all graphical components in one VM, and reload them in
 * another. Note that this class will only save Serializable listeners, and
 * ignore the rest, without causing any serialization exceptions. However, by
 * making a listener serializable, and adding it to another element, you link
 * in that entire element to the state of this component. To get around this,
 * use the idiom shown in the example below - make listeners non-serializable
 * in inner classes, rather than using this object itself as the listener, if
 * external objects do not need to save the state of this object.
 *
 * <p><pre>
 * import java.awt.*;
 * import java.awt.event.*;
 * import java.io.Serializable;
 * class MyApp implements Serializable
 * {
 *   BigObjectThatShouldNotBeSerializedWithAButton bigOne;
 *   // Serializing aButton will not suck in an instance of MyApp, with its
 *   // accompanying field bigOne.
 *   Button aButton = new Button();
 *   class MyActionListener implements ActionListener
 *   {
 *     public void actionPerformed(ActionEvent e)
 *     {
 *       System.out.println("Hello There");
 *     }
 *   }
 *   MyApp()
 *   {
 *     aButton.addActionListener(new MyActionListener());
 *   }
 * }
 *
 * <p>Status: Incomplete. The event dispatch mechanism is implemented. All
 * other methods defined in the J2SE 1.3 API javadoc exist, but are mostly
 * incomplete or only stubs; except for methods relating to the Drag and
 * Drop, Input Method, and Accessibility frameworks: These methods are
 * present but commented out.
 *
 * @author original author unknown
 * @author Eric Blake <ebb9@email.byu.edu>
 * @since 1.0
 * @status still missing 1.4 support
 */
public abstract class Component
  implements ImageObserver, MenuContainer, Serializable
{
  // Word to the wise - this file is huge. Search for '\f' (^L) for logical
  // sectioning by fields, public API, private API, and nested classes.


  /**
   * Compatible with JDK 1.0+.
   */
  private static final long serialVersionUID = -7644114512714619750L;

  /**
   * Constant returned by the <code>getAlignmentY</code> method to indicate
   * that the component wishes to be aligned to the top relative to
   * other components.
   *
   * @see #getAlignmentY()
   */
  public static final float TOP_ALIGNMENT = 0;

  /**
   * Constant returned by the <code>getAlignmentY</code> and
   * <code>getAlignmentX</code> methods to indicate
   * that the component wishes to be aligned to the center relative to
   * other components.
   *
   * @see #getAlignmentX()
   * @see #getAlignmentY()
   */
  public static final float CENTER_ALIGNMENT = 0.5f;

  /**
   * Constant returned by the <code>getAlignmentY</code> method to indicate
   * that the component wishes to be aligned to the bottom relative to
   * other components.
   *
   * @see #getAlignmentY()
   */
  public static final float BOTTOM_ALIGNMENT = 1;

  /**
   * Constant returned by the <code>getAlignmentX</code> method to indicate
   * that the component wishes to be aligned to the right relative to
   * other components.
   *
   * @see #getAlignmentX()
   */
  public static final float RIGHT_ALIGNMENT = 1;

  /**
   * Constant returned by the <code>getAlignmentX</code> method to indicate
   * that the component wishes to be aligned to the left relative to
   * other components.
   *
   * @see #getAlignmentX()
   */
  public static final float LEFT_ALIGNMENT = 0;

  /**
   * Make the treelock a String so that it can easily be identified
   * in debug dumps. We clone the String in order to avoid a conflict in
   * the unlikely event that some other package uses exactly the same string
   * as a lock object.
   */
  static final Object treeLock = new String("AWT_TREE_LOCK");

  // Serialized fields from the serialization spec.

  /**
   * The x position of the component in the parent's coordinate system.
   *
   * @see #getLocation()
   * @serial the x position
   */
  int x;

  /**
   * The y position of the component in the parent's coordinate system.
   *
   * @see #getLocation()
   * @serial the y position
   */
  int y;

  /**
   * The component width.
   *
   * @see #getSize()
   * @serial the width
   */
  int width;

  /**
   * The component height.
   *
   * @see #getSize()
   * @serial the height
   */
  int height;

  /**
   * The foreground color for the component. This may be null.
   *
   * @see #getForeground()
   * @see #setForeground(Color)
   * @serial the foreground color
   */
  Color foreground;

  /**
   * The background color for the component. This may be null.
   *
   * @see #getBackground()
   * @see #setBackground(Color)
   * @serial the background color
   */
  Color background;

  /**
   * The default font used in the component. This may be null.
   *
   * @see #getFont()
   * @see #setFont(Font)
   * @serial the font
   */
  Font font;

  /**
   * The font in use by the peer, or null if there is no peer.
   *
   * @serial the peer's font
   */
  Font peerFont;

  /**
   * The cursor displayed when the pointer is over this component. This may
   * be null.
   *
   * @see #getCursor()
   * @see #setCursor(Cursor)
   */
  Cursor cursor;

  /**
   * The locale for the component.
   *
   * @see #getLocale()
   * @see #setLocale(Locale)
   */
  Locale locale;

  /**
   * True if the object should ignore repaint events (usually because it is
   * not showing).
   *
   * @see #getIgnoreRepaint()
   * @see #setIgnoreRepaint(boolean)
   * @serial true to ignore repaints
   * @since 1.4
   */
  boolean ignoreRepaint;

  /**
   * True when the object is visible (although it is only showing if all
   * ancestors are likewise visible). For component, this defaults to true.
   *
   * @see #isVisible()
   * @see #setVisible(boolean)
   * @serial true if visible
   */
  boolean visible = true;

  /**
   * True if the object is enabled, meaning it can interact with the user.
   * For component, this defaults to true.
   *
   * @see #isEnabled()
   * @see #setEnabled(boolean)
   * @serial true if enabled
   */
  boolean enabled = true;

  /**
   * True if the object is valid. This is set to false any time a size
   * adjustment means the component need to be layed out again.
   *
   * @see #isValid()
   * @see #validate()
   * @see #invalidate()
   * @serial true if layout is valid
   */
  boolean valid;

  /**
   * The DropTarget for drag-and-drop operations.
   *
   * @see #getDropTarget()
   * @see #setDropTarget(DropTarget)
   * @serial the drop target, or null
   * @since 1.2
   */
  DropTarget dropTarget;

  /**
   * The list of popup menus for this component.
   *
   * @see #add(PopupMenu)
   * @serial the list of popups
   */
  Vector popups;

  /**
   * The component's name. May be null, in which case a default name is
   * generated on the first use.
   *
   * @see #getName()
   * @see #setName(String)
   * @serial the name
   */
  String name;

  /**
   * True once the user has set the name. Note that the user may set the name
   * to null.
   *
   * @see #name
   * @see #getName()
   * @see #setName(String)
   * @serial true if the name has been explicitly set
   */
  boolean nameExplicitlySet;

  /**
   * Indicates if the object can be focused. Defaults to true for components.
   *
   * @see #isFocusable()
   * @see #setFocusable(boolean)
   * @since 1.4
   */
  boolean focusable = true;

  /**
   * Tracks whether this component uses default focus traversal, or has a
   * different policy.
   *
   * @see #isFocusTraversableOverridden()
   * @since 1.4
   */
  int isFocusTraversableOverridden;

  /**
   * The focus traversal keys, if not inherited from the parent or default
   * keyboard manager. These sets will contain only AWTKeyStrokes that
   * represent press and release events to use as focus control.
   *
   * @see #getFocusTraversalKeys(int)
   * @see #setFocusTraversalKeys(int, Set)
   * @since 1.4
   */
  Set[] focusTraversalKeys;

  /**
   * True if focus traversal keys are enabled. This defaults to true for
   * Component. If this is true, keystrokes in focusTraversalKeys are trapped
   * and processed automatically rather than being passed on to the component.
   *
   * @see #getFocusTraversalKeysEnabled()
   * @see #setFocusTraversalKeysEnabled(boolean)
   * @since 1.4
   */
  boolean focusTraversalKeysEnabled = true;

  /**
   * Cached information on the minimum size. Should have been transient.
   *
   * @serial ignore
   */
  Dimension minSize;

  /**
   * Cached information on the preferred size. Should have been transient.
   *
   * @serial ignore
   */
  Dimension prefSize;

  /**
   * Set to true if an event is to be handled by this component, false if
   * it is to be passed up the hierarcy.
   *
   * @see #dispatchEvent(AWTEvent)
   * @serial true to process event locally
   */
  boolean newEventsOnly;

  /**
   * Set by subclasses to enable event handling of particular events, and
   * left alone when modifying listeners. For component, this defaults to
   * enabling only input methods.
   *
   * @see #enableInputMethods(boolean)
   * @see AWTEvent
   * @serial the mask of events to process
   */
  long eventMask = AWTEvent.INPUT_ENABLED_EVENT_MASK;

  /**
   * Describes all registered PropertyChangeListeners.
   *
   * @see #addPropertyChangeListener(PropertyChangeListener)
   * @see #removePropertyChangeListener(PropertyChangeListener)
   * @see #firePropertyChange(String, Object, Object)
   * @serial the property change listeners
   * @since 1.2
   */
  PropertyChangeSupport changeSupport;

  /**
   * True if the component has been packed (layed out).
   *
   * @serial true if this is packed
   */
  boolean isPacked;

  /**
   * The serialization version for this class. Currently at version 4.
   *
   * XXX How do we handle prior versions?
   *
   * @serial the serialization version
   */
  int componentSerializedDataVersion = 4;

  /**
   * The accessible context associated with this component. This is only set
   * by subclasses.
   *
   * @see #getAccessibleContext()
   * @serial the accessibility context
   * @since 1.2
   */
  AccessibleContext accessibleContext;


  // Guess what - listeners are special cased in serialization. See
  // readObject and writeObject.

  /** Component listener chain. */
  transient ComponentListener componentListener;

  /** Focus listener chain. */
  transient FocusListener focusListener;

  /** Key listener chain. */
  transient KeyListener keyListener;

  /** Mouse listener chain. */
  transient MouseListener mouseListener;

  /** Mouse motion listener chain. */
  transient MouseMotionListener mouseMotionListener;

  /**
   * Mouse wheel listener chain.
   *
   * @since 1.4
   */
  transient MouseWheelListener mouseWheelListener;

  /**
   * Input method listener chain.
   *
   * @since 1.2
   */
  transient InputMethodListener inputMethodListener;

  /**
   * Hierarcy listener chain.
   *
   * @since 1.3
   */
  transient HierarchyListener hierarchyListener;

  /**
   * Hierarcy bounds listener chain.
   *
   * @since 1.3
   */
  transient HierarchyBoundsListener hierarchyBoundsListener;

  // Anything else is non-serializable, and should be declared "transient".

  /** The parent. */
  transient Container parent;

  /** The associated native peer. */
  transient ComponentPeer peer;

  /** The preferred component orientation. */
  transient ComponentOrientation orientation = ComponentOrientation.UNKNOWN;

  /**
   * The associated graphics configuration.
   *
   * @since 1.4
   */
  transient GraphicsConfiguration graphicsConfig;

  /**
   * The buffer strategy for repainting.
   *
   * @since 1.4
   */
  transient BufferStrategy bufferStrategy;

  /**
   * The system properties that affect image updating.
   */
  private static transient boolean incrementalDraw;
  private static transient Long redrawRate;

  static
  {
    incrementalDraw = Boolean.getBoolean ("awt.image.incrementalDraw");
    redrawRate = Long.getLong ("awt.image.redrawrate");
  }

  // Public and protected API.

  /**
   * Default constructor for subclasses. When Component is extended directly,
   * it forms a lightweight component that must be hosted in an opaque native
   * container higher in the tree.
   */
  protected Component()
  {
  }

  /**
   * Returns the name of this component.
   *
   * @return the name of this component
   * @see #setName(String)
   * @since 1.1
   */
  public String getName()
  {
    if (name == null && ! nameExplicitlySet)
      name = generateName();
    return name;
  }

  /**
   * Sets the name of this component to the specified name.
   *
   * @param name the new name of this component
   * @see #getName()
   * @since 1.1
   */
  public void setName(String name)
  {
    nameExplicitlySet = true;
    this.name = name;
  }

  /**
   * Returns the parent of this component.
   *
   * @return the parent of this component
   */
  public Container getParent()
  {
    return parent;
  }

  /**
   * Returns the native windowing system peer for this component. Only the
   * platform specific implementation code should call this method.
   *
   * @return the peer for this component
   * @deprecated user programs should not directly manipulate peers; use
   *             {@link #isDisplayable()} instead
   */
  // Classpath's Gtk peers rely on this.
  public ComponentPeer getPeer()
  {
    return peer;
  }

  /**
   * Set the associated drag-and-drop target, which receives events when this
   * is enabled.
   *
   * @param dt the new drop target
   * @see #isEnabled()
   */
  public void setDropTarget(DropTarget dt)
  {
    this.dropTarget = dt;
  }

  /**
   * Gets the associated drag-and-drop target, if there is one.
   *
   * @return the drop target
   */
  public DropTarget getDropTarget()
  {
    return dropTarget;
  }

  /**
   * Returns the graphics configuration of this component, if there is one.
   * If it has not been set, it is inherited from the parent.
   *
   * @return the graphics configuration, or null
   * @since 1.3
   */
  public GraphicsConfiguration getGraphicsConfiguration()
  {
    return getGraphicsConfigurationImpl();
  }

  /**
   * Returns the object used for synchronization locks on this component
   * when performing tree and layout functions.
   *
   * @return the synchronization lock for this component
   */
  public final Object getTreeLock()
  {
    return treeLock;
  }

  /**
   * Returns the toolkit in use for this component. The toolkit is associated
   * with the frame this component belongs to.
   *
   * @return the toolkit for this component
   */
  public Toolkit getToolkit()
  {
    if (peer != null)
      {
        Toolkit tk = peer.getToolkit();
        if (tk != null)
          return tk;
      }
    // Get toolkit for lightweight component.
    if (parent != null)
      return parent.getToolkit();
    return Toolkit.getDefaultToolkit();
  }

  /**
   * Tests whether or not this component is valid. A invalid component needs
   * to have its layout redone.
   *
   * @return true if this component is valid
   * @see #validate()
   * @see #invalidate()
   */
  public boolean isValid()
  {
    return valid;
  }

  /**
   * Tests if the component is displayable. It must be connected to a native
   * screen resource, and all its ancestors must be displayable. A containment
   * hierarchy is made displayable when a window is packed or made visible.
   *
   * @return true if the component is displayable
   * @see Container#add(Component)
   * @see Container#remove(Component)
   * @see Window#pack()
   * @see Window#show()
   * @see Window#dispose()
   * @since 1.2
   */
  public boolean isDisplayable()
  {
    if (parent != null)
      return parent.isDisplayable();
    return false;
  }

  /**
   * Tests whether or not this component is visible. Except for top-level
   * frames, components are initially visible.
   *
   * @return true if the component is visible
   * @see #setVisible(boolean)
   */
  public boolean isVisible()
  {
    return visible;
  }

  /**
   * Tests whether or not this component is actually being shown on
   * the screen. This will be true if and only if it this component is
   * visible and its parent components are all visible.
   *
   * @return true if the component is showing on the screen
   * @see #setVisible(boolean)
   */
  public boolean isShowing()
  {
    if (! visible || peer == null)
      return false;

    return parent == null ? true : parent.isShowing();
  }

  /**
   * Tests whether or not this component is enabled. Components are enabled
   * by default, and must be enabled to receive user input or generate events.
   *
   * @return true if the component is enabled
   * @see #setEnabled(boolean)
   */
  public boolean isEnabled()
  {
    return enabled;
  }

  /**
   * Enables or disables this component. The component must be enabled to
   * receive events (except that lightweight components always receive mouse
   * events).
   *
   * @param enabled true to enable this component
   * @see #isEnabled()
   * @see #isLightweight()
   * @since 1.1
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
   * @deprecated use {@link #setEnabled(boolean)} instead
   */
  public void enable()
  {
    setEnabled(true);
  }

  /**
   * Enables or disables this component.
   *
   * @param enabled true to enable this component
   * @deprecated use {@link #setEnabled(boolean)} instead
   */
  public void enable(boolean b)
  {
    setEnabled(b);
  }

  /**
   * Disables this component.
   *
   * @deprecated use {@link #setEnabled(boolean)} instead
   */
  public void disable()
  {
    setEnabled(false);
  }

  /**
   * Checks if this image is painted to an offscreen image buffer that is
   * later copied to screen (double buffering reduces flicker). This version
   * returns false, so subclasses must override it if they provide double
   * buffering.
   *
   * @return true if this is double buffered; defaults to false
   */
  public boolean isDoubleBuffered()
  {
    return false;
  }

  /**
   * Enables or disables input method support for this component. By default,
   * components have this enabled. Input methods are given the opportunity
   * to process key events before this component and its listeners.
   *
   * @param enable true to enable input method processing
   * @see #processKeyEvent(KeyEvent)
   * @since 1.2
   */
  public void enableInputMethods(boolean enable)
  {
    // XXX Implement.
    throw new Error("not implemented");
  }

  /**
   * Makes this component visible or invisible. Note that it wtill might
   * not show the component, if a parent is invisible.
   *
   * @param visible true to make this component visible
   * @see #isVisible()
   * @since 1.1
   */
  public void setVisible(boolean b)
  {
    // Inspection by subclassing shows that Sun's implementation calls
    // show(boolean) which then calls show() or hide(). It is the show()
    // method that is overriden in subclasses like Window.
    if (b)
      show();
    else
      hide();
  }

  /**
   * Makes this component visible on the screen.
   *
   * @deprecated use {@link #setVisible(boolean)} instead
   */
  public void show()
  {
    if (peer != null)
      peer.setVisible(true);
    this.visible = true;
  }

  /**
   * Makes this component visible or invisible.
   *
   * @param visible true to make this component visible
   * @deprecated use {@link #setVisible(boolean)} instead
   */
  public void show(boolean b)
  {
    setVisible(b);
  }

  /**
   * Hides this component so that it is no longer shown on the screen.
   *
   * @deprecated use {@link #setVisible(boolean)} instead
   */
  public void hide()
  {
    if (peer != null)
      peer.setVisible(false);
    this.visible = false;
  }

  /**
   * Returns this component's foreground color. If not set, this is inherited
   * from the parent.
   *
   * @return this component's foreground color, or null
   * @see #setForeground(Color)
   */
  public Color getForeground()
  {
    if (foreground != null)
      return foreground;
    return parent == null ? null : parent.getForeground();
  }

  /**
   * Sets this component's foreground color to the specified color. This is a
   * bound property.
   *
   * @param c the new foreground color
   * @see #getForeground()
   */
  public void setForeground(Color c)
  {
    firePropertyChange("foreground", foreground, c);
    if (peer != null)
      peer.setForeground(c);
    foreground = c;
  }

  /**
   * Tests if the foreground was explicitly set, or just inherited from the
   * parent.
   *
   * @return true if the foreground has been set
   * @since 1.4
   */
  public boolean isForegroundSet()
  {
    return foreground != null;
  }

  /**
   * Returns this component's background color. If not set, this is inherited
   * from the parent.
   *
   * @return the background color of the component, or null
   * @see #setBackground(Color)
   */
  public Color getBackground()
  {
    if (background != null)
      return background;
    return parent == null ? null : parent.getBackground();
  }

  /**
   * Sets this component's background color to the specified color. The parts
   * of the component affected by the background color may by system dependent.
   * This is a bound property.
   *
   * @param c the new background color
   * @see #getBackground()
   */
  public void setBackground(Color c)
  {
    firePropertyChange("background", background, c);
    if (peer != null)
      peer.setBackground(c);
    background = c;
  }

  /**
   * Tests if the background was explicitly set, or just inherited from the
   * parent.
   *
   * @return true if the background has been set
   * @since 1.4
   */
  public boolean isBackgroundSet()
  {
    return background != null;
  }

  /**
   * Returns the font in use for this component. If not set, this is inherited
   * from the parent.
   *
   * @return the font for this component
   * @see #setFont(Font)
   */
  public Font getFont()
  {
    if (font != null)
      return font;
    return parent == null ? null : parent.getFont();
  }

  /**
   * Sets the font for this component to the specified font. This is a bound
   * property.
   *
   * @param font the new font for this component
   * @see #getFont()
   */
  public void setFont(Font f)
  {
    firePropertyChange("font", font, f);
    if (peer != null)
      peer.setFont(f);
    font = f;
  }

  /**
   * Tests if the font was explicitly set, or just inherited from the parent.
   *
   * @return true if the font has been set
   * @since 1.4
   */
  public boolean isFontSet()
  {
    return font != null;
  }

  /**
   * Returns the locale for this component. If this component does not
   * have a locale, the locale of the parent component is returned.
   *
   * @return the locale for this component
   * @throws IllegalComponentStateException if it has no locale or parent
   * @see setLocale(Locale)
   * @since 1.1
   */
  public Locale getLocale()
  {
    if (locale != null)
      return locale;
    if (parent == null)
      throw new IllegalComponentStateException
        ("Component has no parent: can't determine Locale");
    return parent.getLocale();
  }

  /**
   * Sets the locale for this component to the specified locale. This is a
   * bound property.
   *
   * @param locale the new locale for this component
   */
  public void setLocale(Locale l)
  {
    firePropertyChange("locale", locale, l);
    locale = l;
    // New writing/layout direction or more/less room for localized labels.
    invalidate();
  }

  /**
   * Returns the color model of the device this componet is displayed on.
   *
   * @return this object's color model
   * @see Toolkit#getColorModel()
   */
  public ColorModel getColorModel()
  {
    GraphicsConfiguration config = getGraphicsConfiguration();
    return config != null ? config.getColorModel()
      : getToolkit().getColorModel();
  }

  /**
   * Returns the location of this component's top left corner relative to
   * its parent component. This may be outdated, so for synchronous behavior,
   * you should use a component listner.
   *
   * @return the location of this component
   * @see #setLocation(int, int)
   * @see #getLocationOnScreen()
   * @since 1.1
   */
  public Point getLocation()
  {
    return new Point(x, y);
  }

  /**
   * Returns the location of this component's top left corner in screen
   * coordinates.
   *
   * @return the location of this component in screen coordinates
   * @throws IllegalComponentStateException if the component is not showing
   */
  public Point getLocationOnScreen()
  {
    if (! isShowing())
      throw new IllegalComponentStateException("component not showing");
    // We know peer != null here.
    return peer.getLocationOnScreen();
  }

  /**
   * Returns the location of this component's top left corner relative to
   * its parent component.
   *
   * @return the location of this component
   * @deprecated use {@link #getLocation()} instead
   */
  public Point location()
  {
    return getLocation();
  }

  /**
   * Moves this component to the specified location, relative to the parent's
   * coordinates. The coordinates are the new upper left corner of this
   * component.
   *
   * @param x the new X coordinate of this component
   * @param y the new Y coordinate of this component
   * @see #getLocation()
   * @see #setBounds(int, int, int, int)
   */
  public void setLocation(int x, int y)
  {
    if (this.x == x && this.y == y)
      return;
    invalidate();
    this.x = x;
    this.y = y;
    if (peer != null)
      peer.setBounds(x, y, width, height);
  }

  /**
   * Moves this component to the specified location, relative to the parent's
   * coordinates. The coordinates are the new upper left corner of this
   * component.
   *
   * @param x the new X coordinate of this component
   * @param y the new Y coordinate of this component
   * @deprecated use {@link #setLocation(int, int)} instead
   */
  public void move(int x, int y)
  {
    setLocation(x, y);
  }

  /**
   * Moves this component to the specified location, relative to the parent's
   * coordinates. The coordinates are the new upper left corner of this
   * component.
   *
   * @param p new coordinates for this component
   * @throws NullPointerException if p is null
   * @see #getLocation()
   * @see #setBounds(int, int, int, int)
   * @since 1.1
   */
  public void setLocation(Point p)
  {
    setLocation(p.x, p.y);
  }

  /**
   * Returns the size of this object.
   *
   * @return the size of this object
   * @see #setSize(int, int)
   * @since 1.1
   */
  public Dimension getSize()
  {
    return new Dimension(width, height);
  }

  /**
   * Returns the size of this object.
   *
   * @return the size of this object
   * @deprecated use {@link #getSize()} instead
   */
  public Dimension size()
  {
    return getSize();
  }

  /**
   * Sets the size of this component to the specified width and height.
   *
   * @param width the new width of this component
   * @param height the new height of this component
   * @see #getSize()
   * @see #setBounds(int, int, int, int)
   */
  public void setSize(int width, int height)
  {
    if (this.width == width && this.height == height)
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
   * @param width the new width of the component
   * @param height the new height of the component
   * @deprecated use {@link #setSize(int, int)} instead
   */
  public void resize(int width, int height)
  {
    setSize(width, height);
  }

  /**
   * Sets the size of this component to the specified value.
   *
   * @param d the new size of this component
   * @throws NullPointerException if d is null
   * @see #setSize(int, int)
   * @see #setBounds(int, int, int, int)
   * @since 1.1
   */
  public void setSize(Dimension d)
  {
    setSize(d.width, d.height);
  }

  /**
   * Sets the size of this component to the specified value.
   *
   * @param d the new size of this component
   * @throws NullPointerException if d is null
   * @deprecated use {@link #setSize(Dimension)} instead
   */
  public void resize(Dimension d)
  {
    setSize(d.width, d.height);
  }

  /**
   * Returns a bounding rectangle for this component. Note that the
   * returned rectange is relative to this component's parent, not to
   * the screen.
   *
   * @return the bounding rectangle for this component
   * @see #setBounds(int, int, int, int)
   * @see #getLocation()
   * @see #getSize()
   */
  public Rectangle getBounds()
  {
    return new Rectangle(x, y, width, height);
  }

  /**
   * Returns a bounding rectangle for this component. Note that the
   * returned rectange is relative to this component's parent, not to
   * the screen.
   *
   * @return the bounding rectangle for this component
   * @deprecated use {@link #getBounds()} instead
   */
  public Rectangle bounds()
  {
    return getBounds();
  }

  /**
   * Sets the bounding rectangle for this component to the specified values.
   * Note that these coordinates are relative to the parent, not to the screen.
   *
   * @param x the X coordinate of the upper left corner of the rectangle
   * @param y the Y coordinate of the upper left corner of the rectangle
   * @param w the width of the rectangle
   * @param h the height of the rectangle
   * @see #getBounds()
   * @see #setLocation(int, int)
   * @see #setLocation(Point)
   * @see #setSize(int, int)
   * @see #setSize(Dimension)
   * @since 1.1
   */
  public void setBounds(int x, int y, int w, int h)
  {
    if (this.x == x && this.y == y && width == w && height == h)
      return;
    invalidate();
    this.x = x;
    this.y = y;
    width = w;
    height = h;
    if (peer != null)
      peer.setBounds(x, y, w, h);
  }

  /**
   * Sets the bounding rectangle for this component to the specified values.
   * Note that these coordinates are relative to the parent, not to the screen.
   *
   * @param x the X coordinate of the upper left corner of the rectangle
   * @param y the Y coordinate of the upper left corner of the rectangle
   * @param w the width of the rectangle
   * @param h the height of the rectangle
   * @deprecated use {@link #setBounds(int, int, int, int)} instead
   */
  public void reshape(int x, int y, int width, int height)
  {
    setBounds(x, y, width, height);
  }

  /**
   * Sets the bounding rectangle for this component to the specified
   * rectangle. Note that these coordinates are relative to the parent, not
   * to the screen.
   *
   * @param r the new bounding rectangle
   * @throws NullPointerException if r is null
   * @see #getBounds()
   * @see #setLocation(Point)
   * @see #setSize(Dimension)
   * @since 1.1
   */
  public void setBounds(Rectangle r)
  {
    setBounds(r.x, r.y, r.width, r.height);
  }

  /**
   * Gets the x coordinate of the upper left corner. This is more efficient
   * than getBounds().x or getLocation().x.
   *
   * @return the current x coordinate
   * @since 1.2
   */
  public int getX()
  {
    return x;
  }

  /**
   * Gets the y coordinate of the upper left corner. This is more efficient
   * than getBounds().y or getLocation().y.
   *
   * @return the current y coordinate
   * @since 1.2
   */
  public int getY()
  {
    return y;
  }

  /**
   * Gets the width of the component. This is more efficient than
   * getBounds().width or getSize().width.
   *
   * @return the current width
   * @since 1.2
   */
  public int getWidth()
  {
    return width;
  }

  /**
   * Gets the height of the component. This is more efficient than
   * getBounds().height or getSize().height.
   *
   * @return the current width
   * @since 1.2
   */
  public int getHeight()
  {
    return height;
  }

  /**
   * Returns the bounds of this component. This allows reuse of an existing
   * rectangle, if r is non-null.
   *
   * @param r the rectangle to use, or null
   * @return the bounds
   */
  public Rectangle getBounds(Rectangle r)
  {
    if (r == null)
      r = new Rectangle();
    r.x = x;
    r.y = y;
    r.width = width;
    r.height = height;
    return r;
  }

  /**
   * Returns the size of this component. This allows reuse of an existing
   * dimension, if d is non-null.
   *
   * @param d the dimension to use, or null
   * @return the size
   */
  public Dimension getSize(Dimension d)
  {
    if (d == null)
      d = new Dimension();
    d.width = width;
    d.height = height;
    return d;
  }

  /**
   * Returns the location of this component. This allows reuse of an existing
   * point, if p is non-null.
   *
   * @param p the point to use, or null
   * @return the location
   */
  public Point getLocation(Point p)
  {
    if (p == null)
      p = new Point();
    p.x = x;
    p.y = y;
    return p;
  }

  /**
   * Tests if this component is opaque. All "heavyweight" (natively-drawn)
   * components are opaque. A component is opaque if it draws all pixels in
   * the bounds; a lightweight component is partially transparent if it lets
   * pixels underneath show through. Subclasses that guarantee that all pixels
   * will be drawn should override this.
   *
   * @return true if this is opaque
   * @see #isLightweight()
   * @since 1.2
   */
  public boolean isOpaque()
  {
    return ! isLightweight();
  }

  /**
   * Return whether the component is lightweight. That means the component has
   * no native peer, but is displayable. This applies to subclasses of
   * Component not in this package, such as javax.swing.
   *
   * @return true if the component has a lightweight peer
   * @see #isDisplayable()
   * @since 1.2
   */
  public boolean isLightweight()
  {
    return peer instanceof LightweightPeer;
  }

  /**
   * Returns the component's preferred size.
   *
   * @return the component's preferred size
   * @see #getMinimumSize()
   * @see LayoutManager
   */
  public Dimension getPreferredSize()
  {
    return preferredSize();
  }

  /**
   * Returns the component's preferred size.
   *
   * @return the component's preferred size
   * @deprecated use {@link #getPreferredSize()} instead
   */
  public Dimension preferredSize()
  {
    if (prefSize == null)
      if (peer == null)
	return new Dimension(width, height);
      else 
        prefSize = peer.getPreferredSize();
    return prefSize;
  }

  /**
   * Returns the component's minimum size.
   *
   * @return the component's minimum size
   * @see #getPreferredSize()
   * @see LayoutManager
   */
  public Dimension getMinimumSize()
  {
    return minimumSize();
  }

  /**
   * Returns the component's minimum size.
   *
   * @return the component's minimum size
   * @deprecated use {@link #getMinimumSize()} instead
   */
  public Dimension minimumSize()
  {
    if (minSize == null)
      minSize = (peer != null ? peer.getMinimumSize()
                 : new Dimension(width, height));
    return minSize;
  }

  /**
   * Returns the component's maximum size.
   *
   * @return the component's maximum size
   * @see #getMinimumSize()
   * @see #getPreferredSize()
   * @see LayoutManager
   */
  public Dimension getMaximumSize()
  {
    return new Dimension(Integer.MAX_VALUE, Integer.MAX_VALUE);
  }

  /**
   * Returns the preferred horizontal alignment of this component. The value
   * returned will be between {@link #LEFT_ALIGNMENT} and
   * {@link #RIGHT_ALIGNMENT}, inclusive.
   *
   * @return the preferred horizontal alignment of this component
   */
  public float getAlignmentX()
  {
    return CENTER_ALIGNMENT;
  }

  /**
   * Returns the preferred vertical alignment of this component. The value
   * returned will be between {@link #TOP_ALIGNMENT} and
   * {@link #BOTTOM_ALIGNMENT}, inclusive.
   *
   * @return the preferred vertical alignment of this component
   */
  public float getAlignmentY()
  {
    return CENTER_ALIGNMENT;
  }

  /**
   * Calls the layout manager to re-layout the component. This is called
   * during validation of a container in most cases.
   *
   * @see #validate()
   * @see LayoutManager
   */
  public void doLayout()
  {
    // nothing to do unless we're a container
  }

  /**
   * Calls the layout manager to re-layout the component. This is called
   * during validation of a container in most cases.
   *
   * @deprecated use {@link #doLayout()} instead
   */
  public void layout()
  {
    doLayout();
  }

  /**
   * Called to ensure that the layout for this component is valid. This is
   * usually called on containers.
   *
   * @see #invalidate()
   * @see #doLayout()
   * @see LayoutManager
   * @see Container#validate()
   */
  public void validate()
  {
    valid = true;
  }

  /**
   * Invalidates this component and all of its parent components. This will
   * cause them to have their layout redone. This is called frequently, so
   * make it fast.
   */
  public void invalidate()
  {
    valid = false;
    prefSize = null;
    minSize = null;
    if (parent != null && parent.valid)
      parent.invalidate();
  }

  /**
   * Returns a graphics object for this component. Returns <code>null</code>
   * if this component is not currently displayed on the screen.
   *
   * @return a graphics object for this component
   * @see #paint(Graphics)
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
   * @param font the font to retrieve metrics for
   * @return the font metrics for the specified font
   * @throws NullPointerException if font is null
   * @see #getFont()
   * @see Toolkit#getFontMetrics(Font)
   */
  public FontMetrics getFontMetrics(Font font)
  {
    return peer == null ? getToolkit().getFontMetrics(font)
      : peer.getFontMetrics(font);
  }

  /**
   * Sets the cursor for this component to the specified cursor. The cursor
   * is displayed when the point is contained by the component, and the
   * component is visible, displayable, and enabled. This is inherited by
   * subcomponents unless they set their own cursor.
   *
   * @param cursor the new cursor for this component
   * @see #isEnabled()
   * @see #isShowing()
   * @see #getCursor()
   * @see #contains(int, int)
   * @see Toolkit#createCustomCursor(Image, Point, String)
   */
  public void setCursor(Cursor cursor)
  {
    this.cursor = cursor;
    if (peer != null)
      peer.setCursor(cursor);
  }

  /**
   * Returns the cursor for this component. If not set, this is inherited
   * from the parent, or from Cursor.getDefaultCursor().
   *
   * @return the cursor for this component
   */
  public Cursor getCursor()
  {
    if (cursor != null)
      return cursor;
    return parent != null ? parent.getCursor() : Cursor.getDefaultCursor();
  }

  /**
   * Tests if the cursor was explicitly set, or just inherited from the parent.
   *
   * @return true if the cursor has been set
   * @since 1.4
   */
  public boolean isCursorSet()
  {
    return cursor != null;
  }

  /**
   * Paints this component on the screen. The clipping region in the graphics
   * context will indicate the region that requires painting. This is called
   * whenever the component first shows, or needs to be repaired because
   * something was temporarily drawn on top. It is not necessary for
   * subclasses to call <code>super.paint(g)</code>. Components with no area
   * are not painted.
   *
   * @param g the graphics context for this paint job
   * @see #update(Graphics)
   */
  public void paint(Graphics g)
  {
  }

  /**
   * Updates this component. This is called in response to
   * <code>repaint</code>. This method fills the component with the
   * background color, then sets the foreground color of the specified
   * graphics context to the foreground color of this component and calls
   * the <code>paint()</code> method. The coordinates of the graphics are
   * relative to this component. Subclasses should call either
   * <code>super.update(g)</code> or <code>paint(g)</code>.
   *
   * @param graphics the graphics context for this update
   * @see #paint(Graphics)
   * @see #repaint()
   */
  public void update(Graphics g)
  {
    paint(g);
  }

  /**
   * Paints this entire component, including any sub-components.
   *
   * @param graphics the graphics context for this paint job
   * @see #paint(Graphics)
   */
  public void paintAll(Graphics g)
  {
    if (! visible)
      return;
    if (peer != null)
      peer.paint(g);
    paint(g);
  }

  /**
   * Repaint this entire component. The <code>update()</code> method
   * on this component will be called as soon as possible.
   *
   * @see #update(Graphics)
   * @see #repaint(long, int, int, int, int)
   */
  public void repaint()
  {
    repaint(0, 0, 0, width, height);
  }

  /**
   * Repaint this entire component. The <code>update()</code> method on this
   * component will be called in approximate the specified number of
   * milliseconds.
   *
   * @param tm milliseconds before this component should be repainted
   * @see #paint(Graphics)
   * @see #repaint(long, int, int, int, int)
   */
  public void repaint(long tm)
  {
    repaint(tm, 0, 0, width, height);
  }

  /**
   * Repaints the specified rectangular region within this component. The
   * <code>update</code> method on this component will be called as soon as
   * possible. The coordinates are relative to this component.
   *
   * @param x the X coordinate of the upper left of the region to repaint
   * @param y the Y coordinate of the upper left of the region to repaint
   * @param w the width of the region to repaint
   * @param h the height of the region to repaint
   * @see #update(Graphics)
   * @see #repaint(long, int, int, int, int)
   */
  public void repaint(int x, int y, int w, int h)
  {
    repaint(0, x, y, w, h);
  }

  /**
   * Repaints the specified rectangular region within this component. The
   * <code>update</code> method on this component will be called in
   * approximately the specified number of milliseconds. The coordinates
   * are relative to this component.
   *
   * @param tm milliseconds before this component should be repainted
   * @param x the X coordinate of the upper left of the region to repaint
   * @param y the Y coordinate of the upper left of the region to repaint
   * @param w the width of the region to repaint
   * @param h the height of the region to repaint
   * @see #update(Graphics)
   */
  public void repaint(long tm, int x, int y, int width, int height)
  {
    // Handle lightweight repainting by forwarding to native parent
    if (isLightweight() && parent != null)
      {
        if (parent != null)
          parent.repaint(tm, x + getX(), y + getY(), width, height);
      }
    else if (peer != null)
      peer.repaint(tm, x, y, width, height);
  }

  /**
   * Prints this component. This method is provided so that printing can be
   * done in a different manner from painting. However, the implementation
   * in this class simply calls the <code>paint()</code> method.
   *
   * @param graphics the graphics context of the print device
   * @see #paint(Graphics)
   */
  public void print(Graphics g)
  {
    paint(g);
  }

  /**
   * Prints this component, including all sub-components. This method is
   * provided so that printing can be done in a different manner from
   * painting. However, the implementation in this class simply calls the
   * <code>paintAll()</code> method.
   *
   * @param graphics the graphics context of the print device
   * @see #paintAll(Graphics)
   */
  public void printAll(Graphics g)
  {
    paintAll(g);
  }

  /**
   * Called when an image has changed so that this component is repainted.
   * This incrementally draws an image as more bits are available, when
   * possible. Incremental drawing is enabled if the system property
   * <code>awt.image.incrementalDraw</code> is not present or is true, in which
   * case the redraw rate is set to 100ms or the value of the system property
   * <code>awt.image.redrawrate</code>.
   *
   * <p>The coordinate system used depends on the particular flags.
   *
   * @param image the image that has been updated
   * @param flags tlags as specified in <code>ImageObserver</code>
   * @param x the X coordinate
   * @param y the Y coordinate
   * @param w the width
   * @param h the height
   * @return false if the image is completely loaded, loading has been
   * aborted, or an error has occurred.  true if more updates are
   * required.
   * @see ImageObserver
   * @see Graphics#drawImage(Image, int, int, Color, ImageObserver)
   * @see Graphics#drawImage(Image, int, int, ImageObserver)
   * @see Graphics#drawImage(Image, int, int, int, int, Color, ImageObserver)
   * @see Graphics#drawImage(Image, int, int, int, int, ImageObserver)
   * @see ImageObserver#update(Image, int, int, int, int, int)
   */
  public boolean imageUpdate(Image img, int flags, int x, int y, int w, int h)
  {
    if ((flags & (FRAMEBITS | ALLBITS)) != 0)
      repaint ();
    else if ((flags & SOMEBITS) != 0)
      {
	if (incrementalDraw)
	  {
	    if (redrawRate != null)
	      {
		long tm = redrawRate.longValue();
		if (tm < 0)
		  tm = 0;
		repaint (tm);
	      }
	    else
	      repaint (100);
	  }
      }
    return (flags & (ALLBITS | ABORT | ERROR)) == 0;
  }

  /**
   * Creates an image from the specified producer.
   *
   * @param producer the image procedure to create the image from
   * @return the resulting image
   */
  public Image createImage(ImageProducer producer)
  {
    // Sun allows producer to be null.
    if (peer != null)
      return peer.createImage(producer);
    else
      return getToolkit().createImage(producer);
  }

  /**
   * Creates an image with the specified width and height for use in
   * double buffering. Headless environments do not support images.
   *
   * @param width the width of the image
   * @param height the height of the image
   * @return the requested image, or null if it is not supported
   */
  public Image createImage (int width, int height)
  {
    Image returnValue = null;
    if (!GraphicsEnvironment.isHeadless ())
      {
	if (isLightweight () && parent != null)
	  returnValue = parent.createImage (width, height);
	else if (peer != null)
	  returnValue = peer.createImage (width, height);
      }
    return returnValue;
  }

  /**
   * Creates an image with the specified width and height for use in
   * double buffering. Headless environments do not support images.
   *
   * @param width the width of the image
   * @param height the height of the image
   * @return the requested image, or null if it is not supported
   * @since 1.4
   */
  public VolatileImage createVolatileImage(int width, int height)
  {
    if (GraphicsEnvironment.isHeadless())
      return null;
    GraphicsConfiguration config = getGraphicsConfiguration();
    return config == null ? null
      : config.createCompatibleVolatileImage(width, height);
  }

  /**
   * Creates an image with the specified width and height for use in
   * double buffering. Headless environments do not support images. The image
   * will support the specified capabilities.
   *
   * @param width the width of the image
   * @param height the height of the image
   * @param caps the requested capabilities
   * @return the requested image, or null if it is not supported
   * @throws AWTException if a buffer with the capabilities cannot be created
   * @since 1.4
   */
  public VolatileImage createVolatileImage(int width, int height,
                                           ImageCapabilities caps)
    throws AWTException
  {
    if (GraphicsEnvironment.isHeadless())
      return null;
    GraphicsConfiguration config = getGraphicsConfiguration();
    return config == null ? null
      : config.createCompatibleVolatileImage(width, height, caps);
  }

  /**
   * Prepares the specified image for rendering on this component.
   *
   * @param image the image to prepare for rendering
   * @param observer the observer to notify of image preparation status
   * @return true if the image is already fully prepared
   * @throws NullPointerException if image is null
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
   * @param image the image to prepare for rendering
   * @param width the scaled width of the image
   * @param height the scaled height of the image
   * @param observer the observer to notify of image preparation status
   * @return true if the image is already fully prepared
   */
  public boolean prepareImage(Image image, int width, int height,
                              ImageObserver observer)
  {
    if (peer != null)
	return peer.prepareImage(image, width, height, observer);
    else
	return getToolkit().prepareImage(image, width, height, observer);
  }

  /**
   * Returns the status of the loading of the specified image. The value
   * returned will be those flags defined in <code>ImageObserver</code>.
   *
   * @param image the image to check on
   * @param observer the observer to notify of image loading progress
   * @return the image observer flags indicating the status of the load
   * @see #prepareImage(Image, int, int, ImageObserver)
   * @see #Toolkit#checkImage(Image, int, int, ImageObserver)
   * @throws NullPointerException if image is null
   */
  public int checkImage(Image image, ImageObserver observer)
  {
    return checkImage(image, -1, -1, observer);
  }

  /**
   * Returns the status of the loading of the specified image. The value
   * returned will be those flags defined in <code>ImageObserver</code>.
   *
   * @param image the image to check on
   * @param width the scaled image width
   * @param height the scaled image height
   * @param observer the observer to notify of image loading progress
   * @return the image observer flags indicating the status of the load
   * @see #prepareImage(Image, int, int, ImageObserver)
   * @see #Toolkit#checkImage(Image, int, int, ImageObserver)
   */
  public int checkImage(Image image, int width, int height,
                        ImageObserver observer)
  {
    if (peer != null)
      return peer.checkImage(image, width, height, observer);
    return getToolkit().checkImage(image, width, height, observer);
  }

  /**
   * Sets whether paint messages delivered by the operating system should be
   * ignored. This does not affect messages from AWT, except for those
   * triggered by OS messages. Setting this to true can allow faster
   * performance in full-screen mode or page-flipping.
   *
   * @param ignoreRepaint the new setting for ignoring repaint events
   * @see #getIgnoreRepaint()
   * @see BufferStrategy
   * @see GraphicsDevice.setFullScreenWindow(Window)
   * @since 1.4
   */
  public void setIgnoreRepaint(boolean ignoreRepaint)
  {
    this.ignoreRepaint = ignoreRepaint;
  }

  /**
   * Test whether paint events from the operating system are ignored.
   *
   * @return the status of ignoring paint events
   * @see #setIgnoreRepaint(boolean)
   * @since 1.4
   */
  public boolean getIgnoreRepaint()
  {
    return ignoreRepaint;
  }

  /**
   * Tests whether or not the specified point is contained within this
   * component. Coordinates are relative to this component.
   *
   * @param x the X coordinate of the point to test
   * @param y the Y coordinate of the point to test
   * @return true if the point is within this component
   * @see #getComponentAt(int, int)
   */
  public boolean contains(int x, int y)
  {
    return x >= 0 && y >= 0 && x < width && y < height;
  }

  /**
   * Tests whether or not the specified point is contained within this
   * component. Coordinates are relative to this component.
   *
   * @param x the X coordinate of the point to test
   * @param y the Y coordinate of the point to test
   * @return true if the point is within this component
   * @deprecated use {@link #contains(int, int)} instead
   */
  public boolean inside(int x, int y)
  {
    return contains(x, y);
  }

  /**
   * Tests whether or not the specified point is contained within this
   * component. Coordinates are relative to this component.
   *
   * @param p the point to test
   * @return true if the point is within this component
   * @throws NullPointerException if p is null
   * @see #getComponentAt(Point)
   * @since 1.1
   */
  public boolean contains(Point p)
  {
    return contains(p.x, p.y);
  }

  /**
   * Returns the component occupying the position (x,y). This will either
   * be this component, an immediate child component, or <code>null</code>
   * if neither of the first two occupies the specified location.
   *
   * @param x the X coordinate to search for components at
   * @param y the Y coordinate to search for components at
   * @return the component at the specified location, or null
   * @see #contains(int, int)
   */
  public Component getComponentAt(int x, int y)
  {
    return contains(x, y) ? this : null;
  }

  /**
   * Returns the component occupying the position (x,y). This will either
   * be this component, an immediate child component, or <code>null</code>
   * if neither of the first two occupies the specified location.
   *
   * @param x the X coordinate to search for components at
   * @param y the Y coordinate to search for components at
   * @return the component at the specified location, or null
   * @deprecated use {@link #getComponentAt(int, int)} instead
   */
  public Component locate(int x, int y)
  {
    return getComponentAt(x, y);
  }

  /**
   * Returns the component occupying the position (x,y). This will either
   * be this component, an immediate child component, or <code>null</code>
   * if neither of the first two occupies the specified location.
   *
   * @param p the point to search for components at
   * @return the component at the specified location, or null
   * @throws NullPointerException if p is null
   * @see #contains(Point)
   * @since 1.1
   */
  public Component getComponentAt(Point p)
  {
    return getComponentAt(p.x, p.y);
  }

  /**
   * AWT 1.0 event dispatcher.
   *
   * @param e the event to dispatch
   * @deprecated use {@link #dispatchEvent(AWTEvent)} instead
   */
  public void deliverEvent(Event e)
  {
    // XXX Add backward compatibility handling.
  }

  /**
   * Forwards AWT events to processEvent() if:<ul>
   * <li>Events have been enabled for this type of event via
   * <code>enableEvents()</code></li>,
   * <li>There is at least one registered listener for this type of event</li>
   * </ul>
   *
   * @param e the event to dispatch
   */
  public final void dispatchEvent(AWTEvent e)
  {
    // Some subclasses in the AWT package need to override this behavior,
    // hence the use of dispatchEventImpl().
    dispatchEventImpl(e);
    if (peer != null && ! e.consumed)
      peer.handleEvent(e);
  }

  /**
   * AWT 1.0 event dispatcher.
   *
   * @param e the event to dispatch
   * @return false: since the method was deprecated, the return has no meaning
   * @deprecated use {@link #dispatchEvent(AWTEvent)} instead
   */
  public boolean postEvent(Event e)
  {
    // XXX Add backward compatibility handling.
    return false;
  }

  /**
   * Adds the specified listener to this component. This is harmless if the
   * listener is null, but if the listener has already been registered, it
   * will now be registered twice.
   *
   * @param listener the new listener to add
   * @see ComponentEvent
   * @see #removeComponentListener(ComponentListener)
   * @see #getComponentListeners()
   * @since 1.1
   */
  public synchronized void addComponentListener(ComponentListener l)
  {
    componentListener = AWTEventMulticaster.add(componentListener, l);
    if (componentListener != null)
      enableEvents(AWTEvent.COMPONENT_EVENT_MASK);
  }

  /**
   * Removes the specified listener from the component. This is harmless if
   * the listener was not previously registered.
   *
   * @param listener the listener to remove
   * @see ComponentEvent
   * @see #addComponentListener(ComponentListener)
   * @see #getComponentListeners()
   * @since 1.1
   */
  public synchronized void removeComponentListener(ComponentListener l)
  {
    componentListener = AWTEventMulticaster.remove(componentListener, l);
  }

  /**
   * Returns an array of all specified listeners registered on this component.
   *
   * @return an array of listeners
   * @see #addComponentListener(ComponentListener)
   * @see #removeComponentListener(ComponentListener)
   * @since 1.4
   */
  public synchronized ComponentListener[] getComponentListeners()
  {
    return (ComponentListener[])
      AWTEventMulticaster.getListeners(componentListener,
                                       ComponentListener.class);
  }

  /**
   * Adds the specified listener to this component. This is harmless if the
   * listener is null, but if the listener has already been registered, it
   * will now be registered twice.
   *
   * @param listener the new listener to add
   * @see FocusEvent
   * @see #removeFocusListener(FocusListener)
   * @see #getFocusListeners()
   * @since 1.1
   */
  public synchronized void addFocusListener(FocusListener l)
  {
    focusListener = AWTEventMulticaster.add(focusListener, l);
    if (focusListener != null)
      enableEvents(AWTEvent.FOCUS_EVENT_MASK);
  }

  /**
   * Removes the specified listener from the component. This is harmless if
   * the listener was not previously registered.
   *
   * @param listener the listener to remove
   * @see FocusEvent
   * @see #addFocusListener(FocusListener)
   * @see #getFocusListeners()
   * @since 1.1
   */
  public synchronized void removeFocusListener(FocusListener l)
  {
    focusListener = AWTEventMulticaster.remove(focusListener, l);
  }

  /**
   * Returns an array of all specified listeners registered on this component.
   *
   * @return an array of listeners
   * @see #addFocusListener(FocusListener)
   * @see #removeFocusListener(FocusListener)
   * @since 1.4
   */
  public synchronized FocusListener[] getFocusListeners()
  {
    return (FocusListener[])
      AWTEventMulticaster.getListeners(focusListener, FocusListener.class);
  }

  /**
   * Adds the specified listener to this component. This is harmless if the
   * listener is null, but if the listener has already been registered, it
   * will now be registered twice.
   *
   * @param listener the new listener to add
   * @see HierarchyEvent
   * @see #removeHierarchyListener(HierarchyListener)
   * @see #getHierarchyListeners()
   * @since 1.3
   */
  public synchronized void addHierarchyListener(HierarchyListener l)
  {
    hierarchyListener = AWTEventMulticaster.add(hierarchyListener, l);
    if (hierarchyListener != null)
      enableEvents(AWTEvent.HIERARCHY_EVENT_MASK);
  }

  /**
   * Removes the specified listener from the component. This is harmless if
   * the listener was not previously registered.
   *
   * @param listener the listener to remove
   * @see HierarchyEvent
   * @see #addHierarchyListener(HierarchyListener)
   * @see #getHierarchyListeners()
   * @since 1.3
   */
  public synchronized void removeHierarchyListener(HierarchyListener l)
  {
    hierarchyListener = AWTEventMulticaster.remove(hierarchyListener, l);
  }

  /**
   * Returns an array of all specified listeners registered on this component.
   *
   * @return an array of listeners
   * @see #addHierarchyListener(HierarchyListener)
   * @see #removeHierarchyListener(HierarchyListener)
   * @since 1.4
   */
  public synchronized HierarchyListener[] getHierarchyListeners()
  {
    return (HierarchyListener[])
      AWTEventMulticaster.getListeners(hierarchyListener,
                                       HierarchyListener.class);
  }

  /**
   * Adds the specified listener to this component. This is harmless if the
   * listener is null, but if the listener has already been registered, it
   * will now be registered twice.
   *
   * @param listener the new listener to add
   * @see HierarchyEvent
   * @see #removeHierarchyBoundsListener(HierarchyBoundsListener)
   * @see #getHierarchyBoundsListeners()
   * @since 1.3
   */
  public synchronized void
    addHierarchyBoundsListener(HierarchyBoundsListener l)
  {
    hierarchyBoundsListener =
      AWTEventMulticaster.add(hierarchyBoundsListener, l);
    if (hierarchyBoundsListener != null)
      enableEvents(AWTEvent.HIERARCHY_BOUNDS_EVENT_MASK);
  }

  /**
   * Removes the specified listener from the component. This is harmless if
   * the listener was not previously registered.
   *
   * @param listener the listener to remove
   * @see HierarchyEvent
   * @see #addHierarchyBoundsListener(HierarchyBoundsListener)
   * @see #getHierarchyBoundsListeners()
   * @since 1.3
   */
  public synchronized void
    removeHierarchyBoundsListener(HierarchyBoundsListener l)
  {
    hierarchyBoundsListener =
      AWTEventMulticaster.remove(hierarchyBoundsListener, l);
  }

  /**
   * Returns an array of all specified listeners registered on this component.
   *
   * @return an array of listeners
   * @see #addHierarchyBoundsListener(HierarchyBoundsListener)
   * @see #removeHierarchyBoundsListener(HierarchyBoundsListener)
   * @since 1.4
   */
  public synchronized HierarchyBoundsListener[] getHierarchyBoundsListeners()
  {
    return (HierarchyBoundsListener[])
      AWTEventMulticaster.getListeners(hierarchyBoundsListener,
                                       HierarchyBoundsListener.class);
  }

  /**
   * Adds the specified listener to this component. This is harmless if the
   * listener is null, but if the listener has already been registered, it
   * will now be registered twice.
   *
   * @param listener the new listener to add
   * @see KeyEvent
   * @see #removeKeyListener(KeyListener)
   * @see #getKeyListeners()
   * @since 1.1
   */
  public synchronized void addKeyListener(KeyListener l)
  {
    keyListener = AWTEventMulticaster.add(keyListener, l);
    if (keyListener != null)
      enableEvents(AWTEvent.KEY_EVENT_MASK);
  }

  /**
   * Removes the specified listener from the component. This is harmless if
   * the listener was not previously registered.
   *
   * @param listener the listener to remove
   * @see KeyEvent
   * @see #addKeyListener(KeyListener)
   * @see #getKeyListeners()
   * @since 1.1
   */
  public synchronized void removeKeyListener(KeyListener l)
  {
    keyListener = AWTEventMulticaster.remove(keyListener, l);
  }

  /**
   * Returns an array of all specified listeners registered on this component.
   *
   * @return an array of listeners
   * @see #addKeyListener(KeyListener)
   * @see #removeKeyListener(KeyListener)
   * @since 1.4
   */
  public synchronized KeyListener[] getKeyListeners()
  {
    return (KeyListener[])
      AWTEventMulticaster.getListeners(keyListener, KeyListener.class);
  }

  /**
   * Adds the specified listener to this component. This is harmless if the
   * listener is null, but if the listener has already been registered, it
   * will now be registered twice.
   *
   * @param listener the new listener to add
   * @see MouseEvent
   * @see #removeMouseListener(MouseListener)
   * @see #getMouseListeners()
   * @since 1.1
   */
  public synchronized void addMouseListener(MouseListener l)
  {
    mouseListener = AWTEventMulticaster.add(mouseListener, l);
    if (mouseListener != null)
      enableEvents(AWTEvent.MOUSE_EVENT_MASK);
  }

  /**
   * Removes the specified listener from the component. This is harmless if
   * the listener was not previously registered.
   *
   * @param listener the listener to remove
   * @see MouseEvent
   * @see #addMouseListener(MouseListener)
   * @see #getMouseListeners()
   * @since 1.1
   */
  public synchronized void removeMouseListener(MouseListener l)
  {
    mouseListener = AWTEventMulticaster.remove(mouseListener, l);
  }

  /**
   * Returns an array of all specified listeners registered on this component.
   *
   * @return an array of listeners
   * @see #addMouseListener(MouseListener)
   * @see #removeMouseListener(MouseListener)
   * @since 1.4
   */
  public synchronized MouseListener[] getMouseListeners()
  {
    return (MouseListener[])
      AWTEventMulticaster.getListeners(mouseListener, MouseListener.class);
  }

  /**
   * Adds the specified listener to this component. This is harmless if the
   * listener is null, but if the listener has already been registered, it
   * will now be registered twice.
   *
   * @param listener the new listener to add
   * @see MouseEvent
   * @see #removeMouseMotionListener(MouseMotionListener)
   * @see #getMouseMotionListeners()
   * @since 1.1
   */
  public synchronized void addMouseMotionListener(MouseMotionListener l)
  {
    mouseMotionListener = AWTEventMulticaster.add(mouseMotionListener, l);
    if (mouseMotionListener != null)
      enableEvents(AWTEvent.MOUSE_EVENT_MASK);
  }

  /**
   * Removes the specified listener from the component. This is harmless if
   * the listener was not previously registered.
   *
   * @param listener the listener to remove
   * @see MouseEvent
   * @see #addMouseMotionListener(MouseMotionListener)
   * @see #getMouseMotionListeners()
   * @since 1.1
   */
  public synchronized void removeMouseMotionListener(MouseMotionListener l)
  {
    mouseMotionListener = AWTEventMulticaster.remove(mouseMotionListener, l);
  }

  /**
   * Returns an array of all specified listeners registered on this component.
   *
   * @return an array of listeners
   * @see #addMouseMotionListener(MouseMotionListener)
   * @see #removeMouseMotionListener(MouseMotionListener)
   * @since 1.4
   */
  public synchronized MouseMotionListener[] getMouseMotionListeners()
  {
    return (MouseMotionListener[])
      AWTEventMulticaster.getListeners(mouseMotionListener,
                                       MouseMotionListener.class);
  }

  /**
   * Adds the specified listener to this component. This is harmless if the
   * listener is null, but if the listener has already been registered, it
   * will now be registered twice.
   *
   * @param listener the new listener to add
   * @see MouseEvent
   * @see MouseWheelEvent
   * @see #removeMouseWheelListener(MouseWheelListener)
   * @see #getMouseWheelListeners()
   * @since 1.4
   */
  public synchronized void addMouseWheelListener(MouseWheelListener l)
  {
    mouseWheelListener = AWTEventMulticaster.add(mouseWheelListener, l);
    if (mouseWheelListener != null)
      enableEvents(AWTEvent.MOUSE_WHEEL_EVENT_MASK);
  }

  /**
   * Removes the specified listener from the component. This is harmless if
   * the listener was not previously registered.
   *
   * @param listener the listener to remove
   * @see MouseEvent
   * @see MouseWheelEvent
   * @see #addMouseWheelListener(MouseWheelListener)
   * @see #getMouseWheelListeners()
   * @since 1.4
   */
  public synchronized void removeMouseWheelListener(MouseWheelListener l)
  {
    mouseWheelListener = AWTEventMulticaster.remove(mouseWheelListener, l);
  }

  /**
   * Returns an array of all specified listeners registered on this component.
   *
   * @return an array of listeners
   * @see #addMouseWheelListener(MouseWheelListener)
   * @see #removeMouseWheelListener(MouseWheelListener)
   * @since 1.4
   */
  public synchronized MouseWheelListener[] getMouseWheelListeners()
  {
    return (MouseWheelListener[])
      AWTEventMulticaster.getListeners(mouseWheelListener,
                                       MouseWheelListener.class);
  }

  /**
   * Adds the specified listener to this component. This is harmless if the
   * listener is null, but if the listener has already been registered, it
   * will now be registered twice.
   *
   * @param listener the new listener to add
   * @see InputMethodEvent
   * @see #removeInputMethodListener(InputMethodListener)
   * @see #getInputMethodListeners()
   * @see #getInputMethodRequests()
   * @since 1.2
   */
  public synchronized void addInputMethodListener(InputMethodListener l)
  {
    inputMethodListener = AWTEventMulticaster.add(inputMethodListener, l);
    if (inputMethodListener != null)
      enableEvents(AWTEvent.INPUT_METHOD_EVENT_MASK);
  }

  /**
   * Removes the specified listener from the component. This is harmless if
   * the listener was not previously registered.
   *
   * @param listener the listener to remove
   * @see InputMethodEvent
   * @see #addInputMethodListener(InputMethodListener)
   * @see #getInputMethodRequests()
   * @since 1.2
   */
  public synchronized void removeInputMethodListener(InputMethodListener l)
  {
    inputMethodListener = AWTEventMulticaster.remove(inputMethodListener, l);
  }

  /**
   * Returns an array of all specified listeners registered on this component.
   *
   * @return an array of listeners
   * @see #addInputMethodListener(InputMethodListener)
   * @see #removeInputMethodListener(InputMethodListener)
   * @since 1.4
   */
  public synchronized InputMethodListener[] getInputMethodListeners()
  {
    return (InputMethodListener[])
      AWTEventMulticaster.getListeners(inputMethodListener,
                                       InputMethodListener.class);
  }

  /**
   * Returns all registered EventListers of the given listenerType.
   *
   * @param listenerType the class of listeners to filter
   * @return an array of registered listeners
   * @see #getComponentListeners()
   * @see #getFocusListeners()
   * @see #getHierarchyListeners()
   * @see #getHierarchyBoundsListeners()
   * @see #getKeyListeners()
   * @see #getMouseListeners()
   * @see #getMouseMotionListeners()
   * @see #getMouseWheelListeners()
   * @see #getInputMethodListeners()
   * @see #getPropertyChangeListeners()
   * @since 1.3
   */
  public EventListener[] getListeners(Class listenerType)
  {
    if (listenerType == ComponentListener.class)
      return getComponentListeners();
    if (listenerType == FocusListener.class)
      return getFocusListeners();
    if (listenerType == HierarchyListener.class)
      return getHierarchyListeners();
    if (listenerType == HierarchyBoundsListener.class)
      return getHierarchyBoundsListeners();
    if (listenerType == KeyListener.class)
      return getKeyListeners();
    if (listenerType == MouseListener.class)
      return getMouseListeners();
    if (listenerType == MouseMotionListener.class)
      return getMouseMotionListeners();
    if (listenerType == MouseWheelListener.class)
      return getMouseWheelListeners();
    if (listenerType == InputMethodListener.class)
      return getInputMethodListeners();
    if (listenerType == PropertyChangeListener.class)
      return getPropertyChangeListeners();
    return (EventListener[]) Array.newInstance(listenerType, 0);
  }

  /**
   * Returns the input method request handler, for subclasses which support
   * on-the-spot text input. By default, input methods are handled by AWT,
   * and this returns null.
   *
   * @return the input method handler, null by default
   * @since 1.2
   */
  public InputMethodRequests getInputMethodRequests()
  {
    return null;
  }

  /**
   * Gets the input context of this component, which is inherited from the
   * parent unless this is overridden.
   *
   * @return the text input context
   * @since 1.2
   */
  public InputContext getInputContext()
  {
    return parent == null ? null : parent.getInputContext();
  }

  /**
   * Enables the specified events. The events to enable are specified
   * by OR-ing together the desired masks from <code>AWTEvent</code>.
   *
   * <p>Events are enabled by default when a listener is attached to the
   * component for that event type. This method can be used by subclasses
   * to ensure the delivery of a specified event regardless of whether
   * or not a listener is attached.
   *
   * @param eventsToEnable the desired events to enable
   * @see #processEvent(AWTEvent)
   * @see #disableEvents(long)
   * @see AWTEvent
   * @since 1.1
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
    if (isLightweight() && parent != null)
      parent.enableEvents(eventsToEnable);
    else if (peer != null)
      peer.setEventMask(eventMask);
  }

  /**
   * Disables the specified events. The events to disable are specified
   * by OR-ing together the desired masks from <code>AWTEvent</code>.
   *
   * @param eventsToDisable the desired events to disable
   * @see #enableEvents(long)
   * @since 1.1
   */
  protected final void disableEvents(long eventsToDisable)
  {
    eventMask &= ~eventsToDisable;
    // forward new event mask to peer?
  }

  /**
   * This is called by the EventQueue if two events with the same event id
   * and owner component are queued. Returns a new combined event, or null if
   * no combining is done. The coelesced events are currently mouse moves
   * (intermediate ones are discarded) and paint events (a merged paint is
   * created in place of the two events).
   *
   * @param existingEvent the event on the queue
   * @param newEvent the new event that might be entered on the queue
   * @return null if both events are kept, or the replacement coelesced event
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
      default:
        return null;
      }
  }

  /**
   * Processes the specified event. In this class, this method simply
   * calls one of the more specific event handlers.
   *
   * @param event the event to process
   * @throws NullPointerException if e is null
   * @see #processComponentEvent(ComponentEvent)
   * @see #processFocusEvent(FocusEvent)
   * @see #processKeyEvent(KeyEvent)
   * @see #processMouseEvent(MouseEvent)
   * @see #processMouseMotionEvent(MouseEvent)
   * @see #processInputMethodEvent(InputMethodEvent)
   * @see #processHierarchyEvent(HierarchyEvent)
   * @see #processMouseWheelEvent(MouseWheelEvent)
   * @since 1.1
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
    else if (e instanceof MouseWheelEvent)
      processMouseWheelEvent((MouseWheelEvent) e);
    else if (e instanceof MouseEvent)
      {
        if (e.id == MouseEvent.MOUSE_MOVED
            || e.id == MouseEvent.MOUSE_DRAGGED)
          processMouseMotionEvent((MouseEvent) e);
        else
          processMouseEvent((MouseEvent) e);
      }
    else if (e instanceof KeyEvent)
      processKeyEvent((KeyEvent) e);
    else if (e instanceof InputMethodEvent)
      processInputMethodEvent((InputMethodEvent) e);
    else if (e instanceof ComponentEvent)
      processComponentEvent((ComponentEvent) e);
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
   * enabled. This method passes the event along to any listeners
   * that are attached.
   *
   * @param event the <code>ComponentEvent</code> to process
   * @throws NullPointerException if e is null
   * @see ComponentListener
   * @see #addComponentListener(ComponentListener)
   * @see #enableEvents(long)
   * @since 1.1
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
   * enabled. This method passes the event along to any listeners
   * that are attached.
   *
   * @param event the <code>FocusEvent</code> to process
   * @throws NullPointerException if e is null
   * @see FocusListener
   * @see #addFocusListener(FocusListener)
   * @see #enableEvents(long)
   * @since 1.1
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
   * enabled. This method passes the event along to any listeners
   * that are attached.
   *
   * @param event the <code>KeyEvent</code> to process
   * @throws NullPointerException if e is null
   * @see KeyListener
   * @see #addKeyListener(KeyListener)
   * @see #enableEvents(long)
   * @since 1.1
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
   * enabled. This method passes the event along to any listeners
   * that are attached.
   *
   * @param event the <code>MouseEvent</code> to process
   * @throws NullPointerException if e is null
   * @see MouseListener
   * @see #addMouseListener(MouseListener)
   * @see #enableEvents(long)
   * @since 1.1
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
   * enabled. This method passes the event along to any listeners
   * that are attached.
   *
   * @param event the <code>MouseMotionEvent</code> to process
   * @throws NullPointerException if e is null
   * @see MouseMotionListener
   * @see #addMouseMotionListener(MouseMotionListener)
   * @see #enableEvents(long)
   * @since 1.1
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

  /**
   * Called when a mouse wheel event is dispatched and component events are
   * enabled. This method passes the event along to any listeners that are
   * attached.
   *
   * @param event the <code>MouseWheelEvent</code> to process
   * @throws NullPointerException if e is null
   * @see MouseWheelListener
   * @see #addMouseWheelListener(MouseWheelListener)
   * @see #enableEvents(long)
   * @since 1.4
   */
  protected void processMouseWheelEvent(MouseWheelEvent e)
  {
    if (mouseWheelListener != null
        && e.id == MouseEvent.MOUSE_WHEEL)
      mouseWheelListener.mouseWheelMoved(e);
  }

  /**
   * Called when an input method event is dispatched and component events are
   * enabled. This method passes the event along to any listeners that are
   * attached.
   *
   * @param event the <code>InputMethodEvent</code> to process
   * @throws NullPointerException if e is null
   * @see InputMethodListener
   * @see #addInputMethodListener(InputMethodListener)
   * @see #enableEvents(long)
   * @since 1.2
   */
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

  /**
   * Called when a hierarchy change event is dispatched and component events
   * are enabled. This method passes the event along to any listeners that are
   * attached.
   *
   * @param event the <code>HierarchyEvent</code> to process
   * @throws NullPointerException if e is null
   * @see HierarchyListener
   * @see #addHierarchyListener(HierarchyListener)
   * @see #enableEvents(long)
   * @since 1.3
   */
  protected void processHierarchyEvent(HierarchyEvent e)
  {
    if (hierarchyListener == null)
      return;
    if (e.id == HierarchyEvent.HIERARCHY_CHANGED)
      hierarchyListener.hierarchyChanged(e);
  }

  /**
   * Called when a hierarchy bounds event is dispatched and component events
   * are enabled. This method passes the event along to any listeners that are
   * attached.
   *
   * @param event the <code>HierarchyEvent</code> to process
   * @throws NullPointerException if e is null
   * @see HierarchyBoundsListener
   * @see #addHierarchyBoundsListener(HierarchyBoundsListener)
   * @see #enableEvents(long)
   * @since 1.3
   */
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

  /**
   * AWT 1.0 event processor.
   *
   * @param evt the event to handle
   * @return false: since the method was deprecated, the return has no meaning
   * @deprecated use {@link #processEvent(AWTEvent)} instead
   */
  public boolean handleEvent(Event evt)
  {
    // XXX Add backward compatibility handling.
    return false;
  }

  /**
   * AWT 1.0 mouse event.
   *
   * @param evt the event to handle
   * @param x the x coordinate, ignored
   * @param y the y coordinate, ignored
   * @return false: since the method was deprecated, the return has no meaning
   * @deprecated use {@link #processMouseEvent(MouseEvent)} instead
   */
  public boolean mouseDown(Event evt, int x, int y)
  {
    // XXX Add backward compatibility handling.
    return false;
  }

  /**
   * AWT 1.0 mouse event.
   *
   * @param evt the event to handle
   * @param x the x coordinate, ignored
   * @param y the y coordinate, ignored
   * @return false: since the method was deprecated, the return has no meaning
   * @deprecated use {@link #processMouseMotionEvent(MouseEvent)} instead
   */
  public boolean mouseDrag(Event evt, int x, int y)
  {
    // XXX Add backward compatibility handling.
    return false;
  }

  /**
   * AWT 1.0 mouse event.
   *
   * @param evt the event to handle
   * @param x the x coordinate, ignored
   * @param y the y coordinate, ignored
   * @return false: since the method was deprecated, the return has no meaning
   * @deprecated use {@link #processMouseEvent(MouseEvent)} instead
   */
  public boolean mouseUp(Event evt, int x, int y)
  {
    // XXX Add backward compatibility handling.
    return false;
  }

  /**
   * AWT 1.0 mouse event.
   *
   * @param evt the event to handle
   * @param x the x coordinate, ignored
   * @param y the y coordinate, ignored
   * @return false: since the method was deprecated, the return has no meaning
   * @deprecated use {@link #processMouseMotionEvent(MouseEvent)} instead
   */
  public boolean mouseMove(Event evt, int x, int y)
  {
    // XXX Add backward compatibility handling.
    return false;
  }

  /**
   * AWT 1.0 mouse event.
   *
   * @param evt the event to handle
   * @param x the x coordinate, ignored
   * @param y the y coordinate, ignored
   * @return false: since the method was deprecated, the return has no meaning
   * @deprecated use {@link #processMouseEvent(MouseEvent)} instead
   */
  public boolean mouseEnter(Event evt, int x, int y)
  {
    // XXX Add backward compatibility handling.
    return false;
  }

  /**
   * AWT 1.0 mouse event.
   *
   * @param evt the event to handle
   * @param x the x coordinate, ignored
   * @param y the y coordinate, ignored
   * @return false: since the method was deprecated, the return has no meaning
   * @deprecated use {@link #processMouseEvent(MouseEvent)} instead
   */
  public boolean mouseExit(Event evt, int x, int y)
  {
    // XXX Add backward compatibility handling.
    return false;
  }

  /**
   * AWT 1.0 key press event.
   *
   * @param evt the event to handle
   * @param key the key pressed, ignored
   * @return false: since the method was deprecated, the return has no meaning
   * @deprecated use {@link #processKeyEvent(KeyEvent)} instead
   */
  public boolean keyDown(Event evt, int key)
  {
    // XXX Add backward compatibility handling.
    return false;
  }

  /**
   * AWT 1.0 key press event.
   *
   * @param evt the event to handle
   * @param key the key pressed, ignored
   * @return false: since the method was deprecated, the return has no meaning
   * @deprecated use {@link #processKeyEvent(KeyEvent)} instead
   */
  public boolean keyUp(Event evt, int key)
  {
    // XXX Add backward compatibility handling.
    return false;
  }

  /**
   * AWT 1.0 action event processor.
   *
   * @param evt the event to handle
   * @param what the object acted on, ignored
   * @return false: since the method was deprecated, the return has no meaning
   * @deprecated in classes which support actions, use
   *             <code>processActionEvent(ActionEvent)</code> instead
   */
  public boolean action(Event evt, Object what)
  {
    // XXX Add backward compatibility handling.
    return false;
  }

  /**
   * Called to inform this component it has been added to a container.
   * A native peer - if any - is created at this time. This method is
   * called automatically by the AWT system and should not be called by
   * user level code.
   *
   * @see #isDisplayable()
   * @see #removeNotify()
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
   * container. Its native peer - if any - is destroyed at this time.
   * This method is called automatically by the AWT system and should
   * not be called by user level code.
   *
   * @see #isDisplayable()
   * @see #addNotify()
   */
  public void removeNotify()
  {
    if (peer != null)
      peer.dispose();
    peer = null;
  }

  /**
   * AWT 1.0 focus event.
   *
   * @param evt the event to handle
   * @param what the Object focused, ignored
   * @return false: since the method was deprecated, the return has no meaning
   * @deprecated use {@link #processFocusEvent(FocusEvent)} instead
   */
  public boolean gotFocus(Event evt, Object what)
  {
    // XXX Add backward compatibility handling.
    return false;
  }

  /**
   * AWT 1.0 focus event.
   *
   * @param evt the event to handle
   * @param what the Object focused, ignored
   * @return false: since the method was deprecated, the return has no meaning
   * @deprecated use {@link #processFocusEvent(FocusEvent)} instead
   */
  public boolean lostFocus(Event evt, Object what)
  {
    // XXX Add backward compatibility handling.
    return false;
  }

  /**
   * Tests whether or not this component is in the group that can be
   * traversed using the keyboard traversal mechanism (such as the TAB key).
   *
   * @return true if the component is traversed via the TAB key
   * @see #setFocusable(boolean)
   * @since 1.1
   * @deprecated use {@link #isFocusable()} instead
   */
  public boolean isFocusTraversable()
  {
    return enabled && visible && (peer == null || peer.isFocusTraversable());
  }

  /**
   * Tests if this component can receive focus.
   *
   * @return true if this component can receive focus
   * @since 1.4
   */
  public boolean isFocusable()
  {
    return focusable;
  }

  /**
   * Specify whether this component can receive focus.
   *
   * @param focusable the new focusable status
   * @since 1.4
   */
  public void setFocusable(boolean focusable)
  {
    firePropertyChange("focusable", this.focusable, focusable);
    this.focusable = focusable;
  }

  /**
   * Sets the focus traversal keys for a given type of focus events. Normally,
   * the default values should match the operating system's native choices. To
   * disable a given traversal, use <code>Collections.EMPTY_SET</code>. The
   * event dispatcher will consume PRESSED, RELEASED, and TYPED events for the
   * specified key, although focus can only transfer on PRESSED or RELEASED.
   *
   * <p>The defauts are:
   * <table>
   *   <th><td>Identifier</td><td>Meaning</td><td>Default</td></th>
   *   <tr><td>KeyboardFocusManager.FORWARD_TRAVERSAL_KEYS</td>
   *     <td>Normal forward traversal</td>
   *     <td>TAB on KEY_PRESSED, Ctrl-TAB on KEY_PRESSED</td></tr>
   *   <tr><td>KeyboardFocusManager.BACKWARD_TRAVERSAL_KEYS</td>
   *     <td>Normal backward traversal</td>
   *     <td>Shift-TAB on KEY_PRESSED, Ctrl-Shift-TAB on KEY_PRESSED</td></tr>
   *   <tr><td>KeyboardFocusManager.UP_CYCLE_TRAVERSAL_KEYS</td>
   *     <td>Go up a traversal cycle</td><td>None</td></tr>
   * </table>
   *
   * <p>Specifying null allows inheritance from the parent, or from the current
   * KeyboardFocusManager default set. If not null, the set must contain only
   * AWTKeyStrokes that are not already focus keys and are not KEY_TYPED
   * events.
   *
   * @param id one of FORWARD_TRAVERSAL_KEYS, BACKWARD_TRAVERSAL_KEYS, or
   *        UP_CYCLE_TRAVERSAL_KEYS
   * @param keystrokes a set of keys, or null
   * @throws IllegalArgumentException if id or keystrokes is invalid
   * @see #getFocusTraversalKeys(int)
   * @see KeyboardFocusManager#FORWARD_TRAVERSAL_KEYS
   * @see KeyboardFocusManager#BACKWARD_TRAVERSAL_KEYS
   * @see KeyboardFocusManager#UP_CYCLE_TRAVERSAL_KEYS
   * @since 1.4
   */
  public void setFocusTraversalKeys(int id, Set keystrokes)
  {
    if (keystrokes == null)
      throw new IllegalArgumentException();
    Set sa;
    Set sb;
    String name;
    switch (id)
      {
      case KeyboardFocusManager.FORWARD_TRAVERSAL_KEYS:
        sa = getFocusTraversalKeys
          (KeyboardFocusManager.BACKWARD_TRAVERSAL_KEYS);
        sb = getFocusTraversalKeys
          (KeyboardFocusManager.UP_CYCLE_TRAVERSAL_KEYS);
        name = "forwardFocusTraversalKeys";
        break;
      case KeyboardFocusManager.BACKWARD_TRAVERSAL_KEYS:
        sa = getFocusTraversalKeys
          (KeyboardFocusManager.FORWARD_TRAVERSAL_KEYS);
        sb = getFocusTraversalKeys
          (KeyboardFocusManager.UP_CYCLE_TRAVERSAL_KEYS);
        name = "backwardFocusTraversalKeys";
        break;
      case KeyboardFocusManager.UP_CYCLE_TRAVERSAL_KEYS:
        sa = getFocusTraversalKeys
          (KeyboardFocusManager.FORWARD_TRAVERSAL_KEYS);
        sb = getFocusTraversalKeys
          (KeyboardFocusManager.BACKWARD_TRAVERSAL_KEYS);
        name = "upCycleFocusTraversalKeys";
        break;
      default:
        throw new IllegalArgumentException();
      }
    int i = keystrokes.size();
    Iterator iter = keystrokes.iterator();
    while (--i >= 0)
      {
        Object o = iter.next();
        if (! (o instanceof AWTKeyStroke)
            || sa.contains(o) || sb.contains(o)
            || ((AWTKeyStroke) o).keyCode == KeyEvent.VK_UNDEFINED)
          throw new IllegalArgumentException();
      }
    if (focusTraversalKeys == null)
      focusTraversalKeys = new Set[3];
    keystrokes = Collections.unmodifiableSet(new HashSet(keystrokes));
    firePropertyChange(name, focusTraversalKeys[id], keystrokes);
    focusTraversalKeys[id] = keystrokes;
  }

  /**
   * Returns the set of keys for a given focus traversal action, as defined
   * in <code>setFocusTraversalKeys</code>. If not set, this is inherited from
   * the parent component, which may have gotten it from the
   * KeyboardFocusManager.
   *
   * @param id one of FORWARD_TRAVERSAL_KEYS, BACKWARD_TRAVERSAL_KEYS, or
   *        UP_CYCLE_TRAVERSAL_KEYS
   * @throws IllegalArgumentException if id is invalid
   * @see #setFocusTraversalKeys(int, Set)
   * @see KeyboardFocusManager#FORWARD_TRAVERSAL_KEYS
   * @see KeyboardFocusManager#BACKWARD_TRAVERSAL_KEYS
   * @see KeyboardFocusManager#UP_CYCLE_TRAVERSAL_KEYS
   * @since 1.4
   */
  public Set getFocusTraversalKeys(int id)
  {
    if (id < KeyboardFocusManager.FORWARD_TRAVERSAL_KEYS
        || id > KeyboardFocusManager.UP_CYCLE_TRAVERSAL_KEYS)
      throw new IllegalArgumentException();
    Set s = null;
    if (focusTraversalKeys != null)
      s = focusTraversalKeys[id];
    if (s == null && parent != null)
      s = parent.getFocusTraversalKeys(id);
    return s == null ? (KeyboardFocusManager.getCurrentKeyboardFocusManager()
                        .getDefaultFocusTraversalKeys(id)) : s;
  }

  /**
   * Tests whether the focus traversal keys for a given action are explicitly
   * set or inherited.
   *
   * @param id one of FORWARD_TRAVERSAL_KEYS, BACKWARD_TRAVERSAL_KEYS, or
   *        UP_CYCLE_TRAVERSAL_KEYS
   * @return true if that set is explicitly specified
   * @throws IllegalArgumentException if id is invalid
   * @see #getFocusTraversalKeys(int)
   * @see KeyboardFocusManager#FORWARD_TRAVERSAL_KEYS
   * @see KeyboardFocusManager#BACKWARD_TRAVERSAL_KEYS
   * @see KeyboardFocusManager#UP_CYCLE_TRAVERSAL_KEYS
   * @since 1.4
   */
  public boolean areFocusTraversalKeysSet(int id)
  {
    if (id < KeyboardFocusManager.FORWARD_TRAVERSAL_KEYS
        || id > KeyboardFocusManager.UP_CYCLE_TRAVERSAL_KEYS)
      throw new IllegalArgumentException();
    return focusTraversalKeys != null && focusTraversalKeys[id] != null;
  }

  /**
   * Sets whether focus traversal keys are enabled, which consumes traversal
   * keys and performs the focus event automatically.
   *
   * @param focusTraversalKeysEnabled the new value of the flag
   * @see #getFocusTraversalKeysEnabled()
   * @see #setFocusTraversalKeys(int, Set)
   * @see #getFocusTraversalKeys(int)
   * @since 1.4
   */
  public void setFocusTraversalKeysEnabled(boolean focusTraversalKeysEnabled)
  {
    firePropertyChange("focusTraversalKeysEnabled",
                       this.focusTraversalKeysEnabled,
                       focusTraversalKeysEnabled);
    this.focusTraversalKeysEnabled = focusTraversalKeysEnabled;
  }

  /**
   * Tests whether focus traversal keys are enabled. If they are, then focus
   * traversal keys are consumed and focus events performed automatically,
   * without the component seeing the keystrokes.
   *
   * @return true if focus traversal is enabled
   * @see #setFocusTraversalKeysEnabled(boolean)
   * @see #setFocusTraversalKeys(int, Set)
   * @see #getFocusTraversalKeys(int)
   * @since 1.4
   */
  public boolean getFocusTraversalKeysEnabled()
  {
    return focusTraversalKeysEnabled;
  }

  /**
   * Requests that this component be given focus. A <code>FOCUS_GAINED</code>
   * event will be fired if and only if this request is successful. To be
   * successful, the component must be displayable, visible, and focusable,
   * and the top-level Window must be able to receive focus. Thus, this
   * request may fail, or be delayed until the window receives focus. It is
   * recommended that <code>requestFocusInWindow</code> be used where
   * possible to be more platform-independent.
   *
   * @see #requestFocusInWindow()
   * @see FocusEvent
   * @see #addFocusListener(FocusListener)
   * @see #isFocusable()
   * @see #isDisplayable()
   * @see KeyboardFocusManager#clearGlobalFocusOwner()
   */
  public void requestFocus()
  {
    // If there's no peer then this component can't get the focus. We
    // treat it as a silent rejection of the request.
    if (peer != null)
      peer.requestFocus();
  }

  /**
   * Requests that this component be given focus. A <code>FOCUS_GAINED</code>
   * event will be fired if and only if this request is successful. To be
   * successful, the component must be displayable, visible, and focusable,
   * and the top-level Window must be able to receive focus. Thus, this
   * request may fail, or be delayed until the window receives focus. It is
   * recommended that <code>requestFocusInWindow</code> be used where
   * possible to be more platform-independent.
   *
   * <p>If the return value is false, the request is guaranteed to fail. If
   * it is true, it will likely succeed unless the action is vetoed or
   * something in the native windowing system intervenes. The temporary flag,
   * and thus this method in general, is not designed for public use; rather
   * it is a hook for lightweight components to notify their container in
   * an attempt to reduce the amount of repainting necessary.
   *
   * @param temporary true if the focus request is temporary
   * @return true if the request has a chance of success
   * @see #requestFocusInWindow()
   * @see FocusEvent
   * @see #addFocusListener(FocusListener)
   * @see #isFocusable()
   * @see #isDisplayable()
   * @see KeyboardFocusManager#clearGlobalFocusOwner()
   * @since 1.4
   */
  protected boolean requestFocus(boolean temporary)
  {
    // XXX Implement correctly.
    requestFocus();
    return true;
  }

  /**
   * Requests that this component be given focus, if it resides in the
   * top-level window which already has focus. A <code>FOCUS_GAINED</code>
   * event will be fired if and only if this request is successful. To be
   * successful, the component must be displayable, visible, and focusable,
   * and the top-level Window must be focused.
   *
   * <p>If the return value is false, the request is guaranteed to fail. If
   * it is true, it will likely succeed unless the action is vetoed or
   * something in the native windowing system intervenes. The temporary flag,
   * and thus this method in general, is not designed for public use; rather
   * it is a hook for lightweight components to notify their container in
   * an attempt to reduce the amount of repainting necessary.
   *
   * @return true if the request has a chance of success
   * @see #requestFocus()
   * @see FocusEvent
   * @see #addFocusListener(FocusListener)
   * @see #isFocusable()
   * @see #isDisplayable()
   * @see KeyboardFocusManager#clearGlobalFocusOwner()
   * @since 1.4
   */
  public boolean requestFocusInWindow()
  {
    // XXX Implement correctly.
    requestFocus();
    return true;
  }

  /**
   * Requests that this component be given focus, if it resides in the
   * top-level window which already has focus. A <code>FOCUS_GAINED</code>
   * event will be fired if and only if this request is successful. To be
   * successful, the component must be displayable, visible, and focusable,
   * and the top-level Window must be focused.
   *
   * <p>If the return value is false, the request is guaranteed to fail. If
   * it is true, it will likely succeed unless the action is vetoed or
   * something in the native windowing system intervenes. The temporary flag,
   * and thus this method in general, is not designed for public use; rather
   * it is a hook for lightweight components to notify their container in
   * an attempt to reduce the amount of repainting necessary.
   *
   * @param temporary true if the focus request is temporary
   * @return true if the request has a chance of success
   * @see #requestFocus()
   * @see FocusEvent
   * @see #addFocusListener(FocusListener)
   * @see #isFocusable()
   * @see #isDisplayable()
   * @see KeyboardFocusManager#clearGlobalFocusOwner()
   * @since 1.4
   */
  protected boolean requestFocusInWindow(boolean temporary)
  {
    // XXX Implement correctly.
    requestFocus();
    return true;
  }

  /**
   * Transfers focus to the next component in the focus traversal order, as
   * though this were the current focus owner.
   *
   * @see #requestFocus()
   * @since 1.1
   */
  public void transferFocus()
  {
    Component next;
    if (parent == null)
      next = findNextFocusComponent(null);
    else
      next = parent.findNextFocusComponent(this);
    if (next != null && next != this)
      next.requestFocus();
  }

  /**
   * Returns the root container that owns the focus cycle where this component
   * resides. A focus cycle root is in two cycles, one as the ancestor, and
   * one as the focusable element; this call always returns the ancestor.
   *
   * @return the ancestor container that owns the focus cycle
   * @since 1.4
   */
  public Container getFocusCycleRootAncestor()
  {
    // XXX Implement.
    throw new Error("not implemented");
  }

  /**
   * Tests if the container is the ancestor of the focus cycle that this
   * component belongs to.
   *
   * @param c the container to test
   * @return true if c is the focus cycle root
   * @since 1.4
   */
  public boolean isFocusCycleRoot(Container c)
  {
    return c == getFocusCycleRootAncestor();
  }

  /**
   * AWT 1.0 focus event processor.
   *
   * @deprecated use {@link #transferFocus()} instead
   */
  public void nextFocus()
  {
    transferFocus();
  }

  /**
   * Transfers focus to the previous component in the focus traversal order, as
   * though this were the current focus owner.
   *
   * @see #requestFocus()
   * @since 1.4
   */
  public void transferFocusBackward()
  {
    // XXX Implement.
    throw new Error("not implemented");
  }

  /**
   * Transfers focus to the focus cycle root of this component. However, if
   * this is a Window, the default focus owner in the window in the current
   * focus cycle is focused instead.
   *
   * @see #requestFocus()
   * @see #isFocusCycleRoot()
   * @since 1.4
   */
  public void transferFocusUpCycle()
  {
    // XXX Implement.
    throw new Error("not implemented");
  }

  /**
   * Tests if this component is the focus owner. Use {@link #isFocusOwner()}
   * instead.
   *
   * @return true if this component owns focus
   * @since 1.2
   */
  public boolean hasFocus()
  {
    return isFocusOwner();
  }

  /**
   * Tests if this component is the focus owner.
   *
   * @return true if this component owns focus
   * @since 1.4
   */
  public boolean isFocusOwner()
  {
    // XXX Implement.
    throw new Error("not implemented");
  }

  /**
   * Adds the specified popup menu to this component.
   *
   * @param menu the popup menu to be added
   * @see #remove(MenuComponent)
   * @since 1.1
   */
  public synchronized void add(PopupMenu popup)
  {
    if (popups == null)
      popups = new Vector();
    popups.add(popup);
  }

  /**
   * Removes the specified popup menu from this component.
   *
   * @param menu the popup menu to remove
   * @see #add(PopupMenu)
   * @since 1.1
   */
  public synchronized void remove(MenuComponent popup)
  {
    if (popups != null)
      popups.remove(popup);
  }

  /**
   * Returns a debugging string representing this component. The string may
   * be empty but not null.
   *
   * @return a string representing this component
   */
  protected String paramString()
  {
    StringBuffer param = new StringBuffer();
    String name = getName();
    if (name != null)
      param.append(name).append(",");
    param.append(width).append("x").append(height).append("+").append(x)
      .append("+").append(y);
    if (! isValid())
      param.append(",invalid");
    if (! isVisible())
      param.append(",invisible");
    if (! isEnabled())
      param.append(",disabled");
    if (! isOpaque())
      param.append(",translucent");
    if (isDoubleBuffered())
      param.append(",doublebuffered");
    return param.toString();
  }

  /**
   * Returns a string representation of this component. This is implemented
   * as <code>getClass().getName() + '[' + paramString() + ']'</code>.
   *
   * @return a string representation of this component
   */
  public String toString()
  {
    return getClass().getName() + '[' + paramString() + ']';
  }

  /**
   * Prints a listing of this component to <code>System.out</code>.
   *
   * @see #list(PrintStream)
   */
  public void list()
  {
    list(System.out, 0);
  }

  /**
   * Prints a listing of this component to the specified print stream.
   *
   * @param stream the <code>PrintStream</code> to print to
   */
  public void list(PrintStream out)
  {
    list(out, 0);
  }

  /**
   * Prints a listing of this component to the specified print stream,
   * starting at the specified indentation point.
   *
   * @param stream the <code>PrintStream</code> to print to
   * @param indent the indentation point
   */
  public void list(PrintStream out, int indent)
  {
    for (int i = 0; i < indent; ++i)
      out.print(' ');
    out.println(toString());
  }

  /**
   * Prints a listing of this component to the specified print writer.
   *
   * @param writer the <code>PrintWrinter</code> to print to
   * @since 1.1
   */
  public void list(PrintWriter out)
  {
    list(out, 0);
  }

  /**
   * Prints a listing of this component to the specified print writer,
   * starting at the specified indentation point.
   *
   * @param writer the <code>PrintWriter</code> to print to
   * @param indent the indentation point
   * @since 1.1
   */
  public void list(PrintWriter out, int indent)
  {
    for (int i = 0; i < indent; ++i)
      out.print(' ');
    out.println(toString());
  }

  /**
   * Adds the specified property listener to this component. This is harmless
   * if the listener is null, but if the listener has already been registered,
   * it will now be registered twice. The property listener ignores inherited
   * properties. Recognized properties include:<br>
   * <ul>
   * <li>the font (<code>"font"</code>)</li>
   * <li>the background color (<code>"background"</code>)</li>
   * <li>the foreground color (<code>"foreground"</code>)</li>
   * <li>the focusability (<code>"focusable"</code>)</li>
   * <li>the focus key traversal enabled state
   *     (<code>"focusTraversalKeysEnabled"</code>)</li>
   * <li>the set of forward traversal keys
   *     (<code>"forwardFocusTraversalKeys"</code>)</li>
   * <li>the set of backward traversal keys
   *     (<code>"backwardFocusTraversalKeys"</code>)</li>
   * <li>the set of up-cycle traversal keys
   *     (<code>"upCycleFocusTraversalKeys"</code>)</li>
   * </ul>
   *
   * @param listener the new listener to add
   * @see #removePropertyChangeListener(PropertyChangeListener)
   * @see #getPropertyChangeListeners()
   * @see #addPropertyChangeListener(String, PropertyChangeListener)
   * @since 1.1
   */
  public void addPropertyChangeListener(PropertyChangeListener listener)
  {
    if (changeSupport == null)
      changeSupport = new PropertyChangeSupport(this);
    changeSupport.addPropertyChangeListener(listener);
  }

  /**
   * Removes the specified property listener from the component. This is
   * harmless if the listener was not previously registered.
   *
   * @param listener the listener to remove
   * @see #addPropertyChangeListener(PropertyChangeListener)
   * @see #getPropertyChangeListeners()
   * @see #removePropertyChangeListener(String, PropertyChangeListener)
   * @since 1.1
   */
  public void removePropertyChangeListener(PropertyChangeListener listener)
  {
    if (changeSupport != null)
      changeSupport.removePropertyChangeListener(listener);
  }

  /**
   * Returns an array of all specified listeners registered on this component.
   *
   * @return an array of listeners
   * @see #addPropertyChangeListener(PropertyChangeListener)
   * @see #removePropertyChangeListener(PropertyChangeListener)
   * @see #getPropertyChangeListeners(String)
   * @since 1.4
   */
  public PropertyChangeListener[] getPropertyChangeListeners()
  {
    return changeSupport == null ? new PropertyChangeListener[0]
      : changeSupport.getPropertyChangeListeners();
  }

  /**
   * Adds the specified property listener to this component. This is harmless
   * if the listener is null, but if the listener has already been registered,
   * it will now be registered twice. The property listener ignores inherited
   * properties. The listener is keyed to a single property. Recognized
   * properties include:<br>
   * <ul>
   * <li>the font (<code>"font"</code>)</li>
   * <li>the background color (<code>"background"</code>)</li>
   * <li>the foreground color (<code>"foreground"</code>)</li>
   * <li>the focusability (<code>"focusable"</code>)</li>
   * <li>the focus key traversal enabled state
   *     (<code>"focusTraversalKeysEnabled"</code>)</li>
   * <li>the set of forward traversal keys
   *     (<code>"forwardFocusTraversalKeys"</code>)</li>
p   * <li>the set of backward traversal keys
   *     (<code>"backwardFocusTraversalKeys"</code>)</li>
   * <li>the set of up-cycle traversal keys
   *     (<code>"upCycleFocusTraversalKeys"</code>)</li>
   * </ul>
   *
   * @param propertyName the property name to filter on
   * @param listener the new listener to add
   * @see #removePropertyChangeListener(String, PropertyChangeListener)
   * @see #getPropertyChangeListeners(String)
   * @see #addPropertyChangeListener(PropertyChangeListener)
   * @since 1.1
   */
  public void addPropertyChangeListener(String propertyName,
                                        PropertyChangeListener listener)
  {
    if (changeSupport == null)
      changeSupport = new PropertyChangeSupport(this);
    changeSupport.addPropertyChangeListener(propertyName, listener);
  }

  /**
   * Removes the specified property listener on a particular property from
   * the component. This is harmless if the listener was not previously
   * registered.
   *
   * @param propertyName the property name to filter on
   * @param listener the listener to remove
   * @see #addPropertyChangeListener(String, PropertyChangeListener)
   * @see #getPropertyChangeListeners(String)
   * @see #removePropertyChangeListener(PropertyChangeListener)
   * @since 1.1
   */
  public void removePropertyChangeListener(String propertyName,
                                           PropertyChangeListener listener)
  {
    if (changeSupport != null)
      changeSupport.removePropertyChangeListener(propertyName, listener);
  }

  /**
   * Returns an array of all specified listeners on the named property that
   * are registered on this component.
   *
   * @return an array of listeners
   * @see #addPropertyChangeListener(String, PropertyChangeListener)
   * @see #removePropertyChangeListener(String, PropertyChangeListener)
   * @see #getPropertyChangeListeners()
   * @since 1.4
   */
  public PropertyChangeListener[] getPropertyChangeListeners(String property)
  {
    return changeSupport == null ? new PropertyChangeListener[0]
      : changeSupport.getPropertyChangeListeners(property);
  }

  /**
   * Report a change in a bound property to any registered property listeners.
   *
   * @param propertyName the property that changed
   * @param oldValue the old property value
   * @param newValue the new property value
   */
  protected void firePropertyChange(String propertyName, Object oldValue,
                                    Object newValue)
  {
    if (changeSupport != null)
      changeSupport.firePropertyChange(propertyName, oldValue, newValue);
  }

  /**
   * Report a change in a bound property to any registered property listeners.
   *
   * @param propertyName the property that changed
   * @param oldValue the old property value
   * @param newValue the new property value
   */
  protected void firePropertyChange(String propertyName, boolean oldValue,
                                    boolean newValue)
  {
    if (changeSupport != null)
      changeSupport.firePropertyChange(propertyName, oldValue, newValue);
  }

  /**
   * Report a change in a bound property to any registered property listeners.
   *
   * @param propertyName the property that changed
   * @param oldValue the old property value
   * @param newValue the new property value
   */
  protected void firePropertyChange(String propertyName, int oldValue,
                                    int newValue)
  {
    if (changeSupport != null)
      changeSupport.firePropertyChange(propertyName, oldValue, newValue);
  }

  /**
   * Sets the text layout orientation of this component. New components default
   * to UNKNOWN (which behaves like LEFT_TO_RIGHT). This method affects only
   * the current component, while
   * {@link #applyComponentOrientation(ComponentOrientation)} affects the
   * entire hierarchy.
   *
   * @param o the new orientation
   * @throws NullPointerException if o is null
   * @see #getComponentOrientation()
   */
  public void setComponentOrientation(ComponentOrientation o)
  {
    if (o == null)
      throw new NullPointerException();
    orientation = o;
  }

  /**
   * Determines the text layout orientation used by this component.
   *
   * @return the component orientation
   * @see #setComponentOrientation(ComponentOrientation)
   */
  public ComponentOrientation getComponentOrientation()
  {
    return orientation;
  }

  /**
   * Sets the text layout orientation of this component. New components default
   * to UNKNOWN (which behaves like LEFT_TO_RIGHT). This method affects the
   * entire hierarchy, while
   * {@link #setComponentOrientation(ComponentOrientation)} affects only the
   * current component.
   *
   * @param o the new orientation
   * @throws NullPointerException if o is null
   * @see #getComponentOrientation()
   * @since 1.4
   */
  public void applyComponentOrientation(ComponentOrientation o)
  {
    setComponentOrientation(o);
  }

  /**
   * Returns the accessibility framework context of this class. Component is
   * not accessible, so the default implementation returns null. Subclasses
   * must override this behavior, and return an appropriate subclass of
   * {@link AccessibleAWTComponent}.
   *
   * @return the accessibility context
   */
  public AccessibleContext getAccessibleContext()
  {
    return null;
  }


  // Helper methods; some are package visible for use by subclasses.

  /**
   * Subclasses should override this to return unique component names like
   * "menuitem0".
   *
   * @return the generated name for this component
   */
  String generateName()
  {
    // Component is abstract.
    return null;
  }

  /**
   * Sets the peer for this component.
   *
   * @param peer the new peer
   */
  final void setPeer(ComponentPeer peer)
  {
    this.peer = peer;
  }

  /**
   * Implementation method that allows classes such as Canvas and Window to
   * override the graphics configuration without violating the published API.
   *
   * @return the graphics configuration
   */
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
   * Implementation of dispatchEvent. Allows trusted package classes to
   * dispatch additional events first.
   *
   * @param e the event to dispatch
   */
  void dispatchEventImpl(AWTEvent e)
  {
    if (eventTypeEnabled (e.id))
      processEvent(e);
  }

  /**
   * Tells whether or not an event type is enabled.
   */
  boolean eventTypeEnabled (int type)
  {
    if (type > AWTEvent.RESERVED_ID_MAX)
      return true;

    switch (type)
      {
      case ComponentEvent.COMPONENT_HIDDEN:
      case ComponentEvent.COMPONENT_MOVED:
      case ComponentEvent.COMPONENT_RESIZED:
      case ComponentEvent.COMPONENT_SHOWN:
        return (componentListener != null
                || (eventMask & AWTEvent.COMPONENT_EVENT_MASK) != 0);

      case KeyEvent.KEY_PRESSED:
      case KeyEvent.KEY_RELEASED:
      case KeyEvent.KEY_TYPED:
        return (keyListener != null
                || (eventMask & AWTEvent.KEY_EVENT_MASK) != 0);

      case MouseEvent.MOUSE_CLICKED:
      case MouseEvent.MOUSE_ENTERED:
      case MouseEvent.MOUSE_EXITED:
      case MouseEvent.MOUSE_PRESSED:
      case MouseEvent.MOUSE_RELEASED:
        return (mouseListener != null
                || mouseMotionListener != null
                || (eventMask & AWTEvent.MOUSE_EVENT_MASK) != 0);
        
      case FocusEvent.FOCUS_GAINED:
      case FocusEvent.FOCUS_LOST:
        return (focusListener != null
                || (eventMask & AWTEvent.FOCUS_EVENT_MASK) != 0);

      case InputMethodEvent.INPUT_METHOD_TEXT_CHANGED:
      case InputMethodEvent.CARET_POSITION_CHANGED:
        return (inputMethodListener != null
                || (eventMask & AWTEvent.INPUT_METHOD_EVENT_MASK) != 0);
        
      case PaintEvent.PAINT:
      case PaintEvent.UPDATE:
        return (eventMask & AWTEvent.PAINT_EVENT_MASK) != 0;
        
      default:
        return false;
      }
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
   *
   * @param queuedEvent the first paint event
   * @param newEvent the second paint event
   * @return the combined paint event, or null
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
   * Does the work for a paint event.
   *
   * @param event the event to process
   */
  private void processPaintEvent(PaintEvent event)
  {
    // Can't do graphics without peer
    if (peer == null)
      return;

    Graphics gfx = getGraphics();
    try
      {
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
    finally
      {
	gfx.dispose();
      }
  }

  /**
   * This method is used to implement transferFocus(). CHILD is the child
   * making the request. This is overridden by Container; when called for an
   * ordinary component there is no child and so we always return null.
   *
   * @param child the component making the request
   * @return the next component to focus on
   */
  Component findNextFocusComponent(Component child)
  {
    return null;
  }

  /**
   * Deserializes this component. This regenerates all serializable listeners
   * which were registered originally.
   *
   * @param s the stream to read from
   * @throws ClassNotFoundException if deserialization fails
   * @throws IOException if the stream fails
   */
  private void readObject(ObjectInputStream s)
    throws ClassNotFoundException, IOException
  {
    s.defaultReadObject();
    String key = (String) s.readObject();
    while (key != null)
      {
        Object listener = s.readObject();
        if ("componentL".equals(key))
          addComponentListener((ComponentListener) listener);
        else if ("focusL".equals(key))
          addFocusListener((FocusListener) listener);
        else if ("keyL".equals(key))
          addKeyListener((KeyListener) listener);
        else if ("mouseL".equals(key))
          addMouseListener((MouseListener) listener);
        else if ("mouseMotionL".equals(key))
          addMouseMotionListener((MouseMotionListener) listener);
        else if ("inputMethodL".equals(key))
          addInputMethodListener((InputMethodListener) listener);
        else if ("hierarchyL".equals(key))
          addHierarchyListener((HierarchyListener) listener);
        else if ("hierarchyBoundsL".equals(key))
          addHierarchyBoundsListener((HierarchyBoundsListener) listener);
        else if ("mouseWheelL".equals(key))
          addMouseWheelListener((MouseWheelListener) listener);
        key = (String) s.readObject();
      }
  }

  /**
   * Serializes this component. This ignores all listeners which do not
   * implement Serializable, but includes those that do.
   *
   * @param s the stream to write to
   * @throws IOException if the stream fails
   */
  private void writeObject(ObjectOutputStream s) throws IOException
  {
    s.defaultWriteObject();
    AWTEventMulticaster.save(s, "componentL", componentListener);
    AWTEventMulticaster.save(s, "focusL", focusListener);
    AWTEventMulticaster.save(s, "keyL", keyListener);
    AWTEventMulticaster.save(s, "mouseL", mouseListener);
    AWTEventMulticaster.save(s, "mouseMotionL", mouseMotionListener);
    AWTEventMulticaster.save(s, "inputMethodL", inputMethodListener);
    AWTEventMulticaster.save(s, "hierarchyL", hierarchyListener);
    AWTEventMulticaster.save(s, "hierarchyBoundsL", hierarchyBoundsListener);
    AWTEventMulticaster.save(s, "mouseWheelL", mouseWheelListener);
    s.writeObject(null);
  }


  // Nested classes.

  /**
   * This class provides accessibility support for subclasses of container.
   *
   * @author Eric Blake <ebb9@email.byu.edu>
   * @since 1.3
   * @status updated to 1.4
   */
  protected abstract class AccessibleAWTComponent extends AccessibleContext
    implements Serializable, AccessibleComponent
  {
    /**
     * Compatible with JDK 1.3+.
     */
    private static final long serialVersionUID = 642321655757800191L;

    /**
     * Converts show/hide events to PropertyChange events, and is registered
     * as a component listener on this component.
     *
     * @serial the component handler
     */
    protected ComponentListener accessibleAWTComponentHandler
      = new AccessibleAWTComponentHandler();

    /**
     * Converts focus events to PropertyChange events, and is registered
     * as a focus listener on this component.
     *
     * @serial the focus handler
     */
    protected FocusListener accessibleAWTFocusHandler
      = new AccessibleAWTFocusHandler();

    /**
     * The default constructor.
     */
    protected AccessibleAWTComponent()
    {
      Component.this.addComponentListener(accessibleAWTComponentHandler);
      Component.this.addFocusListener(accessibleAWTFocusHandler);
    }

    /**
     * Adds a global property change listener to the accessible component.
     *
     * @param l the listener to add
     * @see #ACCESSIBLE_NAME_PROPERTY
     * @see #ACCESSIBLE_DESCRIPTION_PROPERTY
     * @see #ACCESSIBLE_STATE_PROPERTY
     * @see #ACCESSIBLE_VALUE_PROPERTY
     * @see #ACCESSIBLE_SELECTION_PROPERTY
     * @see #ACCESSIBLE_TEXT_PROPERTY
     * @see #ACCESSIBLE_VISIBLE_DATA_PROPERTY
     */
    public void addPropertyChangeListener(PropertyChangeListener l)
    {
      Component.this.addPropertyChangeListener(l);
      super.addPropertyChangeListener(l);
    }

    /**
     * Removes a global property change listener from this accessible
     * component.
     *
     * @param l the listener to remove
     */
    public void removePropertyChangeListener(PropertyChangeListener l)
    {
      Component.this.removePropertyChangeListener(l);
      super.removePropertyChangeListener(l);
    }

    /**
     * Returns the accessible name of this component. It is almost always
     * wrong to return getName(), since it is not localized. In fact, for
     * things like buttons, this should be the text of the button, not the
     * name of the object. The tooltip text might also be appropriate.
     *
     * @return the name
     * @see #setAccessibleName(String)
     */
    public String getAccessibleName()
    {
      return accessibleName == null ? getName() : accessibleName;
    }

    /**
     * Returns a brief description of this accessible context. This should
     * be localized.
     *
     * @return a description of this component
     * @see #setAccessibleDescription(String)
     */
    public String getAccessibleDescription()
    {
      return accessibleDescription;
    }

    /**
     * Returns the role of this component.
     *
     * @return the accessible role
     */
    public AccessibleRole getAccessibleRole()
    {
      return AccessibleRole.AWT_COMPONENT;
    }

    /**
     * Returns a state set describing this component's state.
     *
     * @return a new state set
     * @see AccessibleState
     */
    public AccessibleStateSet getAccessibleStateSet()
    {
      AccessibleStateSet s = new AccessibleStateSet();
      if (Component.this.isEnabled())
        s.add(AccessibleState.ENABLED);
      if (isFocusable())
        s.add(AccessibleState.FOCUSABLE);
      if (isFocusOwner())
        s.add(AccessibleState.FOCUSED);
      if (isOpaque())
        s.add(AccessibleState.OPAQUE);
      if (Component.this.isShowing())
        s.add(AccessibleState.SHOWING);
      if (Component.this.isVisible())
        s.add(AccessibleState.VISIBLE);
      return s;
    }

    /**
     * Returns the parent of this component, if it is accessible.
     *
     * @return the accessible parent
     */
    public Accessible getAccessibleParent()
    {
      if (accessibleParent == null)
        {
          Container parent = getParent();
          accessibleParent = parent instanceof Accessible
            ? (Accessible) parent : null;
        }
      return accessibleParent;
    }

    /**
     * Returns the index of this component in its accessible parent.
     *
     * @return the index, or -1 if the parent is not accessible
     * @see #getAccessibleParent()
     */
    public int getAccessibleIndexInParent()
    {
      if (getAccessibleParent() == null)
        return -1;
      AccessibleContext context
        = ((Component) accessibleParent).getAccessibleContext();
      if (context == null)
        return -1;
      for (int i = context.getAccessibleChildrenCount(); --i >= 0; )
        if (context.getAccessibleChild(i) == Component.this)
          return i;
      return -1;
    }

    /**
     * Returns the number of children of this component which implement
     * Accessible. Subclasses must override this if they can have children.
     *
     * @return the number of accessible children, default 0
     */
    public int getAccessibleChildrenCount()
    {
      return 0;
    }

    /**
     * Returns the ith accessible child. Subclasses must override this if
     * they can have children.
     *
     * @return the ith accessible child, or null
     * @see #getAccessibleChildrenCount()
     */
    public Accessible getAccessibleChild(int i)
    {
      return null;
    }

    /**
     * Returns the locale of this component.
     *
     * @return the locale
     * @throws IllegalComponentStateException if the locale is unknown
     */
    public Locale getLocale()
    {
      return Component.this.getLocale();
    }

    /**
     * Returns this, since it is an accessible component.
     *
     * @return the accessible component
     */
    public AccessibleComponent getAccessibleComponent()
    {
      return this;
    }

    /**
     * Gets the background color.
     *
     * @return the background color
     * @see #setBackground(Color)
     */
    public Color getBackground()
    {
      return Component.this.getBackground();
    }

    /**
     * Sets the background color.
     *
     * @param c the background color
     * @see #getBackground()
     * @see #isOpaque()
     */
    public void setBackground(Color c)
    {
      Component.this.setBackground(c);
    }

    /**
     * Gets the foreground color.
     *
     * @return the foreground color
     * @see #setForeground(Color)
     */
    public Color getForeground()
    {
      return Component.this.getForeground();
    }

    /**
     * Sets the foreground color.
     *
     * @param c the foreground color
     * @see #getForeground()
     */
    public void setForeground(Color c)
    {
      Component.this.setForeground(c);
    }

    /**
     * Gets the cursor.
     *
     * @return the cursor
     * @see #setCursor(Cursor)
     */
    public Cursor getCursor()
    {
      return Component.this.getCursor();
    }

    /**
     * Sets the cursor.
     *
     * @param cursor the cursor
     * @see #getCursor()
     */
    public void setCursor(Cursor cursor)
    {
      Component.this.setCursor(cursor);
    }

    /**
     * Gets the font.
     *
     * @return the font
     * @see #setFont(Font)
     */
    public Font getFont()
    {
      return Component.this.getFont();
    }

    /**
     * Sets the font.
     *
     * @param f the font
     * @see #getFont()
     */
    public void setFont(Font f)
    {
      Component.this.setFont(f);
    }

    /**
     * Gets the font metrics for a font.
     *
     * @param f the font to look up
     * @return its metrics
     * @throws NullPointerException if f is null
     * @see #getFont()
     */
    public FontMetrics getFontMetrics(Font f)
    {
      return Component.this.getFontMetrics(f);
    }

    /**
     * Tests if the component is enabled.
     *
     * @return true if the component is enabled
     * @see #setEnabled(boolean)
     * @see #getAccessibleStateSet()
     * @see AccessibleState#ENABLED
     */
    public boolean isEnabled()
    {
      return Component.this.isEnabled();
    }

    /**
     * Set whether the component is enabled.
     *
     * @param b the new enabled status
     * @see #isEnabled()
     */
    public void setEnabled(boolean b)
    {
      Component.this.setEnabled(b);
    }

    /**
     * Test whether the component is visible (not necesarily showing).
     *
     * @return true if it is visible
     * @see #setVisible(boolean)
     * @see #getAccessibleStateSet()
     * @see AccessibleState#VISIBLE
     */
    public boolean isVisible()
    {
      return Component.this.isVisible();
    }

    /**
     * Sets the visibility of this component.
     *
     * @param b the desired visibility
     * @see #isVisible()
     */
    public void setVisible(boolean b)
    {
      Component.this.setVisible(b);
    }

    /**
     * Tests if the component is showing.
     *
     * @return true if this is showing
     */
    public boolean isShowing()
    {
      return Component.this.isShowing();
    }

    /**
     * Tests if the point is contained in this component.
     *
     * @param p the point to check
     * @return true if it is contained
     * @throws NullPointerException if p is null
     */
    public boolean contains(Point p)
    {
      return Component.this.contains(p.x, p.y);
    }

    /**
     * Returns the location of this object on the screen, or null if it is
     * not showing.
     *
     * @return the location relative to screen coordinates, if showing
     * @see #getBounds()
     * @see #getLocation()
     */
    public Point getLocationOnScreen()
    {
      return Component.this.isShowing() ? Component.this.getLocationOnScreen()
        : null;
    }

    /**
     * Returns the location of this object relative to its parent's coordinate
     * system, or null if it is not showing.
     *
     * @return the location
     * @see #getBounds()
     * @see #getLocationOnScreen()
     */
    public Point getLocation()
    {
      return Component.this.isShowing() ? Component.this.getLocation() : null;
    }

    /**
     * Sets the location of this relative to its parent's coordinate system.
     *
     * @param p the location
     * @throws NullPointerException if p is null
     * @see #getLocation()
     */
    public void setLocation(Point p)
    {
      Component.this.setLocation(p.x, p.y);
    }

    /**
     * Gets the bounds of this component, or null if it is not on screen.
     *
     * @return the bounds
     * @see #contains(Point)
     * @see #setBounds(Rectangle)
     */
    public Rectangle getBounds()
    {
      return Component.this.isShowing() ? Component.this.getBounds() : null;
    }

    /**
     * Sets the bounds of this component.
     *
     * @param r the bounds
     * @throws NullPointerException if r is null
     * @see #getBounds()
     */
    public void setBounds(Rectangle r)
    {
      Component.this.setBounds(r.x, r.y, r.width, r.height);
    }

    /**
     * Gets the size of this component, or null if it is not showing.
     *
     * @return the size
     * @see #setSize(Dimension)
     */
    public Dimension getSize()
    {
      return Component.this.isShowing() ? Component.this.getSize() : null;
    }

    /**
     * Sets the size of this component.
     *
     * @param d the size
     * @throws NullPointerException if d is null
     * @see #getSize()
     */
    public void setSize(Dimension d)
    {
      Component.this.setSize(d.width, d.height);
    }

    /**
     * Returns the Accessible child at a point relative to the coordinate
     * system of this component, if one exists, or null. Since components
     * have no children, subclasses must override this to get anything besides
     * null.
     *
     * @param p the point to check
     * @return the accessible child at that point
     * @throws NullPointerException if p is null
     */
    public Accessible getAccessibleAt(Point p)
    {
      return null;
    }

    /**
     * Tests whether this component can accept focus.
     *
     * @return true if this is focus traversable
     * @see #getAccessibleStateSet()
     * @see AccessibleState#FOCUSABLE
     * @see AccessibleState#FOCUSED
     */
    public boolean isFocusTraversable()
    {
      return Component.this.isFocusTraversable();
    }

    /**
     * Requests focus for this component.
     *
     * @see #isFocusTraversable()
     */
    public void requestFocus()
    {
      Component.this.requestFocus();
    }

    /**
     * Adds a focus listener.
     *
     * @param l the listener to add
     */
    public void addFocusListener(FocusListener l)
    {
      Component.this.addFocusListener(l);
    }

    /**
     * Removes a focus listener.
     *
     * @param l the listener to remove
     */
    public void removeFocusListener(FocusListener l)
    {
      Component.this.removeFocusListener(l);
    }

    /**
     * Converts component changes into property changes.
     *
     * @author Eric Blake <ebb9@email.byu.edu>
     * @since 1.3
     * @status updated to 1.4
     */
    protected class AccessibleAWTComponentHandler implements ComponentListener
    {
      /**
       * Default constructor.
       */
      protected AccessibleAWTComponentHandler()
      {
      }

      /**
       * Convert a component hidden to a property change.
       *
       * @param e the event to convert
       */
      public void componentHidden(ComponentEvent e)
      {
        AccessibleAWTComponent.this.firePropertyChange
          (ACCESSIBLE_STATE_PROPERTY, AccessibleState.VISIBLE, null);
      }

      /**
       * Convert a component shown to a property change.
       *
       * @param e the event to convert
       */
      public void componentShown(ComponentEvent e)
      {
        AccessibleAWTComponent.this.firePropertyChange
          (ACCESSIBLE_STATE_PROPERTY, null, AccessibleState.VISIBLE);
      }

      /**
       * Moving a component does not affect properties.
       *
       * @param e ignored
       */
      public void componentMoved(ComponentEvent e)
      {
      }

      /**
       * Resizing a component does not affect properties.
       *
       * @param e ignored
       */
      public void componentResized(ComponentEvent e)
      {
      }
    } // class AccessibleAWTComponentHandler

    /**
     * Converts focus changes into property changes.
     *
     * @author Eric Blake <ebb9@email.byu.edu>
     * @since 1.3
     * @status updated to 1.4
     */
    protected class AccessibleAWTFocusHandler implements FocusListener
    {
      /**
       * Default constructor.
       */
      protected AccessibleAWTFocusHandler()
      {
      }

      /**
       * Convert a focus gained to a property change.
       *
       * @param e the event to convert
       */
      public void focusGained(FocusEvent e)
      {
        AccessibleAWTComponent.this.firePropertyChange
          (ACCESSIBLE_STATE_PROPERTY, null, AccessibleState.FOCUSED);
      }

      /**
       * Convert a focus lost to a property change.
       *
       * @param e the event to convert
       */
      public void focusLost(FocusEvent e)
      {
        AccessibleAWTComponent.this.firePropertyChange
          (ACCESSIBLE_STATE_PROPERTY, AccessibleState.FOCUSED, null);
      }
    } // class AccessibleAWTComponentHandler
  } // class AccessibleAWTComponent

  /**
   * This class provides support for blitting offscreen surfaces.
   *
   * @author Eric Blake <ebb9@email.byu.edu>
   * @since 1.4
   * @XXX Shell class, to allow compilation. This needs documentation and
   * correct implementation.
   */
  protected class BltBufferStrategy extends BufferStrategy
  {
    protected BufferCapabilities caps;
    protected VolatileImage[] backBuffers;
    protected boolean validatedContents;
    protected int width;
    protected int height;
    protected BltBufferStrategy(int num, BufferCapabilities caps)
    {
      this.caps = caps;
      createBackBuffers(num);
    }
    protected void createBackBuffers(int num)
    {
      backBuffers = new VolatileImage[num];
    }
    public BufferCapabilities getCapabilities()
    {
      return caps;
    }
    public Graphics getDrawGraphics() { return null; }
    public void show() {}
    protected void revalidate() {}
    public boolean contentsLost() { return false; }
    public boolean contentsRestored() { return false; }
  } // class BltBufferStrategy

  /**
   * This class provides support for flipping component buffers. It is only
   * designed for use by Canvas and Window.
   *
   * @author Eric Blake <ebb9@email.byu.edu>
   * @since 1.4
   * @XXX Shell class, to allow compilation. This needs documentation and
   * correct implementation.
   */
  protected class FlipBufferStrategy extends BufferStrategy
  {
    protected int numBuffers;
    protected BufferCapabilities caps;
    protected Image drawBuffer;
    protected VolatileImage drawVBuffer;
    protected boolean validatedContents;
    protected FlipBufferStrategy(int num, BufferCapabilities caps)
      throws AWTException
    {
      this.caps = caps;
      createBuffers(num, caps);
    }
    protected void createBuffers(int num, BufferCapabilities caps)
      throws AWTException {}
    protected Image getBackBuffer()
    {
      return drawBuffer;
    }
    protected void flip(BufferCapabilities.FlipContents flipAction) {}
    protected void destroyBuffers() {}
    public BufferCapabilities getCapabilities()
    {
      return caps;
    }
    public Graphics getDrawGraphics() { return null; }
    protected void revalidate() {}
    public boolean contentsLost() { return false; }
    public boolean contentsRestored() { return false; }
    public void show() {}
  } // class FlipBufferStrategy
} // class Component
