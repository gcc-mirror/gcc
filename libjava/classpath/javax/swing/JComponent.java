/* JComponent.java -- Every component in swing inherits from this class.
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


package javax.swing;

import java.applet.Applet;
import java.awt.AWTEvent;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.FocusTraversalPolicy;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Window;
import java.awt.dnd.DropTarget;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ContainerEvent;
import java.awt.event.ContainerListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.geom.Rectangle2D;
import java.awt.image.ImageObserver;
import java.awt.peer.LightweightPeer;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyVetoException;
import java.beans.VetoableChangeListener;
import java.io.Serializable;
import java.util.EventListener;
import java.util.Hashtable;
import java.util.Locale;
import java.util.Set;

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleExtendedComponent;
import javax.accessibility.AccessibleKeyBinding;
import javax.accessibility.AccessibleRole;
import javax.accessibility.AccessibleStateSet;
import javax.swing.border.Border;
import javax.swing.event.AncestorEvent;
import javax.swing.event.AncestorListener;
import javax.swing.event.EventListenerList;
import javax.swing.event.SwingPropertyChangeSupport;
import javax.swing.plaf.ComponentUI;

/**
 * The base class of all Swing components.
 * It contains generic methods to manage events, properties and sizes. Actual
 * drawing of the component is channeled to a look-and-feel class that is
 * implemented elsewhere.
 *
 * @author Ronald Veldema (rveldema&064;cs.vu.nl)
 * @author Graydon Hoare (graydon&064;redhat.com)
 */
public abstract class JComponent extends Container implements Serializable
{
  private static final long serialVersionUID = -7908749299918704233L;

  /** 
   * Accessibility support is currently missing.
   */
  protected AccessibleContext accessibleContext;

  /**
   * Basic accessibility support for <code>JComponent</code> derived
   * widgets.
   */
  public abstract class AccessibleJComponent 
    extends AccessibleAWTContainer
    implements AccessibleExtendedComponent
  {
    /**
     * Accessibility support for <code>JComponent</code>'s focus handler.
     */
    protected class AccessibleFocusHandler 
      implements FocusListener
    {
      protected AccessibleFocusHandler(){}
      public void focusGained(FocusEvent event){}
      public void focusLost(FocusEvent valevent){}
    }

    /**
     * Accessibility support for <code>JComponent</code>'s container handler.
     */
    protected class AccessibleContainerHandler 
      implements ContainerListener
    {
      protected AccessibleContainerHandler() {}
      public void componentAdded(ContainerEvent event) {}
      public void componentRemoved(ContainerEvent valevent) {}
    }

    private static final long serialVersionUID = -7047089700479897799L;
  
    protected ContainerListener accessibleContainerHandler;
    protected FocusListener accessibleFocusHandler;

    protected AccessibleJComponent() {}
    public void addPropertyChangeListener(PropertyChangeListener listener) {}
    public void removePropertyChangeListener(PropertyChangeListener listener) {}
    public int getAccessibleChildrenCount() { return 0; }
    public Accessible getAccessibleChild(int value0) { return null; }
    public AccessibleStateSet getAccessibleStateSet() { return null; } 
    public String getAccessibleName() { return null; }
    public String getAccessibleDescription() { return null; }
    public AccessibleRole getAccessibleRole() { return null; }
    protected String getBorderTitle(Border value0) { return null; }
    public String getToolTipText() { return null; }
    public String getTitledBorderText() { return null; }
    public AccessibleKeyBinding getAccessibleKeyBinding() { return null; }
  }

  /** 
   * An explicit value for the component's preferred size; if not set by a
   * user, this is calculated on the fly by delegating to the {@link
   * ComponentUI.getPreferredSize} method on the {@link #ui} property. 
   */
  Dimension preferredSize;

  /** 
   * An explicit value for the component's minimum size; if not set by a
   * user, this is calculated on the fly by delegating to the {@link
   * ComponentUI.getMinimumSize} method on the {@link #ui} property. 
   */
  Dimension minimumSize;

  /** 
   * An explicit value for the component's maximum size; if not set by a
   * user, this is calculated on the fly by delegating to the {@link
   * ComponentUI.getMaximumSize} method on the {@link #ui} property.
   */
  Dimension maximumSize;

  /**
   * A value between 0.0 and 1.0 indicating the preferred horizontal
   * alignment of the component, relative to its siblings. The values
   * {@link #LEFT_ALIGNMENT}, {@link #CENTER_ALIGNMENT}, and {@link
   * #RIGHT_ALIGNMENT} can also be used, as synonyms for <code>0.0</code>,
   * <code>0.5</code>, and <code>1.0</code>, respectively. Not all layout
   * managers use this property.
   *
   * @see #getAlignmentX
   * @see #setAlignmentX
   * @see javax.swing.OverlayLayout
   * @see javax.swing.BoxLayout
   */
  float alignmentX = 0.5f;

  /**
   * A value between 0.0 and 1.0 indicating the preferred vertical
   * alignment of the component, relative to its siblings. The values
   * {@link #TOP_ALIGNMENT}, {@link #CENTER_ALIGNMENT}, and {@link
   * #BOTTOM_ALIGNMENT} can also be used, as synonyms for <code>0.0</code>,
   * <code>0.5</code>, and <code>1.0</code>, respectively. Not all layout
   * managers use this property.
   *
   * @see #getAlignmentY
   * @see #setAlignmentY
   * @see javax.swing.OverlayLayout
   * @see javax.swing.BoxLayout
   */
  float alignmentY = 0.5f;

  /** 
   * The border painted around this component.
   * 
   * @see #paintBorder
   */
  Border border;

  /** 
   * The text to show in the tooltip associated with this component.
   * 
   * @see #setToolTipText
   * @see #getToolTipText
   */
   String toolTipText;

  /** 
   * <p>Whether to double buffer this component when painting. This flag
   * should generally be <code>false</code>, except for top level
   * components such as {@link JFrame} or {@link JApplet}.</p>
   *
   * <p>All children of a double buffered component are painted into the
   * double buffer automatically, so only the top widget in a window needs
   * to be double buffered.</p>
   *
   * @see #setDoubleBuffered
   * @see #isDoubleBuffered
   * @see #paintLock
   * @see #paint
   */
  boolean doubleBuffered = false;

  /**
   * A set of flags indicating which debugging graphics facilities should
   * be enabled on this component. The values should be a combination of
   * {@link DebugGraphics.NONE_OPTION}, {@link DebugGraphics.LOG_OPTION},
   * {@link DebugGraphics.FLASH_OPTION}, or {@link
   * DebugGraphics.BUFFERED_OPTION}.
   *
   * @see setDebugGraphicsOptions
   * @see getDebugGraphicsOptions
   * @see DebugGraphics
   * @see getComponentGraphics
   */
  int debugGraphicsOptions;

  /** 
   * <p>This property controls two independent behaviors simultaneously.</p>
   *
   * <p>First, it controls whether to fill the background of this widget
   * when painting its body. This affects calls to {@link
   * JComponent#paintComponent}, which in turn calls {@link
   * ComponentUI#update} on the component's {@link #ui} property. If the
   * component is opaque during this call, the background will be filled
   * before calling {@link ComponentUI#paint}. This happens merely as a
   * convenience; you may fill the component's background yourself too,
   * but there is no need to do so if you will be filling with the same
   * color.</p>
   *
   * <p>Second, it the opaque property informs swing's repaint system
   * whether it will be necessary to paint the components "underneath" this
   * component, in Z-order. If the component is opaque, it is considered to
   * completely occlude components "underneath" it, so they will not be
   * repainted along with the opaque component.</p>
   *
   * <p>The default value for this property is <code>false</code>, but most
   * components will want to set it to <code>true</code> when installing UI
   * defaults in {@link ComponentUI#installUI}.</p>
   *
   * @see #setOpaque
   * @see #isOpaque
   * @see #paintComponent
   */
  boolean opaque = false;

  /** 
   * The user interface delegate for this component. Event delivery and
   * repainting of the component are usually delegated to this object. 
   *
   * @see #setUI
   * @see #getUIClassID
   * @see #updateUI
   */
  protected ComponentUI ui;

  /**
   * A hint to the focus system that this component should or should not
   * get focus. If this is <code>false</code>, swing will not try to
   * request focus on this component; if <code>true</code>, swing might
   * try to request focus, but the request might fail. Thus it is only 
   * a hint guiding swing's behavior.
   *
   * @see #requestFocus
   * @see #isRequestFocusEnabled
   * @see #setRequestFocusEnabled
   */
  boolean requestFocusEnabled;

  /**
   * Flag indicating behavior of this component when the mouse is dragged
   * outside the component and the mouse <em>stops moving</em>. If
   * <code>true</code>, synthetic mouse events will be delivered on regular
   * timed intervals, continuing off in the direction the mouse exited the
   * component, until the mouse is released or re-enters the component.
   *
   * @see setAutoscrolls
   * @see getAutoscrolls
   */
  boolean autoscrolls = false;

  /**
   * Listeners for events other than {@link PropertyChangeEvent} are
   * handled by this listener list. PropertyChangeEvents are handled in
   * {@link #changeSupport}.
   */
  protected EventListenerList listenerList = new EventListenerList();

  /** 
   * Support for {@link PropertyChangeEvent} events. This is constructed
   * lazily when the component gets its first {@link
   * PropertyChangeListener} subscription; until then it's an empty slot.
   */
  private SwingPropertyChangeSupport changeSupport;


  /** 
   * Storage for "client properties", which are key/value pairs associated
   * with this component by a "client", such as a user application or a
   * layout manager. This is lazily constructed when the component gets its
   * first client property.
   */
  private Hashtable clientProperties;
  
  private InputMap inputMap_whenFocused;
  private InputMap inputMap_whenAncestorOfFocused;
  private InputMap inputMap_whenInFocusedWindow;
  private ActionMap actionMap;
  /** @since 1.3 */
  private boolean verifyInputWhenFocusTarget;
  private InputVerifier inputVerifier;

  private TransferHandler transferHandler;

  /** 
   * A lock held during recursive painting; this is used to serialize
   * access to the double buffer, and also to select the "top level" 
   * object which should acquire the double buffer in a given widget
   * tree (which may have multiple double buffered children).
   *
   * @see #doubleBuffered
   * @see #paint
   */
  private static final Object paintLock = new Object();

  /**
   * The default locale of the component.
   * 
   * @see #getDefaultLocale
   * @see #setDefaultLocale
   */
  private static Locale defaultLocale;
  
  public static final String TOOL_TIP_TEXT_KEY = "ToolTipText";

  /**
   * Constant used to indicate that no condition has been assigned to a
   * particular action.
   *
   * @see #registerKeyboardAction(ActionListener, KeyStroke, int)
   */
  public static final int UNDEFINED_CONDITION = -1;

  /**
   * Constant used to indicate that an action should be performed only when 
   * the component has focus.
   *
   * @see #registerKeyboardAction(ActionListener, KeyStroke, int)
   */
  public static final int WHEN_FOCUSED = 0;

  /**
   * Constant used to indicate that an action should be performed only when 
   * the component is an ancestor of the component which has focus.
   *
   * @see #registerKeyboardAction(ActionListener, KeyStroke, int)
   */
  public static final int WHEN_ANCESTOR_OF_FOCUSED_COMPONENT = 1;

  /**
   * Constant used to indicate that an action should be performed only when 
   * the component is in the window which has focus.
   *
   * @see #registerKeyboardAction(ActionListener, KeyStroke, int)
   */
  public static final int WHEN_IN_FOCUSED_WINDOW = 2;

  /**
   * Creates a new <code>JComponent</code> instance.
   */
  public JComponent()
  {
    super();
    super.setLayout(new FlowLayout());
    setDropTarget(new DropTarget());
    defaultLocale = Locale.getDefault();
    debugGraphicsOptions = DebugGraphics.NONE_OPTION;
    setRequestFocusEnabled(true);
  }

  /**
   * Helper to lazily construct and return the client properties table.
   * 
   * @return The current client properties table
   *
   * @see #clientProperties
   * @see #getClientProperty
   * @see #putClientProperty
   */
  private Hashtable getClientProperties()
  {
    if (clientProperties == null)
      clientProperties = new Hashtable();
    return clientProperties;
  }

  /**
   * Get a client property associated with this component and a particular
   * key.
   *
   * @param key The key with which to look up the client property
   *
   * @return A client property associated with this object and key
   *
   * @see #clientProperties
   * @see #getClientProperties
   * @see #putClientProperty
   */
  public final Object getClientProperty(Object key)
  {
    return getClientProperties().get(key);
  }

  /**
   * Add a client property <code>value</code> to this component, associated
   * with <code>key</code>. If there is an existing client property
   * associated with <code>key</code>, it will be replaced.
   *
   * @param key The key of the client property association to add
   * @param value The value of the client property association to add
   *
   * @see #clientProperties
   * @see #getClientProperties
   * @see #getClientProperty
   */
  public final void putClientProperty(Object key, Object value)
  {
    if (value != null)
      getClientProperties().put(key, value);
    else
      getClientProperties().remove(key);
  }

  /**
   * Unregister an <code>AncestorListener</code>.
   *
   * @param listener The listener to unregister
   * 
   * @see #addAncestorListener
   */
  public void removeAncestorListener(AncestorListener listener)
  {
    listenerList.remove(AncestorListener.class, listener);
  }

  /**
   * Unregister a <code>PropertyChangeListener</code>.
   *
   * @param listener The listener to register
   *
   * @see #addPropertyChangeListener(PropertyChangeListener)
   * @see #changeSupport
   */
  public void removePropertyChangeListener(PropertyChangeListener listener)
  {
    if (changeSupport != null)
      changeSupport.removePropertyChangeListener(listener);
  }

  /**
   * Unregister a <code>PropertyChangeListener</code>.
   *
   * @param propertyName The property name to unregister the listener from
   * @param listener The listener to unregister
   *
   * @see #addPropertyChangeListener(String, PropertyChangeListener)
   * @see #changeSupport
   */
  public void removePropertyChangeListener(String propertyName,
                                           PropertyChangeListener listener)
  {
    if (changeSupport != null)
      changeSupport.removePropertyChangeListener(propertyName, listener);
  }

  /**
   * Unregister a <code>VetoableChangeChangeListener</code>.
   *
   * @param listener The listener to unregister
   *
   * @see #addVetoableChangeListener
   */
  public void removeVetoableChangeListener(VetoableChangeListener listener)
  {
    listenerList.remove(VetoableChangeListener.class, listener);
  }

  /**
   * Register an <code>AncestorListener</code>.
   *
   * @param listener The listener to register
   *
   * @see #removeVetoableChangeListener
   */
  public void addAncestorListener(AncestorListener listener)
  {
    listenerList.add(AncestorListener.class, listener);
  }

  /**
   * Register a <code>PropertyChangeListener</code>. This listener will
   * receive any PropertyChangeEvent, regardless of property name. To
   * listen to a specific property name, use {@link
   * #addPropertyChangeListener(String,PropertyChangeListener)} instead.
   *
   * @param listener The listener to register
   *
   * @see #removePropertyChangeListener(PropertyChangeListener)
   * @see #changeSupport
   */
  public void addPropertyChangeListener(PropertyChangeListener listener)
  {
    if (changeSupport == null)
      changeSupport = new SwingPropertyChangeSupport(this);
    changeSupport.addPropertyChangeListener(listener);
  }

  /**
   * Register a <code>PropertyChangeListener</code> for a specific, named
   * property. To listen to all property changes, regardless of name, use
   * {@link #addPropertyChangeListener(PropertyChangeListener)} instead.
   *
   * @param propertyName The property name to listen to
   * @param listener The listener to register
   *
   * @see #removePropertyChangeListener(String, PropertyChangeListener)
   * @see #changeSupport
   */
  public void addPropertyChangeListener(String propertyName,
                                        PropertyChangeListener listener)
  {
    listenerList.add(PropertyChangeListener.class, listener);
  }

  /**
   * Register a <code>VetoableChangeListener</code>.
   *
   * @param listener The listener to register
   *
   * @see #removeVetoableChangeListener
   * @see #listenerList
   */
  public void addVetoableChangeListener(VetoableChangeListener listener)
  {
    listenerList.add(VetoableChangeListener.class, listener);
  }

  /**
   * Return all registered listeners of a particular type.
   *
   * @param listenerType The type of listener to return
   *
   * @return All listeners in the {@link #listenerList} which 
   * are of the specified type
   *
   * @see #listenerList
   */
  public EventListener[] getListeners(Class listenerType)
  {
    return listenerList.getListeners(listenerType);
  }

  /**
   * Return all registered <code>AncestorListener</code> objects.
   *
   * @return The set of <code>AncestorListener</code> objects in {@link
   * #listenerList}
   */
  public AncestorListener[] getAncestorListeners()
  {
    return (AncestorListener[]) getListeners(AncestorListener.class);
  }

  /**
   * Return all registered <code>VetoableChangeListener</code> objects.
   *
   * @return The set of <code>VetoableChangeListener</code> objects in {@link
   * #listenerList}
   */
  public VetoableChangeListener[] getVetoableChangeListeners()
  {
    return (VetoableChangeListener[]) getListeners(VetoableChangeListener.class);
  }

  /**
   * Return all <code>PropertyChangeListener</code> objects registered to listen
   * for a particular property.
   *
   * @param property The property to return the listeners of
   *
   * @return The set of <code>PropertyChangeListener</code> objects in 
   *     {@link #changeSupport} registered to listen on the specified property
   */
  public PropertyChangeListener[] getPropertyChangeListeners(String property)
  {
    return changeSupport == null ? new PropertyChangeListener[0]
                          : changeSupport.getPropertyChangeListeners(property);
  }

  /**
   * A variant of {@link #firePropertyChange(String,Object,Object)} 
   * for properties with <code>boolean</code> values.
   */
  public void firePropertyChange(String propertyName, boolean oldValue,
                                 boolean newValue)
  {
    if (changeSupport != null)
      changeSupport.firePropertyChange(propertyName, Boolean.valueOf(oldValue),
                                       Boolean.valueOf(newValue));
  }

  /**
   * A variant of {@link #firePropertyChange(String,Object,Object)} 
   * for properties with <code>byte</code> values.
   */
  public void firePropertyChange(String propertyName, byte oldValue,
                                 byte newValue)
  {
    if (changeSupport != null)
      changeSupport.firePropertyChange(propertyName, new Byte(oldValue),
                                       new Byte(newValue));
  }

  /**
   * A variant of {@link #firePropertyChange(String,Object,Object)} 
   * for properties with <code>char</code> values.
   */
  public void firePropertyChange(String propertyName, char oldValue,
                                 char newValue)
  {
    if (changeSupport != null)
      changeSupport.firePropertyChange(propertyName, new Character(oldValue),
                                       new Character(newValue));
  }

  /**
   * A variant of {@link #firePropertyChange(String,Object,Object)} 
   * for properties with <code>double</code> values.
   */
  public void firePropertyChange(String propertyName, double oldValue,
                                 double newValue)
  {
    if (changeSupport != null)
      changeSupport.firePropertyChange(propertyName, new Double(oldValue),
                                       new Double(newValue));
  }

  /**
   * A variant of {@link #firePropertyChange(String,Object,Object)} 
   * for properties with <code>float</code> values.
   */
  public void firePropertyChange(String propertyName, float oldValue,
                                 float newValue)
  {
    if (changeSupport != null)
      changeSupport.firePropertyChange(propertyName, new Float(oldValue),
                                       new Float(newValue));
  }

  /**
   * A variant of {@link #firePropertyChange(String,Object,Object)} 
   * for properties with <code>int</code> values.
   */
  public void firePropertyChange(String propertyName, int oldValue,
                                 int newValue)
  {
    if (changeSupport != null)
      changeSupport.firePropertyChange(propertyName, new Integer(oldValue),
                                       new Integer(newValue));
  }

  /**
   * A variant of {@link #firePropertyChange(String,Object,Object)} 
   * for properties with <code>long</code> values.
   */
  public void firePropertyChange(String propertyName, long oldValue,
                                 long newValue)
  {
    if (changeSupport != null)
      changeSupport.firePropertyChange(propertyName, new Long(oldValue),
                                       new Long(newValue));
  }

  /**
   * Call {@link PropertyChangeListener#propertyChange} on all listeners
   * registered to listen to a given property. Any method which changes
   * the specified property of this component should call this method.
   *
   * @param propertyName The property which changed
   * @param oldValue The old value of the property
   * @param newValue The new value of the property
   *
   * @see #changeSupport
   * @see #addPropertyChangeListener(PropertyChangeListener)
   * @see #removePropertyChangeListener(PropertyChangeListener)
   */
  protected void firePropertyChange(String propertyName, Object oldValue,
                                    Object newValue)
  {
    if (changeSupport != null)
      changeSupport.firePropertyChange(propertyName, oldValue, newValue);
  }

  /**
   * A variant of {@link #firePropertyChange(String,Object,Object)} 
   * for properties with <code>short</code> values.
   */
  public void firePropertyChange(String propertyName, short oldValue,
                                 short newValue)
  {
    if (changeSupport != null)
      changeSupport.firePropertyChange(propertyName, new Short(oldValue),
                                       new Short(newValue));
  }

  /**
   * Call {@link VetoableChangeListener#vetoableChange} on all listeners
   * registered to listen to a given property. Any method which changes
   * the specified property of this component should call this method.
   *
   * @param propertyName The property which changed
   * @param oldValue The old value of the property
   * @param newValue The new value of the property
   *
   * @throws PropertyVetoException if the change was vetoed by a listener
   *
   * @see #addVetoableChangeListener
   * @see #removeVetoableChangeListener
   */
  protected void fireVetoableChange(String propertyName, Object oldValue,
                                    Object newValue)
    throws PropertyVetoException
  {
    VetoableChangeListener[] listeners = getVetoableChangeListeners();

    PropertyChangeEvent evt = new PropertyChangeEvent(this, propertyName, oldValue, newValue);

    for (int i = 0; i < listeners.length; i++)
      listeners[i].vetoableChange(evt);
  }

  /**
   * Get the value of the accessibleContext property for this component.
   *
   * @return the current value of the property
   */
  public AccessibleContext getAccessibleContext()
  {
    return null;
  }

  /**
   * Get the value of the {@link #alignmentX} property.
   *
   * @return The current value of the property.
   *
   * @see #setAlignmentX
   * @see #alignmentY
   */
  public float getAlignmentX()
  {
    return alignmentX;
  }

  /**
   * Get the value of the {@link #alignmentY} property.
   *
   * @return The current value of the property.
   *
   * @see #setAlignmentY
   * @see #alignmentX
   */
  public float getAlignmentY()
  {
    return alignmentY;
  }

  /**
   * Get the current value of the {@link #autoscrolls} property.
   *
   * @return The current value of the property
   */
  public boolean getAutoscrolls()
  {
    return autoscrolls;
  }

  /**
   * Set the value of the {@link #border} property.
   *   
   * @param newBorder The new value of the property
   *
   * @see #getBorder
   */
  public void setBorder(Border newBorder)
  {
    Border oldBorder = border;
    border = newBorder;
    firePropertyChange("border", oldBorder, newBorder);
  }

  /**
   * Get the value of the {@link #border} property.
   *
   * @return The property's current value
   *
   * @see #setBorder
   */
  public Border getBorder()
  {
    return border;
  }

  /**
   * Get the component's current bounding box. If a rectangle is provided,
   * use this as the return value (adjusting its fields in place);
   * otherwise (of <code>null</code> is provided) return a new {@link
   * Rectangle}.
   *
   * @param rv Optional return value to use
   *
   * @return A rectangle bounding the component
   */
  public Rectangle getBounds(Rectangle rv)
  {
    if (rv == null)
      return new Rectangle(getX(), getY(), getWidth(), getHeight());
    else
      {
        rv.setBounds(getX(), getY(), getWidth(), getHeight());
        return rv;
      }
  }

  /**
   * Prepares a graphics context for painting this object. If {@link
   * #debugGraphicsOptions} is not equal to {@link
   * DebugGraphics#NONE_OPTION}, produce a new {@link DebugGraphics} object
   * wrapping the parameter. Otherwise configure the parameter with this
   * component's foreground color and font.
   *
   * @param g The graphics context to wrap or configure
   *
   * @return A graphics context to paint this object with
   *
   * @see #debugGraphicsOptions
   * @see #paint
   */
  protected Graphics getComponentGraphics(Graphics g)
  {    
    g.setFont (this.getFont());
    g.setColor (this.getForeground());
    return g;
  }

  /**
   * Get the value of the {@link #debugGraphicsOptions} property.
   *
   * @return The current value of the property.
   *
   * @see #setDebugGraphicsOptions
   * @see #debugGraphicsOptions
   */
  public int getDebugGraphicsOptions()
  {
    return 0;
  }

  /**
   * Get the component's insets, which are calculated from
   * the {@link #border} property. If the border is <code>null</code>,
   * calls {@link Container#getInsets}.
   *
   * @return The component's current insets
   */
  public Insets getInsets()
  {
    if (border == null)
      return super.getInsets();
    return getBorder().getBorderInsets(this);
  }

  /**
   * Get the component's insets, which are calculated from the {@link
   * #border} property. If the border is <code>null</code>, calls {@link
   * Container#getInsets}. The passed-in {@link Insets} value will be
   * used as the return value, if possible.
   *
   * @param insets Return value object to reuse, if possible
   *
   * @return The component's current insets
   */
  public Insets getInsets(Insets insets)
  {
    Insets t = getInsets();

    if (insets == null)
      return t;

    insets.left = t.left;
    insets.right = t.right;
    insets.top = t.top;
    insets.bottom = t.bottom;
    return insets;
  }

  /**
   * Get the component's location. The passed-in {@link Point} value
   * will be used as the return value, if possible.
   *
   * @param rv Return value object to reuse, if possible
   *
   * @return The component's current location
   */
  public Point getLocation(Point rv)
  {
    if (rv == null)
      return new Point(getX(), getY());

    rv.setLocation(getX(), getY());
    return rv;
  }

  /**
   * Get the component's maximum size. If the {@link #maximumSize} property
   * has been explicitly set, it is returned. If the {@link #maximumSize}
   * property has not been set but the {@link #ui} property has been, the
   * result of {@link ComponentUI#getMaximumSize} is returned. If neither
   * property has been set, the result of {@link Container#getMaximumSize}
   * is returned.
   *
   * @return The maximum size of the component
   *
   * @see #maximumSize
   * @see #setMaximumSize
   */
  public Dimension getMaximumSize()
  {
    if (maximumSize != null)
      return maximumSize;

    if (ui != null)
      {
        Dimension s = ui.getMaximumSize(this);
        if (s != null)
          return s;
      }

    Dimension p = super.getMaximumSize();
    return p;
  }

  /**
   * Get the component's minimum size. If the {@link #minimumSize} property
   * has been explicitly set, it is returned. If the {@link #minimumSize}
   * property has not been set but the {@link #ui} property has been, the
   * result of {@link ComponentUI#getMinimumSize} is returned. If neither
   * property has been set, the result of {@link Container#getMinimumSize}
   * is returned.
   *
   * @return The minimum size of the component
   *
   * @see #minimumSize
   * @see #setMinimumSize
   */
  public Dimension getMinimumSize()
  {
    if (minimumSize != null)
      return minimumSize;

    if (ui != null)
      {
        Dimension s = ui.getMinimumSize(this);
        if (s != null)
          return s;
      }

    Dimension p = super.getMinimumSize();
    return p;
  }

  /**
   * Get the component's preferred size. If the {@link #preferredSize}
   * property has been explicitly set, it is returned. If the {@link
   * #preferredSize} property has not been set but the {@link #ui} property
   * has been, the result of {@link ComponentUI#getPreferredSize} is
   * returned. If neither property has been set, the result of {@link
   * Container#getPreferredSize} is returned.
   *
   * @return The preferred size of the component
   *
   * @see #preferredSize
   * @see #setPreferredSize
   */
  public Dimension getPreferredSize()
  {
    Dimension prefSize = null;
    if (preferredSize != null)
      prefSize = preferredSize;

    else if (ui != null)
      {
        Dimension s = ui.getPreferredSize(this);
        if (s != null)
          prefSize = s;
      }

    if (prefSize == null)
      prefSize = super.getPreferredSize();
    // make sure that prefSize is not smaller than minSize
    if (minimumSize != null && prefSize != null
        && (minimumSize.width > prefSize.width
            || minimumSize.height > prefSize.height))
        prefSize = new Dimension(Math.max(minimumSize.width, prefSize.width),
                                 Math.max(minimumSize.height, prefSize.height));
    return prefSize;
  }

  /**
   * Checks if a maximum size was explicitely set on the component.
   *
   * @return <code>true</code> if a maximum size was set,
   * <code>false</code> otherwise
   * 
   * @since 1.3
   */
  public boolean isMaximumSizeSet()
  {
    return maximumSize != null;
  }

  /**
   * Checks if a minimum size was explicitely set on the component.
   *
   * @return <code>true</code> if a minimum size was set,
   * <code>false</code> otherwise
   * 
   * @since 1.3
   */
  public boolean isMinimumSizeSet()
  {
    return minimumSize != null;
  }

  /**
   * Checks if a preferred size was explicitely set on the component.
   *
   * @return <code>true</code> if a preferred size was set,
   * <code>false</code> otherwise
   * 
   * @since 1.3
   */
  public boolean isPreferredSizeSet()
  {
    return preferredSize != null;
  }
  
  /**
   * Return the value of the <code>nextFocusableComponent</code> property.
   *
   * @return The current value of the property, or <code>null</code>
   * if none has been set.
   * 
   * @deprecated See {@link java.awt.FocusTraversalPolicy}
   */
  public Component getNextFocusableComponent()
  {
    return null;
  }

  /**
   * Return the set of {@link KeyStroke} objects which are registered
   * to initiate actions on this component.
   *
   * @return An array of the registered keystrokes
   */
  public KeyStroke[] getRegisteredKeyStrokes()
  {
    return null;
  }

  /**
   * Returns the first ancestor of this component which is a {@link JRootPane}.
   * Equivalent to calling <code>SwingUtilities.getRootPane(this);</code>.
   *
   * @return An ancestral JRootPane, or <code>null</code> if none exists.
   */
  public JRootPane getRootPane()
  {
    JRootPane p = SwingUtilities.getRootPane(this);
    return p;
  }

  /**
   * Get the component's size. The passed-in {@link Dimension} value
   * will be used as the return value, if possible.
   *
   * @param rv Return value object to reuse, if possible
   *
   * @return The component's current size
   */
  public Dimension getSize(Dimension rv)
  {
    if (rv == null)
      return new Dimension(getWidth(), getHeight());
    else
      {
        rv.setSize(getWidth(), getHeight());
        return rv;
      }
  }

  /**
   * Return the <code>toolTip</code> property of this component, creating it and
   * setting it if it is currently <code>null</code>. This method can be
   * overridden in subclasses which wish to control the exact form of
   * tooltip created.
   *
   * @return The current toolTip
   */
  public JToolTip createToolTip()
  {
    JToolTip toolTip = new JToolTip();
    toolTip.setComponent(this);
    toolTip.setTipText(toolTipText);

    return toolTip;
  }

  /**
   * Return the location at which the {@link #toolTipText} property should be
   * displayed, when triggered by a particular mouse event. 
   *
   * @param event The event the tooltip is being presented in response to
   *
   * @return The point at which to display a tooltip, or <code>null</code>
   *     if swing is to choose a default location.
   */
  public Point getToolTipLocation(MouseEvent event)
  {
    return null;
  }

  /**
   * Set the value of the {@link #toolTipText} property.
   *
   * @param text The new property value
   *
   * @see #getToolTipText()
   */
  public void setToolTipText(String text)
  {
    if (text == null)
    {
      ToolTipManager.sharedInstance().unregisterComponent(this);
      toolTipText = null;
      return;
    }

    // XXX: The tip text doesn't get updated unless you set it to null
    // and then to something not-null. This is consistent with the behaviour
    // of Sun's ToolTipManager.

    String oldText = toolTipText;
    toolTipText = text;

    if (oldText == null)
      ToolTipManager.sharedInstance().registerComponent(this);
  }

  /**
   * Get the value of the {@link #toolTipText} property.
   *
   * @return The current property value
   *
   * @see #setToolTipText
   */
  public String getToolTipText()
  {
    return toolTipText;
  }

  /**
   * Get the value of the {@link #toolTipText} property, in response to a
   * particular mouse event.
   *
   * @param event The mouse event which triggered the tooltip
   *
   * @return The current property value
   *
   * @see #setToolTipText
   */
  public String getToolTipText(MouseEvent event)
  {
    return getToolTipText();
  }

  /**
   * Return the top level ancestral container (usually a {@link
   * java.awt.Window} or {@link java.applet.Applet}) which this component is
   * contained within, or <code>null</code> if no ancestors exist.
   *
   * @return The top level container, if it exists
   */
  public Container getTopLevelAncestor()
  {
    Container c = getParent();
    for (Container peek = c; peek != null; peek = peek.getParent())
      c = peek;
    return c;
  }

  /**
   * Compute the component's visible rectangle, which is defined
   * recursively as either the component's bounds, if it has no parent, or
   * the intersection of the component's bounds with the visible rectangle
   * of its parent.
   *
   * @param rect The return value slot to place the visible rectangle in
   */
  public void computeVisibleRect(Rectangle rect)
  {
    Component c = getParent();
    if (c != null && c instanceof JComponent)
      {
        ((JComponent) c).computeVisibleRect(rect);
        rect.translate(-getX(), -getY());
        Rectangle2D.intersect(rect,
                              new Rectangle(0, 0, getWidth(), getHeight()),
                              rect);
      }
    else
      rect.setRect(0, 0, getWidth(), getHeight());
  }

  /**
   * Return the component's visible rectangle in a new {@link Rectangle},
   * rather than via a return slot.
   *
   * @return The component's visible rectangle
   *
   * @see #computeVisibleRect(Rectangle)
   */
  public Rectangle getVisibleRect()
  {
    Rectangle r = new Rectangle();
    computeVisibleRect(r);
    return r;
  }

  /**
   * <p>Requests that this component receive input focus, giving window
   * focus to the top level ancestor of this component. Only works on
   * displayable, focusable, visible components.</p>
   *
   * <p>This method should not be called by clients; it is intended for
   * focus implementations. Use {@link Component#requestFocus()} instead.</p>
   *
   * @see Component#requestFocus()
   */
  public void grabFocus()
  {
  }

  /**
   * Get the value of the {@link #doubleBuffered} property.
   *
   * @return The property's current value
   */
  public boolean isDoubleBuffered()
  {
    return doubleBuffered;
  }

  /**
   * Return <code>true</code> if the provided component has no native peer;
   * in other words, if it is a "lightweight component".
   *
   * @param c The component to test for lightweight-ness
   *
   * @return Whether or not the component is lightweight
   */
  public static boolean isLightweightComponent(Component c)
  {
    return c.getPeer() instanceof LightweightPeer;
  }

  /**
   * Return <code>true</code> if you wish this component to manage its own
   * focus. In particular: if you want this component to be sent
   * <code>TAB</code> and <code>SHIFT+TAB</code> key events, and to not
   * have its children considered as focus transfer targets. If
   * <code>true</code>, focus traversal around this component changes to
   * <code>CTRL+TAB</code> and <code>CTRL+SHIFT+TAB</code>.
   *
   * @return <code>true</code> if you want this component to manage its own
   *     focus, otherwise (by default) <code>false</code>
   *
   * @deprecated 1.4 Use {@link Component#setFocusTraversalKeys(int, Set)} and
   *     {@link Container#setFocusCycleRoot(boolean)} instead
   */
  public boolean isManagingFocus()
  {
    return false;
  }

  /**
   * Return the current value of the {@link #opaque} property. 
   *
   * @return The current property value
   */
  public boolean isOpaque()
  {
    return opaque;
  }

  /**
   * Return <code>true</code> if the component can guarantee that none of its
   * children will overlap in Z-order. This is a hint to the painting system.
   * The default is to return <code>true</code>, but some components such as
   * {@link JLayeredPane} should override this to return <code>false</code>.
   *
   * @return Whether the component tiles its children
   */
  public boolean isOptimizedDrawingEnabled()
  {
    return true;
  }

  /**
   * Return <code>true</code> if this component is currently painting a tile.
   *
   * @return Whether the component is painting a tile
   */
  public boolean isPaintingTile()
  {
    return false;
  }

  /**
   * Get the value of the {@link #requestFocusEnabled} property.
   *
   * @return The current value of the property
   */
  public boolean isRequestFocusEnabled()
  {
    return requestFocusEnabled;
  }

  /**
   * Return <code>true</code> if this component is a validation root; this
   * will cause calls to {@link #invalidate()} in this component's children
   * to be "captured" at this component, and not propagate to its parents.
   * For most components this should return <code>false</code>, but some
   * components such as {@link JViewport} will want to return
   * <code>true</code>.
   *
   * @return Whether this component is a validation root
   */
  public boolean isValidateRoot()
  {
    return false;
  }

  /**
   * <p>Paint the component. This is a delicate process, and should only be
   * called from the repaint thread, under control of the {@link
   * RepaintManager}. Client code should usually call {@link #repaint()} to
   * trigger painting.</p>
   *
   * <p>This method will acquire a double buffer from the {@link
   * RepaintManager} if the component's {@link #doubleBuffered} property is
   * <code>true</code> and the <code>paint</code> call is the
   * <em>first</em> recursive <code>paint</code> call inside swing.</p>
   *
   * <p>The method will also modify the provided {@link Graphics} context
   * via the {@link #getComponentGraphics} method. If you want to customize
   * the graphics object used for painting, you should override that method
   * rather than <code>paint</code>.</p>
   *
   * <p>The body of the <code>paint</code> call involves calling {@link
   * #paintComponent}, {@link #paintBorder}, and {@link #paintChildren} in
   * order. If you want to customize painting behavior, you should override
   * one of these methods rather than <code>paint</code>.</p>
   *
   * <p>For more details on the painting sequence, see <a
   * href="http://java.sun.com/products/jfc/tsc/articles/painting/index.html">
   * this article</a>.</p>
   *
   * @param g The graphics context to paint with
   *
   * @see #paintImmediately(Rectangle)
   */
  public void paint(Graphics g)
  {
    Graphics g2 = g;
    Image doubleBuffer = null;
    RepaintManager rm = RepaintManager.currentManager(this);

    if (isDoubleBuffered()
        && (rm.isDoubleBufferingEnabled())
        && (! Thread.holdsLock(paintLock)))
      {
        doubleBuffer = rm.getOffscreenBuffer(this, getWidth(), getHeight());
      }

    synchronized (paintLock)
      {
        if (doubleBuffer != null)
          {
            g2 = doubleBuffer.getGraphics();
            g2.setClip(g.getClipBounds());
          }
          
        g2 = getComponentGraphics(g2);
        paintComponent(g2);
        paintBorder(g2);
        paintChildren(g2);
        
        if (doubleBuffer != null)
          g.drawImage(doubleBuffer, 0, 0, (ImageObserver) null);
      }
  }

  /**
   * Paint the component's border. This usually means calling {@link
   * Border#paintBorder} on the {@link #border} property, if it is
   * non-<code>null</code>. You may override this if you wish to customize
   * border painting behavior. The border is painted after the component's
   * body, but before the component's children.
   *
   * @param g The graphics context with which to paint the border
   *
   * @see #paint
   * @see #paintChildren
   * @see #paintComponent
   */
  protected void paintBorder(Graphics g)
  {
    if (getBorder() != null)
      getBorder().paintBorder(this, g, 0, 0, getWidth(), getHeight());
  }

  /**
   * Paint the component's children. This usually means calling {@link
   * Container#paint}, which recursively calls {@link #paint} on any of the
   * component's children, with appropriate changes to coordinate space and
   * clipping region. You may override this if you wish to customize
   * children painting behavior. The children are painted after the
   * component's body and border.
   *
   * @param g The graphics context with which to paint the children
   *
   * @see #paint
   * @see #paintBorder
   * @see #paintComponent
   */
  protected void paintChildren(Graphics g)
  {
    super.paint(g);
  }

  /**
   * Paint the component's body. This usually means calling {@link
   * ComponentUI#update} on the {@link #ui} property of the component, if
   * it is non-<code>null</code>. You may override this if you wish to
   * customize the component's body-painting behavior. The component's body
   * is painted first, before the border and children.
   *
   * @param g The graphics context with which to paint the body
   *
   * @see #paint
   * @see #paintBorder
   * @see #paintChildren
   */
  protected void paintComponent(Graphics g)
  {
    if (ui != null)
      {
        Graphics g2 = g;
        if (!(g instanceof Graphics2D))
          g2 = g.create();
        ui.update(getComponentGraphics(g2), this);
        if (!(g instanceof Graphics2D))
          g2.dispose();
      }
  }

  /**
   * A variant of {@link #paintImmediately(Rectangle)} which takes
   * integer parameters.
   *
   * @param x The left x coordinate of the dirty region
   * @param y The top y coordinate of the dirty region
   * @param w The width of the dirty region
   * @param h The height of the dirty region
   */
  public void paintImmediately(int x, int y, int w, int h)
  {
    paintImmediately(new Rectangle(x, y, w, h));
  }

  /**
   * Transform the provided dirty rectangle for this component into the
   * appropriate ancestral {@link JRootPane} and call {@link #paint} on
   * that root pane. This method is called from the {@link RepaintManager}
   * and should always be called within the painting thread.
   *
   * @param r The dirty rectangle to paint
   */
  public void paintImmediately(Rectangle r)
  {
    Component root = SwingUtilities.getRoot(this);
    if (root == null || ! root.isShowing())
      return;
    Graphics g = root.getGraphics();
    if (g == null)
      return;

    Rectangle clip = SwingUtilities.convertRectangle(this, r, root);
    g.setClip(clip);
    root.paint(g);
    g.dispose();
  }

  /**
   * Return a string representation for this component, for use in
   * debugging.
   *
   * @return A string describing this component.
   */
  protected String paramString()
  {
    StringBuffer sb = new StringBuffer();
    sb.append(super.paramString());
    sb.append(",alignmentX=").append(getAlignmentX());
    sb.append(",alignmentY=").append(getAlignmentY());
    sb.append(",border=");
    if (getBorder() != null)
      sb.append(getBorder());
    sb.append(",maximumSize=");
    if (getMaximumSize() != null)
      sb.append(getMaximumSize());
    sb.append(",minimumSize=");
    if (getMinimumSize() != null)
      sb.append(getMinimumSize());
    sb.append(",preferredSize=");
    if (getPreferredSize() != null)
      sb.append(getPreferredSize());
    return sb.toString();
  }

  /**
   * A variant of {@link
   * #registerKeyboardAction(ActionListener,String,KeyStroke,int)} which
   * provides <code>null</code> for the command name.   
   */
  public void registerKeyboardAction(ActionListener act,
                                     KeyStroke stroke, 
                                     int cond)
  {
    registerKeyboardAction(act, null, stroke, cond);
  }

  /* 
   * There is some charmingly undocumented behavior sun seems to be using
   * to simulate the old register/unregister keyboard binding API. It's not
   * clear to me why this matters, but we shall endeavour to follow suit.
   *
   * Two main thing seem to be happening when you do registerKeyboardAction():
   * 
   *  - no actionMap() entry gets created, just an entry in inputMap()
   *
   *  - the inputMap() entry is a proxy class which invokes the the
   *  binding's actionListener as a target, and which clobbers the command
   *  name sent in the ActionEvent, providing the binding command name
   *  instead.
   *
   * This much you can work out just by asking the input and action maps
   * what they contain after making bindings, and watching the event which
   * gets delivered to the recipient. Beyond that, it seems to be a
   * sun-private solution so I will only immitate it as much as it matters
   * to external observers.
   */
  private static class ActionListenerProxy
    extends AbstractAction
  {
    ActionListener target;
    String bindingCommandName;

    public ActionListenerProxy(ActionListener li, 
                               String cmd)
    {
      target = li;
      bindingCommandName = cmd;
    }

    public void actionPerformed(ActionEvent e)
    {
      ActionEvent derivedEvent = new ActionEvent(e.getSource(),
                                                 e.getID(),
                                                 bindingCommandName,
                                                 e.getModifiers());
      target.actionPerformed(derivedEvent);
    }
  }

  
  /**
   * An obsolete method to register a keyboard action on this component.
   * You should use <code>getInputMap</code> and <code>getActionMap</code>
   * to fetch mapping tables from keystrokes to commands, and commands to
   * actions, respectively, and modify those mappings directly.
   *
   * @param act The action to be registered
   * @param cmd The command to deliver in the delivered {@link
   *     java.awt.event.ActionEvent}
   * @param stroke The keystroke to register on
   * @param cond One of the values {@link #UNDEFINED_CONDITION},
   *     {@link #WHEN_ANCESTOR_OF_FOCUSED_COMPONENT}, {@link #WHEN_FOCUSED}, or
   *     {@link #WHEN_IN_FOCUSED_WINDOW}, indicating the condition which must
   *     be met for the action to be fired
   *
   * @see #unregisterKeyboardAction
   * @see #getConditionForKeyStroke
   * @see #resetKeyboardActions
   */
  public void registerKeyboardAction(ActionListener act, 
                                     String cmd,
                                     KeyStroke stroke, 
                                     int cond)
  {
    getInputMap(cond).put(stroke, new ActionListenerProxy(act, cmd));
  }

  public final void setInputMap(int condition, InputMap map)
  {
    enableEvents(AWTEvent.KEY_EVENT_MASK);
    switch (condition)
      {
      case WHEN_FOCUSED:
        inputMap_whenFocused = map;
        break;

      case WHEN_ANCESTOR_OF_FOCUSED_COMPONENT:
        inputMap_whenAncestorOfFocused = map;
        break;

      case WHEN_IN_FOCUSED_WINDOW:
        inputMap_whenInFocusedWindow = map;
        break;
        
      case UNDEFINED_CONDITION:
      default:
        throw new IllegalArgumentException();
      }
  }

  public final InputMap getInputMap(int condition)
  {
    enableEvents(AWTEvent.KEY_EVENT_MASK);
    switch (condition)
      {
      case WHEN_FOCUSED:
        if (inputMap_whenFocused == null)
          inputMap_whenFocused = new InputMap();
        return inputMap_whenFocused;

      case WHEN_ANCESTOR_OF_FOCUSED_COMPONENT:
        if (inputMap_whenAncestorOfFocused == null)
          inputMap_whenAncestorOfFocused = new InputMap();
        return inputMap_whenAncestorOfFocused;

      case WHEN_IN_FOCUSED_WINDOW:
        if (inputMap_whenInFocusedWindow == null)
          inputMap_whenInFocusedWindow = new InputMap();
        return inputMap_whenInFocusedWindow;

      case UNDEFINED_CONDITION:
      default:
        return null;
      }
  }

  public final InputMap getInputMap()
  {
    return getInputMap(WHEN_FOCUSED);
  }

  public final ActionMap getActionMap()
  {
    if (actionMap == null)
      actionMap = new ActionMap();
    return actionMap;
  }

  public final void setActionMap(ActionMap map)
  {
    actionMap = map;
  }

  /**
   * Return the condition that determines whether a registered action
   * occurs in response to the specified keystroke.
   *
   * @param ks The keystroke to return the condition of
   *
   * @return One of the values {@link #UNDEFINED_CONDITION}, {@link
   *     #WHEN_ANCESTOR_OF_FOCUSED_COMPONENT}, {@link #WHEN_FOCUSED}, or {@link
   *     #WHEN_IN_FOCUSED_WINDOW}
   *
   * @deprecated As of 1.3 KeyStrokes can be registered with multiple
   *     simultaneous conditions.
   *
   * @see #registerKeyboardAction(ActionListener, KeyStroke, int)   
   * @see #unregisterKeyboardAction   
   * @see #resetKeyboardActions
   */
  public int getConditionForKeyStroke(KeyStroke ks)
  {
    if (inputMap_whenFocused != null 
        && inputMap_whenFocused.get(ks) != null)
      return WHEN_FOCUSED;
    else if (inputMap_whenAncestorOfFocused != null 
             && inputMap_whenAncestorOfFocused.get(ks) != null)
      return WHEN_ANCESTOR_OF_FOCUSED_COMPONENT;
    else if (inputMap_whenInFocusedWindow != null 
             && inputMap_whenInFocusedWindow.get(ks) != null)
      return WHEN_IN_FOCUSED_WINDOW;
    else
      return UNDEFINED_CONDITION;
  }

  /**
   * Get the ActionListener (typically an {@link Action} object) which is
   * associated with a particular keystroke. 
   *
   * @param ks The keystroke to retrieve the action of
   *
   * @return The action associated with the specified keystroke
   *
   * @deprecated Use {@link #getActionMap()}
   */
  public ActionListener getActionForKeyStroke(KeyStroke ks)
  {
    Object cmd = getInputMap().get(ks);
    if (cmd != null)
      {
        if (cmd instanceof ActionListenerProxy)
          return (ActionListenerProxy) cmd;
        else if (cmd instanceof String)
          return getActionMap().get(cmd);
      }
    return null;
  }

  /**
   * A hook for subclasses which want to customize event processing.
   */
  protected void processComponentKeyEvent(KeyEvent e)
  {
  }

  /**
   * Override the default key dispatch system from Component to hook into
   * the swing {@link InputMap} / {@link ActionMap} system.
   *
   * See <a
   * href="http://java.sun.com/products/jfc/tsc/special_report/kestrel/keybindings.html">
   * this report</a> for more details, it's somewhat complex.
   */
  protected void processKeyEvent(KeyEvent e)
  {
    // let the AWT event processing send KeyEvents to registered listeners
    super.processKeyEvent(e);
    processComponentKeyEvent(e);

    if (e.isConsumed())
      return;

    // Input maps are checked in this order:
    // 1. The focused component's WHEN_FOCUSED map is checked.
    // 2. The focused component's WHEN_ANCESTOR_OF_FOCUSED_COMPONENT map.
    // 3. The WHEN_ANCESTOR_OF_FOCUSED_COMPONENT maps of the focused
    //    component's parent, then its parent's parent, and so on.
    //    Note: Input maps for disabled components are skipped.
    // 4. The WHEN_IN_FOCUSED_WINDOW maps of all the enabled components in
    //    the focused window are searched.
    
    if (processKeyBinding(KeyStroke.getKeyStrokeForEvent(e), 
                          e, WHEN_FOCUSED, e.getID() == KeyEvent.KEY_PRESSED))
      // This is step 1 from above comment.
      e.consume();
    else if (processKeyBinding(KeyStroke.getKeyStrokeForEvent(e),
                               e, WHEN_ANCESTOR_OF_FOCUSED_COMPONENT,
                               e.getID() == KeyEvent.KEY_PRESSED))
      // This is step 2 from above comment.
      e.consume();
    else
      {
        // This is step 3 from above comment.
        Container current = this;
        while ((current = current.getParent()) instanceof JComponent)
          {
            if (((JComponent)current).processKeyBinding
                (KeyStroke.getKeyStrokeForEvent(e), e, 
                 WHEN_ANCESTOR_OF_FOCUSED_COMPONENT, 
                 e.getID() == KeyEvent.KEY_PRESSED))
              {
                e.consume();
                break;
              }
            if (current instanceof Window || current instanceof Applet
                || current instanceof JInternalFrame)
              break;
          }
        if (e.isConsumed())
          return;
        
        // This is step 4 from above comment.
        // FIXME: Implement.  Note, should use ComponentInputMaps rather
        // than walking the entire containment hierarchy.
      }
  }

  protected boolean processKeyBinding(KeyStroke ks,
                                      KeyEvent e,
                                      int condition,
                                      boolean pressed)
  { 
    if (isEnabled())
      {
        Action act = null;
        InputMap map = getInputMap(condition);
        if (map != null)
          {
            Object cmd = map.get(ks);
            if (cmd != null)
              {
                if (cmd instanceof ActionListenerProxy)
                  act = (Action) cmd;
                else 
                  act = (Action) getActionMap().get(cmd);
              }
          }
        if (act != null && act.isEnabled())
          return SwingUtilities.notifyAction(act, ks, e, this, e.getModifiers());
      }
    return false;
  }
  
  /**
   * Remove a keyboard action registry.
   *
   * @param aKeyStroke The keystroke to unregister
   *
   * @see #registerKeyboardAction(ActionListener, KeyStroke, int)
   * @see #getConditionForKeyStroke
   * @see #resetKeyboardActions
   */
  public void unregisterKeyboardAction(KeyStroke aKeyStroke)
  {
    // FIXME: Must be implemented.
  }


  /**
   * Reset all keyboard action registries.
   *
   * @see #registerKeyboardAction(ActionListener, KeyStroke, int)
   * @see #unregisterKeyboardAction
   * @see #getConditionForKeyStroke
   */
  public void resetKeyboardActions()
  {
    if (inputMap_whenFocused != null)
      inputMap_whenFocused.clear();
    if (inputMap_whenAncestorOfFocused != null)
      inputMap_whenAncestorOfFocused.clear();
    if (inputMap_whenInFocusedWindow != null)
      inputMap_whenInFocusedWindow.clear();
    if (actionMap != null)
      actionMap.clear();
  }

  /**
   * Mark the described region of this component as dirty in the current
   * {@link RepaintManager}. This will queue an asynchronous repaint using
   * the system painting thread in the near future.
   *
   * @param tm ignored
   * @param x coordinate of the region to mark as dirty
   * @param y coordinate of the region to mark as dirty
   * @param width dimension of the region to mark as dirty
   * @param height dimension of the region to mark as dirty
   */
  public void repaint(long tm, int x, int y, int width, int height)
  {
    Rectangle dirty = new Rectangle(x, y, width, height);
    Rectangle vis = getVisibleRect();
    dirty = dirty.intersection(vis);
    RepaintManager.currentManager(this).addDirtyRegion(this, dirty.x, dirty.y,
                                                       dirty.width,
                                                       dirty.height);
  }

  /**
   * Mark the described region of this component as dirty in the current
   * {@link RepaintManager}. This will queue an asynchronous repaint using
   * the system painting thread in the near future.
   *
   * @param r The rectangle to mark as dirty
   */
  public void repaint(Rectangle r)
  {
    repaint((long) 0, (int) r.getX(), (int) r.getY(), (int) r.getWidth(),
            (int) r.getHeight());
  }

  /**
   * Request focus on the default component of this component's {@link
   * FocusTraversalPolicy}.
   *
   * @return The result of {@link #requestFocus()}
   *
   * @deprecated Use {@link #requestFocus()} on the default component provided
   *     from the {@link FocusTraversalPolicy} instead.
   */
  public boolean requestDefaultFocus()
  {
    return false;
  }

  /**
   * Queue a an invalidation and revalidation of this component, using 
   * {@link RepaintManager#addInvalidComponent}.
   */
  public void revalidate()
  {
    invalidate();
    RepaintManager.currentManager(this).addInvalidComponent(this);
  }

  /**
   * Calls <code>scrollRectToVisible</code> on the component's parent. 
   * Components which can service this call should override.
   *
   * @param r The rectangle to make visible
   */
  public void scrollRectToVisible(Rectangle r)
  {
    Component p = getParent();
    if (p instanceof JComponent)
      ((JComponent) p).scrollRectToVisible(r);
  }

  /**
   * Set the value of the {@link #alignmentX} property.
   *
   * @param a The new value of the property
   */
  public void setAlignmentX(float a)
  {
    alignmentX = a;
  }

  /**
   * Set the value of the {@link #alignmentY} property.
   *
   * @param a The new value of the property
   */
  public void setAlignmentY(float a)
  {
    alignmentY = a;
  }

  /**
   * Set the value of the {@link #autoscrolls} property.
   *
   * @param a The new value of the property
   */
  public void setAutoscrolls(boolean a)
  {
    autoscrolls = a;
  }

  /**
   * Set the value of the {@link #debugGraphicsOptions} property.
   *
   * @param debugOptions The new value of the property
   */
  public void setDebugGraphicsOptions(int debugOptions)
  {
    debugGraphicsOptions = debugOptions;
  }

  /**
   * Set the value of the {@link #doubleBuffered} property.
   *
   * @param db The new value of the property
   */
  public void setDoubleBuffered(boolean db)
  {
    doubleBuffered = db;
  }

  /**
   * Set the value of the <code>enabled</code> property.
   *
   * @param enable The new value of the property
   */
  public void setEnabled(boolean enable)
  {
    boolean oldEnabled = isEnabled();
    super.setEnabled(enable);
    firePropertyChange("enabled", oldEnabled, enable);
  }

  /**
   * Set the value of the <code>font</code> property.
   *
   * @param f The new value of the property
   */
  public void setFont(Font f)
  {
    super.setFont(f);
  }

  /**
   * Set the value of the <code>background</code> property.
   *
   * @param bg The new value of the property
   */
  public void setBackground(Color bg)
  {
    super.setBackground(bg);
  }

  /**
   * Set the value of the <code>foreground</code> property.
   *
   * @param fg The new value of the property
   */
  public void setForeground(Color fg)
  {
    super.setForeground(fg);
  }

  /**
   * Set the value of the {@link #maximumSize} property.
   *
   * @param max The new value of the property
   */
  public void setMaximumSize(Dimension max)
  {
    Dimension oldMaximumSize = maximumSize;
    maximumSize = max;
    firePropertyChange("maximumSize", oldMaximumSize, maximumSize);
  }

  /**
   * Set the value of the {@link #minimumSize} property.
   *
   * @param min The new value of the property
   */
  public void setMinimumSize(Dimension min)
  {
    Dimension oldMinimumSize = minimumSize;
    minimumSize = min;
    firePropertyChange("minimumSize", oldMinimumSize, minimumSize);
  }

  /**
   * Set the value of the {@link #preferredSize} property.
   *
   * @param pref The new value of the property
   */
  public void setPreferredSize(Dimension pref)
  {
    Dimension oldPreferredSize = preferredSize;
    preferredSize = pref;
    firePropertyChange("preferredSize", oldPreferredSize, preferredSize);
  }

  /**
   * Set the specified component to be the next component in the 
   * focus cycle, overriding the {@link FocusTraversalPolicy} for
   * this component.
   *
   * @param aComponent The component to set as the next focusable
   *
   * @deprecated Use FocusTraversalPolicy instead
   */
  public void setNextFocusableComponent(Component aComponent)
  {
  }

  /**
   * Set the value of the {@link #requestFocusEnabled} property.
   *
   * @param e The new value of the property
   */
  public void setRequestFocusEnabled(boolean e)
  {
    requestFocusEnabled = e;
  }

  /**
   * Get the value of the {@link #transferHandler} property.
   *
   * @return The current value of the property
   *
   * @see #setTransferHandler
   */

  public TransferHandler getTransferHandler()
  {
    return transferHandler;
  }

  /**
   * Set the value of the {@link #transferHandler} property.
   *
   * @param newHandler The new value of the property
   *
   * @see #getTransferHandler
   */

  public void setTransferHandler(TransferHandler newHandler)
  {
    if (transferHandler == newHandler)
      return;

    TransferHandler oldHandler = transferHandler;
    transferHandler = newHandler;
    firePropertyChange("transferHandler", oldHandler, newHandler);
  }

  /**
   * Set the value of the {@link #opaque} property.
   *
   * @param isOpaque The new value of the property
   *
   * @see ComponentUI#update
   */
  public void setOpaque(boolean isOpaque)
  {
    boolean oldOpaque = opaque;
    opaque = isOpaque;
    firePropertyChange("opaque", oldOpaque, opaque);
  }

  /**
   * Set the value of the visible property.
   *
   * @param v The new value of the property
   */
  public void setVisible(boolean v)
  {
    super.setVisible(v);
  }

  /**
   * Call {@link #paint}. 
   * 
   * @param g The graphics context to paint into
   */
  public void update(Graphics g)
  {
    paint(g);
  }

  /**
   * Get the value of the UIClassID property. This property should be a key
   * in the {@link UIDefaults} table managed by {@link UIManager}, the
   * value of which is the name of a class to load for the component's
   * {@link #ui} property.
   *
   * @return A "symbolic" name which will map to a class to use for the
   * component's UI, such as <code>"ComponentUI"</code>
   *
   * @see #setUI
   * @see #updateUI
   */
  public String getUIClassID()
  {
    return "ComponentUI";
  }

  /**
   * Install a new UI delegate as the component's {@link #ui} property. In
   * the process, this will call {@link ComponentUI#uninstallUI} on any
   * existing value for the {@link #ui} property, and {@link
   * ComponentUI#installUI} on the new UI delegate.
   *
   * @param newUI The new UI delegate to install
   *
   * @see #updateUI
   * @see #getUIClassID
   */
  protected void setUI(ComponentUI newUI)
  {
    if (ui != null)
      ui.uninstallUI(this);

    ComponentUI oldUI = ui;
    ui = newUI;

    if (ui != null)
      ui.installUI(this);

    firePropertyChange("UI", oldUI, newUI);
    
  }

  /**
   * This method should be overridden in subclasses. In JComponent, the
   * method does nothing. In subclasses, it should a UI delegate
   * (corresponding to the symbolic name returned from {@link
   * #getUIClassID}) from the {@link UIManager}, and calls {@link #setUI}
   * with the new delegate.
   */
  public void updateUI()
  {
    System.out.println("update UI not overwritten in class: " + this);
  }

  public static Locale getDefaultLocale()
  {
    return defaultLocale;
  }
  
  public static void setDefaultLocale(Locale l)
  {
    defaultLocale = l;
  }
  
  /**
   * Returns the currently set input verifier for this component.
   *
   * @return the input verifier, or <code>null</code> if none
   */
  public InputVerifier getInputVerifier()
  {
    return inputVerifier;
  }

  /**
   * Sets the input verifier to use by this component.
   *
   * @param verifier the input verifier, or <code>null</code>
   */
  public void setInputVerifier(InputVerifier verifier)
  {
    InputVerifier oldVerifier = inputVerifier;
    inputVerifier = verifier;
    firePropertyChange("inputVerifier", oldVerifier, verifier);
  }

  /**
   * @since 1.3
   */
  public boolean getVerifyInputWhenFocusTarget()
  {
    return verifyInputWhenFocusTarget;
  }

  /**
   * @since 1.3
   */
  public void setVerifyInputWhenFocusTarget(boolean verifyInputWhenFocusTarget)
  {
    if (this.verifyInputWhenFocusTarget == verifyInputWhenFocusTarget)
      return;

    this.verifyInputWhenFocusTarget = verifyInputWhenFocusTarget;
    firePropertyChange("verifyInputWhenFocusTarget",
                       ! verifyInputWhenFocusTarget,
                       verifyInputWhenFocusTarget);
  }

  /**
   * Requests that this component gets the input focus if the
   * requestFocusEnabled property is set to <code>true</code>.
   * This also means that this component's top-level window becomes
   * the focused window, if that is not already the case.
   *
   * The preconditions that have to be met to become a focus owner is that
   * the component must be displayable, visible and focusable.
   *
   * Note that this signals only a request for becoming focused. There are
   * situations in which it is not possible to get the focus. So developers
   * should not assume that the component has the focus until it receives
   * a {@link java.awt.event.FocusEvent} with a value of
   * {@link java.awt.event.FocusEvent#FOCUS_GAINED}.
   *
   * @see Component#requestFocus()
   */
  public void requestFocus()
  {
    if (isRequestFocusEnabled())
      super.requestFocus();
  }

  /**
   * This method is overridden to make it public so that it can be used
   * by look and feel implementations.
   *
   * You should not use this method directly. Instead you are strongly
   * encouraged to call {@link #requestFocus()} or 
   * {@link #requestFocusInWindow()} instead.
   *
   * @param temporary if the focus change is temporary
   *
   * @return <code>false</code> if the focus change request will definitly
   *     fail, <code>true</code> if it will likely succeed
   *
   * @see Component#requestFocus(boolean)
   *
   * @since 1.4
   */
  public boolean requestFocus(boolean temporary)
  {
    return super.requestFocus(temporary);
  }

  /**
   * Requests that this component gets the input focus if the top level
   * window that contains this component has the focus and the
   * requestFocusEnabled property is set to <code>true</code>.
   *
   * The preconditions that have to be met to become a focus owner is that
   * the component must be displayable, visible and focusable.
   *
   * Note that this signals only a request for becoming focused. There are
   * situations in which it is not possible to get the focus. So developers
   * should not assume that the component has the focus until it receives
   * a {@link java.awt.event.FocusEvent} with a value of
   * {@link java.awt.event.FocusEvent#FOCUS_GAINED}.
   *
   * @return <code>false</code> if the focus change request will definitly
   *     fail, <code>true</code> if it will likely succeed
   *
   * @see Component#requestFocusInWindow()
   */
  public boolean requestFocusInWindow()
  {
    if (isRequestFocusEnabled())
      return super.requestFocusInWindow();
    else
      return false;
  }

  /**
   * This method is overridden to make it public so that it can be used
   * by look and feel implementations.
   *
   * You should not use this method directly. Instead you are strongly
   * encouraged to call {@link #requestFocus()} or 
   * {@link #requestFocusInWindow()} instead.
   *
   * @param temporary if the focus change is temporary
   *
   * @return <code>false</code> if the focus change request will definitly
   *     fail, <code>true</code> if it will likely succeed
   *
   * @see Component#requestFocus(boolean)
   *
   * @since 1.4
   */
  public boolean requestFocusInWindow(boolean temporary)
  {
    return super.requestFocusInWindow(temporary);
  }

  /**
   * Receives notification if this component is added to a parent component.
   *
   * Notification is sent to all registered AncestorListeners about the
   * new parent.
   *
   * This method sets up ActionListeners for all registered KeyStrokes of
   * this component in the chain of parent components.
   *
   * A PropertyChange event is fired to indicate that the ancestor property
   * has changed.
   *
   * This method is used internally and should not be used in applications.
   */
  public void addNotify()
  {
    super.addNotify();

    // let parents inherit the keybord mapping
    InputMap input = getInputMap();
    ActionMap actions = getActionMap();

    Container parent = getParent();
    while ((parent != null) && (parent instanceof JComponent))
      {
        JComponent jParent = (JComponent) parent;
        InputMap parentInput = jParent.getInputMap();
        ActionMap parentAction = jParent.getActionMap();

        KeyStroke[] ikeys = input.keys();
        for (int i = 0; i < ikeys.length; i++)
          {
            Object o = input.get(ikeys[i]);
            parentInput.put(ikeys[i], o);
          }

        Object[] akeys = actions.keys();
        for (int i = 0; i < akeys.length; i++)
          {
            Action a = actions.get(akeys[i]);
            parentAction.put(akeys[i], a);
          }

        parent = jParent.getParent();
      }

    // notify ancestor listeners
    AncestorListener[] ls = getAncestorListeners();
    AncestorEvent ev = new AncestorEvent(this, AncestorEvent.ANCESTOR_ADDED,
                                         this, parent);
    for (int i = 0; i < ls.length; i++)
      {
        ls[i].ancestorAdded(ev);
      }

    // fire property change event for 'ancestor'
    firePropertyChange("ancestor", null, parent);
  }

  /**
   * Receives notification that this component no longer has a parent.
   *
   * This method sends an AncestorEvent to all registered AncestorListeners,
   * notifying them that the parent is gone.
   *
   * The keybord actions of this component are removed from the parent and
   * its ancestors.
   *
   * A PropertyChangeEvent is fired to indicate that the 'ancestor' property
   * has changed.
   *
   * This method is called before the component is actually removed from
   * its parent, so the parent is still visible through 
   * {@link Component#getParent}.
   */
  public void removeNotify()
  {
    super.removeNotify();

    // let parents inherit the keybord mapping
    InputMap input = getInputMap();
    ActionMap actions = getActionMap();

    Container parent = getParent();
    while ((parent != null) && (parent instanceof JComponent))
      {
        JComponent jParent = (JComponent) parent;
        InputMap parentInput = jParent.getInputMap();
        ActionMap parentAction = jParent.getActionMap();

        KeyStroke[] ikeys = input.allKeys();
        for (int i = 0; i < ikeys.length; i++)
          {
            parentInput.remove(ikeys[i]);
          }

        Object[] akeys = actions.allKeys();
        for (int i = 0; i < akeys.length; i++)
          {
            parentAction.remove(akeys[i]);
          }

        parent = jParent.getParent();
      }

    // notify ancestor listeners
    AncestorListener[] ls = getAncestorListeners();
    AncestorEvent ev = new AncestorEvent(this, AncestorEvent.ANCESTOR_ADDED,
                                         this, parent);
    for (int i = 0; i < ls.length; i++)
      {
        ls[i].ancestorAdded(ev);
      }

    // fire property change event for 'ancestor'
    firePropertyChange("ancestor", parent, null);
  }

  /**
   * Returns <code>true</code> if the coordinates (x, y) lie within
   * the bounds of this component and <code>false</code> otherwise.
   * x and y are relative to the coordinate space of the component.
   *
   * @param x the X coordinate of the point to check
   * @param y the Y coordinate of the point to check
   *
   * @return <code>true</code> if the specified point lies within the bounds
   *     of this component, <code>false</code> otherwise
   */
  public boolean contains(int x, int y)
  {
    if (ui == null)
      return super.contains(x, y);
    else
      return ui.contains(this, x, y);
  }

  /**
   * Disables this component.
   *
   * @deprecated replaced by {@link #setEnabled(boolean)}
   */
  public void disable()
  {
    super.disable();
  }

  /**
   * Enables this component.
   *
   * @deprecated replaced by {@link #setEnabled(boolean)}
   */
  public void enable()
  {
    super.enable();
  }

  /**
   * Returns the Graphics context for this component. This can be used
   * to draw on a component.
   *
   * @return the Graphics context for this component
   */
  public Graphics getGraphics()
  {
    return super.getGraphics();
  }

  /**
   * Returns the X coordinate of the upper left corner of this component.
   * Prefer this method over {@link #getBounds} or {@link #getLocation}
   * because it does not cause any heap allocation.
   *
   * @return the X coordinate of the upper left corner of the component
   */
  public int getX()
  {
    return super.getX();
  }

  /**
   * Returns the Y coordinate of the upper left corner of this component.
   * Prefer this method over {@link #getBounds} or {@link #getLocation}
   * because it does not cause any heap allocation.
   *
   * @return the Y coordinate of the upper left corner of the component
   */
  public int getY()
  {
    return super.getY();
  }

  /**
   * Returns the height of this component. Prefer this method over
   * {@link #getBounds} or {@link #getSize} because it does not cause
   * any heap allocation.
   *
   * @return the height of the component
   */
  public int getHeight()
  {
    return super.getHeight();
  }

  /**
   * Returns the width of this component. Prefer this method over
   * {@link #getBounds} or {@link #getSize} because it does not cause
   * any heap allocation.
   *
   * @return the width of the component
   */
  public int getWidth()
  {
    return super.getWidth();
  }

  /**
   * Return all <code>PropertyChangeListener</code> objects registered.
   *
   * @return The set of <code>PropertyChangeListener</code> objects
   */
  public PropertyChangeListener[] getPropertyChangeListeners()
  {
    if (changeSupport == null)
      return new PropertyChangeListener[0];
    else
      return changeSupport.getPropertyChangeListeners();
  }

  /**
   * Prints this component to the given Graphics context. A call to this
   * method results in calls to the methods {@link #printComponent},
   * {@link #printBorder} and {@link #printChildren} in this order.
   *
   * Double buffering is temporarily turned off so the painting goes directly
   * to the supplied Graphics context.
   *
   * @param g the Graphics context to print onto
   */
  public void print(Graphics g)
  {
    boolean doubleBufferState = isDoubleBuffered();
    setDoubleBuffered(false);
    printComponent(g);
    printBorder(g);
    printChildren(g);
    setDoubleBuffered(doubleBufferState);
  }

  /**
   * Prints this component to the given Graphics context. This invokes
   * {@link #print}.
   *
   * @param g the Graphics context to print onto
   */
  public void printAll(Graphics g)
  {
    print(g);
  }

  /**
   * Prints this component to the specified Graphics context. The default
   * behaviour is to invoke {@link #paintComponent}. Override this
   * if you want special behaviour for printing.
   *
   * @param g the Graphics context to print onto
   *
   * @since 1.3
   */
  public void printComponent(Graphics g)
  {
    paintComponent(g);
  }

  /**
   * Print this component's children to the specified Graphics context.
   * The default behaviour is to invoke {@link #paintChildren}. Override this
   * if you want special behaviour for printing.
   *
   * @param g the Graphics context to print onto
   *
   * @since 1.3
   */
  public void printChildren(Graphics g)
  {
    paintChildren(g);
  }

  /**
   * Print this component's border to the specified Graphics context.
   * The default behaviour is to invoke {@link #paintBorder}. Override this
   * if you want special behaviour for printing.
   *
   * @param g the Graphics context to print onto
   *
   * @since 1.3
   */
  public void printBorder(Graphics g)
  {
    paintBorder(g);
  }

  /**
   * Processes mouse motion event, like dragging and moving.
   *
   * @param ev the MouseEvent describing the mouse motion
   */
  protected void processMouseMotionEvent(MouseEvent ev)
  {
    super.processMouseMotionEvent(ev);
  }

  /**
   * Moves and resizes the component.
   *
   * @param x the new horizontal location
   * @param y the new vertial location
   * @param w the new width
   * @param h the new height
   */
  public void reshape(int x, int y, int w, int h)
  {
    super.reshape(x, y, w, h);
  }
}
