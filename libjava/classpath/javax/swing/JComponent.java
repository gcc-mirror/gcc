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
import java.awt.EventQueue;
import java.awt.FlowLayout;
import java.awt.FocusTraversalPolicy;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
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
import javax.swing.border.CompoundBorder;
import javax.swing.border.TitledBorder;
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
      protected AccessibleFocusHandler()
      {
        // TODO: Implement this properly.
      }
      public void focusGained(FocusEvent event)
      {
        // TODO: Implement this properly.
      }
      public void focusLost(FocusEvent valevent)
      {
        // TODO: Implement this properly.
      }
    }

    /**
     * Accessibility support for <code>JComponent</code>'s container handler.
     */
    protected class AccessibleContainerHandler 
      implements ContainerListener
    {
      protected AccessibleContainerHandler()
      {
        // TODO: Implement this properly.
      }
      public void componentAdded(ContainerEvent event)
      {
        // TODO: Implement this properly.
      }
      public void componentRemoved(ContainerEvent valevent)
      {
        // TODO: Implement this properly.
      }
    }

    private static final long serialVersionUID = -7047089700479897799L;
  
    protected ContainerListener accessibleContainerHandler;
    protected FocusListener accessibleFocusHandler;

    /**
     * Manages the property change listeners;
     */
    private SwingPropertyChangeSupport changeSupport;

    protected AccessibleJComponent()
    {
      changeSupport = new SwingPropertyChangeSupport(this);
    }

    /**
     * Adds a property change listener to the list of registered listeners.
     *
     * @param listener the listener to add
     */
    public void addPropertyChangeListener(PropertyChangeListener listener)
    {
      changeSupport.addPropertyChangeListener(listener);
    }

    /**
     * Removes a propery change listener from the list of registered listeners.
     *
     * @param listener the listener to remove
     */
    public void removePropertyChangeListener(PropertyChangeListener listener)
    {
      changeSupport.removePropertyChangeListener(listener);
    }

    /**
     * Returns the number of accessible children of this object.
     *
     * @return  the number of accessible children of this object
     */
    public int getAccessibleChildrenCount()
    {
      int count = 0;
      Component[] children = getComponents();
      for (int i = 0; i < children.length; ++i)
        {
          if (children[i] instanceof Accessible)
            count++;
        }
      return count;
    }

    /**
     * Returns the accessible child component at index <code>i</code>.
     *
     * @param i the index of the accessible child to return
     *
     * @return the accessible child component at index <code>i</code>
     */
    public Accessible getAccessibleChild(int i)
    {
      int index = 0;
      Component[] children = getComponents();
      Accessible found = null;
      for (int j = 0; index != i; j++)
        {
          if (children[j] instanceof Accessible)
            index++;
          if (index == i)
            found = (Accessible) children[index];
        }
      // TODO: Figure out what to do when i is not a valid index.
      return found;
    }

    /**
     * Returns the accessible state set of this component.
     *
     * @return the accessible state set of this component
     */
    public AccessibleStateSet getAccessibleStateSet()
    {
      // FIXME: Figure out which states should be set here, and which are
      // inherited from the super class.
      return super.getAccessibleStateSet();
    }

    /**
     * Returns the localized name for this object. Generally this should
     * almost never return {@link Component#getName()} since that is not
     * a localized name. If the object is some kind of text component (like
     * a menu item), then the value of the object may be returned. Also, if
     * the object has a tooltip, the value of the tooltip may also be
     * appropriate.
     *
     * @return the localized name for this object or <code>null</code> if this
     *         object has no name
     */
    public String getAccessibleName()
    {
      // TODO: Figure out what exactly to return here. It's possible that this
      // method simply should return null.
      return null;
    }

    /**
     * Returns the localized description of this object.
     *
     * @return the localized description of this object or <code>null</code>
     *         if this object has no description
     */
    public String getAccessibleDescription()
    {
      // TODO: Figure out what exactly to return here. It's possible that this
      // method simply should return null.
      return null;
    }

    /**
     * Returns the accessible role of this component.
     *
     * @return the accessible role of this component
     *
     * @see AccessibleRole
     */
    public AccessibleRole getAccessibleRole()
    {
      // TODO: Check if this is correct.
      return AccessibleRole.SWING_COMPONENT;
    }

    /**
     * Recursivly searches a border hierarchy (starting at <code>border) for
     * a titled border and returns the title if one is found, <code>null</code>
     * otherwise.
     *
     * @param border the border to start search from
     *
     * @return the border title of a possibly found titled border
     */
    protected String getBorderTitle(Border border)
    {
      String title = null;
      if (border instanceof CompoundBorder)
        {
          CompoundBorder compound = (CompoundBorder) border;
          Border inner = compound.getInsideBorder();
          title = getBorderTitle(inner);
          if (title == null)
            {
              Border outer = compound.getOutsideBorder();
              title = getBorderTitle(outer);
            }
        }
      else if (border instanceof TitledBorder)
        {
          TitledBorder titled = (TitledBorder) border;
          title = titled.getTitle(); 
        }
      return title;
    }

    /**
     * Returns the tooltip text for this accessible component.
     *
     * @return the tooltip text for this accessible component
     */
    public String getToolTipText()
    {
      return JComponent.this.getToolTipText();
    }

    /**
     * Returns the title of the border of this accessible component if
     * this component has a titled border, otherwise returns <code>null</code>.
     *
     * @return the title of the border of this accessible component if
     *         this component has a titled border, otherwise returns
     *         <code>null</code>
     */
    public String getTitledBorderText()
    {
      return getBorderTitle(getBorder()); 
    }

    /**
     * Returns the keybindings associated with this accessible component or
     * <code>null</code> if the component does not support key bindings.
     *
     * @return the keybindings associated with this accessible component
     */
    public AccessibleKeyBinding getAccessibleKeyBinding()
    {
      // TODO: Implement this properly.
      return null;
    }
  }

  /** 
   * An explicit value for the component's preferred size; if not set by a
   * user, this is calculated on the fly by delegating to the {@link
   * ComponentUI#getPreferredSize} method on the {@link #ui} property. 
   */
  Dimension preferredSize;

  /** 
   * An explicit value for the component's minimum size; if not set by a
   * user, this is calculated on the fly by delegating to the {@link
   * ComponentUI#getMinimumSize} method on the {@link #ui} property. 
   */
  Dimension minimumSize;

  /** 
   * An explicit value for the component's maximum size; if not set by a
   * user, this is calculated on the fly by delegating to the {@link
   * ComponentUI#getMaximumSize} method on the {@link #ui} property.
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
  float alignmentX = -1.0F;

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
  float alignmentY = -1.0F;

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
   * @see #getToolTipText()
   */
   String toolTipText;

  /** 
   * <p>Whether to double buffer this component when painting. This flag
   * should generally be <code>true</code>, to ensure good painting
   * performance.</p>
   *
   * <p>All children of a double buffered component are painted into the
   * double buffer automatically, so only the top widget in a window needs
   * to be double buffered.</p>
   *
   * @see #setDoubleBuffered
   * @see #isDoubleBuffered
   * @see #paint
   */
  boolean doubleBuffered = true;

  /**
   * A set of flags indicating which debugging graphics facilities should
   * be enabled on this component. The values should be a combination of
   * {@link DebugGraphics#NONE_OPTION}, {@link DebugGraphics#LOG_OPTION},
   * {@link DebugGraphics#FLASH_OPTION}, or {@link
   * DebugGraphics#BUFFERED_OPTION}.
   *
   * @see #setDebugGraphicsOptions
   * @see #getDebugGraphicsOptions
   * @see DebugGraphics
   * @see #getComponentGraphics
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
   * @see #requestFocus()
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
   * @see #setAutoscrolls
   * @see #getAutoscrolls
   */
  boolean autoscrolls = false;

  /**
   * Indicates whether the current paint call is already double buffered or
   * not. 
   */
  static boolean isPaintingDoubleBuffered = false;

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
  private ComponentInputMap inputMap_whenInFocusedWindow;
  private ActionMap actionMap;
  /** @since 1.3 */
  private boolean verifyInputWhenFocusTarget;
  private InputVerifier inputVerifier;

  private TransferHandler transferHandler;

  /**
   * Indicates if this component is currently painting a tile or not.
   */
  private boolean paintingTile;

  /**
   * A cached Rectangle object to be reused. Be careful when you use that,
   * so that it doesn't get modified in another context within the same
   * method call chain.
   */
  private static transient Rectangle rectCache;

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
   * Indicates if this component is completely dirty or not. This is used
   * by the RepaintManager's
   * {@link RepaintManager#isCompletelyDirty(JComponent)} method.
   */
  boolean isCompletelyDirty = false;

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
   * associated with <code>key</code>, it will be replaced.  A
   * {@link PropertyChangeEvent} is sent to registered listeners (with the
   * name of the property being <code>key.toString()</code>).
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
    Hashtable t = getClientProperties();
    Object old = t.get(key);
    if (value != null)
      t.put(key, value);
    else
      t.remove(key);
    firePropertyChange(key.toString(), old, value);
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

    PropertyChangeEvent evt = 
      new PropertyChangeEvent(this, propertyName, oldValue, newValue);

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
    float ret = alignmentX;
    if (alignmentX < 0)
      // alignment has not been set explicitly.
      ret = super.getAlignmentX();

    return ret;
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
    float ret = alignmentY;
    if (alignmentY < 0)
      // alignment has not been set explicitly.
      ret = super.getAlignmentY();

    return ret;
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
    Border oldBorder = getBorder();
    if (oldBorder == newBorder)
      return;

    border = newBorder;
    firePropertyChange("border", oldBorder, newBorder);
    repaint();
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
    Graphics g2 = g;
    int options = getDebugGraphicsOptions();
    if (options != DebugGraphics.NONE_OPTION)
      {
        if (!(g2 instanceof DebugGraphics))
          g2 = new DebugGraphics(g);
        DebugGraphics dg = (DebugGraphics) g2;
        dg.setDebugOptions(dg.getDebugOptions() | options);
      }
    g2.setFont(this.getFont());
    g2.setColor(this.getForeground());
    return g2;
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
    String option = System.getProperty("gnu.javax.swing.DebugGraphics");
    int options = debugGraphicsOptions;
    if (option != null && option.length() != 0)
      {
        if (options < 0)
          options = 0;

        if (option.equals("LOG"))
          options |= DebugGraphics.LOG_OPTION;
        else if (option.equals("FLASH"))
          options |= DebugGraphics.FLASH_OPTION;
      }
    return options;
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
    // TODO: Implement this properly.
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
   * Return <code>true</code> if this component is currently painting a tile,
   * this means that paint() is called again on another child component. This
   * method returns <code>false</code> if this component does not paint a tile
   * or if the last tile is currently painted.
   *
   * @return whether the component is painting a tile
   */
  public boolean isPaintingTile()
  {
    return paintingTile;
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
    RepaintManager rm = RepaintManager.currentManager(this);
    // We do a little stunt act here to switch on double buffering if it's
    // not already on. If we are not already doublebuffered, then we jump
    // into the method paintDoubleBuffered, which turns on the double buffer
    // and then calls paint(g) again. In the second call we go into the else
    // branch of this if statement and actually paint things to the double
    // buffer. When this method completes, the call stack unwinds back to
    // paintDoubleBuffered, where the buffer contents is finally drawn to the
    // screen.
    if (!isPaintingDoubleBuffered && isDoubleBuffered()
        && rm.isDoubleBufferingEnabled())
      paintDoubleBuffered(g);
    else
      {
        if (g.getClip() == null)
          g.setClip(0, 0, getWidth(), getHeight());
        Graphics g2 = getComponentGraphics(g);
        paintComponent(g2);
        paintBorder(g2);
        paintChildren(g2);
        Rectangle clip = g2.getClipBounds();
        if (clip.x == 0 && clip.y == 0 && clip.width == getWidth()
            && clip.height == getHeight())
          RepaintManager.currentManager(this).markCompletelyClean(this);
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
    Shape originalClip = g.getClip();
    Rectangle inner = SwingUtilities.calculateInnerArea(this, rectCache);
    g.clipRect(inner.x, inner.y, inner.width, inner.height);
    Component[] children = getComponents();

    // Find the bottommost component that needs to be painted. This is a
    // component that completely covers the current clip and is opaque. In
    // this case we don't need to paint the components below it.
    int startIndex = children.length - 1;
    // No need to check for overlapping components when this component is
    // optimizedDrawingEnabled (== it tiles its children).
    if (! isOptimizedDrawingEnabled())
      {
        Rectangle clip = g.getClipBounds();
        for (int i = 0; i < children.length; i++)
          {
            Rectangle childBounds = children[i].getBounds();
            if (children[i].isOpaque()
                && SwingUtilities.isRectangleContainingRectangle(childBounds,
                                                            g.getClipBounds()))
              {
                startIndex = i;
                break;
              }
          }
      }
    // paintingTile becomes true just before we start painting the component's
    // children.
    paintingTile = true;
    for (int i = startIndex; i >= 0; --i)
      {
        // paintingTile must be set to false before we begin to start painting
        // the last tile.
        if (i == 0)
          paintingTile = false;

        if (!children[i].isVisible())
          continue;

        Rectangle bounds = children[i].getBounds(rectCache);
        Rectangle oldClip = g.getClipBounds();
        if (oldClip == null)
          oldClip = bounds;

        if (!g.hitClip(bounds.x, bounds.y, bounds.width, bounds.height))
          continue;

        boolean translated = false;
        try
          {
            g.clipRect(bounds.x, bounds.y, bounds.width, bounds.height);
            g.translate(bounds.x, bounds.y);
            translated = true;
            children[i].paint(g);
          }
        finally
          {
            if (translated)
              g.translate(-bounds.x, -bounds.y);
            g.setClip(oldClip);
          }
      }
    g.setClip(originalClip);
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
        ui.update(g2, this);
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
   * @param r The dirty rectangle to paint
   */
  public void paintImmediately(Rectangle r)
  {
    // Try to find a root pane for this component.
    //Component root = findPaintRoot(r);
    Component root = findPaintRoot(r);
    // If no paint root is found, then this component is completely overlapped
    // by another component and we don't need repainting.
    if (root == null)
      return;
    if (root == null || !root.isShowing())
      return;

    Rectangle rootClip = SwingUtilities.convertRectangle(this, r, root);
    if (root instanceof JComponent)
      ((JComponent) root).paintImmediately2(rootClip);
    else
      root.repaint(rootClip.x, rootClip.y, rootClip.width, rootClip.height);
  }

  /**
   * Performs the actual work of paintImmediatly on the repaint root.
   *
   * @param r the area to be repainted
   */
  void paintImmediately2(Rectangle r)
  {
    RepaintManager rm = RepaintManager.currentManager(this);
    Graphics g = getGraphics();
    g.setClip(r.x, r.y, r.width, r.height);
    if (rm.isDoubleBufferingEnabled() && isDoubleBuffered())
      paintDoubleBuffered(g);
    else
      paintSimple(g);
    g.dispose();
  }

  /**
   * Performs double buffered repainting.
   *
   * @param g the graphics context to paint to
   */
  void paintDoubleBuffered(Graphics g)
  {
    
    Rectangle r = g.getClipBounds();
    if (r == null)
      r = new Rectangle(0, 0, getWidth(), getHeight());
    RepaintManager rm = RepaintManager.currentManager(this);

    // Paint on the offscreen buffer.
    Image buffer = rm.getOffscreenBuffer(this, getWidth(), getHeight());
    Graphics g2 = buffer.getGraphics();
    g2 = getComponentGraphics(g2);
    g2.setClip(r.x, r.y, r.width, r.height);
    isPaintingDoubleBuffered = true;
    paint(g2);
    isPaintingDoubleBuffered = false;
    g2.dispose();
    
    // Paint the buffer contents on screen.
    g.drawImage(buffer, 0, 0, this);
  }

  /**
   * Performs normal painting without double buffering.
   *
   * @param g the graphics context to use
   */
  void paintSimple(Graphics g)
  {
    Graphics g2 = getComponentGraphics(g);
    paint(g2);
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
        if (map != null && !(map instanceof ComponentInputMap))
            throw new 
              IllegalArgumentException("WHEN_IN_FOCUSED_WINDOW " + 
                                       "InputMap must be a ComponentInputMap");
        inputMap_whenInFocusedWindow = (ComponentInputMap)map;
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
          inputMap_whenInFocusedWindow = new ComponentInputMap(this);
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
    // This method does nothing, it is meant to be overridden by subclasses.
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
    
    KeyStroke keyStroke = KeyStroke.getKeyStrokeForEvent(e);
    boolean pressed = e.getID() == KeyEvent.KEY_PRESSED;
    
    if (processKeyBinding(keyStroke, e, WHEN_FOCUSED, pressed))
      {
        // This is step 1 from above comment.
        e.consume();
        return;
      }
    else if (processKeyBinding
             (keyStroke, e, WHEN_ANCESTOR_OF_FOCUSED_COMPONENT, pressed))
      {
        // This is step 2 from above comment.
        e.consume();
        return;
      }
    
    // This is step 3 from above comment.
    Container current = getParent();    
    while (current != null)
      { 
        // If current is a JComponent, see if it handles the event in its
        // WHEN_ANCESTOR_OF_FOCUSED_COMPONENT maps.
        if ((current instanceof JComponent) && 
            ((JComponent)current).processKeyBinding 
            (keyStroke, e,WHEN_ANCESTOR_OF_FOCUSED_COMPONENT, pressed))
          {
            e.consume();
            return;
          }     
        
        // Stop when we've tried a top-level container and it didn't handle it
        if (current instanceof Window || current instanceof Applet)
          break;        
        
        // Move up the hierarchy
        current = current.getParent();
      }
    
    // Current being null means the JComponent does not currently have a
    // top-level ancestor, in which case we don't need to check 
    // WHEN_IN_FOCUSED_WINDOW bindings.
    if (current == null || e.isConsumed())
      return;
    
    // This is step 4 from above comment.  KeyboardManager maintains mappings
    // related to WHEN_IN_FOCUSED_WINDOW bindings so that we don't have to 
    // traverse the containment hierarchy each time.
    if (KeyboardManager.getManager().processKeyStroke(current, keyStroke, e))
      e.consume();
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
    if (! EventQueue.isDispatchThread())
      SwingUtilities.invokeLater(new Runnable()
        {
          public void run()
          {
            revalidate();
          }
        });
    else
      {
        invalidate();
        RepaintManager.currentManager(this).addInvalidComponent(this);
      }
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
    if (a < 0.0F)
      alignmentX = 0.0F;
    else if (a > 1.0)
      alignmentX = 1.0F;
    else
      alignmentX = a;
  }

  /**
   * Set the value of the {@link #alignmentY} property.
   *
   * @param a The new value of the property
   */
  public void setAlignmentY(float a)
  {
    if (a < 0.0F)
      alignmentY = 0.0F;
    else if (a > 1.0)
      alignmentY = 1.0F;
    else
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
    if (enable == isEnabled())
      return;
    super.setEnabled(enable);
    firePropertyChange("enabled", !enable, enable);
    repaint();
  }

  /**
   * Set the value of the <code>font</code> property.
   *
   * @param f The new value of the property
   */
  public void setFont(Font f)
  {
    if (f == getFont())
      return;
    super.setFont(f);
    revalidate();
    repaint();
  }

  /**
   * Set the value of the <code>background</code> property.
   *
   * @param bg The new value of the property
   */
  public void setBackground(Color bg)
  {
    if (bg == getBackground())
      return;
    super.setBackground(bg);
    repaint();
  }

  /**
   * Set the value of the <code>foreground</code> property.
   *
   * @param fg The new value of the property
   */
  public void setForeground(Color fg)
  {
    if (fg == getForeground())
      return;
    super.setForeground(fg);
    repaint();
  }

  /**
   * Set the value of the {@link #maximumSize} property. The passed value is
   * copied, the later direct changes on the argument have no effect on the
   * property value.
   *
   * @param max The new value of the property
   */
  public void setMaximumSize(Dimension max)
  {
    Dimension oldMaximumSize = maximumSize;
    maximumSize = new Dimension(max);
    firePropertyChange("maximumSize", oldMaximumSize, maximumSize);
  }

  /**
   * Set the value of the {@link #minimumSize} property. The passed value is
   * copied, the later direct changes on the argument have no effect on the
   * property value.
   *
   * @param min The new value of the property
   */
  public void setMinimumSize(Dimension min)
  {
    Dimension oldMinimumSize = minimumSize;
    minimumSize = new Dimension(min);
    firePropertyChange("minimumSize", oldMinimumSize, minimumSize);
  }

  /**
   * Set the value of the {@link #preferredSize} property. The passed value is
   * copied, the later direct changes on the argument have no effect on the
   * property value.
   *
   * @param pref The new value of the property
   */
  public void setPreferredSize(Dimension pref)
  {
    Dimension oldPreferredSize = preferredSize;
    preferredSize = new Dimension(pref);
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
    // TODO: Implement this properly.
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
   * If the value is changed, then the AncestorListeners of this component
   * and all its children (recursivly) are notified.
   *
   * @param v The new value of the property
   */
  public void setVisible(boolean v)
  {
    // No need to do anything if the actual value doesn't change.
    if (isVisible() == v)
      return;

    super.setVisible(v);

    // Notify AncestorListeners.
    if (v == true)
      fireAncestorEvent(this, AncestorEvent.ANCESTOR_ADDED);
    else
      fireAncestorEvent(this, AncestorEvent.ANCESTOR_REMOVED);

    Container parent = getParent();
    if (parent != null)
      parent.repaint(getX(), getY(), getWidth(), getHeight());
    revalidate();
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
    revalidate();
    repaint();
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
    // Register the WHEN_IN_FOCUSED_WINDOW keyboard bindings
    // Note that here we unregister all bindings associated with
    // this component and then re-register them.  This may be more than
    // necessary if the top-level ancestor hasn't changed.  Should
    // maybe improve this.
    KeyboardManager km = KeyboardManager.getManager();
    km.clearBindingsForComp(this);
    km.registerEntireMap((ComponentInputMap)
                         this.getInputMap(WHEN_IN_FOCUSED_WINDOW));
    super.addNotify();

    // Notify AncestorListeners.
    fireAncestorEvent(this, AncestorEvent.ANCESTOR_ADDED);

    // fire property change event for 'ancestor'
    firePropertyChange("ancestor", null, getParent());
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

    // FIXME: remove the WHEN_IN_FOCUSED_WINDOW bindings from the 
    // KeyboardManager
    
    // Notify ancestor listeners.
    fireAncestorEvent(this, AncestorEvent.ANCESTOR_REMOVED);

    // fire property change event for 'ancestor'
    firePropertyChange("ancestor", getParent(), null);
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
    int oldX = getX();
    int oldY = getY();
    super.reshape(x, y, w, h);
    // Notify AncestorListeners.
    if (oldX != getX() || oldY != getY())
      fireAncestorEvent(this, AncestorEvent.ANCESTOR_MOVED);
  }

  /**
   * Fires an AncestorEvent to this component's and all of its child
   * component's AncestorListeners.
   *
   * @param ancestor the component that triggered the event
   * @param id the kind of ancestor event that should be fired
   */
  void fireAncestorEvent(JComponent ancestor, int id)
  {
    // Fire event for registered ancestor listeners of this component.
    AncestorListener[] listeners = getAncestorListeners();
    if (listeners.length > 0)
      {
        AncestorEvent ev = new AncestorEvent(this, id,
                                             ancestor, ancestor.getParent());
        for (int i = 0; i < listeners.length; i++)
          {
            switch (id)
              {
              case AncestorEvent.ANCESTOR_MOVED:
                listeners[i].ancestorMoved(ev);
                break;
              case AncestorEvent.ANCESTOR_ADDED:
                listeners[i].ancestorAdded(ev);
                break;
              case AncestorEvent.ANCESTOR_REMOVED:
                listeners[i].ancestorRemoved(ev);
                break;
              }
          }
      }
    // Dispatch event to all children.
    Component[] children = getComponents();
    for (int i = 0; i < children.length; i++)
      {
        if (!(children[i] instanceof JComponent))
          continue;
        JComponent jc = (JComponent) children[i];
        jc.fireAncestorEvent(ancestor, id);
      }
  }

  /**
   * Finds a suitable paint root for painting this component. This method first
   * checks if this component is overlapped using
   * {@link #findOverlapFreeParent(Rectangle)}. The returned paint root is then
   * feeded to {@link #findOpaqueParent(Component)} to find the nearest opaque
   * component for this paint root. If no paint is necessary, then we return
   * <code>null</code>.
   *
   * @param c the clip of this component
   *
   * @return the paint root or <code>null</code> if no painting is necessary
   */
  private Component findPaintRoot(Rectangle c)
  {
    Component p = findOverlapFreeParent(c);
    if (p == null)
      return null;
    Component root = findOpaqueParent(p);
    return root;
  }

  /**
   * Scans the containment hierarchy upwards for components that overlap the
   * this component in the specified clip. This method returns
   * <code>this</code>, if no component overlaps this component. It returns
   * <code>null</code> if another component completely covers this component
   * in the specified clip (no repaint necessary). If another component partly
   * overlaps this component in the specified clip, then the parent of this
   * component is returned (this is the component that must be used as repaint
   * root). For efficient lookup, the method
   * {@link #isOptimizedDrawingEnabled()} is used.
   *
   * @param clip the clip of this component
   *
   * @return the paint root, or <code>null</code> if no paint is necessary
   */
  private Component findOverlapFreeParent(Rectangle clip)
  {
    Rectangle currentClip = clip;
    Component found = this;
    Container parent = this; 
    while (parent != null && !(parent instanceof Window))
      {
        Container newParent = parent.getParent();
        if (newParent == null)
          break;
        // If the parent is optimizedDrawingEnabled, then its children are
        // tiled and cannot have an overlapping child. Go directly to next
        // parent.
        if (newParent instanceof JComponent
            && ((JComponent) newParent).isOptimizedDrawingEnabled())
          {
            parent = newParent;
            continue;
          }

        // First we must check if the new parent itself somehow clips the
        // target rectangle. This can happen in JViewports.
        Rectangle parRect = new Rectangle(0, 0, newParent.getWidth(),
                                          newParent.getHeight());
        Rectangle target = SwingUtilities.convertRectangle(found,
                                                           currentClip,
                                                           newParent);
        if (target.contains(parRect) || target.intersects(parRect))
          {
            found = newParent;
            currentClip = target;
            parent = newParent;
            continue;
          }

        // Otherwise we must check if one of the children of this parent
        // overlaps with the current component.
        Component[] children = newParent.getComponents();
        // This flag is used to skip components that are 'below' the component
        // in question.
        boolean skip = true;
        for (int i = children.length - 1; i >= 0; i--)
          {
            if (children[i] == parent)
              skip = false;
            if (skip)
              continue;
            Component c = children[i];
            Rectangle compBounds = c.getBounds();
            // If the component completely overlaps the clip in question, we
            // don't need to repaint. Return null.
            if (compBounds.contains(target))
              return null;
            if (compBounds.intersects(target))
              {
                // We found a parent whose children overlap with our current
                // component. Make this the current component.
                found = newParent;
                currentClip = target;
                break;
              }
          }
        parent = newParent;
      }
    return found;
  }

  /**
   * Finds the nearest component to <code>c</code> (upwards in the containment
   * hierarchy), that is opaque. If <code>c</code> itself is opaque,
   * this returns <code>c</code> itself.
   *
   * @param c the start component for the search
   * @return the nearest component to <code>c</code> (upwards in the containment
   *         hierarchy), that is opaque; If <code>c</code> itself is opaque,
   *         this returns <code>c</code> itself
   */
  private Component findOpaqueParent(Component c)
  {
    Component found = c;
    while (true)
      {
        if ((found instanceof JComponent) && ((JComponent) found).isOpaque())
          break;
        else if (!(found instanceof JComponent))
          break;
        Container p = found.getParent();
        if (p == null)
          break;
        else
          found = p;
      }
    return found;
  }
  
  /**
   * This is the method that gets called when the WHEN_IN_FOCUSED_WINDOW map
   * is changed.
   *
   * @param changed the JComponent associated with the WHEN_IN_FOCUSED_WINDOW
   *        map
   */
  void updateComponentInputMap(ComponentInputMap changed)
  {
    // Since you can change a component's input map via
    // setInputMap, we have to check if <code>changed</code>
    // is still in our WHEN_IN_FOCUSED_WINDOW map hierarchy
    InputMap curr = getInputMap(WHEN_IN_FOCUSED_WINDOW);
    while (curr != null && curr != changed)
      curr = curr.getParent();
    
    // If curr is null then changed is not in the hierarchy
    if (curr == null)
      return;
    
    // Now we have to update the keyboard manager's hashtable
    KeyboardManager km = KeyboardManager.getManager();
    
    // This is a poor strategy, should be improved.  We currently 
    // delete all the old bindings for the component and then register
    // the current bindings.
    km.clearBindingsForComp(changed.getComponent());
    km.registerEntireMap((ComponentInputMap) 
                         getInputMap(WHEN_IN_FOCUSED_WINDOW));
  }
}
