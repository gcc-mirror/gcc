/* JInternalFrame.java --
   Copyright (C) 2002, 2004, 2005, 2006,  Free Software Foundation, Inc.

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

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.Graphics;
import java.awt.IllegalComponentStateException;
import java.awt.KeyboardFocusManager;
import java.awt.LayoutManager;
import java.awt.Rectangle;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyVetoException;

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;
import javax.accessibility.AccessibleValue;
import javax.swing.event.InternalFrameEvent;
import javax.swing.event.InternalFrameListener;
import javax.swing.plaf.DesktopIconUI;
import javax.swing.plaf.InternalFrameUI;

/**
 * This class implements a Swing widget that looks and acts like a native
 * frame. The frame can be dragged, resized, closed, etc. Typically,
 * JInternalFrames are placed in JDesktopPanes. The actions that the
 * JInternalFrame performs (maximizing, minimizing, etc.) are performed by a
 * DesktopManager. As with regular frames, components are added by calling
 * frame.getContentPane().add.
 */
public class JInternalFrame extends JComponent implements Accessible,
                                                          WindowConstants,
                                                          RootPaneContainer
{

  private static final long serialVersionUID = -5425177187760785402L;

  /**
   * Provides the accessibility features for the <code>JInternalFrame</code>
   * component.
   */
  protected class AccessibleJInternalFrame extends AccessibleJComponent
    implements AccessibleValue
  {
    private static final long serialVersionUID = 5931936924175476797L;

    /**
     * Creates a new <code>AccessibleJInternalFrame</code> instance.
     */
    protected AccessibleJInternalFrame()
    {
      super();
    }

    /**
     * Returns the frame title.
     *
     * @return The frame title.
     */
    public String getAccessibleName()
    {
      return getTitle();
    }

    /**
     * Returns the accessible role for the <code>JInternalFrame</code>
     * component.
     *
     * @return {@link AccessibleRole#INTERNAL_FRAME}.
     */
    public AccessibleRole getAccessibleRole()
    {
      return AccessibleRole.INTERNAL_FRAME;
    }

    /**
     * Returns an object that provides access to the current, minimum and
     * maximum values for the {@link JInternalFrame}.  Since this class
     * implements {@link AccessibleValue}, it returns itself.
     *
     * @return The accessible value.
     */
    public AccessibleValue getAccessibleValue()
    {
      return this;
    }

    /**
     * Returns the current layer for the {@link JInternalFrame} component,
     * as an {@link Integer}.
     *
     * @return The layer for the {@link JInternalFrame} component.
     */
    public Number getCurrentAccessibleValue()
    {
      return new Integer(getLayer());
    }

    /**
     * Returns the maximum permitted accessible value.
     *
     * @return <code>Integer(Integer.MAX_VALUE)</code>.
     */
    public Number getMaximumAccessibleValue()
    {
      return new Integer(Integer.MAX_VALUE);
    }

    /**
     * Returns the minimum permitted accessible value.
     *
     * @return <code>Integer(Integer.MIN_VALUE)</code>.
     */
    public Number getMinimumAccessibleValue()
    {
      return new Integer(Integer.MIN_VALUE);
    }

    /**
     * Sets the layer for the internal frame.
     *
     * @param n  the layer (see the constants defined in {@link JLayeredPane}).
     *
     * @return <code>true</code> if the value is set, and <code>false</code>
     *         if it was not set.
     */
    public boolean setCurrentAccessibleValue(Number n)
    {
      if (n == null)
        return false;
      setLayer(n.intValue());
      return true;
    }
  }

  /**
   * This class represents the JInternalFrame while it is iconified.
   */
  public static class JDesktopIcon extends JComponent implements Accessible
  {
    /**
     * Provides the accessibility features for the <code>JDesktopIcon</code>
     * component.
     */
    protected class AccessibleJDesktopIcon extends AccessibleJComponent
      implements AccessibleValue
    {
      private static final long serialVersionUID = 5035560458941637802L;

      /**
       * Creates a new <code>AccessibleJDesktopIcon</code> instance.
       */
      protected AccessibleJDesktopIcon()
      {
        super();
      }

      /**
       * Returns the accessible role for the <code>JDesktopIcon</code>
       * component.
       *
       * @return {@link AccessibleRole#DESKTOP_ICON}.
       */
      public AccessibleRole getAccessibleRole()
      {
        return AccessibleRole.DESKTOP_ICON;
      }

      /**
       * Returns an object that provides access to the current, minimum and
       * maximum values for the {@link JDesktopIcon}.  Since this class
       * implements {@link AccessibleValue}, it returns itself.
       *
       * @return The accessible value.
       */
      public AccessibleValue getAccessibleValue()
      {
        return this;
      }

      /**
       * Returns the current layer for the {@link JInternalFrame} component
       * represented by this <code>JDesktopIcon</code>, as an {@link Integer}.
       *
       * @return The layer.
       */
      public Number getCurrentAccessibleValue()
      {
        return new Integer(frame.getLayer());
      }

      /**
       * Returns the maximum permitted accessible value.
       *
       * @return <code>Integer(Integer.MAX_VALUE)</code>.
       */
      public Number getMaximumAccessibleValue()
      {
        return new Integer(Integer.MAX_VALUE);
      }

      /**
       * Returns the minimum permitted accessible value.
       *
       * @return <code>Integer(Integer.MIN_VALUE)</code>.
       */
      public Number getMinimumAccessibleValue()
      {
        return new Integer(Integer.MIN_VALUE);
      }

      /**
       * Sets the layer for the internal frame represented by this
       * <code>JDesktopIcon</code> component.
       *
       * @param n  the layer (see the constants defined in
       *           {@link JLayeredPane}).
       *
       * @return <code>true</code> if the value is set, and <code>false</code>
       *         if it was not set.
       */
      public boolean setCurrentAccessibleValue(Number n)
      {
        if (n == null)
          return false;
        frame.setLayer(n.intValue());
        return true;
      }
    }

    private static final long serialVersionUID = 4672973344731387687L;

    /** The JInternalFrame this DesktopIcon represents. */
    JInternalFrame frame;

    /**
     * Creates a new JDesktopIcon object for representing the given frame.
     *
     * @param f The JInternalFrame to represent.
     */
    public JDesktopIcon(JInternalFrame f)
    {
      frame = f;
      updateUI();
    }

  /**
   * Returns the object that provides accessibility features for this
   * <code>JDesktopIcon</code> component.
   *
   * @return The accessible context (an instance of
   *         {@link AccessibleJDesktopIcon}).
   */
    public AccessibleContext getAccessibleContext()
    {
      if (accessibleContext == null)
        accessibleContext = new AccessibleJDesktopIcon();
      return accessibleContext;
    }

    /**
     * This method returns the JDesktopPane this JDesktopIcon is in.
     *
     * @return The JDesktopPane this JDesktopIcon is in.
     */
    public JDesktopPane getDesktopPane()
    {
      JDesktopPane p = (JDesktopPane) SwingUtilities.getAncestorOfClass(JDesktopPane.class,
                                                                        this);
      return p;
    }

    /**
     * This method returns the JInternalFrame this JDesktopIcon represents.
     *
     * @return The JInternalFrame this JDesktopIcon represents.
     */
    public JInternalFrame getInternalFrame()
    {
      return frame;
    }

    /**
     * This method returns the UI that is responsible for the JDesktopIcon.
     *
     * @return The UI that is responsible for the JDesktopIcon.
     */
    public DesktopIconUI getUI()
    {
      return (DesktopIconUI) ui;
    }

    /**
     * This method returns the String identifier that is used to determine
     * which class is used for JDesktopIcon's UI.
     *
     * @return A String identifier for the UI class.
     */
    public String getUIClassID()
    {
      return "DesktopIconUI";
    }

    /**
     * This method sets the JInternalFrame that this JDesktopIcon represents.
     *
     * @param f The JInternalFrame that this JDesktopIcon represents.
     */
    public void setInternalFrame(JInternalFrame f)
    {
      frame = f;
    }

    /**
     * This method sets the UI used for this JDesktopIcon.
     *
     * @param ui The UI to use.
     */
    public void setUI(DesktopIconUI ui)
    {
      super.setUI(ui);
    }

    /**
     * This method restores the UI property to the defaults.
     */
    public void updateUI()
    {
      setUI((DesktopIconUI) UIManager.getUI(this));
    }
  }

  /**
   * The property fired in a PropertyChangeEvent when the contentPane property
   * changes.
   */
  public static final String CONTENT_PANE_PROPERTY = "contentPane";

  /**
   * The property fired in a PropertyChangeEvent when the frameIcon property
   * changes.
   */
  public static final String FRAME_ICON_PROPERTY = "frameIcon";

  /**
   * The property fired in a PropertyChangeEvent when the glassPane property
   * changes.
   */
  public static final String GLASS_PANE_PROPERTY = "glassPane";

  /**
   * The property fired in a PropertyChangeEvent when the closed property
   * changes.
   */
  public static final String IS_CLOSED_PROPERTY = "closed";

  /**
   * The property fired in a PropertyChangeEvent when the icon property
   * changes.
   */
  public static final String IS_ICON_PROPERTY = "icon";

  /**
   * The property fired in a PropertyChangeEvent when the maximum property
   * changes.
   */
  public static final String IS_MAXIMUM_PROPERTY = "maximum";

  /**
   * The property fired in a PropertyChangeEvent when the selected property
   * changes.
   */
  public static final String IS_SELECTED_PROPERTY = "selected";

  /**
   * The property fired in a PropertyChangeEvent when the layeredPane property
   * changes.
   */
  public static final String LAYERED_PANE_PROPERTY = "layeredPane";

  /**
   * The property fired in a PropertyChangeEvent when the jMenuBar property
   * changes.
   */
  public static final String MENU_BAR_PROPERTY = "JMenuBar";

  /**
   * The property fired in a PropertyChangeEvent when the rootPane property
   * changes.
   */
  public static final String ROOT_PANE_PROPERTY = "rootPane";

  /**
   * The property fired in a PropertyChangeEvent when the title property
   * changes.
   */
  public static final String TITLE_PROPERTY = "title";

  /** Whether the JInternalFrame is closable. */
  protected boolean closable;

  /** Whether the JInternalFrame can be iconified. */
  protected boolean iconable;

  /** Whether the JInternalFrame is closed. */
  protected boolean isClosed;

  /** Whether the JInternalFrame has been iconified. */
  protected boolean isIcon;

  /** Whether the JInternalFrame has been maximized. */
  protected boolean isMaximum;

  /** Whether the JInternalFrame is the active frame. */
  protected boolean isSelected;

  /** Whether the JInternalFrame can be maximized. */
  protected boolean maximizable;

  /**
   * Whether the JInternalFrame has rootPaneChecking enabled.
   *
   * @specnote Should be false to comply with J2SE 5.0
   */
  protected boolean rootPaneCheckingEnabled = false;

  /** Whether the JInternalFrame is resizable. */
  protected boolean resizable;

  /**
   * The JDesktopIcon that represents the JInternalFrame while it is
   * iconified.
   */
  protected JDesktopIcon desktopIcon;

  /** The icon used in the JMenuBar in the TitlePane. */
  protected Icon frameIcon;

  /** The rootPane of the JInternalFrame. */
  protected JRootPane rootPane;

  /** The title on the TitlePane of the JInternalFrame. */
  protected String title;

  /** The bounds of the JInternalFrame before it was maximized. */
  private transient Rectangle storedBounds;

  /** The Component that receives focus by default. */
  private transient Component defaultFocus;

  /** The default close action taken, */
  private transient int defaultCloseOperation = DISPOSE_ON_CLOSE;

  /** Whether the JInternalFrame has become visible for the very first time. */
  private transient boolean isFirstTimeVisible = true;

  /** DOCUMENT ME! */
  private transient boolean wasIcon = false;

  /**
   * Creates a new JInternalFrame object that has an empty string for its
   * title, and is non-resizable, non-maximizable, non-iconifiable, and
   * non-closable.
   */
  public JInternalFrame()
  {
    this("", false, false, false, false);
  }

  /**
   * Creates a new JInternalFrame object with the given title and is
   * non-resizable, non-maximizable, non-iconifiable, and non-closable.
   *
   * @param title The title displayed in the JInternalFrame.
   */
  public JInternalFrame(String title)
  {
    this(title, false, false, false, false);
  }

  /**
   * Creates a new JInternalFrame object with the given title and resizable
   * properties. The JInternalFrame is non-maximizable, non-iconifiable, and
   * non-closable.
   *
   * @param title The title displayed in the JInternalFrame.
   * @param resizable Whether the JInternalFrame is resizable.
   */
  public JInternalFrame(String title, boolean resizable)
  {
    this(title, resizable, false, false, false);
  }

  /**
   * Creates a new JInternalFrame object with the given title, resizable, and
   * closable properties. The JInternalFrame is non-maximizable and
   * non-iconifiable.
   *
   * @param title The title displayed in the JInternalFrame.
   * @param resizable Whether the JInternalFrame is resizable.
   * @param closable Whether the JInternalFrame is closable.
   */
  public JInternalFrame(String title, boolean resizable, boolean closable)
  {
    this(title, resizable, closable, false, false);
  }

  /**
   * Creates a new JInternalFrame object with the given title, resizable,
   * closable and maximizable properties. The JInternalFrame is
   * non-iconifiable.
   *
   * @param title The title displayed in the JInternalFrame.
   * @param resizable Whether the JInternalFrame is resizable.
   * @param closable Whether the JInternalFrame is closable.
   * @param maximizable Whether the JInternalFrame is maximizable.
   */
  public JInternalFrame(String title, boolean resizable, boolean closable,
                        boolean maximizable)
  {
    this(title, resizable, closable, maximizable, false);
  }

  /**
   * Creates a new JInternalFrame object with the given title, resizable,
   * closable, maximizable and iconifiable properties.
   *
   * @param title The title displayed in the JInternalFrame.
   * @param resizable Whether the JInternalFrame is resizable.
   * @param closable Whether the JInternalFrame is closable.
   * @param maximizable Whether the JInternalFrame is maximizable.
   * @param iconifiable Whether the JInternalFrame is iconifiable.
   */
  public JInternalFrame(String title, boolean resizable, boolean closable,
                        boolean maximizable, boolean iconifiable)
  {
    this.title = title;
    this.resizable = resizable;
    this.closable = closable;
    this.maximizable = maximizable;
    this.iconable = iconifiable;
    isMaximum = false;
    setRootPane(createRootPane());
    // JInternalFrames are invisible and opaque by default.
    setVisible(false);
    setOpaque(true);
    desktopIcon = new JDesktopIcon(this);
    updateUI();
    setRootPaneCheckingEnabled(true); // Done the init stage, now adds go to content pane.
  }

  /**
   * This method adds Components to this Container. For JInternalFrames,
   * instead of calling add directly on the JInternalFrame, it should be
   * called with JInternalFrame.getContentPane().add. If root pane checking
   * is enabled, calling this method will cause an exception to be thrown.
   *
   * @param comp The Component to add.
   * @param constraints The constraints on the Component added.
   * @param index The position to place the Component.
   *
   * @throws Error DOCUMENT ME!
   */
  protected void addImpl(Component comp, Object constraints, int index)
  {
    // If we're in the initialization stage use super.add. Here we add the
    // rootPane as well as the title bar and other stuff.
    // Otherwise pass the add onto the content pane.
    if (isRootPaneCheckingEnabled())
      getContentPane().add(comp, constraints, index);
    else
      super.addImpl(comp,constraints, index);
  }

  /**
   * This method adds an InternalFrameListener to this JInternalFrame.
   *
   * @param l The listener to add.
   */
  public void addInternalFrameListener(InternalFrameListener l)
  {
    listenerList.add(InternalFrameListener.class, l);
  }

  /**
   * This method is used to create a root pane for the JInternalFrame. This
   * method is called by the constructors.
   *
   * @return A root pane for the JInternalFrame to use.
   */
  protected JRootPane createRootPane()
  {
    return new JRootPane();
  }

  /**
   * This method makes this JInternalFrame invisible, unselected and closed.
   * If this JInternalFrame is not closed already, it will fire an
   * INTERNAL_FRAME_CLoSED event. This method is similar to setClosed but it
   * doesn't give vetoable listeners a chance to veto and it will not fire an
   * INTERNAL_FRAME_CLOSING event.
   */
  public void dispose()
  {
    if (isVisible())
      setVisible(false);
    if (isSelected())
      {
        try
          {
            setSelected(false);
          }
        catch (PropertyVetoException e)
          {
            // Do nothing if they don't want to be unselected.
          }
      }
    if (! isClosed)
      {
        firePropertyChange(IS_CLOSED_PROPERTY, Boolean.FALSE, Boolean.TRUE);
        isClosed = true;
      }
    fireInternalFrameEvent(InternalFrameEvent.INTERNAL_FRAME_CLOSED);
  }

  /**
   * This method is used for closing this JInternalFrame. It fires an
   * INTERNAL_FRAME_CLOSING event and then performs the action specified by
   * the default close operation.
   */
  public void doDefaultCloseAction()
  {
    fireInternalFrameEvent(InternalFrameEvent.INTERNAL_FRAME_CLOSING);
    switch (getDefaultCloseOperation())
      {
      case HIDE_ON_CLOSE:
            setVisible(false);
            break;
      case DISPOSE_ON_CLOSE:
            dispose();
            break;
      }
  }

  /**
   * This method fires an InternalFrameEvent to the listeners.
   *
   * @param id The type of event being fired. See InternalFrameEvent.
   */
  protected void fireInternalFrameEvent(int id)
  {
    Object[] ifListeners = listenerList.getListenerList();
    InternalFrameEvent evt = new InternalFrameEvent(this, id);
    switch (id)
      {
      case InternalFrameEvent.INTERNAL_FRAME_CLOSING:
        for (int i = ifListeners.length - 2; i >= 0; i -= 2)
          {
            if (ifListeners[i] == InternalFrameListener.class)
              ((InternalFrameListener) ifListeners[i + 1])
              .internalFrameClosing(evt);
          }
        break;
      case InternalFrameEvent.INTERNAL_FRAME_ACTIVATED:
        for (int i = ifListeners.length - 2; i >= 0; i -= 2)
          {
            if (ifListeners[i] == InternalFrameListener.class)
              ((InternalFrameListener) ifListeners[i + 1])
              .internalFrameActivated(evt);
          }
        break;
      case InternalFrameEvent.INTERNAL_FRAME_CLOSED:
        for (int i = ifListeners.length - 2; i >= 0; i -= 2)
          {
            if (ifListeners[i] == InternalFrameListener.class)
              ((InternalFrameListener) ifListeners[i + 1]).internalFrameClosed(evt);
          }
        break;
      case InternalFrameEvent.INTERNAL_FRAME_DEACTIVATED:
        for (int i = ifListeners.length - 2; i >= 0; i -= 2)
          {
            if (ifListeners[i] == InternalFrameListener.class)
              ((InternalFrameListener) ifListeners[i + 1])
              .internalFrameDeactivated(evt);
          }
        break;
      case InternalFrameEvent.INTERNAL_FRAME_DEICONIFIED:
        for (int i = ifListeners.length - 2; i >= 0; i -= 2)
          {
            if (ifListeners[i] == InternalFrameListener.class)
              ((InternalFrameListener) ifListeners[i + 1])
              .internalFrameDeiconified(evt);
          }
        break;
      case InternalFrameEvent.INTERNAL_FRAME_ICONIFIED:
        for (int i = ifListeners.length - 2; i >= 0; i -= 2)
          {
            if (ifListeners[i] == InternalFrameListener.class)
              ((InternalFrameListener) ifListeners[i + 1])
              .internalFrameIconified(evt);
          }
        break;
      case InternalFrameEvent.INTERNAL_FRAME_OPENED:
        for (int i = ifListeners.length - 2; i >= 0; i -= 2)
          {
            if (ifListeners[i] == InternalFrameListener.class)
              ((InternalFrameListener) ifListeners[i + 1]).internalFrameOpened(evt);
          }
        break;
      }
  }

  /**
   * Returns the object that provides accessibility features for this
   * <code>JInternalFrame</code> component.
   *
   * @return The accessible context (an instance of
   *     {@link AccessibleJInternalFrame}).
   */
  public AccessibleContext getAccessibleContext()
  {
    if (accessibleContext == null)
      accessibleContext = new AccessibleJInternalFrame();
    return accessibleContext;
  }

  /**
   * This method returns the Content Pane for this JInternalFrame.
   *
   * @return The Content Pane for this JInternalFrame.
   */
  public Container getContentPane()
  {
    return getRootPane().getContentPane();
  }

  /**
   * Returns a code for the default action taken when this
   * <code>JInternalFrame</code> is closed.
   *
   * @return The action code (usually one of
   *     {@link WindowConstants#DO_NOTHING_ON_CLOSE},
   *     {@link WindowConstants#HIDE_ON_CLOSE}, or
   *     {@link WindowConstants#DISPOSE_ON_CLOSE}).
   *
   * @see #setDefaultCloseOperation(int)
   * @see #doDefaultCloseAction()
   */
  public int getDefaultCloseOperation()
  {
    return defaultCloseOperation;
  }

  /**
   * Returns the <code>JDesktopIcon</code> that represents this
   * <code>JInternalFrame</code> while it is iconified.
   *
   * @return The desktop icon component.
   */
  public JDesktopIcon getDesktopIcon()
  {
    return desktopIcon;
  }

  /**
   * This method searches this JInternalFrame ancestors for an instance of
   * JDesktopPane. If one is found, it is returned. If none is found, then it
   * will search the JDesktopIcon for a JDesktopPane.
   *
   * @return The JDesktopPane that this JInternalFrame belongs to.
   */
  public JDesktopPane getDesktopPane()
  {
    JDesktopPane value = (JDesktopPane) SwingUtilities.getAncestorOfClass(JDesktopPane.class,
                                                                          this);
    if (value == null && desktopIcon != null)
      value = desktopIcon.getDesktopPane();
    return value;
  }

  /**
   * This method returns null because this must always be the root of a focus
   * traversal.
   *
   * @return always null
   *
   * @since 1.4
   */
  public final Container getFocusCycleRootAncestor()
  {
    // as defined.
    return null;
  }

  /**
   * This method returns the child Component that will receive focus if this
   * JInternalFrame is selected.
   *
   * @return The child Component that will receive focus.
   */
  public Component getFocusOwner()
  {
    if (isSelected())
      {
        Component focus = KeyboardFocusManager.getCurrentKeyboardFocusManager().getFocusOwner();
        if (SwingUtilities.isDescendingFrom(focus, this))
          {
            defaultFocus = focus;
            return focus;
          }
      }
    return null;
  }

  /**
   * This method returns the Frame Icon (the icon used in the JInternalFrame
   * TitlePane and iconified frame).
   *
   * @return The Frame Icon.
   */
  public Icon getFrameIcon()
  {
    return frameIcon;
  }

  /**
   * This method returns the Glass Pane used with this JInternalFrame.
   *
   * @return The Glass Pane used with this JInternalFrame.
   */
  public Component getGlassPane()
  {
    return getRootPane().getGlassPane();
  }

  /**
   * This method returns an array of InternalFrameListeners that are listening
   * to this JInternalFrame.
   *
   * @return An array of InternalFrameListeners that are listening to this
   *         JInternalFrame.
   */
  public InternalFrameListener[] getInternalFrameListeners()
  {
    return (InternalFrameListener[]) listenerList.getListeners(InternalFrameListener.class);
  }

  /**
   * This method returns the JMenuBar for this JInternalFrame.
   *
   * @return The JMenuBar for this JInternalFrame.
   */
  public JMenuBar getJMenuBar()
  {
    return getRootPane().getJMenuBar();
  }

  /**
   * This method returns the layer that this JInternalFrame resides in.
   *
   * @return The layer that this JInternalFrame resides in.
   */
  public int getLayer()
  {
    return JLayeredPane.getLayer(this);
  }

  /**
   * This method returns the LayeredPane for this JInternalFrame.
   *
   * @return The LayeredPane for this JInternalFrame.
   */
  public JLayeredPane getLayeredPane()
  {
    return getRootPane().getLayeredPane();
  }

  /**
   * This method is deprecated. This method returns the JMenuBar for this
   * JInternalFrame.
   *
   * @return The JMenuBar for this JInternalFrame.
   *
   * @deprecated 1.0.3
   */
  public JMenuBar getMenuBar()
  {
    return getJMenuBar();
  }

  /**
   * This method returns the child Component that will receive focus when the
   * JInternalFrame is selected. If the JInternalFrame is selected, this
   * method returns getFocusOwner(). Otherwise, it will return the child
   * Component that most recently requested focus. If that is null, then the
   * initial focus Component is returned. If that is null, then the default
   * focus component is returned.
   *
   * @return The most recent focus owner.
   */
  public Component getMostRecentFocusOwner()
  {
    if (isSelected())
      return getFocusOwner();
    else
      return defaultFocus;
  }

  /**
   * This method returns the bounds of the JInternalFrame if it is not
   * maximized. If it is maximized, it returns the bounds of the
   * JInternalFrame before it was maximized (the bounds that it will be
   * restored to).
   *
   * @return A Rectangle that contains this JInternalFrame's normal bounds (or
   *         just its bounds if it is not maximized).
   */
  public Rectangle getNormalBounds()
  {
    if (storedBounds == null)
      return getBounds();
    else
      return storedBounds;
  }

  /**
   * This method returns the Root Pane for this JInternalFrame.
   *
   * @return The Root Pane for this JInternalFrame.
   */
  public JRootPane getRootPane()
  {
    return rootPane;
  }

  /**
   * Returns the frame's title.
   *
   * @return The frame's title (can be <code>null</code>).
   *
   * @see #setTitle(String)
   */
  public String getTitle()
  {
    return title;
  }

  /**
   * This method returns the UI used to represent the JInternalFrame.
   *
   * @return The UI used to represent the JInternalFrame.
   */
  public InternalFrameUI getUI()
  {
    return (InternalFrameUI) ui;
  }

  /**
   * This method returns a String identifier that is used to determine which
   * class acts as the JInternalFrame's UI.
   *
   * @return A String identifier to determine a UI class.
   */
  public String getUIClassID()
  {
    return "InternalFrameUI";
  }

  /**
   * This method returns null.
   *
   * @return null.
   */
  public final String getWarningString()
  {
    // as defined.
    return null;
  }

  /**
   * This method deselects this JInternalFrame and hides it.
   */
  public void hide()
  {
    if (isIcon())
      getDesktopIcon().hide();
    super.hide();
  }

  /**
   * This method returns whether this JInternalFrame is closable.
   *
   * @return Whether this JInternalFrame is closable.
   */
  public boolean isClosable()
  {
    return closable;
  }

  /**
   * This method returns whether this JInternalFrame has been closed.
   *
   * @return Whether this JInternalFrame is closed.
   */
  public boolean isClosed()
  {
    return isClosed;
  }

  /**
   * This must always return true.
   *
   * @return always true
   *
   * @since 1.4
   */
  public final boolean isFocusCycleRoot()
  {
    return true;
  }

  /**
   * This method returns whether this JInternalFrame is currently iconified.
   *
   * @return Whether this JInternalFrame is currently iconified.
   */
  public boolean isIcon()
  {
    return isIcon;
  }

  /**
   * This method returns whether the JInternalFrame can be iconified.
   *
   * @return Whether the JInternalFrame can be iconified.
   */
  public boolean isIconifiable()
  {
    return iconable;
  }

  /**
   * This method returns whether this JInternalFrame can be maximized.
   *
   * @return Whether this JInternalFrame can be maximized.
   */
  public boolean isMaximizable()
  {
    return maximizable;
  }

  /**
   * This method returns whether this JInternalFrame is currently maximized.
   *
   * @return Whether this JInternalFrame is maximized.
   */
  public boolean isMaximum()
  {
    return isMaximum;
  }

  /**
   * This method returns whether this JInternalFrame is resizable.
   *
   * @return Whether this JInternalFrame is resizable.
   */
  public boolean isResizable()
  {
    return resizable;
  }

  /**
   * This method returns whether root pane checking is enabled. If root pane
   * checking is enabled, then calls to addImpl and setLayout will throw
   * exceptions.
   *
   * @return Whether root pane checking is enabled.
   */
  protected boolean isRootPaneCheckingEnabled()
  {
    return rootPaneCheckingEnabled;
  }

  /**
   * This method returns whether this JInternalFrame is selected.
   *
   * @return Whether this JInternalFrame is selected.
   */
  public boolean isSelected()
  {
    return isSelected;
  }

  /**
   * A helper method that moves this JInternalFrame to the back if the parent
   * is a JLayeredPane.
   */
  public void moveToBack()
  {
    Container p = getParent();
    if (p instanceof JLayeredPane)
      ((JLayeredPane) p).moveToBack(this);
  }

  /**
   * A helper method that moves this JInternalFrame to the front if the parent
   * is a JLayeredPane.
   */
  public void moveToFront()
  {
    Container p = getParent();
    if (p != null && p instanceof JLayeredPane)
      ((JLayeredPane) p).moveToFront(this);
  }

  /**
   * This method causes the children of this JInternalFrame to be laid out.
   * Before it begins, if this JInternalFrame is an icon, then it will be
   * deiconified. If it is maximized, then it will be restored. If either
   * operation fails, then this method will return.
   */
  public void pack()
  {
    try
      {
        if (isIcon())
          setIcon(false);
        else if (isMaximum())
          setMaximum(false);
      }
    catch (PropertyVetoException e)
      {
        // Do nothing if they don't want to be restored first.
      }
    setSize(getPreferredSize());
    validate();
  }

  /**
   * This method is overridden to allow for speedier painting while this
   * JInternalFramme is being dragged.
   *
   * @param g The Graphics object to paint with.
   */
  protected void paintComponent(Graphics g)
  {
    super.paintComponent(g);
  }

  /**
   * An implementation dependent string describing the current state of this
   * <code>JInternalFrame</code> instance.
   *
   * @return A string describing the current state of this
   *     <code>JInternalFrame</code> instance.
   */
  protected String paramString()
  {
    return super.paramString() + ",title=" + getTitle();
  }

  /**
   * This method removes the given Component from the Container.
   *
   * @param comp The Component to remove.
   */
  public void remove(Component comp)
  {
    // If we're removing the root pane, use super.remove.  Otherwise
    // pass it on to the content pane instead.
    if (comp==rootPane || ! isRootPaneCheckingEnabled())
      super.remove(comp);
    else
      getContentPane().remove(comp);
  }

  /**
   * This method removes an InternalFrameListener from this JInternalFrame.
   *
   * @param l The listener to remove.
   */
  public void removeInternalFrameListener(InternalFrameListener l)
  {
    listenerList.remove(InternalFrameListener.class, l);
  }

  /**
   * This method resizes and positions this JInternalFrame. It also forces a
   * relayout of the Container.
   *
   * @param x The x position of this JInternalFrame.
   * @param y The y position of this JInternalFrame.
   * @param width The width of this JInternalFrame.
   * @param height The height of this JInternalFrame.
   */
  public void reshape(int x, int y, int width, int height)
  {
    super.reshape(x, y, width, height);
    revalidate();
  }

  /**
   * This method gives focus to the last child Component that had focus. This
   * is used by the UI when this JInternalFrame is activated.
   */
  public void restoreSubcomponentFocus()
  {
    Component c = getMostRecentFocusOwner();
    if (c != null)
      c.requestFocus();
  }

  /**
   * This method sets whether this JInternalFrame can be closed.
   *
   * @param b Whether this JInternalFrame can be closed.
   */
  public void setClosable(boolean b)
  {
    if (closable != b)
      {
        closable = b;
        firePropertyChange("closable", ! closable, closable);
      }
  }

  /**
   * This method closes the JInternalFrame if the given boolean is true. If it
   * is false, then the result of this method is unspecified. If the
   * JInternalFrame is closed, this method does nothing. This method will
   * first fire an INTERNAL_FRAME_CLOSING event and give a chance for veto
   * listeners to cancel the close. If no listener vetoes the change, the
   * closed property is set to true and the JInternalFrame is hidden and
   * unselected. The method will finish by firing an INTERNAL_FRAME_CLOSED
   * event.
   *
   * @param b Whether the JInternalFrame will be closed.
   *
   * @throws PropertyVetoException If a VetoableChangeListener vetoes the change.
   */
  public void setClosed(boolean b) throws PropertyVetoException
  {
    if (b && ! isClosed())
      {
        fireInternalFrameEvent(InternalFrameEvent.INTERNAL_FRAME_CLOSING);
        fireVetoableChange(IS_CLOSED_PROPERTY, false, true);

        isClosed = b;
        dispose();

        firePropertyChange(IS_CLOSED_PROPERTY, false, true);
      }
  }

  /**
   * This method sets the Container to be used as a Content Pane for this
   * JInternalFrame.
   *
   * @param c The Container to use as a Content Pane.
   */
  public void setContentPane(Container c)
  {
    if (c != getContentPane())
      {
        Container old = getContentPane();
        getRootPane().setContentPane(c);
        firePropertyChange(CONTENT_PANE_PROPERTY, old, c);
      }
  }

  /**
   * Sets a code for the action to be taken when this
   * <code>JInternalFrame</code> is closed.  Note that no validation is
   * performed on the <code>operation</code> code, any integer will be
   * accepted (nevertheless, you should pass in one of the listed values).
   *
   * @param operation  one of {@link WindowConstants#DO_NOTHING_ON_CLOSE},
   *   {@link WindowConstants#HIDE_ON_CLOSE} or
   *   {@link WindowConstants#DISPOSE_ON_CLOSE}.
   *
   * @see #getDefaultCloseOperation()
   * @see #doDefaultCloseAction()
   */
  public void setDefaultCloseOperation(int operation)
  {
    /* Reference implementation allows invalid operations to be specified.
       In that case, behaviour defaults to DO_NOTHING_ON_CLOSE.
       processWindowEvent handles the behaviour. getDefaultCloseOperation
       must return the invalid operator code. */
    defaultCloseOperation = operation;
  }

  /**
   * Sets the <code>JDesktopIcon</code> instance that represents this
   * <code>JInternalFrame</code> while it is iconified and, if the new icon is
   * not the same instance as the existing icon, sends a
   * {@link PropertyChangeEvent} (with the property name
   * <code>"desktopIcon"</code>) to all registered listeners..
   *
   * @param d  the icon.
   *
   * @see #getDesktopIcon()
   */
  public void setDesktopIcon(JDesktopIcon d)
  {
    if (desktopIcon != d)
      {
        JDesktopIcon oldIcon = desktopIcon;
        desktopIcon = d;
        firePropertyChange("desktopIcon", oldIcon, d);
      }
  }

  /**
   * This method does nothing because this must be the root of a focus
   * traversal cycle.
   *
   * @param focusCycleRoot Not used.
   */
  public final void setFocusCycleRoot(boolean focusCycleRoot)
  {
    // Do nothing
  }

  /**
   * This method sets the Icon to be used in two places. The first is icon
   * that is painted at the top left corner of the JInternalFrame when it is
   * not iconified (clicking on that icon will activate the TitlePane
   * JMenuBar). When the JInternalFrame is iconified, it will be the icon
   * displayed in the JDesktopIcon. If no icon is set, the JInternalFrame
   * will use a Look and Feel default.
   *
   * @param icon The Icon used in the TitlePane JMenuBar and iconified frames.
   */
  public void setFrameIcon(Icon icon)
  {
    if (icon != frameIcon)
      {
        Icon old = frameIcon;
        frameIcon = icon;
        firePropertyChange(FRAME_ICON_PROPERTY, old, frameIcon);
      }
  }

  /**
   * This method sets the Glass Pane used with this JInternalFrame.
   *
   * @param glass The Glass Pane to use with this JInternalFrame.
   */
  public void setGlassPane(Component glass)
  {
    if (glass != getGlassPane())
      {
        Component old = getGlassPane();
        getRootPane().setGlassPane(glass);
        firePropertyChange(GLASS_PANE_PROPERTY, old, glass);
      }
  }

  /**
   * This method iconifies or deiconifies this JInternalFrame given the
   * boolean argument. If the JInternalFrame becomes iconified, it will fire
   * an INTERNAL_FRAME_ICONIFIED event. If the JInternalFrame becomes
   * deiconified, it will fire anINTERNAL_FRAME_DEICONIFIED event.
   *
   * @param b Whether this JInternalFrame is to be iconified or deiconified.
   *
   * @throws PropertyVetoException DOCUMENT ME!
   */
  public void setIcon(boolean b) throws PropertyVetoException
  {
    if (b != isIcon())
      {
        fireVetoableChange(IS_ICON_PROPERTY, b, isIcon);

        isIcon = b;

        firePropertyChange(IS_ICON_PROPERTY, ! isIcon, isIcon);
        if (b)
          fireInternalFrameEvent(InternalFrameEvent.INTERNAL_FRAME_ICONIFIED);
        else
          fireInternalFrameEvent(InternalFrameEvent.INTERNAL_FRAME_DEICONIFIED);
      }
  }

  /**
   * This method sets whether the JInternalFrame can be iconified. (This means
   * that the JInternalFrame can be turned into an icon if minimized).
   *
   * @param b Whether the JInternalFrame can be iconified.
   */
  public void setIconifiable(boolean b)
  {
    if (iconable != b)
      {
        iconable = b;
        firePropertyChange("iconable", ! iconable, iconable);
      }
  }

  /**
   * This method sets the JMenuBar to be used with this JInternalFrame.
   *
   * @param b The JMenuBar to be used with this JInternalFrame.
   */
  public void setJMenuBar(JMenuBar b)
  {
    JMenuBar old = getJMenuBar();
    getRootPane().setJMenuBar(b);
    firePropertyChange(MENU_BAR_PROPERTY, old, b);
  }

  /**
   * A helper method that set the layer that this JInternalFrame resides in.
   * Using this version of the method means that the user should not set it
   * to values that are already defined in JLayeredPane. If predefined values
   * are to be used, the user should use the setLayer(Integer) version.
   *
   * @param layer The layer to place this JInternalFrame in.
   */
  public void setLayer(int layer)
  {
    setLayer(new Integer(layer));
  }

  /**
   * A helper method that sets the layer that this JInternalFrame resides in.
   * Calling this version of the method should use layer values that are
   * already defined in JLayeredPane.
   *
   * @param layer The layer to place this JInternalFrame in.
   */
  public void setLayer(Integer layer)
  {
    Container p = getParent();
    if (p instanceof JLayeredPane)
      {
        JLayeredPane lp = (JLayeredPane) p;
        lp.setLayer(this, layer.intValue(), lp.getPosition(this));
      }
    else
      {
        JLayeredPane.putLayer(this, layer.intValue());
        if (p != null)
          p.repaint(getX(), getY(), getWidth(), getHeight());
      }
  }

  /**
   * This method sets the JLayeredPane to use with this JInternalFrame.
   *
   * @param layered The JLayeredPane to use as a layeredPane.
   */
  public void setLayeredPane(JLayeredPane layered)
  {
    if (layered == null)
      throw new IllegalComponentStateException("LayeredPane must not be null");

    if (layered != getLayeredPane())
      {
        JLayeredPane old = getLayeredPane();
        getRootPane().setLayeredPane(layered);
        firePropertyChange(LAYERED_PANE_PROPERTY, old, layered);
      }
  }

  /**
   * This method sets whether the JInternalFrame can be maximized.
   *
   * @param b Whether this JInternalFrame can be maximized.
   */
  public void setMaximizable(boolean b)
  {
    if (maximizable != b)
      {
        maximizable = b;
        firePropertyChange("maximizable", ! maximizable, maximizable);
      }
  }

  /**
   * This method sets the Layout Manager used in the JInternalFrame. SetLayout
   * should not be called on the JInternalFrame directly. Instead, it should
   * be called with JInternalFrame.getContentPane().setLayout. Calls to this
   * method with root pane checking enabled will cause exceptions to be
   * thrown.
   *
   * @param manager The Layout Manager to be used with the JInternalFrame.
   *
   * @throws Error If rootPaneChecking is enabled.
   */
  public void setLayout(LayoutManager manager)
  {
    // Check if we're in initialization stage.  If so, call super.setLayout
    // otherwise, valid calls go to the content pane.
    if (isRootPaneCheckingEnabled())
      getContentPane().setLayout(manager);
    else
      super.setLayout(manager);
  }

  /**
   * This method sets the JInternalFrame to maximized (if the given argument
   * is true) or restores the JInternalFrame to its normal bounds otherwise.
   *
   * @param b Whether this JInteralFrame will be maximized or restored.
   *
   * @throws PropertyVetoException If a VetoableChangeListener vetoes the change.
   */
  public void setMaximum(boolean b) throws PropertyVetoException
  {
    if (b != isMaximum)
      {
        fireVetoableChange(IS_MAXIMUM_PROPERTY, isMaximum, b);
        isMaximum = b;
        firePropertyChange(IS_MAXIMUM_PROPERTY, ! isMaximum, isMaximum);
      }
  }

  /**
   * This method is deprecated. This method sets the JMenuBar used with this
   * JInternalFrame.
   *
   * @param m The JMenuBar to use with this JInternalFrame.
   *
   * @deprecated 1.0.3
   */
  public void setMenuBar(JMenuBar m)
  {
    setJMenuBar(m);
  }

  /**
   * This method sets the bounds that this JInternalFrame will be restored to.
   *
   * @param r The bounds that this JInternalFrame will be restored to.
   */
  public void setNormalBounds(Rectangle r)
  {
    storedBounds = r;
  }

  /**
   * This method sets whether the JInternalFrame can be resized by a user
   * action (like dragging at the frame borders).
   *
   * @param b Whether this JInternalFramer can be resized.
   */
  public void setResizable(boolean b)
  {
    if (b != resizable)
      {
        resizable = b;
        firePropertyChange("resizable", ! resizable, resizable);
      }
  }

  /**
   * This method sets the Root Pane for this JInternalFrame.
   *
   * @param root The Root Pane for this JInternalFrame.
   */
  protected void setRootPane(JRootPane root)
  {
    if (rootPane != null)
      remove(rootPane);

    JRootPane old = rootPane;
    rootPane = root;

    if (rootPane != null)
      {
        boolean checkingEnabled = isRootPaneCheckingEnabled();
        try
          {
            setRootPaneCheckingEnabled(false);
            add(rootPane, BorderLayout.CENTER);
          }
        finally
          {
            setRootPaneCheckingEnabled(checkingEnabled);
          }
      }
    firePropertyChange(ROOT_PANE_PROPERTY, old, rootPane);
  }

  /**
   * This method sets whether root pane checking is enabled. If root pane
   * checking is enabled, then calls to addImpl and setLayout will throw
   * exceptions.
   *
   * @param enabled Whether root pane checking is enabled.
   */
  protected void setRootPaneCheckingEnabled(boolean enabled)
  {
    rootPaneCheckingEnabled = enabled;
  }

  /**
   * This method sets whether this JInternalFrame is the selected frame in the
   * JDesktopPane (or other container). When selected, a JInternalFrame will
   * have focus and paint its TitlePane differently (usually a different
   * colour). If this method selects the frame, this JInternalFrame will fire
   * an INTERNAL_FRAME_ACTIVATED event. If it deselects this frame, it will
   * fire an INTERNAL_FRAME_DEACTIVATED event.
   *
   * @param selected Whether this JInternalFrame will become selected or
   *        deselected.
   *
   * @throws PropertyVetoException If a VetoableChangeListener vetoes the change.
   */
  public void setSelected(boolean selected) throws PropertyVetoException
  {
    if (selected != isSelected
        && (! selected || (isIcon ? desktopIcon.isShowing() : isShowing())))
      {
        fireVetoableChange(IS_SELECTED_PROPERTY, isSelected, selected);

        if (! selected)
          defaultFocus = getMostRecentFocusOwner();

        isSelected = selected;
        firePropertyChange(IS_SELECTED_PROPERTY, ! isSelected, isSelected);

        if (isSelected)
          fireInternalFrameEvent(InternalFrameEvent.INTERNAL_FRAME_ACTIVATED);
        else
          fireInternalFrameEvent(InternalFrameEvent.INTERNAL_FRAME_DEACTIVATED);

        if (selected)
          restoreSubcomponentFocus();

        repaint();
      }
  }

  /**
   * Sets the title for the <code>JInternalFrame</code> and sends a
   * {@link PropertyChangeEvent} (with the property name
   * {@link #TITLE_PROPERTY}) to all registered listeners.
   *
   * @param title  the new title (<code>null</code> permitted).
   *
   * @see #getTitle()
   */
  public void setTitle(String title)
  {
    String old = this.title;
    this.title = title;
    firePropertyChange(TITLE_PROPERTY, old, this.title);
  }

  /**
   * This method displays the JInternalFrame. If it is not visible, this
   * method will bring this JInternalFrame to the front, make it visible and
   * select it. If this is the first time this JInternalFrame is made
   * visible, an INTERNAL_FRAME_OPENED event will be fired.
   */
  public void show()
  {
    if (! isVisible())
      {
        if (isFirstTimeVisible)
          {
            isFirstTimeVisible = false;
            fireInternalFrameEvent(InternalFrameEvent.INTERNAL_FRAME_OPENED);
          }

        getDesktopIcon().setVisible(true);

        toFront();
        super.show();

        if (isIcon())
          return;

        if (! isSelected())
          {
            try
              {
                setSelected(true);
              }
            catch (PropertyVetoException e)
              {
                // Do nothing. if they don't want to be selected.
              }
          }
      }
  }

  /**
   * This method is used to set the UI responsible for the JInternalFrame.
   *
   * @param ui The UI responsible for the JInternalFrame.
   */
  public void setUI(InternalFrameUI ui)
  {
    // We must temporarily go into init mode so that the UI can directly
    // manipulate the JInternalFrame.
    boolean old = isRootPaneCheckingEnabled();
    setRootPaneCheckingEnabled(false);
    super.setUI(ui);
    setRootPaneCheckingEnabled(old);
  }

  /**
   * This method causes the JInternalFrame to be brough to back in the
   * z-order.
   */
  public void toBack()
  {
    moveToBack();
  }

  /**
   * This method causes the JInternalFrame to be brought to front in the
   * z-order.
   */
  public void toFront()
  {
    moveToFront();
  }

  /**
   * This method resets the UI to the Look and Feel defaults.
   */
  public void updateUI()
  {
    // We must go into the init stage when updating the UI, so the UI can
    // set layout and components directly on the internal frame, not its
    // content pane.
        boolean old = isRootPaneCheckingEnabled();
    setRootPaneCheckingEnabled(false);
    setUI((InternalFrameUI) UIManager.getUI(this));
    setRootPaneCheckingEnabled(old);
  }

  /**
   * This helper method allows JInternalFrames to signal that they were
   * iconned for the first time.
   *
   * @param b Whether the JInternalFrame was iconned.
   * @param ID The identifier of the property change event to fire if the
   *        JInternalFrame is iconned for the first time.
   */
  void setWasIcon(boolean b, String ID)
  {
    if (b && ! wasIcon)
      {
        wasIcon = b;
        firePropertyChange(ID, ! b, b);
      }
  }

  /**
   * This helper method returns whether the JInternalFrame has been iconned
   * once already.
   *
   * @return Whether the JInternalFrame has been iconned once already.
   */
  boolean getWasIcon()
  {
    return wasIcon;
  }

  /**
   * This method is a convenience method to fire vetoable property changes.
   *
   * @param name The identifier of the property change.
   * @param oldValue The old value.
   * @param newValue The new value.
   *
   * @throws PropertyVetoException Fired if a vetoable change listener vetoes
   *         the change.
   */
  private void fireVetoableChange(String name, boolean oldValue,
                                  boolean newValue)
                           throws PropertyVetoException
  {
    super.fireVetoableChange(name, Boolean.valueOf(oldValue), Boolean.valueOf(newValue));
  }
}
