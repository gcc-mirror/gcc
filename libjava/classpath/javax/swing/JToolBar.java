/* JToolBar.java --
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

import gnu.java.lang.CPStringBuilder;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.LayoutManager;
import java.beans.PropertyChangeListener;

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;
import javax.accessibility.AccessibleStateSet;
import javax.swing.plaf.ToolBarUI;

/**
 * JToolBar is a component that provides a toolbar to Swing programs. Users
 * can add buttons (or actions that will be represented by JButtons) as well
 * as other components to the JToolBar. JToolBars can be dragged in and out
 * of their parent components. If the JToolBar is dragged out of the parent,
 * then it will be displayed in its own RootPaneContainer. For dragging to
 * work properly, JToolBars need to be placed in a Container that has a
 * BorderLayout. That parent Container cannot have components in the NORTH,
 * EAST, SOUTH,  or WEST components (that is not the JToolBar).
 */
public class JToolBar extends JComponent implements SwingConstants, Accessible
{
  /**
   * Provides the accessibility features for the <code>JToolBar</code>
   * component.
   */
  protected class AccessibleJToolBar extends AccessibleJComponent
  {
    private static final long serialVersionUID = -5516888265903814215L;

    /**
     * Creates a new <code>AccessibleJToolBar</code> instance.
     */
    protected AccessibleJToolBar()
    {
      // Nothing to do here.
    }

    /**
     * Returns a set containing the current state of the {@link JToolBar}
     * component.  The current implementation simply calls the superclass.
     *
     * @return The accessible state set.
     */
    public AccessibleStateSet getAccessibleStateSet()
    {
      // running tests against the reference implementation, I was unable
      // to find any state information that is set specifically by the
      // tool bar...
      return super.getAccessibleStateSet();
    }

    /**
     * Returns the accessible role for the <code>JToolBar</code> component.
     *
     * @return {@link AccessibleRole#TOOL_BAR}.
     */
    public AccessibleRole getAccessibleRole()
    {
      return AccessibleRole.TOOL_BAR;
    }
  }

  /**
   * This is the private JToolBar layout manager.
   */
  private class DefaultToolBarLayout implements LayoutManager
  {
    /**
     * This method is called when a new component is added to the container.
     *
     * @param name The name of the component added.
     * @param comp The component that was added.
     */
    public void addLayoutComponent(String name, Component comp)
    {
      // Do nothing.
    }

    /**
     * This method is called to lay out the given container  to position and
     * size the child components.
     *
     * @param c The container to lay out.
     *
     * @throws Error DOCUMENT ME!
     */
    public void layoutContainer(Container c)
    {
      if (! (c instanceof JToolBar))
        throw new Error("DefaultToolBarLayout can only be used on JToolBars.");
      Insets insets = getInsets();
      Insets margin = getMargin();
      int middle;
      if (margin != null)
        {
          insets.left += margin.left;
          insets.top += margin.top;
          insets.bottom += margin.bottom;
          insets.right += margin.right;
        }
      Component[] components = c.getComponents();
      Dimension tdims = c.getSize();
      int start = 0;
      Dimension pref;

      if (getOrientation() == SwingUtilities.HORIZONTAL)
        {
          start += insets.left;
          for (int i = 0; i < components.length; i++)
            {
              if (components[i] != null && components[i].isVisible())
                {
                  pref = components[i].getPreferredSize();
                  if (pref != null)
                    {
                      middle = (tdims.height - pref.height) / 2;
                      components[i].setBounds(start, middle, pref.width,
                                              pref.height);
                      start += pref.width;
                    }
                }
            }
        }
      else
        {
          start += insets.top;
          for (int i = 0; i < components.length; i++)
            {
              if (components[i] != null && components[i].isVisible())
                {
                  pref = components[i].getPreferredSize();
                  if (pref != null)
                    {
                      middle = (tdims.width - pref.width) / 2;
                      components[i].setBounds(middle, start, pref.width,
                                              pref.height);
                      start += pref.height;
                    }
                }
            }
        }
    }

    /**
     * This method returns the minimum size of the given container given the
     * child components.
     *
     * @param parent The container to measure.
     *
     * @return The minimum size of the given container.
     */
    public Dimension minimumLayoutSize(Container parent)
    {
      return preferredLayoutSize(parent);
    }

    /**
     * This method returns the preferred size of the given container given the
     * child components.
     *
     * @param parent The container to measure.
     *
     * @return The preferred size of the given container.
     */
    public Dimension preferredLayoutSize(Container parent)
    {
      int orientation = getOrientation();
      Component[] components = getComponents();

      int limit = 0;
      int total = 0;
      Dimension dims;

      int w = 0;
      int h = 0;

      if (orientation == SwingConstants.HORIZONTAL)
        {
          for (int i = 0; i < components.length; i++)
            {
              dims = components[i].getPreferredSize();
              if (dims != null)
                {
                  if (dims.height > limit)
                    limit = dims.height;
                  total += dims.width;
                }
            }
          w = total;
          h = limit;
        }
      else
        {
          for (int i = 0; i < components.length; i++)
            {
              dims = components[i].getPreferredSize();
              if (dims != null)
                {
                  if (dims.width > limit)
                    limit = dims.width;
                  total += dims.height;
                }
            }
          w = limit;
          h = total;
        }

      Insets insets = getInsets();
      w += insets.left + insets.right;
      h += insets.top + insets.bottom;

      Insets margin = getMargin();
      if (margin != null)
        {
          w += margin.left + margin.right;
          h += margin.top + margin.bottom;
        }

      return new Dimension(w, h);
    }

    /**
     * This method is called when the given component  is removed from the
     * container.
     *
     * @param comp The component removed.
     */
    public void removeLayoutComponent(Component comp)
    {
      // Do nothing.
    }
  }

  /**
   * This is an extension of JSeparator used in toolbars. Unlike JSeparator,
   * nothing is painted for this Separator, it is only blank space that
   * separates components.
   */
  public static class Separator extends JSeparator
  {
    /** DOCUMENT ME! */
    private static final long serialVersionUID = -1656745644823105219L;

    /**
     * Creates a new Separator object.
     */
    public Separator()
    {
      super();
    } // Separator()

    /**
     * Creates a new Separator object with the given size.
     *
     * @param size The size of the separator.
     */
    public Separator(Dimension size)
    {
      setPreferredSize(size);
    } // Separator()

    /**
     * This method returns the String ID of the UI class of  Separator.
     *
     * @return The UI class' String ID.
     */
    public String getUIClassID()
    {
      return "ToolBarSeparatorUI";
    } // getUIClassID()

    /**
     * This method returns the preferred size of the Separator.
     *
     * @return The preferred size of the Separator.
     */
    public Dimension getPreferredSize()
    {
      return super.getPreferredSize();
    } // getPreferredSize()

    /**
     * This method returns the maximum size of the Separator.
     *
     * @return The maximum size of the Separator.
     */
    public Dimension getMaximumSize()
    {
      return super.getPreferredSize();
    } // getMaximumSize()

    /**
     * This method returns the minimum size of the Separator.
     *
     * @return The minimum size of the Separator.
     */
    public Dimension getMinimumSize()
    {
      return super.getPreferredSize();
    } // getMinimumSize()

    /**
     * This method returns the size of the Separator.
     *
     * @return The size of the Separator.
     */
    public Dimension getSeparatorSize()
    {
      return super.getPreferredSize();
    } // getSeparatorSize()

    /**
     * This method sets the size of the Separator.
     *
     * @param size The new size of the Separator.
     */
    public void setSeparatorSize(Dimension size)
    {
      setPreferredSize(size);
    } // setSeparatorSize()
  } // Separator

  /** DOCUMENT ME! */
  private static final long serialVersionUID = -1269915519555129643L;

  /** Whether the JToolBar paints its border. */
  private transient boolean paintBorder = true;

  /** The extra insets around the JToolBar. */
  private transient Insets margin;

  /** Whether the JToolBar can float (and be dragged around). */
  private transient boolean floatable = true;

  /** Whether the buttons will have rollover borders. */
  private transient boolean rollover;

  /** The orientation of the JToolBar. */
  private int orientation = HORIZONTAL;

  /**
   * This method creates a new JToolBar object with horizontal orientation
   * and no name.
   */
  public JToolBar()
  {
    this(null, HORIZONTAL);
  } // JToolBar()

  /**
   * This method creates a new JToolBar with the given orientation and  no
   * name.
   *
   * @param orientation JToolBar orientation (HORIZONTAL or VERTICAL)
   */
  public JToolBar(int orientation)
  {
    this(null, orientation);
  } // JToolBar()

  /**
   * This method creates a new JToolBar object with the given name and
   * horizontal orientation.
   *
   * @param name Name assigned to undocked tool bar.
   */
  public JToolBar(String name)
  {
    this(name, HORIZONTAL);
  } // JToolBar()

  /**
   * This method creates a new JToolBar object with the given name and
   * orientation.
   *
   * @param name Name assigned to undocked tool bar.
   * @param orientation JToolBar orientation (HORIZONTAL or VERTICAL)
   */
  public JToolBar(String name, int orientation)
  {
    setName(name);
    setOrientation(orientation);
    setLayout(new DefaultToolBarLayout());
    revalidate();
    setOpaque(true);
    updateUI();
  }

  /**
   * This method adds a new JButton that performs the given Action to the
   * JToolBar.
   *
   * @param action The Action to add to the JToolBar.
   *
   * @return The JButton that wraps the Action.
   */
  public JButton add(Action action)
  {
    JButton b = createActionComponent(action);
    add(b);
    return b;
  } // add()

  /**
   * This method paints the border if the borderPainted property is true.
   *
   * @param graphics The graphics object to paint with.
   */
  protected void paintBorder(Graphics graphics)
  {
    if (paintBorder && isFloatable())
      super.paintBorder(graphics);
  } // paintBorder()

  /**
   * This method returns the UI class used to paint this JToolBar.
   *
   * @return The UI class for this JToolBar.
   */
  public ToolBarUI getUI()
  {
    return (ToolBarUI) ui;
  } // getUI()

  /**
   * This method sets the UI used with the JToolBar.
   *
   * @param ui The UI used with the JToolBar.
   */
  public void setUI(ToolBarUI ui)
  {
    super.setUI(ui);
  } // setUI()

  /**
   * This method resets the UI used to the Look and Feel defaults.
   */
  public void updateUI()
  {
    setUI((ToolBarUI) UIManager.getUI(this));
  }

  /**
   * This method returns the String identifier for the UI class to the used
   * with the JToolBar.
   *
   * @return The String identifier for the UI class.
   */
  public String getUIClassID()
  {
    return "ToolBarUI";
  } // getUIClassID()

  /**
   * This method sets the rollover property for the JToolBar. In rollover
   * mode, JButtons inside the JToolBar will only display their borders when
   * the mouse is moving over them.
   *
   * @param b The new rollover property.
   */
  public void setRollover(boolean b)
  {
    if (b != rollover)
      {
        rollover = b;
        firePropertyChange("rollover", ! rollover, rollover);
        revalidate();
        repaint();
      }
  }

  /**
   * This method returns the rollover property.
   *
   * @return The rollover property.
   */
  public boolean isRollover()
  {
    return rollover;
  }

  /**
   * This method returns the index of the given component.
   *
   * @param component The component to find.
   *
   * @return The index of the given component.
   */
  public int getComponentIndex(Component component)
  {
    Component[] components = getComponents();
    if (components == null)
      return -1;

    for (int i = 0; i < components.length; i++)
      if (components[i] == component)
        return i;

    return -1;
  } // getComponentIndex()

  /**
   * This method returns the component at the given index.
   *
   * @param index The index of the component.
   *
   * @return The component at the given index.
   */
  public Component getComponentAtIndex(int index)
  {
    return getComponent(index);
  } // getComponentAtIndex()

  /**
   * This method returns the margin property.
   *
   * @return The margin property.
   */
  public Insets getMargin()
  {
    return margin;
  } // getMargin()

  /**
   * This method sets the margin property. The margin property determines the
   * extra space between the children components of the JToolBar and the
   * border.
   *
   * @param margin The margin property.
   */
  public void setMargin(Insets margin)
  {
    if ((this.margin != null && margin == null)
        || (this.margin == null && margin != null)
        || (margin != null && this.margin != null
        && (margin.left != this.margin.left
        || margin.right != this.margin.right || margin.top != this.margin.top
        || margin.bottom != this.margin.bottom)))
      {
        Insets oldMargin = this.margin;
        this.margin = margin;
        firePropertyChange("margin", oldMargin, this.margin);
        revalidate();
        repaint();
      }
  } // setMargin()

  /**
   * This method returns the borderPainted property.
   *
   * @return The borderPainted property.
   */
  public boolean isBorderPainted()
  {
    return paintBorder;
  } // isBorderPainted()

  /**
   * This method sets the borderPainted property. If set to false, the border
   * will not be painted.
   *
   * @param painted Whether the border will be painted.
   */
  public void setBorderPainted(boolean painted)
  {
    if (painted != paintBorder)
      {
        paintBorder = painted;
        firePropertyChange("borderPainted", ! paintBorder,
                           paintBorder);
        repaint();
      }
  } // setBorderPainted()

  /**
   * This method returns the floatable property.
   *
   * @return The floatable property.
   */
  public boolean isFloatable()
  {
    return floatable;
  } // isFloatable()

  /**
   * This method sets the floatable property. If set to false, the JToolBar
   * cannot be dragged.
   *
   * @param floatable Whether the JToolBar can be dragged.
   */
  public void setFloatable(boolean floatable)
  {
    if (floatable != this.floatable)
      {
        this.floatable = floatable;
        firePropertyChange("floatable", ! floatable, floatable);
      }
  } // setFloatable()

  /**
   * This method returns the orientation of the JToolBar.
   *
   * @return The orientation of the JToolBar.
   */
  public int getOrientation()
  {
    return orientation;
  } // getOrientation()

  /**
   * This method sets the layout manager to be used with the JToolBar.
   *
   * @param mgr The Layout Manager used with the JToolBar.
   */
  public void setLayout(LayoutManager mgr)
  {
    super.setLayout(mgr);
    revalidate();
    repaint();
  } // setLayout()

  /**
   * This method sets the orientation property for JToolBar.
   *
   * @param orientation The new orientation for JToolBar.
   *
   * @throws IllegalArgumentException If the orientation is not HORIZONTAL or
   *         VERTICAL.
   */
  public void setOrientation(int orientation)
  {
    if (orientation != HORIZONTAL && orientation != VERTICAL)
      throw new IllegalArgumentException(orientation
                                         + " is not a legal orientation");
    if (orientation != this.orientation)
      {
        int oldOrientation = this.orientation;
        this.orientation = orientation;
        firePropertyChange("orientation", oldOrientation, this.orientation);
        revalidate();
        repaint();
      }
  } // setOrientation()

  /**
   * This method adds a Separator of default size to the JToolBar.
   */
  public void addSeparator()
  {
    add(new Separator());
  } // addSeparator()

  /**
   * This method adds a Separator with the given size to the JToolBar.
   *
   * @param size The size of the Separator.
   */
  public void addSeparator(Dimension size)
  {
    add(new Separator(size));
  } // addSeparator()

  /**
   * This method is used to create JButtons which can be added to the JToolBar
   * for the given action.
   *
   * @param action The action to create a JButton for.
   *
   * @return The JButton created from the action.
   */
  protected JButton createActionComponent(Action action)
  {
    return new JButton(action);
  } // createActionComponent()

  /**
   * This method creates a pre-configured PropertyChangeListener which updates
   * the control as changes are made to the Action. However, this is no
   * longer the recommended way of adding Actions to Containers. As such,
   * this method returns null.
   *
   * @param button The JButton to configure a PropertyChangeListener for.
   *
   * @return null.
   */
  protected PropertyChangeListener createActionChangeListener(JButton button)
  {
    // XXX: As specified, this returns null. But seems kind of strange, usually deprecated methods don't just return null, verify!
    return null;
  } // createActionChangeListener()

  /**
   * This method overrides Container's addImpl method. If a JButton is added,
   * it is disabled.
   *
   * @param component The Component to add.
   * @param constraints The Constraints placed on the component.
   * @param index The index to place the Component at.
   */
  protected void addImpl(Component component, Object constraints, int index)
  {
    // XXX: Sun says disable button but test cases show otherwise.
    super.addImpl(component, constraints, index);

    // if we added a Swing Button then adjust this a little
    if (component instanceof AbstractButton)
      {
        AbstractButton b = (AbstractButton) component;
        b.setRolloverEnabled(rollover);
      }

  } // addImpl()

  /**
   * Returns a string describing the attributes for the <code>JToolBar</code>
   * component, for use in debugging.  The return value is guaranteed to be
   * non-<code>null</code>, but the format of the string may vary between
   * implementations.
   *
   * @return A string describing the attributes of the <code>JToolBar</code>.
   */
  protected String paramString()
  {
    CPStringBuilder sb = new CPStringBuilder(super.paramString());
    sb.append(",floatable=").append(floatable);
    sb.append(",margin=");
    if (margin != null)
      sb.append(margin);
    sb.append(",orientation=");
    if (orientation == HORIZONTAL)
      sb.append("HORIZONTAL");
    else
      sb.append(VERTICAL);
    sb.append(",paintBorder=").append(paintBorder);
    return sb.toString();
  }

  /**
   * Returns the object that provides accessibility features for this
   * <code>JToolBar</code> component.
   *
   * @return The accessible context (an instance of {@link AccessibleJToolBar}).
   */
  public AccessibleContext getAccessibleContext()
  {
    if (accessibleContext == null)
      accessibleContext = new AccessibleJToolBar();

    return accessibleContext;
  }
}
