/* BasicSplitPaneUI.java --
   Copyright (C) 2003, 2004, 2005, 2006, Free Software Foundation, Inc.

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


package javax.swing.plaf.basic;

import java.awt.Canvas;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.LayoutManager2;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.AbstractAction;
import javax.swing.ActionMap;
import javax.swing.InputMap;
import javax.swing.JComponent;
import javax.swing.JSlider;
import javax.swing.JSplitPane;
import javax.swing.KeyStroke;
import javax.swing.LookAndFeel;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.plaf.ActionMapUIResource;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.SplitPaneUI;
import javax.swing.plaf.UIResource;

/**
 * This is the Basic Look and Feel implementation of the SplitPaneUI  class.
 */
public class BasicSplitPaneUI extends SplitPaneUI
{
  /**
   * This Layout Manager controls the position and size of the components when
   * the JSplitPane's orientation is HORIZONTAL_SPLIT.
   *
   * @specnote Apparently this class was intended to be protected,
   *           but was made public by a compiler bug and is now
   *           public for compatibility.
   */
  public class BasicHorizontalLayoutManager implements LayoutManager2
  {
    // 3 components at a time.
    // LEFT/TOP = 0
    // RIGHT/BOTTOM = 1
    // DIVIDER = 2

    /**
     * This array contains the components in the JSplitPane. The  left/top
     * component is at index 0, the right/bottom is at 1, and the divider is
     * at 2.
     */
    protected Component[] components = new Component[3];

    // These are the _current_ widths of the associated component.

    /**
     * This array contains the current width (for HORIZONTAL_SPLIT) or height
     * (for VERTICAL_SPLIT) of the components. The indices are the same as
     * for components.
     */
    protected int[] sizes = new int[3];

    /**
     * This is used to determine if we are vertical or horizontal layout.
     * In the JDK, the BasicVerticalLayoutManager seems to have no more
     * methods implemented (as of JDK5), so we keep this state here.
     */
    private int axis;

    /**
     * Creates a new instance. This is package private because the reference
     * implementation has no public constructor either. Still, we need to
     * call it from BasicVerticalLayoutManager.
     */
    BasicHorizontalLayoutManager()
    {
      this(SwingConstants.HORIZONTAL);
    }

    /**
     * Creates a new instance for a specified axis. This is provided for
     * compatibility, since the BasicVerticalLayoutManager seems to have
     * no more implementation in the RI, according to the specs. So
     * we handle all the axis specific stuff here.
     *
     * @param a the axis, either SwingConstants#HORIZONTAL,
     *        or SwingConstants#VERTICAL
     */
    BasicHorizontalLayoutManager(int a)
    {
      axis = a;
    }

    /**
     * This method adds the component given to the JSplitPane. The position of
     * the component is given by the constraints object.
     *
     * @param comp The Component to add.
     * @param constraints The constraints that bind the object.
     */
    public void addLayoutComponent(Component comp, Object constraints)
    {
      addLayoutComponent((String) constraints, comp);
    }

    /**
     * This method is called to add a Component to the JSplitPane. The
     * placement string determines where the Component will be placed. The
     * string should be one of LEFT, RIGHT, TOP, BOTTOM or null (signals that
     * the component is the divider).
     *
     * @param place The placement of the Component.
     * @param component The Component to add.
     *
     * @throws IllegalArgumentException DOCUMENT ME!
     */
    public void addLayoutComponent(String place, Component component)
    {
      int i = 0;
      if (place == null)
        i = 2;
      else if (place.equals(JSplitPane.TOP) || place.equals(JSplitPane.LEFT))
        i = 0;
      else if (place.equals(JSplitPane.BOTTOM)
               || place.equals(JSplitPane.RIGHT))
        i = 1;
      else
        throw new IllegalArgumentException("Illegal placement in JSplitPane");
      components[i] = component;
      resetSizeAt(i);
      splitPane.revalidate();
      splitPane.repaint();
    }

    /**
     * This method returns the width of the JSplitPane minus the insets.
     *
     * @param containerSize The Dimensions of the JSplitPane.
     * @param insets The Insets of the JSplitPane.
     *
     * @return The width of the JSplitPane minus the insets.
     */
    protected int getAvailableSize(Dimension containerSize, Insets insets)
    {
      int size;
      if (axis == SwingConstants.HORIZONTAL)
        size = containerSize.width - insets.left - insets.right;
      else
        size = containerSize.height - insets.top - insets.bottom;
      return size;
    }

    /**
     * This method returns the given insets left value. If the  given inset is
     * null, then 0 is returned.
     *
     * @param insets The Insets to use with the JSplitPane.
     *
     * @return The inset's left value.
     */
    protected int getInitialLocation(Insets insets)
    {
      int loc = 0;
      if (insets != null)
        {
          if (axis == SwingConstants.HORIZONTAL)
            loc = insets.left;
          else
            loc = insets.top;
        }
      return loc;
    }

    /**
     * This specifies how a component is aligned with respect to  other
     * components in the x fdirection.
     *
     * @param target The container.
     *
     * @return The component's alignment.
     */
    public float getLayoutAlignmentX(Container target)
    {
      return 0.0f;
    }

    /**
     * This specifies how a component is aligned with respect to  other
     * components in the y direction.
     *
     * @param target The container.
     *
     * @return The component's alignment.
     */
    public float getLayoutAlignmentY(Container target)
    {
      return 0.0f;
    }

    /**
     * This method returns the preferred width of the component.
     *
     * @param c The component to measure.
     *
     * @return The preferred width of the component.
     */
    protected int getPreferredSizeOfComponent(Component c)
    {
      int size = 0;
      Dimension dims = c.getPreferredSize();
      if (axis == SwingConstants.HORIZONTAL)
        {
          if (dims != null)
            size = dims.width;
        }
      else
        {
          if (dims != null)
            size = dims.height;
        }
      return size;
    }

    /**
     * This method returns the current width of the component.
     *
     * @param c The component to measure.
     *
     * @return The width of the component.
     */
    protected int getSizeOfComponent(Component c)
    {
      int size;
      if (axis == SwingConstants.HORIZONTAL)
        size = c.getHeight();
      else
        size = c.getWidth();
      return size;
    }

    /**
     * This method returns the sizes array.
     *
     * @return The sizes array.
     */
    protected int[] getSizes()
    {
      return sizes;
    }

    /**
     * This method invalidates the layout. It does nothing.
     *
     * @param c The container to invalidate.
     */
    public void invalidateLayout(Container c)
    {
      // DO NOTHING
    }

    /**
     * This method lays out the components in the container.
     *
     * @param container The container to lay out.
     */
    public void layoutContainer(Container container)
    {
      if (container instanceof JSplitPane)
        {
          JSplitPane split = (JSplitPane) container;
          distributeExtraSpace();
          Insets insets = split.getInsets();
          Dimension dims = split.getSize();
          int loc = getInitialLocation(insets);
          int available = getAvailableSize(dims, insets);
          sizes[0] = split.getDividerLocation();
          sizes[1] = available - sizes[0] - sizes[2];

          // According to a Mauve test we only honour the minimum
          // size of the components, when the dividerLocation hasn't
          // been excplicitly set.
          if (! dividerLocationSet)
            {
              sizes[0] = Math.max(sizes[0], minimumSizeOfComponent(0));
              sizes[1] = Math.max(sizes[1], minimumSizeOfComponent(1));
            }
          // The size of the divider won't change.

          // Layout component#1.
          setComponentToSize(components[0], sizes[0], loc, insets, dims);
          // Layout divider.
          loc += sizes[0];
          setComponentToSize(components[2], sizes[2], loc, insets, dims);
          // Layout component#2.
          loc += sizes[2];
          setComponentToSize(components[1], sizes[1], loc, insets, dims);
        }
    }

    /**
     * This method returns the maximum size for the container given the
     * components. It returns a new Dimension object that has width and
     * height equal to Integer.MAX_VALUE.
     *
     * @param target The container to measure.
     *
     * @return The maximum size.
     */
    public Dimension maximumLayoutSize(Container target)
    {
      return new Dimension(Integer.MAX_VALUE, Integer.MAX_VALUE);
    }

    /**
     * This method returns the container's minimum size. The  minimum width is
     * the sum of all the component's minimum widths. The minimum height is
     * the maximum of  all the components' minimum heights.
     *
     * @param target The container to measure.
     *
     * @return The minimum size.
     */
    public Dimension minimumLayoutSize(Container target)
    {
      Dimension dim = new Dimension();
      if (target instanceof JSplitPane)
        {
          int primary = 0;
          int secondary = 0;
          for (int i = 0; i < components.length; i++)
            {
              if (components[i] != null)
                {
                  Dimension dims = components[i].getMinimumSize();
                  primary += axis == SwingConstants.HORIZONTAL ? dims.width
                                                               : dims.height;
                  int sec = axis == SwingConstants.HORIZONTAL ? dims.height
                                                              : dims.width;
                  secondary = Math.max(sec, secondary);
                }
            }
          int width = axis == SwingConstants.HORIZONTAL ? primary : secondary;
          int height = axis == SwingConstants.VERTICAL ? secondary : primary;

          Insets i = splitPane.getInsets();
          dim.setSize(width + i.left + i.right, height + i.top + i.bottom);
        }
      return dim;
    }

    /**
     * This method returns the container's preferred size. The preferred width
     * is the sum of all the component's preferred widths. The preferred
     * height is the maximum of all the components' preferred heights.
     *
     * @param target The container to measure.
     *
     * @return The preferred size.
     */
    public Dimension preferredLayoutSize(Container target)
    {
      Dimension dim = new Dimension();
      if (target instanceof JSplitPane)
        {
          int primary = 0;
          int secondary = 0;
          for (int i = 0; i < components.length; i++)
            {
              if (components[i] != null)
                {
                  Dimension dims = components[i].getPreferredSize();
                  primary += axis == SwingConstants.HORIZONTAL ? dims.width
                                                               : dims.height;
                  int sec = axis == SwingConstants.HORIZONTAL ? dims.height
                                                              : dims.width;
                  secondary = Math.max(sec, secondary);
                }
            }
          int width = axis == SwingConstants.HORIZONTAL ? primary : secondary;
          int height = axis == SwingConstants.VERTICAL ? secondary : primary;

          Insets i = splitPane.getInsets();
          dim.setSize(width + i.left + i.right, height + i.top + i.bottom);
        }
      return dim;
    }

    /**
     * This method removes the component from the layout.
     *
     * @param component The component to remove from the layout.
     */
    public void removeLayoutComponent(Component component)
    {
      for (int i = 0; i < components.length; i++)
        {
          if (component == components[i])
            {
              components[i] = null;
              sizes[i] = 0;
            }
        }
    }

    /**
     * This method resets the size of Component to the preferred size.
     *
     * @param index The index of the component to reset.
     */
    protected void resetSizeAt(int index)
    {
      if (components[index] != null)
        sizes[index] = getPreferredSizeOfComponent(components[index]);
    }

    /**
     * This method resets the sizes of all the components.
     */
    public void resetToPreferredSizes()
    {
      for (int i = 0; i < components.length; i++)
        resetSizeAt(i);
    }

    /**
     * This methods sets the bounds of the given component. The width is the
     * size. The height is the container size minus the  top and bottom
     * inset. The x coordinate is the location given.  The y coordinate is
     * the top inset.
     *
     * @param c The component to set.
     * @param size The width of the component.
     * @param location The x coordinate.
     * @param insets The insets to use.
     * @param containerSize The height of the container.
     */
    protected void setComponentToSize(Component c, int size, int location,
                                      Insets insets, Dimension containerSize)
    {
      if (insets != null)
        {
          if (axis == SwingConstants.HORIZONTAL)
            c.setBounds(location, insets.top, size,
                        containerSize.height - insets.top - insets.bottom);
          else
            c.setBounds(insets.left, location,
                        containerSize.width - insets.left - insets.right,
                        size);
        }
      else
        {
          if (axis == SwingConstants.HORIZONTAL)
            c.setBounds(location, 0, size, containerSize.height);
          else
            c.setBounds(0, location, containerSize.width, size);
        }
    }

    /**
     * This method stores the given int array as the new sizes array.
     *
     * @param newSizes The array to use as sizes.
     */
    protected void setSizes(int[] newSizes)
    {
      sizes = newSizes;
    }

    /**
     * This method determines the size of each  component. It should be called
     * when a new Layout Manager is created for an existing JSplitPane.
     */
    protected void updateComponents()
    {
      Component left = splitPane.getLeftComponent();
      Component right = splitPane.getRightComponent();

      if (left != null)
        {
          components[0] = left;
          resetSizeAt(0);
        }
      if (right != null)
        {
          components[1] = right;
          resetSizeAt(1);
        }
      components[2] = divider;
    }

    /**
     * This method resizes the left and right components to fit inside the
     * JSplitPane when there is extra space.
     */
    void distributeExtraSpace()
    {
      // FIXME: This needs to be reimplemented correctly.
    }

    /**
     * This method returns the minimum width of the  component at the given
     * index.
     *
     * @param index The index to check.
     *
     * @return The minimum width.
     */
    int minimumSizeOfComponent(int index)
    {
      Dimension dims = components[index].getMinimumSize();
      int size = 0;
      if (dims != null)
        if (axis == SwingConstants.HORIZONTAL)
          size = dims.width;
        else
          size = dims.height;
        return size;
    }
  } //end BasicHorizontalLayoutManager

  /**
   * This class is the Layout Manager for the JSplitPane when the orientation
   * is VERTICAL_SPLIT.
   *
   * @specnote Apparently this class was intended to be protected,
   *           but was made public by a compiler bug and is now
   *           public for compatibility.
   */
  public class BasicVerticalLayoutManager
    extends BasicHorizontalLayoutManager
  {
    /**
     * Creates a new instance.
     */
    public BasicVerticalLayoutManager()
    {
      super(SwingConstants.VERTICAL);
    }
  }

  /**
   * This class handles FocusEvents from the JComponent.
   *
   * @specnote Apparently this class was intended to be protected,
   *           but was made public by a compiler bug and is now
   *           public for compatibility.
   */
  public class FocusHandler extends FocusAdapter
  {
    /**
     * This method is called when the JSplitPane gains focus.
     *
     * @param ev The FocusEvent.
     */
    public void focusGained(FocusEvent ev)
    {
      // repaint the divider because its background color may change due to
      // the focus state...
      divider.repaint();
    }

    /**
     * This method is called when the JSplitPane loses focus.
     *
     * @param ev The FocusEvent.
     */
    public void focusLost(FocusEvent ev)
    {
      // repaint the divider because its background color may change due to
      // the focus state...
      divider.repaint();
    }
  }

  /**
   * This is a deprecated class. It is supposed to be used for handling down
   * and right key presses.
   *
   * @specnote Apparently this class was intended to be protected,
   *           but was made public by a compiler bug and is now
   *           public for compatibility.
   */
  public class KeyboardDownRightHandler implements ActionListener
  {
    /**
     * This method is called when the down or right keys are pressed.
     *
     * @param ev The ActionEvent
     */
    public void actionPerformed(ActionEvent ev)
    {
      // FIXME: implement.
    }
  }

  /**
   * This is a deprecated class. It is supposed to be used for handling end
   * key presses.
   *
   * @specnote Apparently this class was intended to be protected,
   *           but was made public by a compiler bug and is now
   *           public for compatibility.
   */
  public class KeyboardEndHandler implements ActionListener
  {
    /**
     * This method is called when the end key is pressed.
     *
     * @param ev The ActionEvent.
     */
    public void actionPerformed(ActionEvent ev)
    {
      // FIXME: implement.
    }
  }

  /**
   * This is a deprecated class. It is supposed to be used for handling home
   * key presses.
   *
   * @specnote Apparently this class was intended to be protected,
   *           but was made public by a compiler bug and is now
   *           public for compatibility.
   */
  public class KeyboardHomeHandler implements ActionListener
  {
    /**
     * This method is called when the home key is pressed.
     *
     * @param ev The ActionEvent.
     */
    public void actionPerformed(ActionEvent ev)
    {
      // FIXME: implement.
    }
  }

  /**
   * This is a deprecated class. It is supposed to be used for handling resize
   * toggles.
   *
   * @specnote Apparently this class was intended to be protected,
   *           but was made public by a compiler bug and is now
   *           public for compatibility.
   */
  public class KeyboardResizeToggleHandler implements ActionListener
  {
    /**
     * This method is called when a resize is toggled.
     *
     * @param ev The ActionEvent.
     */
    public void actionPerformed(ActionEvent ev)
    {
      // FIXME: implement.
    }
  }

  /**
   * This is a deprecated class. It is supposed to be used for handler up and
   * left key presses.
   *
   * @specnote Apparently this class was intended to be protected,
   *           but was made public by a compiler bug and is now
   *           public for compatibility.
   */
  public class KeyboardUpLeftHandler implements ActionListener
  {
    /**
     * This method is called when the left or up keys are pressed.
     *
     * @param ev The ActionEvent.
     */
    public void actionPerformed(ActionEvent ev)
    {
      // FIXME: implement.
    }
  }

  /**
   * This helper class handles PropertyChangeEvents from the JSplitPane. When
   * a property changes, this will update the UI accordingly.
   *
   * @specnote Apparently this class was intended to be protected,
   *           but was made public by a compiler bug and is now
   *           public for compatibility.
   */
  public class PropertyHandler implements PropertyChangeListener
  {
    /**
     * This method is called whenever one of the JSplitPane's properties
     * change.
     *
     * @param e DOCUMENT ME!
     */
    public void propertyChange(PropertyChangeEvent e)
    {
      if (e.getPropertyName().equals(JSplitPane.DIVIDER_SIZE_PROPERTY))
        {
          int newSize = splitPane.getDividerSize();
          int[] tmpSizes = layoutManager.getSizes();
          dividerSize = tmpSizes[2];
          int newSpace = newSize - tmpSizes[2];
          tmpSizes[2] = newSize;

          tmpSizes[0] += newSpace / 2;
          tmpSizes[1] += newSpace / 2;

          layoutManager.setSizes(tmpSizes);
        }
      else if (e.getPropertyName().equals(JSplitPane.ORIENTATION_PROPERTY))
        {
          int max = layoutManager.getAvailableSize(splitPane.getSize(),
                                                   splitPane.getInsets());
          int dividerLoc = getDividerLocation(splitPane);
          double prop = ((double) dividerLoc) / max;

          resetLayoutManager();
          if (prop <= 1 && prop >= 0)
            splitPane.setDividerLocation(prop);
        }
      // Don't have to deal with continuous_layout - only
      // necessary in dragging modes (and it's checked
      // every time you drag there)
      // Don't have to deal with resize_weight (as there
      // will be no extra space associated with this
      // event - the changes to the weighting will
      // be taken into account the next time the
      // sizes change.)
      // Don't have to deal with divider_location
      // The method in JSplitPane calls our setDividerLocation
      // so we'll know about those anyway.
      // Don't have to deal with last_divider_location
      // Although I'm not sure why, it doesn't seem to
      // have any effect on Sun's JSplitPane.
      // one_touch_expandable changes are dealt with
      // by our divider.
    }
  }

  /** The location of the divider when dragging began. */
  protected int beginDragDividerLocation;

  /** The size of the divider while dragging. */
  protected int dividerSize;

  /** The location where the last drag location ended. */
  transient int lastDragLocation = -1;

  /** The distance the divider is moved when moved by keyboard actions. */
  // Sun defines this as 3
  protected static int KEYBOARD_DIVIDER_MOVE_OFFSET = 3;

  /** The divider that divides this JSplitPane. */
  protected BasicSplitPaneDivider divider;

  /** The listener that listens for PropertyChangeEvents from the JSplitPane. */
  protected PropertyChangeListener propertyChangeListener;

  /** The JSplitPane's focus handler. */
  protected FocusListener focusListener;

  /** @deprecated The handler for down and right key presses. */
  protected ActionListener keyboardDownRightListener;

  /** @deprecated The handler for end key presses. */
  protected ActionListener keyboardEndListener;

  /** @deprecated The handler for home key presses. */
  protected ActionListener keyboardHomeListener;

  /** @deprecated The handler for toggling resizes. */
  protected ActionListener keyboardResizeToggleListener;

  /** @deprecated The handler for up and left key presses. */
  protected ActionListener keyboardUpLeftListener;

  /** The JSplitPane's current layout manager. */
  protected BasicHorizontalLayoutManager layoutManager;

  /** @deprecated The divider resize toggle key. */
  protected KeyStroke dividerResizeToggleKey;

  /** @deprecated The down key. */
  protected KeyStroke downKey;

  /** @deprecated The end key. */
  protected KeyStroke endKey;

  /** @deprecated The home key. */
  protected KeyStroke homeKey;

  /** @deprecated The left key. */
  protected KeyStroke leftKey;

  /** @deprecated The right key. */
  protected KeyStroke rightKey;

  /** @deprecated The up key. */
  protected KeyStroke upKey;

  /** Set to true when dragging heavy weight components. */
  protected boolean draggingHW;

  /**
   * The constraints object used when adding the non-continuous divider to the
   * JSplitPane.
   */
  protected static final String NON_CONTINUOUS_DIVIDER
    = "nonContinuousDivider";

  /** The dark divider used when dragging in non-continuous layout mode. */
  protected Component nonContinuousLayoutDivider;

  /** The JSplitPane that this UI draws. */
  protected JSplitPane splitPane;

  /**
   * True, when setDividerLocation() has been called at least
   * once on the JSplitPane, false otherwise.
   *
   * This is package private to avoid a synthetic accessor method.
   */
  boolean dividerLocationSet;

  /**
   * Creates a new BasicSplitPaneUI object.
   */
  public BasicSplitPaneUI()
  {
    // Nothing to do here.
  }

  /**
   * This method creates a new BasicSplitPaneUI for the given JComponent.
   *
   * @param x The JComponent to create a UI for.
   *
   * @return A new BasicSplitPaneUI.
   */
  public static ComponentUI createUI(JComponent x)
  {
    return new BasicSplitPaneUI();
  }

  /**
   * This method installs the BasicSplitPaneUI for the given JComponent.
   *
   * @param c The JComponent to install the UI for.
   */
  public void installUI(JComponent c)
  {
    if (c instanceof JSplitPane)
      {
        splitPane = (JSplitPane) c;
        dividerLocationSet = false;
        installDefaults();
        installListeners();
        installKeyboardActions();
      }
  }

  /**
   * This method uninstalls the BasicSplitPaneUI for the given JComponent.
   *
   * @param c The JComponent to uninstall the UI for.
   */
  public void uninstallUI(JComponent c)
  {
    uninstallKeyboardActions();
    uninstallListeners();
    uninstallDefaults();

    dividerLocationSet = false;
    splitPane = null;
  }

  /**
   * This method installs the defaults given by the Look and Feel.
   */
  protected void installDefaults()
  {
    LookAndFeel.installColors(splitPane, "SplitPane.background",
                              "SplitPane.foreground");
    LookAndFeel.installBorder(splitPane, "SplitPane.border");
    divider = createDefaultDivider();
    divider.setBorder(UIManager.getBorder("SplitPaneDivider.border"));
    resetLayoutManager();
    nonContinuousLayoutDivider = createDefaultNonContinuousLayoutDivider();
    splitPane.add(divider, JSplitPane.DIVIDER);

    // There is no need to add the nonContinuousLayoutDivider.
    dividerSize = UIManager.getInt("SplitPane.dividerSize");
    splitPane.setDividerSize(dividerSize);
    divider.setDividerSize(dividerSize);
    splitPane.setOpaque(true);
  }

  /**
   * This method uninstalls the defaults and nulls any objects created during
   * install.
   */
  protected void uninstallDefaults()
  {
    layoutManager = null;
    splitPane.remove(divider);
    divider = null;
    nonContinuousLayoutDivider = null;

    if (splitPane.getBackground() instanceof UIResource)
      splitPane.setBackground(null);
    if (splitPane.getBorder() instanceof UIResource)
      splitPane.setBorder(null);
  }

  /**
   * This method installs the listeners needed for this UI to function.
   */
  protected void installListeners()
  {
    propertyChangeListener = createPropertyChangeListener();
    focusListener = createFocusListener();

    splitPane.addPropertyChangeListener(propertyChangeListener);
    splitPane.addFocusListener(focusListener);
  }

  /**
   * This method uninstalls all listeners registered for the UI.
   */
  protected void uninstallListeners()
  {
    splitPane.removePropertyChangeListener(propertyChangeListener);
    splitPane.removeFocusListener(focusListener);

    focusListener = null;
    propertyChangeListener = null;
  }

  /**
   * Returns the input map for the specified condition.
   *
   * @param condition  the condition.
   *
   * @return The input map.
   */
  InputMap getInputMap(int condition)
  {
    if (condition == JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT)
      return (InputMap) UIManager.get("SplitPane.ancestorInputMap");
    return null;
  }

  /**
   * Returns the action map for the {@link JSplitPane}.  All sliders share
   * a single action map which is created the first time this method is
   * called, then stored in the UIDefaults table for subsequent access.
   *
   * @return The shared action map.
   */
  ActionMap getActionMap()
  {
    ActionMap map = (ActionMap) UIManager.get("SplitPane.actionMap");

    if (map == null) // first time here
      {
        map = createActionMap();
        if (map != null)
          UIManager.put("SplitPane.actionMap", map);
      }
    return map;
  }

  /**
   * Creates the action map shared by all {@link JSlider} instances.
   * This method is called once by {@link #getActionMap()} when it
   * finds no action map in the UIDefaults table...after the map is
   * created, it gets added to the defaults table so that subsequent
   * calls to {@link #getActionMap()} will return the same shared
   * instance.
   *
   * @return The action map.
   */
  ActionMap createActionMap()
  {
    ActionMap map = new ActionMapUIResource();
    map.put("toggleFocus",
            new AbstractAction("toggleFocus") {
              public void actionPerformed(ActionEvent event)
              {
                // FIXME: What to do here?
              }
            }
    );
    map.put("startResize",
            new AbstractAction("startResize") {
              public void actionPerformed(ActionEvent event)
              {
                splitPane.requestFocus();
              }
            }
    );
    map.put("selectMax",
            new AbstractAction("selectMax") {
              public void actionPerformed(ActionEvent event)
              {
                splitPane.setDividerLocation(1.0);
              }
            }
    );
    map.put("selectMin",
            new AbstractAction("selectMin") {
              public void actionPerformed(ActionEvent event)
              {
                splitPane.setDividerLocation(0.0);
              }
            }
    );
    map.put("negativeIncrement",
            new AbstractAction("negativeIncrement") {
              public void actionPerformed(ActionEvent event)
              {
                int oldLoc = splitPane.getDividerLocation();
                int newLoc =
                  Math.max(oldLoc - KEYBOARD_DIVIDER_MOVE_OFFSET, 0);
                splitPane.setDividerLocation(newLoc);
              }
            }
    );
    map.put("positiveIncrement",
            new AbstractAction("positiveIncrement") {
              public void actionPerformed(ActionEvent event)
              {
                int oldLoc = splitPane.getDividerLocation();
                int newLoc =
                  Math.max(oldLoc + KEYBOARD_DIVIDER_MOVE_OFFSET, 0);
                splitPane.setDividerLocation(newLoc);
              }
            }
    );
    map.put("focusOutBackward",
            new AbstractAction("focusOutBackward") {
              public void actionPerformed(ActionEvent event)
              {
                // FIXME: implement this
              }
            }
    );
    map.put("focusOutForward",
            new AbstractAction("focusOutForward") {
              public void actionPerformed(ActionEvent event)
              {
                // FIXME: implement this
              }
            }
    );
    return map;
  }

  /**
   * Installs any keyboard actions. The list of keys that need to be bound are
   * listed in Basic look and feel's defaults.
   */
  protected void installKeyboardActions()
  {
    InputMap keyMap = getInputMap(
        JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);
    SwingUtilities.replaceUIInputMap(splitPane,
        JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT, keyMap);
    ActionMap map = getActionMap();
    SwingUtilities.replaceUIActionMap(splitPane, map);
  }

  /**
   * This method reverses the work done in installKeyboardActions.
   */
  protected void uninstallKeyboardActions()
  {
    SwingUtilities.replaceUIActionMap(splitPane, null);
    SwingUtilities.replaceUIInputMap(splitPane,
        JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT, null);
  }

  /**
   * This method creates a new PropertyChangeListener.
   *
   * @return A new PropertyChangeListener.
   */
  protected PropertyChangeListener createPropertyChangeListener()
  {
    return new PropertyHandler();
  }

  /**
   * This method creates a new FocusListener.
   *
   * @return A new FocusListener.
   */
  protected FocusListener createFocusListener()
  {
    return new FocusHandler();
  }

  /**
   * This method creates a new ActionListener for up and left key presses.
   *
   * @return A new ActionListener for up and left keys.
   *
   * @deprecated 1.3
   */
  protected ActionListener createKeyboardUpLeftListener()
  {
    return new KeyboardUpLeftHandler();
  }

  /**
   * This method creates a new ActionListener for down and right key presses.
   *
   * @return A new ActionListener for down and right keys.
   *
   * @deprecated 1.3
   */
  protected ActionListener createKeyboardDownRightListener()
  {
    return new KeyboardDownRightHandler();
  }

  /**
   * This method creates a new ActionListener for home key presses.
   *
   * @return A new ActionListener for home keys.
   *
   * @deprecated
   */
  protected ActionListener createKeyboardHomeListener()
  {
    return new KeyboardHomeHandler();
  }

  /**
   * This method creates a new ActionListener for end key presses.i
   *
   * @return A new ActionListener for end keys.
   *
   * @deprecated 1.3
   */
  protected ActionListener createKeyboardEndListener()
  {
    return new KeyboardEndHandler();
  }

  /**
   * This method creates a new ActionListener for resize toggle key events.
   *
   * @return A new ActionListener for resize toggle keys.
   *
   * @deprecated 1.3
   */
  protected ActionListener createKeyboardResizeToggleListener()
  {
    return new KeyboardResizeToggleHandler();
  }

  /**
   * This method returns the orientation of the JSplitPane.
   *
   * @return The orientation of the JSplitPane.
   */
  public int getOrientation()
  {
    return splitPane.getOrientation();
  }

  /**
   * This method sets the orientation of the JSplitPane.
   *
   * @param orientation The new orientation of the JSplitPane.
   */
  public void setOrientation(int orientation)
  {
    splitPane.setOrientation(orientation);
  }

  /**
   * This method returns true if the JSplitPane is using continuous layout.
   *
   * @return True if the JSplitPane is using continuous layout.
   */
  public boolean isContinuousLayout()
  {
    return splitPane.isContinuousLayout();
  }

  /**
   * This method sets the continuous layout property of the JSplitPane.
   *
   * @param b True if the JsplitPane is to use continuous layout.
   */
  public void setContinuousLayout(boolean b)
  {
    splitPane.setContinuousLayout(b);
  }

  /**
   * This method returns the last location the divider was dragged to.
   *
   * @return The last location the divider was dragged to.
   */
  public int getLastDragLocation()
  {
    return lastDragLocation;
  }

  /**
   * This method sets the last location the divider was dragged to.
   *
   * @param l The last location the divider was dragged to.
   */
  public void setLastDragLocation(int l)
  {
    lastDragLocation = l;
  }

  /**
   * This method returns the BasicSplitPaneDivider that divides this
   * JSplitPane.
   *
   * @return The divider for the JSplitPane.
   */
  public BasicSplitPaneDivider getDivider()
  {
    return divider;
  }

  /**
   * This method creates a nonContinuousLayoutDivider for use with the
   * JSplitPane in nonContinousLayout mode. The default divider is a gray
   * Canvas.
   *
   * @return The default nonContinousLayoutDivider.
   */
  protected Component createDefaultNonContinuousLayoutDivider()
  {
    if (nonContinuousLayoutDivider == null)
      {
        nonContinuousLayoutDivider = new Canvas();
        Color c = UIManager.getColor("SplitPaneDivider.draggingColor");
        nonContinuousLayoutDivider.setBackground(c);
      }
    return nonContinuousLayoutDivider;
  }

  /**
   * This method sets the component to use as the nonContinuousLayoutDivider.
   *
   * @param newDivider The component to use as the nonContinuousLayoutDivider.
   */
  protected void setNonContinuousLayoutDivider(Component newDivider)
  {
    setNonContinuousLayoutDivider(newDivider, true);
  }

  /**
   * This method sets the component to use as the nonContinuousLayoutDivider.
   *
   * @param newDivider The component to use as the nonContinuousLayoutDivider.
   * @param rememberSizes FIXME: document.
   */
  protected void setNonContinuousLayoutDivider(Component newDivider,
                                               boolean rememberSizes)
  {
    // FIXME: use rememberSizes for something
    nonContinuousLayoutDivider = newDivider;
  }

  /**
   * This method returns the nonContinuousLayoutDivider.
   *
   * @return The nonContinuousLayoutDivider.
   */
  public Component getNonContinuousLayoutDivider()
  {
    return nonContinuousLayoutDivider;
  }

  /**
   * This method returns the JSplitPane that this BasicSplitPaneUI draws.
   *
   * @return The JSplitPane.
   */
  public JSplitPane getSplitPane()
  {
    return splitPane;
  }

  /**
   * This method creates the divider used normally with the JSplitPane.
   *
   * @return The default divider.
   */
  public BasicSplitPaneDivider createDefaultDivider()
  {
    if (divider == null)
      divider = new BasicSplitPaneDivider(this);
    return divider;
  }

  /**
   * This method is called when JSplitPane's resetToPreferredSizes is called.
   * It resets the sizes of all components in the JSplitPane.
   *
   * @param jc The JSplitPane to reset.
   */
  public void resetToPreferredSizes(JSplitPane jc)
  {
    layoutManager.resetToPreferredSizes();
  }

  /**
   * This method sets the location of the divider.
   *
   * @param jc The JSplitPane to set the divider location in.
   * @param location The new location of the divider.
   */
  public void setDividerLocation(JSplitPane jc, int location)
  {
    dividerLocationSet = true;
    splitPane.revalidate();
    splitPane.repaint();
  }

  /**
   * This method returns the location of the divider.
   *
   * @param jc The JSplitPane to retrieve the location for.
   *
   * @return The location of the divider.
   */
  public int getDividerLocation(JSplitPane jc)
  {
    int loc;
    if (jc.getOrientation() == JSplitPane.HORIZONTAL_SPLIT)
      loc = divider.getX();
    else
      loc = divider.getY();
    return loc;
  }

  /**
   * This method returns the smallest value possible for the location of the
   * divider.
   *
   * @param jc The JSplitPane.
   *
   * @return The minimum divider location.
   */
  public int getMinimumDividerLocation(JSplitPane jc)
  {
    int value = layoutManager.getInitialLocation(jc.getInsets());
    if (layoutManager.components[0] != null)
      value += layoutManager.minimumSizeOfComponent(0);
    return value;
  }

  /**
   * This method returns the largest value possible for the location of the
   * divider.
   *
   * @param jc The JSplitPane.
   *
   * @return The maximum divider location.
   */
  public int getMaximumDividerLocation(JSplitPane jc)
  {
    int value = layoutManager.getInitialLocation(jc.getInsets())
                + layoutManager.getAvailableSize(jc.getSize(), jc.getInsets())
                - splitPane.getDividerSize();
    if (layoutManager.components[1] != null)
      value -= layoutManager.minimumSizeOfComponent(1);
    return value;
  }

  /**
   * This method is called after the children of the JSplitPane are painted.
   *
   * @param jc The JSplitPane.
   * @param g The Graphics object to paint with.
   */
  public void finishedPaintingChildren(JSplitPane jc, Graphics g)
  {
    if (! splitPane.isContinuousLayout() && nonContinuousLayoutDivider != null
        && nonContinuousLayoutDivider.isVisible())
      javax.swing.SwingUtilities.paintComponent(g, nonContinuousLayoutDivider,
                                                null,
                                                nonContinuousLayoutDivider
                                                .getBounds());
  }

  /**
   * This method is called to paint the JSplitPane.
   *
   * @param g The Graphics object to paint with.
   * @param jc The JSplitPane to paint.
   */
  public void paint(Graphics g, JComponent jc)
  {
    // TODO: What should be done here?
  }

  /**
   * This method returns the preferred size of the JSplitPane.
   *
   * @param jc The JSplitPane.
   *
   * @return The preferred size of the JSplitPane.
   */
  public Dimension getPreferredSize(JComponent jc)
  {
    return layoutManager.preferredLayoutSize(jc);
  }

  /**
   * This method returns the minimum size of the JSplitPane.
   *
   * @param jc The JSplitPane.
   *
   * @return The minimum size of the JSplitPane.
   */
  public Dimension getMinimumSize(JComponent jc)
  {
    return layoutManager.minimumLayoutSize(jc);
  }

  /**
   * This method returns the maximum size of the JSplitPane.
   *
   * @param jc The JSplitPane.
   *
   * @return The maximum size of the JSplitPane.
   */
  public Dimension getMaximumSize(JComponent jc)
  {
    return layoutManager.maximumLayoutSize(jc);
  }

  /**
   * This method returns the border insets of the current border.
   *
   * @param jc The JSplitPane.
   *
   * @return The current border insets.
   */
  public Insets getInsets(JComponent jc)
  {
    return splitPane.getBorder().getBorderInsets(splitPane);
  }

  /**
   * This method resets the current layout manager. The type of layout manager
   * is dependent on the current orientation.
   */
  protected void resetLayoutManager()
  {
    if (getOrientation() == JSplitPane.HORIZONTAL_SPLIT)
      layoutManager = new BasicHorizontalLayoutManager();
    else
      layoutManager = new BasicVerticalLayoutManager();
    getSplitPane().setLayout(layoutManager);
    layoutManager.updateComponents();

    // invalidating by itself does not invalidate the layout.
    getSplitPane().revalidate();
  }

  /**
   * This method is called when dragging starts. It resets lastDragLocation
   * and dividerSize.
   */
  protected void startDragging()
  {
    Component left = splitPane.getLeftComponent();
    Component right = splitPane.getRightComponent();
    dividerSize = divider.getDividerSize();
    setLastDragLocation(-1);

    if ((left != null && !left.isLightweight())
        || (right != null && !right.isLightweight()))
      draggingHW = true;

    if (splitPane.isContinuousLayout())
      nonContinuousLayoutDivider.setVisible(false);
    else
      {
        nonContinuousLayoutDivider.setVisible(true);
        nonContinuousLayoutDivider.setBounds(divider.getBounds());
      }
  }

  /**
   * This method is called whenever the divider is dragged. If the JSplitPane
   * is in continuousLayout mode, the divider needs to be moved and the
   * JSplitPane needs to be laid out.
   *
   * @param location The new location of the divider.
   */
  protected void dragDividerTo(int location)
  {
    location = validLocation(location);
    if (beginDragDividerLocation == -1)
      beginDragDividerLocation = location;

    if (splitPane.isContinuousLayout())
      splitPane.setDividerLocation(location);
    else
      {
        Point p = nonContinuousLayoutDivider.getLocation();
        if (getOrientation() == JSplitPane.HORIZONTAL_SPLIT)
          p.x = location;
        else
          p.y = location;
        nonContinuousLayoutDivider.setLocation(p);
      }
    setLastDragLocation(location);
    splitPane.repaint();
  }

  /**
   * This method is called when the dragging is finished.
   *
   * @param location The location where the drag finished.
   */
  protected void finishDraggingTo(int location)
  {
    if (nonContinuousLayoutDivider != null)
      nonContinuousLayoutDivider.setVisible(false);
    draggingHW = false;
    location = validLocation(location);
    splitPane.setDividerLocation(location);
    splitPane.setLastDividerLocation(beginDragDividerLocation);
    beginDragDividerLocation = -1;
  }

  /**
   * This method returns the width of one of the sides of the divider's border.
   *
   * @return The width of one side of the divider's border.
   *
   * @deprecated 1.3
   */
  protected int getDividerBorderSize()
  {
    if (getOrientation() == JSplitPane.HORIZONTAL_SPLIT)
      return divider.getBorder().getBorderInsets(divider).left;
    else
      return divider.getBorder().getBorderInsets(divider).top;
  }

  /**
   * This is a helper method that returns a valid location for the divider
   * when dragging.
   *
   * @param location The location to check.
   *
   * @return A valid location.
   */
  private int validLocation(int location)
  {
    int min = getMinimumDividerLocation(splitPane);
    int max = getMaximumDividerLocation(splitPane);
    if (min > 0 && location < min)
      return min;
    if (max > 0 && location > max)
      return max;
    return location;
  }
}
