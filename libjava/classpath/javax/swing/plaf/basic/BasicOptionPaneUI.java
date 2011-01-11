/* BasicOptionPaneUI.java --
   Copyright (C) 2004, 2005 Free Software Foundation, Inc.

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

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.LayoutManager;
import java.awt.Polygon;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyVetoException;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ActionMap;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.InputMap;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JInternalFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.LookAndFeel;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.border.Border;
import javax.swing.plaf.ActionMapUIResource;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.OptionPaneUI;

/**
 * This class is the UI delegate for JOptionPane in the Basic Look and Feel.
 */
public class BasicOptionPaneUI extends OptionPaneUI
{
  /**
   * Implements the "close" keyboard action.
   */
  static class OptionPaneCloseAction
    extends AbstractAction
  {

    public void actionPerformed(ActionEvent event)
    {
      JOptionPane op = (JOptionPane) event.getSource();
      op.setValue(new Integer(JOptionPane.CLOSED_OPTION));
    }

  }

  /**
   * This is a helper class that listens to the buttons located at the bottom
   * of the JOptionPane.
   *
   * @specnote Apparently this class was intended to be protected,
   *           but was made public by a compiler bug and is now
   *           public for compatibility.
   */
  public class ButtonActionListener implements ActionListener
  {
    /** The index of the option this button represents. */
    protected int buttonIndex;

    /**
     * Creates a new ButtonActionListener object with the given buttonIndex.
     *
     * @param buttonIndex The index of the option this button represents.
     */
    public ButtonActionListener(int buttonIndex)
    {
      this.buttonIndex = buttonIndex;
    }

    /**
     * This method is called when one of the option buttons are pressed.
     *
     * @param e The ActionEvent.
     */
    public void actionPerformed(ActionEvent e)
    {
      Object value = new Integer(JOptionPane.CLOSED_OPTION);
      Object[] options = optionPane.getOptions();
      if (options != null)
        value = new Integer(buttonIndex);
      else
        {
          String text = ((JButton) e.getSource()).getText();
          if (text.equals(OK_STRING))
            value = new Integer(JOptionPane.OK_OPTION);
          if (text.equals(CANCEL_STRING))
            value = new Integer(JOptionPane.CANCEL_OPTION);
          if (text.equals(YES_STRING))
            value = new Integer(JOptionPane.YES_OPTION);
          if (text.equals(NO_STRING))
            value = new Integer(JOptionPane.NO_OPTION);
        }
      optionPane.setValue(value);
      resetInputValue();

      Window owner = SwingUtilities.windowForComponent(optionPane);

      if (owner instanceof JDialog)
        ((JDialog) owner).dispose();

      //else we probably have some kind of internal frame.
      JInternalFrame inf = (JInternalFrame) SwingUtilities.getAncestorOfClass(
          JInternalFrame.class, optionPane);
      if (inf != null)
        {
          try
            {
              inf.setClosed(true);
            }
          catch (PropertyVetoException pve)
            {
              // We do nothing if attempt has been vetoed.
            }
        }
    }
  }

  /**
   * This helper layout manager is responsible for the layout of the button
   * area. The button area is the panel that holds the buttons which
   * represent the options.
   *
   * @specnote Apparently this class was intended to be protected,
   *           but was made public by a compiler bug and is now
   *           public for compatibility.
   */
  public static class ButtonAreaLayout implements LayoutManager
  {
    /** Whether this layout will center the buttons. */
    protected boolean centersChildren = true;

    /** The space between the buttons. */
    protected int padding;

    /** Whether the buttons will share the same widths. */
    protected boolean syncAllWidths;

    /** The width of the widest button. */
    private transient int widthOfWidestButton;

    /** The height of the tallest button. */
    private transient int tallestButton;

    /**
     * Creates a new ButtonAreaLayout object with the given sync widths
     * property and padding.
     *
     * @param syncAllWidths Whether the buttons will share the same widths.
     * @param padding The padding between the buttons.
     */
    public ButtonAreaLayout(boolean syncAllWidths, int padding)
    {
      this.syncAllWidths = syncAllWidths;
      this.padding = padding;
    }

    /**
     * This method is called when a component is added to the container.
     *
     * @param string The constraints string.
     * @param comp The component added.
     */
    public void addLayoutComponent(String string, Component comp)
    {
      // Do nothing.
    }

    /**
     * This method returns whether the children will be centered.
     *
     * @return Whether the children will be centered.
     */
    public boolean getCentersChildren()
    {
      return centersChildren;
    }

    /**
     * This method returns the amount of space between components.
     *
     * @return The amount of space between components.
     */
    public int getPadding()
    {
      return padding;
    }

    /**
     * This method returns whether all components will share widths (set to
     * largest width).
     *
     * @return Whether all components will share widths.
     */
    public boolean getSyncAllWidths()
    {
      return syncAllWidths;
    }

    /**
     * This method lays out the given container.
     *
     * @param container The container to lay out.
     */
    public void layoutContainer(Container container)
    {
      Component[] buttonList = container.getComponents();
      int x = container.getInsets().left;
      if (getCentersChildren())
        x += (int) ((double) (container.getSize().width) / 2
        - (double) (buttonRowLength(container)) / 2);
      for (int i = 0; i < buttonList.length; i++)
        {
          Dimension dims = buttonList[i].getPreferredSize();
          if (syncAllWidths)
            {
              buttonList[i].setBounds(x, 0, widthOfWidestButton, dims.height);
              x += widthOfWidestButton + getPadding();
            }
          else
            {
              buttonList[i].setBounds(x, 0, dims.width, dims.height);
              x += dims.width + getPadding();
            }
        }
    }

    /**
     * This method returns the width of the given container taking into
     * consideration the padding and syncAllWidths.
     *
     * @param c The container to calculate width for.
     *
     * @return The width of the given container.
     */
    private int buttonRowLength(Container c)
    {
      Component[] buttonList = c.getComponents();

      int buttonLength = 0;
      int widest = 0;
      int tallest = 0;

      for (int i = 0; i < buttonList.length; i++)
        {
          Dimension dims = buttonList[i].getPreferredSize();
          buttonLength += dims.width + getPadding();
          widest = Math.max(widest, dims.width);
          tallest = Math.max(tallest, dims.height);
        }

      widthOfWidestButton = widest;
      tallestButton = tallest;

      int width;
      if (getSyncAllWidths())
        width = widest * buttonList.length
                + getPadding() * (buttonList.length - 1);
      else
        width = buttonLength;

      Insets insets = c.getInsets();
      width += insets.left + insets.right;

      return width;
    }

    /**
     * This method returns the minimum layout size for the given container.
     *
     * @param c The container to measure.
     *
     * @return The minimum layout size.
     */
    public Dimension minimumLayoutSize(Container c)
    {
      return preferredLayoutSize(c);
    }

    /**
     * This method returns the preferred size of the given container.
     *
     * @param c The container to measure.
     *
     * @return The preferred size.
     */
    public Dimension preferredLayoutSize(Container c)
    {
      int w = buttonRowLength(c);

      return new Dimension(w, tallestButton);
    }

    /**
     * This method removes the given component from the layout manager's
     * knowledge.
     *
     * @param c The component to remove.
     */
    public void removeLayoutComponent(Component c)
    {
      // Do nothing.
    }

    /**
     * This method sets whether the children will be centered.
     *
     * @param newValue Whether the children will be centered.
     */
    public void setCentersChildren(boolean newValue)
    {
      centersChildren = newValue;
    }

    /**
     * This method sets the amount of space between each component.
     *
     * @param newPadding The padding between components.
     */
    public void setPadding(int newPadding)
    {
      padding = newPadding;
    }

    /**
     * This method sets whether the widths will be synced.
     *
     * @param newValue Whether the widths will be synced.
     */
    public void setSyncAllWidths(boolean newValue)
    {
      syncAllWidths = newValue;
    }
  }

  /**
   * This helper class handles property change events from the JOptionPane.
   *
   * @specnote Apparently this class was intended to be protected,
   *           but was made public by a compiler bug and is now
   *           public for compatibility.
   */
  public class PropertyChangeHandler implements PropertyChangeListener
  {
    /**
     * This method is called when one of the properties of the JOptionPane
     * changes.
     *
     * @param e The PropertyChangeEvent.
     */
    public void propertyChange(PropertyChangeEvent e)
    {
      String property = e.getPropertyName();
      if (property.equals(JOptionPane.ICON_PROPERTY)
          || property.equals(JOptionPane.INITIAL_SELECTION_VALUE_PROPERTY)
          || property.equals(JOptionPane.INITIAL_VALUE_PROPERTY)
          || property.equals(JOptionPane.MESSAGE_PROPERTY)
          || property.equals(JOptionPane.MESSAGE_TYPE_PROPERTY)
          || property.equals(JOptionPane.OPTION_TYPE_PROPERTY)
          || property.equals(JOptionPane.OPTIONS_PROPERTY)
          || property.equals(JOptionPane.WANTS_INPUT_PROPERTY))
        {
          uninstallComponents();
          installComponents();
          optionPane.validate();
        }
    }
  }

  /**
   * The minimum width for JOptionPanes.
   */
  public static final int MinimumWidth = 262;

  /**
   * The minimum height for JOptionPanes.
   */
  public static final int MinimumHeight = 90;

  /** Whether the JOptionPane contains custom components. */
  protected boolean hasCustomComponents;

  // The initialFocusComponent seems to always be set to a button (even if
  // I try to set initialSelectionValue). This is different from what the
  // javadocs state (which should switch this reference to the input component
  // if one is present since that is what's going to get focus).

  /**
   * The button that will receive focus based on initialValue when no input
   * component is present. If an input component is present, then the input
   * component will receive focus instead.
   */
  protected Component initialFocusComponent;

  /** The component that receives input when the JOptionPane needs it. */
  protected JComponent inputComponent;

  /** The minimum dimensions of the JOptionPane. */
  protected Dimension minimumSize;

  /** The propertyChangeListener for the JOptionPane. */
  protected PropertyChangeListener propertyChangeListener;

  /** The JOptionPane this UI delegate is used for. */
  protected JOptionPane optionPane;

  /** The size of the icons. */
  private static final int ICON_SIZE = 36;

  /** The string used to describe OK buttons. */
  private static final String OK_STRING = "OK";

  /** The string used to describe Yes buttons. */
  private static final String YES_STRING = "Yes";

  /** The string used to describe No buttons. */
  private static final String NO_STRING = "No";

  /** The string used to describe Cancel buttons. */
  private static final String CANCEL_STRING = "Cancel";

  /** The container for the message area.
   * This is package-private to avoid an accessor method. */
  transient Container messageAreaContainer;

  /** The container for the buttons.
   * This is package-private to avoid an accessor method.  */
  transient Container buttonContainer;

  /**
   * A helper class that implements Icon. This is used temporarily until
   * ImageIcons are fixed.
   */
  private static class MessageIcon implements Icon
  {
    /**
     * This method returns the width of the icon.
     *
     * @return The width of the icon.
     */
    public int getIconWidth()
    {
      return ICON_SIZE;
    }

    /**
     * This method returns the height of the icon.
     *
     * @return The height of the icon.
     */
    public int getIconHeight()
    {
      return ICON_SIZE;
    }

    /**
     * This method paints the icon as a part of the given component using the
     * given graphics and the given x and y position.
     *
     * @param c The component that owns this icon.
     * @param g The Graphics object to paint with.
     * @param x The x coordinate.
     * @param y The y coordinate.
     */
    public void paintIcon(Component c, Graphics g, int x, int y)
    {
      // Nothing to do here.
    }
  }

  /** The icon displayed for ERROR_MESSAGE. */
  private static MessageIcon errorIcon = new MessageIcon()
    {
      public void paintIcon(Component c, Graphics g, int x, int y)
      {
        Polygon oct = new Polygon(new int[] { 0, 0, 9, 27, 36, 36, 27, 9 },
                                  new int[] { 9, 27, 36, 36, 27, 9, 0, 0 }, 8);
        g.translate(x, y);

        Color saved = g.getColor();
        g.setColor(Color.RED);

        g.fillPolygon(oct);

        g.setColor(Color.BLACK);
        g.drawRect(13, 16, 10, 4);

        g.setColor(saved);
        g.translate(-x, -y);
      }
    };

  /** The icon displayed for INFORMATION_MESSAGE. */
  private static MessageIcon infoIcon = new MessageIcon()
    {
      public void paintIcon(Component c, Graphics g, int x, int y)
      {
        g.translate(x, y);
        Color saved = g.getColor();

        // Should be purple.
        g.setColor(Color.RED);

        g.fillOval(0, 0, ICON_SIZE, ICON_SIZE);

        g.setColor(Color.BLACK);
        g.drawOval(16, 6, 4, 4);

        Polygon bottomI = new Polygon(new int[] { 15, 15, 13, 13, 23, 23, 21, 21 },
                                      new int[] { 12, 28, 28, 30, 30, 28, 28, 12 },
                                      8);
        g.drawPolygon(bottomI);

        g.setColor(saved);
        g.translate(-x, -y);
      }
    };

  /** The icon displayed for WARNING_MESSAGE. */
  private static MessageIcon warningIcon = new MessageIcon()
    {
      public void paintIcon(Component c, Graphics g, int x, int y)
      {
        g.translate(x, y);
        Color saved = g.getColor();
        g.setColor(Color.YELLOW);

        Polygon triangle = new Polygon(new int[] { 0, 18, 36 },
                                       new int[] { 36, 0, 36 }, 3);
        g.fillPolygon(triangle);

        g.setColor(Color.BLACK);

        Polygon excl = new Polygon(new int[] { 15, 16, 20, 21 },
                                   new int[] { 8, 26, 26, 8 }, 4);
        g.drawPolygon(excl);
        g.drawOval(16, 30, 4, 4);

        g.setColor(saved);
        g.translate(-x, -y);
      }
    };

  /** The icon displayed for MESSAGE_ICON. */
  private static MessageIcon questionIcon = new MessageIcon()
    {
      public void paintIcon(Component c, Graphics g, int x, int y)
      {
        g.translate(x, y);
        Color saved = g.getColor();
        g.setColor(Color.GREEN);

        g.fillRect(0, 0, ICON_SIZE, ICON_SIZE);

        g.setColor(Color.BLACK);

        g.drawOval(11, 2, 16, 16);
        g.drawOval(14, 5, 10, 10);

        g.setColor(Color.GREEN);
        g.fillRect(0, 10, ICON_SIZE, ICON_SIZE - 10);

        g.setColor(Color.BLACK);

        g.drawLine(11, 10, 14, 10);

        g.drawLine(24, 10, 17, 22);
        g.drawLine(27, 10, 20, 22);
        g.drawLine(17, 22, 20, 22);

        g.drawOval(17, 25, 3, 3);

        g.setColor(saved);
        g.translate(-x, -y);
      }
    };

  /**
   * Creates a new BasicOptionPaneUI object.
   */
  public BasicOptionPaneUI()
  {
    // Nothing to do here.
  }

  /**
   * This method is messaged to add the buttons to the given container.
   *
   * @param container The container to add components to.
   * @param buttons The buttons to add. (If it is an instance of component,
   *        the Object is added directly. If it is an instance of Icon, it is
   *        packed into a label and added. For all other cases, the string
   *        representation of the Object is retreived and packed into a
   *        label.)
   * @param initialIndex The index of the component that is the initialValue.
   */
  protected void addButtonComponents(Container container, Object[] buttons,
                                     int initialIndex)
  {
    if (buttons == null)
      return;
    for (int i = 0; i < buttons.length; i++)
      {
        if (buttons[i] != null)
          {
            Component toAdd;
            if (buttons[i] instanceof Component)
              toAdd = (Component) buttons[i];
            else
              {
                if (buttons[i] instanceof Icon)
                  toAdd = new JButton((Icon) buttons[i]);
                else
                  toAdd = new JButton(buttons[i].toString());
                hasCustomComponents = true;
              }
            if (toAdd instanceof JButton)
              ((JButton) toAdd).addActionListener(createButtonActionListener(i));
            if (i == initialIndex)
              initialFocusComponent = toAdd;
            container.add(toAdd);
          }
      }
    selectInitialValue(optionPane);
  }

  /**
   * This method adds the appropriate icon the given container.
   *
   * @param top The container to add an icon to.
   */
  protected void addIcon(Container top)
  {
    JLabel iconLabel = null;
    Icon icon = getIcon();
    if (icon != null)
      {
        iconLabel = new JLabel(icon);
        configureLabel(iconLabel);
        top.add(iconLabel, BorderLayout.WEST);
      }
  }

  /**
   * A helper method that returns an instance of GridBagConstraints to be used
   * for creating the message area.
   *
   * @return An instance of GridBagConstraints.
   */
  private static GridBagConstraints createConstraints()
  {
    GridBagConstraints constraints = new GridBagConstraints();
    constraints.gridx = GridBagConstraints.REMAINDER;
    constraints.gridy = GridBagConstraints.REMAINDER;
    constraints.gridwidth = 0;
    constraints.anchor = GridBagConstraints.LINE_START;
    constraints.fill = GridBagConstraints.NONE;
    constraints.insets = new Insets(0, 0, 3, 0);

    return constraints;
  }

  /**
   * This method creates the proper object (if necessary) to represent msg.
   * (If msg is an instance of Component, it will add it directly. If it is
   * an icon, then it will pack it in a label and add it. Otherwise, it gets
   * treated as a string. If the string is longer than maxll, a box is
   * created and the burstStringInto is called with the box as the container.
   * The box is then added to the given container. Otherwise, the string is
   * packed in a label and placed in the given container.) This method is
   * also used for adding the inputComponent to the container.
   *
   * @param container The container to add to.
   * @param cons The constraints when adding.
   * @param msg The message to add.
   * @param maxll The max line length.
   * @param internallyCreated Whether the msg is internally created.
   */
  protected void addMessageComponents(Container container,
                                      GridBagConstraints cons, Object msg,
                                      int maxll, boolean internallyCreated)
  {
    if (msg == null)
      return;
    hasCustomComponents = internallyCreated;
    if (msg instanceof Object[])
      {
        Object[] arr = (Object[]) msg;
        for (int i = 0; i < arr.length; i++)
          addMessageComponents(container, cons, arr[i], maxll,
                               internallyCreated);
        return;
      }
    else if (msg instanceof Component)
      {
        container.add((Component) msg, cons);
        cons.gridy++;
      }
    else if (msg instanceof Icon)
      {
        JLabel label = new JLabel((Icon) msg);
        configureLabel(label);
        container.add(label, cons);
        cons.gridy++;
      }
    else
      {
        // Undocumented behaviour.
        // if msg.toString().length greater than maxll
        // it will create a box and burst the string.
        // otherwise, it will just create a label and re-call
        // this method with the label o.O
        if (msg.toString().length() > maxll || msg.toString().contains("\n"))
          {
            Box tmp = new Box(BoxLayout.Y_AXIS);
            burstStringInto(tmp, msg.toString(), maxll);
            addMessageComponents(container, cons, tmp, maxll, true);
          }
        else
          {
            JLabel label = new JLabel(msg.toString());
            configureLabel(label);
            addMessageComponents(container, cons, label, maxll, true);
          }
      }
  }

  /**
   * This method creates instances of d (recursively if necessary based on
   * maxll) and adds to c.
   *
   * @param c The container to add to.
   * @param d The string to burst.
   * @param maxll The max line length.
   */
  protected void burstStringInto(Container c, String d, int maxll)
  {
    if (d == null || c == null)
      return;

    int newlineIndex = d.indexOf('\n');
    String line;
    String remainder;
    if (newlineIndex >= 0 && newlineIndex < maxll)
      {
        line = d.substring(0, newlineIndex);
        remainder = d.substring(newlineIndex + 1);
      }
    else
      {
        line = d.substring(0, maxll);
        remainder = d.substring(maxll);
      }
    JLabel label = new JLabel(line);
    configureLabel(label);
    c.add(label);

    // If there is nothing left to burst, then we can stop.
    if (remainder.length() == 0)
      return;

    // Recursively call ourselves to burst the remainder of the string,
    if (remainder.length() > maxll || remainder.contains("\n"))
      burstStringInto(c, remainder, maxll);
    else
      {
        // Add the remainder to the container and be done.
        JLabel l = new JLabel(remainder);
        configureLabel(l);
        c.add(l);
      }
  }

  /**
   * This method returns true if the given JOptionPane contains custom
   * components.
   *
   * @param op The JOptionPane to check.
   *
   * @return True if the JOptionPane contains custom components.
   */
  public boolean containsCustomComponents(JOptionPane op)
  {
    return hasCustomComponents;
  }

  /**
   * This method creates a button action listener for the given button index.
   *
   * @param buttonIndex The index of the button in components.
   *
   * @return A new ButtonActionListener.
   */
  protected ActionListener createButtonActionListener(int buttonIndex)
  {
    return new ButtonActionListener(buttonIndex);
  }

  /**
   * This method creates the button area.
   *
   * @return A new Button Area.
   */
  protected Container createButtonArea()
  {
    JPanel buttonPanel = new JPanel();
    Border b = UIManager.getBorder("OptionPane.buttonAreaBorder");
    if (b != null)
      buttonPanel.setBorder(b);

    buttonPanel.setLayout(createLayoutManager());
    addButtonComponents(buttonPanel, getButtons(), getInitialValueIndex());

    return buttonPanel;
  }

  /**
   * This method creates a new LayoutManager for the button area.
   *
   * @return A new LayoutManager for the button area.
   */
  protected LayoutManager createLayoutManager()
  {
    return new ButtonAreaLayout(getSizeButtonsToSameWidth(), 6);
  }

  /**
   * This method creates the message area.
   *
   * @return A new message area.
   */
  protected Container createMessageArea()
  {
    JPanel messageArea = new JPanel();
    Border messageBorder = UIManager.getBorder("OptionPane.messageAreaBorder");
    if (messageBorder != null)
      messageArea.setBorder(messageBorder);

    messageArea.setLayout(new BorderLayout());
    addIcon(messageArea);

    JPanel rightSide = new JPanel();
    rightSide.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
    rightSide.setLayout(new GridBagLayout());
    GridBagConstraints con = createConstraints();

    addMessageComponents(rightSide, con, getMessage(),
                         getMaxCharactersPerLineCount(), false);

    if (optionPane.getWantsInput())
      {
        Object[] selection = optionPane.getSelectionValues();

        if (selection == null)
          inputComponent = new JTextField(15);
        else if (selection.length < 20)
          inputComponent = new JComboBox(selection);
        else
          inputComponent = new JList(selection);
        if (inputComponent != null)
          {
            addMessageComponents(rightSide, con, inputComponent,
                                 getMaxCharactersPerLineCount(), false);
            resetSelectedValue();
            selectInitialValue(optionPane);
          }
      }

    messageArea.add(rightSide, BorderLayout.CENTER);

    return messageArea;
  }

  /**
   * This method creates a new PropertyChangeListener for listening to the
   * JOptionPane.
   *
   * @return A new PropertyChangeListener.
   */
  protected PropertyChangeListener createPropertyChangeListener()
  {
    return new PropertyChangeHandler();
  }

  /**
   * This method creates a Container that will separate the message and button
   * areas.
   *
   * @return A Container that will separate the message and button areas.
   */
  protected Container createSeparator()
  {
    // The reference implementation returns null here. When overriding
    // to return something non-null, the component gets added between
    // the message area and the button area. See installComponents().
    return null;
  }

  /**
   * This method creates a new BasicOptionPaneUI for the given component.
   *
   * @param x The component to create a UI for.
   *
   * @return A new BasicOptionPaneUI.
   */
  public static ComponentUI createUI(JComponent x)
  {
    return new BasicOptionPaneUI();
  }

  /**
   * This method returns the buttons for the JOptionPane. If no options are
   * set, a set of options will be created based upon the optionType.
   *
   * @return The buttons that will be added.
   */
  protected Object[] getButtons()
  {
    if (optionPane.getOptions() != null)
      return optionPane.getOptions();
    switch (optionPane.getOptionType())
      {
      case JOptionPane.YES_NO_OPTION:
        return new Object[] { YES_STRING, NO_STRING };
      case JOptionPane.YES_NO_CANCEL_OPTION:
        return new Object[] { YES_STRING, NO_STRING, CANCEL_STRING };
      case JOptionPane.OK_CANCEL_OPTION:
        return new Object[] { OK_STRING, CANCEL_STRING };
      case JOptionPane.DEFAULT_OPTION:
        return (optionPane.getWantsInput()) ?
               new Object[] { OK_STRING, CANCEL_STRING } :
               (optionPane.getMessageType() == JOptionPane.QUESTION_MESSAGE) ?
               new Object[] { YES_STRING, NO_STRING, CANCEL_STRING } :
               // ERROR_MESSAGE, INFORMATION_MESSAGE, WARNING_MESSAGE, PLAIN_MESSAGE
               new Object[] { OK_STRING };
      }
    return null;
  }

  /**
   * This method will return the icon the user has set or the icon that will
   * be used based on message type.
   *
   * @return The icon to use in the JOptionPane.
   */
  protected Icon getIcon()
  {
    if (optionPane.getIcon() != null)
      return optionPane.getIcon();
    else
      return getIconForType(optionPane.getMessageType());
  }

  /**
   * This method returns the icon for the given messageType.
   *
   * @param messageType The type of message.
   *
   * @return The icon for the given messageType.
   */
  protected Icon getIconForType(int messageType)
  {
    Icon tmp = null;
    switch (messageType)
      {
      case JOptionPane.ERROR_MESSAGE:
        tmp = errorIcon;
        break;
      case JOptionPane.INFORMATION_MESSAGE:
        tmp = infoIcon;
        break;
      case JOptionPane.WARNING_MESSAGE:
        tmp = warningIcon;
        break;
      case JOptionPane.QUESTION_MESSAGE:
        tmp = questionIcon;
        break;
      }
    return tmp;
    // FIXME: Don't cast till the default icons are in.
    // return new IconUIResource(tmp);
  }

  /**
   * This method returns the index of the initialValue in the options array.
   *
   * @return The index of the initalValue.
   */
  protected int getInitialValueIndex()
  {
    Object[] buttons = getButtons();

    if (buttons == null)
      return -1;

    Object select = optionPane.getInitialValue();

    for (int i = 0; i < buttons.length; i++)
      {
        if (select == buttons[i])
          return i;
      }
    return 0;
  }

  /**
   * This method returns the maximum number of characters that should be
   * placed on a line.
   *
   * @return The maximum number of characteres that should be placed on a
   *         line.
   */
  protected int getMaxCharactersPerLineCount()
  {
    return optionPane.getMaxCharactersPerLineCount();
  }

  /**
   * This method returns the maximum size.
   *
   * @param c The JComponent to measure.
   *
   * @return The maximum size.
   */
  public Dimension getMaximumSize(JComponent c)
  {
    return getPreferredSize(c);
  }

  /**
   * This method returns the message of the JOptionPane.
   *
   * @return The message.
   */
  protected Object getMessage()
  {
    return optionPane.getMessage();
  }

  /**
   * This method returns the minimum size of the JOptionPane.
   *
   * @return The minimum size.
   */
  public Dimension getMinimumOptionPaneSize()
  {
    return minimumSize;
  }

  /**
   * This method returns the minimum size.
   *
   * @param c The JComponent to measure.
   *
   * @return The minimum size.
   */
  public Dimension getMinimumSize(JComponent c)
  {
    return getPreferredSize(c);
  }

  /**
   * This method returns the preferred size of the JOptionPane. The preferred
   * size is the maximum of the size desired by the layout and the minimum
   * size.
   *
   * @param c The JComponent to measure.
   *
   * @return The preferred size.
   */
  public Dimension getPreferredSize(JComponent c)
  {
    Dimension d = optionPane.getLayout().preferredLayoutSize(optionPane);
    Dimension d2 = getMinimumOptionPaneSize();

    int w = Math.max(d.width, d2.width);
    int h = Math.max(d.height, d2.height);
    return new Dimension(w, h);
  }

  /**
   * This method returns whether all buttons should have the same width.
   *
   * @return Whether all buttons should have the same width.
   */
  protected boolean getSizeButtonsToSameWidth()
  {
    return true;
  }

  /**
   * This method installs components for the JOptionPane.
   */
  protected void installComponents()
  {
    // First thing is the message area.
    optionPane.add(createMessageArea());

    // Add separator when createSeparator() is overridden to return
    // something other than null.
    Container sep = createSeparator();
    if (sep != null)
      optionPane.add(sep);

    // Last thing is the button area.
    optionPane.add(createButtonArea());
  }

  /**
   * This method installs defaults for the JOptionPane.
   */
  protected void installDefaults()
  {
    LookAndFeel.installColorsAndFont(optionPane, "OptionPane.background",
                                     "OptionPane.foreground",
                                     "OptionPane.font");
    LookAndFeel.installBorder(optionPane, "OptionPane.border");
    optionPane.setOpaque(true);

    minimumSize = UIManager.getDimension("OptionPane.minimumSize");

    // FIXME: Image icons don't seem to work properly right now.
    // Once they do, replace the synthetic icons with these ones.

    /*
    warningIcon = (IconUIResource) defaults.getIcon("OptionPane.warningIcon");
    infoIcon = (IconUIResource) defaults.getIcon("OptionPane.informationIcon");
    errorIcon = (IconUIResource) defaults.getIcon("OptionPane.errorIcon");
    questionIcon = (IconUIResource) defaults.getIcon("OptionPane.questionIcon");
    */
  }

  /**
   * This method installs keyboard actions for the JOptionpane.
   */
  protected void installKeyboardActions()
  {
    // Install the input map.
    Object[] bindings =
      (Object[]) SharedUIDefaults.get("OptionPane.windowBindings");
    InputMap inputMap = LookAndFeel.makeComponentInputMap(optionPane,
                                                          bindings);
    SwingUtilities.replaceUIInputMap(optionPane,
                                     JComponent.WHEN_IN_FOCUSED_WINDOW,
                                     inputMap);

    // FIXME: The JDK uses a LazyActionMap for parentActionMap
    SwingUtilities.replaceUIActionMap(optionPane, getActionMap());
  }

  /**
   * Fetches the action map from  the UI defaults, or create a new one
   * if the action map hasn't been initialized.
   *
   * @return the action map
   */
  private ActionMap getActionMap()
  {
    ActionMap am = (ActionMap) UIManager.get("OptionPane.actionMap");
    if (am == null)
      {
        am = createDefaultActions();
        UIManager.getLookAndFeelDefaults().put("OptionPane.actionMap", am);
      }
    return am;
  }

  private ActionMap createDefaultActions()
  {
    ActionMapUIResource am = new ActionMapUIResource();
    Action action = new OptionPaneCloseAction();

    am.put("close", action);
    return am;
  }

  /**
   * This method installs listeners for the JOptionPane.
   */
  protected void installListeners()
  {
    propertyChangeListener = createPropertyChangeListener();

    optionPane.addPropertyChangeListener(propertyChangeListener);
  }

  /**
   * This method installs the UI for the JOptionPane.
   *
   * @param c The JComponent to install the UI for.
   */
  public void installUI(JComponent c)
  {
    if (c instanceof JOptionPane)
      {
        optionPane = (JOptionPane) c;

        installDefaults();
        installComponents();
        installListeners();
        installKeyboardActions();
      }
  }

  /**
   * Changes the inputValue property in the JOptionPane based on the current
   * value of the inputComponent.
   */
  protected void resetInputValue()
  {
    if (optionPane.getWantsInput() && inputComponent != null)
      {
        Object output = null;
        if (inputComponent instanceof JTextField)
          output = ((JTextField) inputComponent).getText();
        else if (inputComponent instanceof JComboBox)
          output = ((JComboBox) inputComponent).getSelectedItem();
        else if (inputComponent instanceof JList)
          output = ((JList) inputComponent).getSelectedValue();

        if (output != null)
          optionPane.setInputValue(output);
      }
  }

  /**
   * This method requests focus to the inputComponent (if one is present) and
   * the initialFocusComponent otherwise.
   *
   * @param op The JOptionPane.
   */
  public void selectInitialValue(JOptionPane op)
  {
    if (inputComponent != null)
      {
        inputComponent.requestFocus();
        return;
      }
    if (initialFocusComponent != null)
      initialFocusComponent.requestFocus();
  }

  /**
   * This method resets the value in the inputComponent to the
   * initialSelectionValue property.
   * This is package-private to avoid an accessor method.
   */
  void resetSelectedValue()
  {
    if (inputComponent != null)
      {
        Object init = optionPane.getInitialSelectionValue();
        if (init == null)
          return;
        if (inputComponent instanceof JTextField)
          ((JTextField) inputComponent).setText((String) init);
        else if (inputComponent instanceof JComboBox)
          ((JComboBox) inputComponent).setSelectedItem(init);
        else if (inputComponent instanceof JList)
          {
            //  ((JList) inputComponent).setSelectedValue(init, true);
          }
      }
  }

  /**
   * This method uninstalls all the components in the JOptionPane.
   */
  protected void uninstallComponents()
  {
    optionPane.removeAll();
    buttonContainer = null;
    messageAreaContainer = null;
  }

  /**
   * This method uninstalls the defaults for the JOptionPane.
   */
  protected void uninstallDefaults()
  {
    optionPane.setFont(null);
    optionPane.setForeground(null);
    optionPane.setBackground(null);

    minimumSize = null;

    // FIXME: ImageIcons don't seem to work properly

    /*
    warningIcon = null;
    errorIcon = null;
    questionIcon = null;
    infoIcon = null;
    */
  }

  /**
   * This method uninstalls keyboard actions for the JOptionPane.
   */
  protected void uninstallKeyboardActions()
  {
    SwingUtilities.replaceUIInputMap(optionPane, JComponent.
                                     WHEN_ANCESTOR_OF_FOCUSED_COMPONENT, null);
    SwingUtilities.replaceUIActionMap(optionPane, null);
  }

  /**
   * This method uninstalls listeners for the JOptionPane.
   */
  protected void uninstallListeners()
  {
    optionPane.removePropertyChangeListener(propertyChangeListener);
    propertyChangeListener = null;
  }

  /**
   * This method uninstalls the UI for the given JComponent.
   *
   * @param c The JComponent to uninstall for.
   */
  public void uninstallUI(JComponent c)
  {
    uninstallKeyboardActions();
    uninstallListeners();
    uninstallComponents();
    uninstallDefaults();

    optionPane = null;
  }

  /**
   * Applies the proper UI configuration to labels that are added to
   * the OptionPane.
   *
   * @param l the label to configure
   */
  private void configureLabel(JLabel l)
  {
    Color c = UIManager.getColor("OptionPane.messageForeground");
    if (c != null)
      l.setForeground(c);
    Font f = UIManager.getFont("OptionPane.messageFont");
    if (f != null)
      l.setFont(f);
  }
}
