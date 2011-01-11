/* JOptionPane.java
   Copyright (C) 2004, 2005, 2006, Free Software Foundation, Inc.

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

import java.awt.AWTEvent;
import java.awt.ActiveEvent;
import java.awt.Component;
import java.awt.Container;
import java.awt.EventQueue;
import java.awt.Frame;
import java.awt.MenuComponent;
import java.awt.Toolkit;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseMotionAdapter;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;
import javax.swing.plaf.OptionPaneUI;

/**
 * This class creates different types of JDialogs and JInternalFrames that can
 * ask users for input or pass on information. JOptionPane can be used by
 * calling one of the show static methods or  by creating an instance of
 * JOptionPane and calling createDialog or createInternalFrame.
 */
public class JOptionPane extends JComponent implements Accessible
{
  /**
   * Provides the accessibility features for the <code>JOptionPane</code>
   * component.
   */
  protected class AccessibleJOptionPane extends JComponent.AccessibleJComponent
  {
    private static final long serialVersionUID = 686071432213084821L;

    /**
     * Creates a new <code>AccessibleJOptionPane</code> instance.
     */
    protected AccessibleJOptionPane()
    {
      // Nothing to do here.
    }

    /**
     * Returns the accessible role of this object, which is always
     * {@link AccessibleRole#OPTION_PANE}.
     *
     * @return the accessible role of this object
     */
    public AccessibleRole getAccessibleRole()
    {
      return AccessibleRole.OPTION_PANE;
    }
  }

  private static final long serialVersionUID = 5231143276678566796L;

  /** The value returned when cancel option is selected. */
  public static final int CANCEL_OPTION = 2;

  /** The value returned when the dialog is closed without a selection. */
  public static final int CLOSED_OPTION = -1;

  /** An option used in confirmation dialog methods. */
  public static final int DEFAULT_OPTION = -1;

  /** The value returned when the no option is selected. */
  public static final int NO_OPTION = 1;

  /** An option used in confirmation dialog methods. */
  public static final int OK_CANCEL_OPTION = 2;

  /** The value returned when the ok option is selected. */
  public static final int OK_OPTION = 0;

  /** An option used in confirmation dialog methods. */
  public static final int YES_NO_CANCEL_OPTION = 1;

  /** An option used in confirmation dialog methods. */
  public static final int YES_NO_OPTION = 0;

  /** The value returned when the yes option is selected. */
  public static final int YES_OPTION = 0;

  /** Identifier for the error message type. */
  public static final int ERROR_MESSAGE = 0;

  /** Identifier for the information message type. */
  public static final int INFORMATION_MESSAGE = 1;

  /** Identifier for the plain message type. */
  public static final int PLAIN_MESSAGE = -1;

  /** Identifier for the question message type. */
  public static final int QUESTION_MESSAGE = 3;

  /** Identifier for the warning message type. */
  public static final int WARNING_MESSAGE = 2;

  /**
   * The identifier for the propertyChangeEvent when the icon property
   * changes.
   */
  public static final String ICON_PROPERTY = "icon";

  /**
   * The identifier for the propertyChangeEvent when the initialSelectionValue
   * property changes.
   */
  public static final String INITIAL_SELECTION_VALUE_PROPERTY = "initialSelectionValue";

  /**
   * The identifier for the propertyChangeEvent when the initialValue property
   * changes.
   */
  public static final String INITIAL_VALUE_PROPERTY = "initialValue";

  /**
   * The identifier for the propertyChangeEvent when the inputValue property
   * changes.
   */
  public static final String INPUT_VALUE_PROPERTY = "inputValue";

  /**
   * The identifier for the propertyChangeEvent when the message property
   * changes.
   */
  public static final String MESSAGE_PROPERTY = "message";

  /**
   * The identifier for the propertyChangeEvent when the messageType property
   * changes.
   */
  public static final String MESSAGE_TYPE_PROPERTY = "messageType";

  /**
   * The identifier for the propertyChangeEvent when the optionType property
   * changes.
   */
  public static final String OPTION_TYPE_PROPERTY = "optionType";

  /**
   * The identifier for the propertyChangeEvent when the options property
   * changes.
   */
  public static final String OPTIONS_PROPERTY = "options";

  /**
   * The identifier for the propertyChangeEvent when the selectionValues
   * property changes.
   */
  public static final String SELECTION_VALUES_PROPERTY = "selectionValues";

  /**
   * The identifier for the propertyChangeEvent when the value property
   * changes.
   */
  public static final String VALUE_PROPERTY = "value";

  /**
   * The identifier for the propertyChangeEvent when the wantsInput property
   * changes.
   */
  public static final String WANTS_INPUT_PROPERTY = "wantsInput";

  /** The value returned when the inputValue is uninitialized. */
  public static final Object UNINITIALIZED_VALUE = "uninitializedValue";

  /** The icon displayed in the dialog/internal frame. */
  protected Icon icon;

  /** The initial selected value in the input component. */
  protected Object initialSelectionValue;

  /** The object that is initially selected for options. */
  protected Object initialValue;

  /** The value the user inputs. */
  protected Object inputValue = UNINITIALIZED_VALUE;

  /** The message displayed in the dialog/internal frame. */
  protected Object message;

  /** The type of message displayed. */
  protected int messageType = PLAIN_MESSAGE;

  /**
   * The options (usually buttons) aligned at the bottom for the user to
   * select.
   */
  protected Object[] options;

  /** The type of options to display. */
  protected int optionType = DEFAULT_OPTION;

  /** The input values the user can select. */
  protected Object[] selectionValues;

  /** The value returned by selecting an option. */
  protected Object value = UNINITIALIZED_VALUE;

  /** Whether the Dialog/InternalFrame needs input. */
  protected boolean wantsInput;

  /** The common frame used when no parent is provided. */
  private static Frame privFrame = (Frame) SwingUtilities.getOwnerFrame(null);

  /**
   * Creates a new JOptionPane object using a message of "JOptionPane
   * message", using the PLAIN_MESSAGE type and DEFAULT_OPTION.
   */
  public JOptionPane()
  {
    this("JOptionPane message", PLAIN_MESSAGE, DEFAULT_OPTION, null, null, null);
  }

  /**
   * Creates a new JOptionPane object using the given message using the
   * PLAIN_MESSAGE type and DEFAULT_OPTION.
   *
   * @param message The message to display.
   */
  public JOptionPane(Object message)
  {
    this(message, PLAIN_MESSAGE, DEFAULT_OPTION, null, null, null);
  }

  /**
   * Creates a new JOptionPane object using the given message and messageType
   * and DEFAULT_OPTION.
   *
   * @param message The message to display.
   * @param messageType The type of message.
   */
  public JOptionPane(Object message, int messageType)
  {
    this(message, messageType, DEFAULT_OPTION, null, null, null);
  }

  /**
   * Creates a new JOptionPane object using the given message, messageType and
   * optionType.
   *
   * @param message The message to display.
   * @param messageType The type of message.
   * @param optionType The type of options.
   */
  public JOptionPane(Object message, int messageType, int optionType)
  {
    this(message, messageType, optionType, null, null, null);
  }

  /**
   * Creates a new JOptionPane object using the given message, messageType,
   * optionType and icon.
   *
   * @param message The message to display.
   * @param messageType The type of message.
   * @param optionType The type of options.
   * @param icon The icon to display.
   */
  public JOptionPane(Object message, int messageType, int optionType, Icon icon)
  {
    this(message, messageType, optionType, icon, null, null);
  }

  /**
   * Creates a new JOptionPane object using the given message, messageType,
   * optionType, icon and options.
   *
   * @param message The message to display.
   * @param messageType The type of message.
   * @param optionType The type of options.
   * @param icon The icon to display.
   * @param options The options given.
   */
  public JOptionPane(Object message, int messageType, int optionType,
                     Icon icon, Object[] options)
  {
    this(message, messageType, optionType, icon, options, null);
  }

  /**
   * Creates a new JOptionPane object using the given message, messageType,
   * optionType, icon, options and initialValue. The initialValue will be
   * focused initially.
   *
   * @param message The message to display.
   * @param messageType The type of message.
   * @param optionType The type of options.
   * @param icon The icon to display.
   * @param options The options given.
   * @param initialValue The component to focus on initially.
   *
   * @throws IllegalArgumentException If the messageType or optionType are not
   *         legal values.
   */
  public JOptionPane(Object message, int messageType, int optionType,
                     Icon icon, Object[] options, Object initialValue)
  {
    this.message = message;
    if (! validMessageType(messageType))
      throw new IllegalArgumentException("Message Type not legal value.");
    this.messageType = messageType;
    if (! validOptionType(optionType))
      throw new IllegalArgumentException("Option Type not legal value.");
    this.optionType = optionType;
    this.icon = icon;
    this.options = options;
    this.initialValue = initialValue;

    setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));

    updateUI();
  }

  /**
   * This method creates a new JDialog that is either centered around the
   * parent's frame or centered on the screen (if the parent is null). The
   * JDialog will not be resizable and will be modal. Once the JDialog is
   * disposed, the inputValue and value properties will  be set by the
   * optionPane.
   *
   * @param parentComponent The parent of the Dialog.
   * @param title The title in the bar of the JDialog.
   *
   * @return A new JDialog based on the JOptionPane configuration.
   */
  public JDialog createDialog(Component parentComponent, String title)
  {
    Frame toUse = getFrameForComponent(parentComponent);
    if (toUse == null)
      toUse = getRootFrame();

    JDialog dialog = new JDialog(toUse, title);
    inputValue = UNINITIALIZED_VALUE;
    value = UNINITIALIZED_VALUE;

    dialog.getContentPane().add(this);
    dialog.setModal(true);
    dialog.setResizable(false);
    dialog.pack();
    dialog.setLocationRelativeTo(parentComponent);

    addPropertyChangeListener(new ValuePropertyHandler(dialog));
    return dialog;
  }

  /**
   * Handles changes of the value property. Whenever this property changes,
   * the JOptionPane dialog should be closed.
   */
  private static class ValuePropertyHandler
    implements PropertyChangeListener
  {
    /**
     * The dialog to close.
     */
    JDialog dialog;

    /**
     * Creates a new instance.
     *
     * @param d the dialog to be closed
     */
    ValuePropertyHandler(JDialog d)
    {
      dialog = d;
    }

    /**
     * Receives notification when any of the properties change.
     */
    public void propertyChange(PropertyChangeEvent p)
    {
      String prop = p.getPropertyName();
      Object val = p.getNewValue();
      if (prop.equals(VALUE_PROPERTY) && val != null
          && val != UNINITIALIZED_VALUE)
        {
          dialog.setVisible(false);
        }
    }
  }

  /**
   * This method creates a new JInternalFrame that is in the JLayeredPane
   * which contains the parentComponent given. If no suitable JLayeredPane
   * can be found from the parentComponent given, a RuntimeException will be
   * thrown.
   *
   * @param parentComponent The parent to find a JDesktopPane from.
   * @param title The title of the JInternalFrame.
   *
   * @return A new JInternalFrame based on the JOptionPane configuration.
   *
   * @throws RuntimeException If no suitable JDesktopPane is found.
   *
   * @specnote The specification says that the internal frame is placed
   *           in the nearest <code>JDesktopPane</code> that is found in
   *           <code>parent</code>'s ancestors. The behaviour of the JDK
   *           is that it actually looks up the nearest
   *           <code>JLayeredPane</code> in <code>parent</code>'s ancestors.
   *           So do we.
   */
  public JInternalFrame createInternalFrame(Component parentComponent,
                                            String title)
                                     throws RuntimeException
  {
    // Try to find a JDesktopPane.
    JLayeredPane toUse = getDesktopPaneForComponent(parentComponent);
    // If we don't have a JDesktopPane, we try to find a JLayeredPane.
    if (toUse == null)
      toUse = JLayeredPane.getLayeredPaneAbove(parentComponent);
    // If this still fails, we throw a RuntimeException.
    if (toUse == null)
      throw new RuntimeException
        ("parentComponent does not have a valid parent");

    JInternalFrame frame = new JInternalFrame(title);

    inputValue = UNINITIALIZED_VALUE;
    value = UNINITIALIZED_VALUE;

    frame.setContentPane(this);
    frame.setClosable(true);

    toUse.add(frame);
    frame.setLayer(JLayeredPane.MODAL_LAYER);

    frame.pack();
    frame.setVisible(true);

    return frame;
  }

  /**
   * Returns the object that provides accessibility features for this
   * <code>JOptionPane</code> component.
   *
   * @return The accessible context (an instance of
   *     {@link AccessibleJOptionPane}).
   */
  public AccessibleContext getAccessibleContext()
  {
    if (accessibleContext == null)
      accessibleContext = new AccessibleJOptionPane();
    return accessibleContext;
  }

  /**
   * This method returns the JDesktopPane for the given parentComponent or
   * null if none can be found.
   *
   * @param parentComponent The component to look in.
   *
   * @return The JDesktopPane for the given component or null if none can be
   *         found.
   */
  public static JDesktopPane getDesktopPaneForComponent(Component parentComponent)
  {
    return (JDesktopPane) SwingUtilities.getAncestorOfClass(JDesktopPane.class,
                                                            parentComponent);
  }

  /**
   * This method returns the Frame for the given parentComponent or null if
   * none can be found.
   *
   * @param parentComponent The component to look in.
   *
   * @return The Frame for the given component or null if none can be found.
   */
  public static Frame getFrameForComponent(Component parentComponent)
  {
    return (Frame) SwingUtilities.getAncestorOfClass(Frame.class,
                                                     parentComponent);
  }

  /**
   * This method returns the icon displayed.
   *
   * @return The icon displayed.
   */
  public Icon getIcon()
  {
    return icon;
  }

  /**
   * This method returns the value initially selected from the list of values
   * the user can input.
   *
   * @return The initial selection value.
   */
  public Object getInitialSelectionValue()
  {
    return initialSelectionValue;
  }

  /**
   * This method returns the value that is focused from the list of options.
   *
   * @return The initial value from options.
   */
  public Object getInitialValue()
  {
    return initialValue;
  }

  /**
   * This method returns the value that the user input.
   *
   * @return The user's input value.
   */
  public Object getInputValue()
  {
    if (getValue().equals(new Integer(CANCEL_OPTION)))
      setInputValue(null);
    return inputValue;
  }

  /**
   * This method returns the maximum characters per line. By default, this is
   * Integer.MAX_VALUE.
   *
   * @return The maximum characters per line.
   */
  public int getMaxCharactersPerLineCount()
  {
    return Integer.MAX_VALUE;
  }

  /**
   * This method returns the message displayed.
   *
   * @return The message displayed.
   */
  public Object getMessage()
  {
    return message;
  }

  /**
   * This method returns the message type.
   *
   * @return The message type.
   */
  public int getMessageType()
  {
    return messageType;
  }

  /**
   * This method returns the options.
   *
   * @return The options.
   */
  public Object[] getOptions()
  {
    return options;
  }

  /**
   * This method returns the option type.
   *
   * @return The option type.
   */
  public int getOptionType()
  {
    return optionType;
  }

  /**
   * This method returns the Frame used by JOptionPane dialog's that have no
   * parent.
   *
   * @return The Frame used by dialogs that have no parent.
   */
  public static Frame getRootFrame()
  {
    return privFrame;
  }

  /**
   * This method returns the selection values.
   *
   * @return The selection values.
   */
  public Object[] getSelectionValues()
  {
    return selectionValues;
  }

  /**
   * This method returns the UI used by the JOptionPane.
   *
   * @return The UI used by the JOptionPane.
   */
  public OptionPaneUI getUI()
  {
    return (OptionPaneUI) ui;
  }

  /**
   * This method returns an identifier to determine which UI class will act as
   * the UI.
   *
   * @return The UI identifier.
   */
  public String getUIClassID()
  {
    return "OptionPaneUI";
  }

  /**
   * This method returns the value that the user selected out of options.
   *
   * @return The value that the user selected out of options.
   */
  public Object getValue()
  {
    return value;
  }

  /**
   * This method returns whether this JOptionPane wants input.
   *
   * @return Whether this JOptionPane wants input.
   */
  public boolean getWantsInput()
  {
    return wantsInput;
  }

  /**
   * This method returns a String that describes this JOptionPane.
   *
   * @return A String that describes this JOptionPane.
   */
  protected String paramString()
  {
    return "JOptionPane";
  }

  /**
   * This method requests focus for the initial value.
   */
  public void selectInitialValue()
  {
    if (ui != null)
      ((OptionPaneUI) ui).selectInitialValue(this);
  }

  /**
   * This method changes the icon property.
   *
   * @param newIcon The new icon to use.
   */
  public void setIcon(Icon newIcon)
  {
    if (icon != newIcon)
      {
        Icon old = icon;
        icon = newIcon;
        firePropertyChange(ICON_PROPERTY, old, icon);
      }
  }

  /**
   * This method changes the initial selection property.
   *
   * @param newValue The new initial selection.
   */
  public void setInitialSelectionValue(Object newValue)
  {
    if (initialSelectionValue != newValue)
      {
        Object old = initialSelectionValue;
        initialSelectionValue = newValue;
        firePropertyChange(INITIAL_SELECTION_VALUE_PROPERTY, old,
                           initialSelectionValue);
      }
  }

  /**
   * This method changes the initial value property.
   *
   * @param newValue The new initial value.
   */
  public void setInitialValue(Object newValue)
  {
    if (initialValue != newValue)
      {
        Object old = initialValue;
        initialValue = newValue;
        firePropertyChange(INITIAL_VALUE_PROPERTY, old, initialValue);
      }
  }

  /**
   * This method changes the inputValue property.
   *
   * @param newValue The new inputValue.
   */
  public void setInputValue(Object newValue)
  {
    if (inputValue != newValue)
      {
        Object old = inputValue;
        inputValue = newValue;
        firePropertyChange(INPUT_VALUE_PROPERTY, old, inputValue);
      }
  }

  /**
   * This method changes the message property.
   *
   * @param newMessage The new message.
   */
  public void setMessage(Object newMessage)
  {
    if (message != newMessage)
      {
        Object old = message;
        message = newMessage;
        firePropertyChange(MESSAGE_PROPERTY, old, message);
      }
  }

  /**
   * This method changes the messageType property.
   *
   * @param newType The new messageType.
   *
   * @throws IllegalArgumentException If the messageType is not valid.
   */
  public void setMessageType(int newType)
  {
    if (! validMessageType(newType))
      throw new IllegalArgumentException("Message Type not legal value.");
    if (newType != messageType)
      {
        int old = messageType;
        messageType = newType;
        firePropertyChange(MESSAGE_TYPE_PROPERTY, old, messageType);
      }
  }

  /**
   * This method changes the options property.
   *
   * @param newOptions The new options.
   */
  public void setOptions(Object[] newOptions)
  {
    if (options != newOptions)
      {
        Object[] old = options;
        options = newOptions;
        firePropertyChange(OPTIONS_PROPERTY, old, options);
      }
  }

  /**
   * This method changes the optionType property.
   *
   * @param newType The new optionType.
   *
   * @throws IllegalArgumentException If the optionType is not valid.
   */
  public void setOptionType(int newType)
  {
    if (! validOptionType(newType))
      throw new IllegalArgumentException("Option Type not legal value.");
    if (newType != optionType)
      {
        int old = optionType;
        optionType = newType;
        firePropertyChange(OPTION_TYPE_PROPERTY, old, optionType);
      }
  }

  /**
   * This method changes the Frame used for JOptionPane dialogs that have no
   * parent.
   *
   * @param newRootFrame The Frame to use for dialogs that have no parent.
   */
  public static void setRootFrame(Frame newRootFrame)
  {
    privFrame = newRootFrame;
  }

  /**
   * This method changes the selectionValues property.
   *
   * @param newValues The new selectionValues.
   */
  public void setSelectionValues(Object[] newValues)
  {
    if (newValues != selectionValues)
      {
        if (newValues != null)
          wantsInput = true;
        Object[] old = selectionValues;
        selectionValues = newValues;
        firePropertyChange(SELECTION_VALUES_PROPERTY, old, selectionValues);
      }
  }

  /**
   * This method sets the UI used with the JOptionPane.
   *
   * @param ui The UI used with the JOptionPane.
   */
  public void setUI(OptionPaneUI ui)
  {
    super.setUI(ui);
  }

  /**
   * This method sets the value has been selected out of options.
   *
   * @param newValue The value that has been selected out of options.
   */
  public void setValue(Object newValue)
  {
    if (value != newValue)
      {
        Object old = value;
        value = newValue;
        firePropertyChange(VALUE_PROPERTY, old, value);
      }
  }

  /**
   * This method changes the wantsInput property.
   *
   * @param newValue Whether this JOptionPane requires input.
   */
  public void setWantsInput(boolean newValue)
  {
    if (wantsInput != newValue)
      {
        boolean old = wantsInput;
        wantsInput = newValue;
        firePropertyChange(WANTS_INPUT_PROPERTY, old, wantsInput);
      }
  }

  /**
   * This method shows a confirmation dialog with the title "Select an Option"
   * and displays the given message. The parent frame will be the same as the
   * parent frame of the given parentComponent. This method returns the
   * option chosen by the user.
   *
   * @param parentComponent The parentComponent to find a frame in.
   * @param message The message to display.
   *
   * @return The option that was selected.
   */
  public static int showConfirmDialog(Component parentComponent, Object message)
  {
    JOptionPane pane = new JOptionPane(message, QUESTION_MESSAGE);
    JDialog dialog = pane.createDialog(parentComponent, "Select an Option");
    dialog.show();

    if (pane.getValue() instanceof Integer)
      return ((Integer) pane.getValue()).intValue();
    return -1;
  }

  /**
   * This method shows a confirmation dialog with the given message,
   * optionType and title. The frame that owns the dialog will be the same
   * frame that holds the given parentComponent. This method returns the
   * option that was chosen.
   *
   * @param parentComponent The component to find a frame in.
   * @param message The message displayed.
   * @param title The title of the dialog.
   * @param optionType The optionType.
   *
   * @return The option that was chosen.
   */
  public static int showConfirmDialog(Component parentComponent,
                                      Object message, String title,
                                      int optionType)
  {
    JOptionPane pane = new JOptionPane(message, PLAIN_MESSAGE, optionType);
    JDialog dialog = pane.createDialog(parentComponent, title);
    dialog.show();

    if (pane.getValue() instanceof Integer)
      return ((Integer) pane.getValue()).intValue();
    return -1;
  }

  /**
   * This method shows a confirmation dialog with the given message, title,
   * messageType and optionType. The frame owner will be the same frame as
   * the one that holds the given parentComponent. This method returns the
   * option selected by the user.
   *
   * @param parentComponent The component to find a frame in.
   * @param message The message displayed.
   * @param title The title of the dialog.
   * @param optionType The optionType.
   * @param messageType The messageType.
   *
   * @return The selected option.
   */
  public static int showConfirmDialog(Component parentComponent,
                                      Object message, String title,
                                      int optionType, int messageType)
  {
    JOptionPane pane = new JOptionPane(message, messageType, optionType);
    JDialog dialog = pane.createDialog(parentComponent, title);
    dialog.show();

    if (pane.getValue() instanceof Integer)
      return ((Integer) pane.getValue()).intValue();
    return -1;
  }

  /**
   * This method shows a confirmation dialog with the given message, title,
   * optionType, messageType and icon. The frame owner will be the same as
   * the one that holds the given parentComponent. This method returns the
   * option selected by the user.
   *
   * @param parentComponent The component to find a frame in.
   * @param message The message displayed.
   * @param title The title of the dialog.
   * @param optionType The optionType.
   * @param messageType The messsageType.
   * @param icon The icon displayed.
   *
   * @return The selected option.
   */
  public static int showConfirmDialog(Component parentComponent,
                                      Object message, String title,
                                      int optionType, int messageType,
                                      Icon icon)
  {
    JOptionPane pane = new JOptionPane(message, messageType, optionType, icon);
    JDialog dialog = pane.createDialog(parentComponent, title);
    dialog.show();

    if (pane.getValue() instanceof Integer)
      return ((Integer) pane.getValue()).intValue();
    return -1;
  }

  /**
   * This method will show a QUESTION_MESSAGE input dialog with the given
   * message. No selectionValues is set so the Look and Feel will usually
   * give the user a TextField to fill out. The frame owner will be the same
   * frame that holds the given parentComponent. This method will return the
   * value entered by the user.
   *
   * @param parentComponent The component to find a frame in.
   * @param message The message displayed.
   *
   * @return The value entered by the user.
   */
  public static String showInputDialog(Component parentComponent,
                                       Object message)
  {
    JOptionPane pane = new JOptionPane(message, QUESTION_MESSAGE);
    pane.setWantsInput(true);
    JDialog dialog = pane.createDialog(parentComponent, null);
    dialog.show();

    return (String) pane.getInputValue();
  }

  /**
   * This method will show a QUESTION_MESSAGE type input dialog with the given
   * message and initialSelectionValue. Since there is no selectionValues
   * set, the Look and Feel will usually give a TextField to fill out. The
   * frame owner will be the same as the one that holds the given
   * parentComponent. This method will return the value entered by the user.
   *
   * @param parentComponent The component to find a frame in.
   * @param message The message to display.
   * @param initialSelectionValue The initially selected value.
   *
   * @return The value the user input.
   */
  public static String showInputDialog(Component parentComponent,
                                       Object message,
                                       Object initialSelectionValue)
  {
    JOptionPane pane = new JOptionPane(message, QUESTION_MESSAGE);
    pane.setInitialSelectionValue(initialSelectionValue);
    pane.setWantsInput(true);
    JDialog dialog = pane.createDialog(parentComponent, null);
    dialog.show();

    return (String) pane.getInputValue();
  }

  /**
   * This method displays a new input dialog with the given message, title and
   * messageType. Since no selectionValues value is given, the Look and Feel
   * will usually give the user a TextField to input data to. This method
   * returns the value the user inputs.
   *
   * @param parentComponent The component to find a frame in.
   * @param message The message to display.
   * @param title The title of the dialog.
   * @param messageType The messageType.
   *
   * @return The value the user input.
   */
  public static String showInputDialog(Component parentComponent,
                                       Object message, String title,
                                       int messageType)
  {
    JOptionPane pane = new JOptionPane(message, messageType);
    pane.setWantsInput(true);
    JDialog dialog = pane.createDialog(parentComponent, title);
    dialog.show();

    return (String) pane.getInputValue();
  }

  /**
   * This method shows an input dialog with the given message, title,
   * messageType, icon, selectionValues, and initialSelectionValue. This
   * method returns the value that the user selects.
   *
   * @param parentComponent The component to find a frame in.
   * @param message The message displayed.
   * @param title The title of the dialog.
   * @param messageType The messageType.
   * @param icon The icon displayed.
   * @param selectionValues The list of values to select from.
   * @param initialSelectionValue The initially selected value.
   *
   * @return The user selected value.
   */
  public static Object showInputDialog(Component parentComponent,
                                       Object message, String title,
                                       int messageType, Icon icon,
                                       Object[] selectionValues,
                                       Object initialSelectionValue)
  {
    JOptionPane pane = new JOptionPane(message, messageType);
    pane.setWantsInput(true);
    pane.setIcon(icon);
    pane.setSelectionValues(selectionValues);
    pane.setInitialSelectionValue(initialSelectionValue);
    JDialog dialog = pane.createDialog(parentComponent, title);
    dialog.show();

    return pane.getInputValue();
  }

  /**
   * This method shows a QUESTION_MESSAGE type input dialog. Since no
   * selectionValues is set, the Look and Feel will usually give the user a
   * TextField to input data to. This method returns the value the user
   * inputs.
   *
   * @param message The message to display.
   *
   * @return The user selected value.
   */
  public static String showInputDialog(Object message)
  {
    JOptionPane pane = new JOptionPane(message, QUESTION_MESSAGE);
    pane.setWantsInput(true);
    JDialog dialog = pane.createDialog(null, null);
    dialog.show();

    return (String) pane.getInputValue();
  }

  /**
   * This method shows a QUESTION_MESSAGE type input dialog. Since no
   * selectionValues is set, the Look and Feel will usually give the user a
   * TextField to input data to. The input component will be initialized with
   * the initialSelectionValue. This method returns the value the user
   * inputs.
   *
   * @param message The message to display.
   * @param initialSelectionValue The initialSelectionValue.
   *
   * @return The user selected value.
   */
  public static String showInputDialog(Object message,
                                       Object initialSelectionValue)
  {
    JOptionPane pane = new JOptionPane(message, QUESTION_MESSAGE);
    pane.setWantsInput(true);
    pane.setInitialSelectionValue(initialSelectionValue);
    JDialog dialog = pane.createDialog(null, null);
    dialog.show();

    return (String) pane.getInputValue();
  }

  /**
   * This method shows an internal confirmation dialog with the given message.
   * The internal frame dialog will be placed in the first JDesktopPane
   * ancestor of the given parentComponent. This method will return the value
   * selected.
   *
   * @param parentComponent The parent to find a JDesktopPane in.
   * @param message The message to display.
   *
   * @return The value selected.
   */
  public static int showInternalConfirmDialog(Component parentComponent,
                                              Object message)
  {
    JOptionPane pane = new JOptionPane(message);
    JInternalFrame frame = pane.createInternalFrame(parentComponent, null);

    startModal(frame);

    if (pane.getValue() instanceof Integer)
      return ((Integer) pane.getValue()).intValue();
    return -1;
  }

  /**
   * This method shows an internal confirmation dialog with the given message,
   * optionType and title. The internal frame dialog will be placed in the
   * first JDesktopPane ancestor of the given parentComponent.  This method
   * will return the selected value.
   *
   * @param parentComponent The parent to find a JDesktopPane in.
   * @param message The message to display.
   * @param title The title to display.
   * @param optionType The option type.
   *
   * @return The selected value.
   */
  public static int showInternalConfirmDialog(Component parentComponent,
                                              Object message, String title,
                                              int optionType)
  {
    JOptionPane pane = new JOptionPane(message, PLAIN_MESSAGE, optionType);
    JInternalFrame frame = pane.createInternalFrame(parentComponent, title);

    startModal(frame);

    if (pane.getValue() instanceof Integer)
      return ((Integer) pane.getValue()).intValue();
    return -1;
  }

  /**
   * This method shows an internal confirmation dialog with the given message,
   * title, optionTypes and icon for the given message type. The internal
   * confirmation dialog will be placed in the first  instance of
   * JDesktopPane ancestor of the given parentComponent.
   *
   * @param parentComponent The component to find a JDesktopPane in.
   * @param message The message to display.
   * @param title The title of the dialog.
   * @param optionType The option type.
   * @param messageType The message type.
   *
   * @return The selected value.
   */
  public static int showInternalConfirmDialog(Component parentComponent,
                                              Object message, String title,
                                              int optionType, int messageType)
  {
    JOptionPane pane = new JOptionPane(message, messageType, optionType);
    JInternalFrame frame = pane.createInternalFrame(parentComponent, title);

    startModal(frame);

    if (pane.getValue() instanceof Integer)
      return ((Integer) pane.getValue()).intValue();
    return -1;
  }

  /**
   * This method shows an internal confirmation dialog with the given message,
   * title, option type, message type, and icon. The internal frame dialog
   * will be placed in the first JDesktopPane ancestor  that is found in the
   * given parentComponent. This method returns  the selected value.
   *
   * @param parentComponent The parent to find a JDesktopPane in.
   * @param message The message to display.
   * @param title The title to display.
   * @param optionType The option type.
   * @param messageType The message type.
   * @param icon The icon to display.
   *
   * @return The selected value.
   */
  public static int showInternalConfirmDialog(Component parentComponent,
                                              Object message, String title,
                                              int optionType, int messageType,
                                              Icon icon)
  {
    JOptionPane pane = new JOptionPane(message, messageType, optionType, icon);
    JInternalFrame frame = pane.createInternalFrame(parentComponent, title);

    startModal(frame);

    if (pane.getValue() instanceof Integer)
      return ((Integer) pane.getValue()).intValue();
    return -1;
  }

  /**
   * This method shows an internal input dialog with the given message. The
   * internal frame dialog will be placed in the first JDesktopPane ancestor
   * of the given parent component. This method returns the value input by
   * the user.
   *
   * @param parentComponent The parent to find a JDesktopPane in.
   * @param message The message to display.
   *
   * @return The user selected value.
   */
  public static String showInternalInputDialog(Component parentComponent,
                                               Object message)
  {
    JOptionPane pane = new JOptionPane(message);
    pane.setWantsInput(true);
    JInternalFrame frame = pane.createInternalFrame(parentComponent, null);

    startModal(frame);

    return (String) pane.getInputValue();
  }

  /**
   * This method shows an internal input dialog with the given message,  title
   * and message type. The internal input dialog will be placed in the first
   * JDesktopPane ancestor found in the given parent component. This method
   * will return the input value given by the user.
   *
   * @param parentComponent The component to find a JDesktopPane in.
   * @param message The message to display.
   * @param title The title to display.
   * @param messageType The message type.
   *
   * @return The user input value.
   */
  public static String showInternalInputDialog(Component parentComponent,
                                               Object message, String title,
                                               int messageType)
  {
    JOptionPane pane = new JOptionPane(message, messageType);
    pane.setWantsInput(true);
    JInternalFrame frame = pane.createInternalFrame(parentComponent, title);

    startModal(frame);

    return (String) pane.getInputValue();
  }

  /**
   * This method shows an internal input dialog with the given message, title
   * message type, icon, selection value list and initial selection value.
   * The internal frame dialog will be placed in the first JDesktopPane
   * ancestor found in the given parent component. This method returns the
   * input value from the user.
   *
   * @param parentComponent The parent to find a JDesktopPane in.
   * @param message The message to display.
   * @param title The title to display.
   * @param messageType The message type.
   * @param icon The icon to display.
   * @param selectionValues The selection value list.
   * @param initialSelectionValue The initial selection value.
   *
   * @return The user input value.
   */
  public static Object showInternalInputDialog(Component parentComponent,
                                               Object message, String title,
                                               int messageType, Icon icon,
                                               Object[] selectionValues,
                                               Object initialSelectionValue)
  {
    JOptionPane pane = new JOptionPane(message, messageType);
    pane.setWantsInput(true);
    pane.setIcon(icon);
    pane.setSelectionValues(selectionValues);
    pane.setInitialSelectionValue(initialSelectionValue);
    JInternalFrame frame = pane.createInternalFrame(parentComponent, title);

    startModal(frame);

    return pane.getInputValue();
  }

  /**
   * This method shows an internal message dialog with the given message. The
   * internal frame dialog will be placed in the first JDesktopPane ancestor
   * found in the given parent component.
   *
   * @param parentComponent The component to find a JDesktopPane in.
   * @param message The message to display.
   */
  public static void showInternalMessageDialog(Component parentComponent,
                                               Object message)
  {
    JOptionPane pane = new JOptionPane(message);
    JInternalFrame frame = pane.createInternalFrame(parentComponent, null);

    startModal(frame);
  }

  /**
   * This method shows an internal message dialog with the given message,
   * title and message type. The internal message dialog is placed in the
   * first JDesktopPane ancestor found in the given parent component.
   *
   * @param parentComponent The parent component to find a JDesktopPane in.
   * @param message The message to display.
   * @param title The title to display.
   * @param messageType The message type.
   */
  public static void showInternalMessageDialog(Component parentComponent,
                                               Object message, String title,
                                               int messageType)
  {
    JOptionPane pane = new JOptionPane(message, messageType);
    JInternalFrame frame = pane.createInternalFrame(parentComponent, title);

    startModal(frame);
  }

  /**
   * This method shows an internal message dialog with the given message,
   * title, message type and icon. The internal message dialog is placed in
   * the first JDesktopPane ancestor found in the given parent component.
   *
   * @param parentComponent The component to find a JDesktopPane in.
   * @param message The message to display.
   * @param title The title to display.
   * @param messageType The message type.
   * @param icon The icon to display.
   */
  public static void showInternalMessageDialog(Component parentComponent,
                                               Object message, String title,
                                               int messageType, Icon icon)
  {
    JOptionPane pane = new JOptionPane(message, messageType);
    pane.setIcon(icon);
    JInternalFrame frame = pane.createInternalFrame(parentComponent, title);

    startModal(frame);
  }

  /**
   * This method displays an internal option dialog with the given message,
   * title, option type, message type, icon, option list, and initial option
   * value. The internal option dialog is placed in the first JDesktopPane
   * ancestor found in the parent component. This method returns the option
   * selected.
   *
   * @param parentComponent The parent to find a JDesktopPane in.
   * @param message The message displayed.
   * @param title The title displayed.
   * @param optionType The option type.
   * @param messageType The message type.
   * @param icon The icon to display.
   * @param options The array of options.
   * @param initialValue The initial value selected.
   *
   * @return The option that was selected.
   */
  public static int showInternalOptionDialog(Component parentComponent,
                                             Object message, String title,
                                             int optionType, int messageType,
                                             Icon icon, Object[] options,
                                             Object initialValue)
  {
    JOptionPane pane = new JOptionPane(message, messageType, optionType, icon,
                                       options, initialValue);

    JInternalFrame frame = pane.createInternalFrame(parentComponent, title);

    startModal(frame);

    if (pane.getValue() instanceof Integer)
      return ((Integer) pane.getValue()).intValue();
    return -1;
  }

  /**
   * This method shows an INFORMATION_MESSAGE type message dialog.
   *
   * @param parentComponent The component to find a frame in.
   * @param message The message displayed.
   */
  public static void showMessageDialog(Component parentComponent,
                                       Object message)
  {
    JOptionPane pane = new JOptionPane(message, INFORMATION_MESSAGE);
    JDialog dialog = pane.createDialog(parentComponent, null);
    dialog.show();
  }

  /**
   * This method shows a message dialog with the given message, title and
   * messageType.
   *
   * @param parentComponent The component to find a frame in.
   * @param message The message displayed.
   * @param title The title of the dialog.
   * @param messageType The messageType.
   */
  public static void showMessageDialog(Component parentComponent,
                                       Object message, String title,
                                       int messageType)
  {
    JOptionPane pane = new JOptionPane(message, messageType);
    JDialog dialog = pane.createDialog(parentComponent, title);
    dialog.show();
  }

  /**
   * This method shows a message dialog with the given message, title,
   * messageType and icon.
   *
   * @param parentComponent The component to find a frame in.
   * @param message The message displayed.
   * @param title The title of the dialog.
   * @param messageType The messageType.
   * @param icon The icon displayed.
   */
  public static void showMessageDialog(Component parentComponent,
                                       Object message, String title,
                                       int messageType, Icon icon)
  {
    JOptionPane pane = new JOptionPane(message, messageType);
    pane.setIcon(icon);
    JDialog dialog = pane.createDialog(parentComponent, title);
    dialog.show();
  }

  /**
   * This method shows an option dialog with the given message, title,
   * optionType, messageType, icon, options and initialValue. This method
   * returns the option that was selected.
   *
   * @param parentComponent The component to find a frame in.
   * @param message The message displayed.
   * @param title The title of the dialog.
   * @param optionType The optionType.
   * @param messageType The messageType.
   * @param icon The icon displayed.
   * @param options The options to choose from.
   * @param initialValue The initial value.
   *
   * @return The selected option.
   */
  public static int showOptionDialog(Component parentComponent,
                                     Object message, String title,
                                     int optionType, int messageType,
                                     Icon icon, Object[] options,
                                     Object initialValue)
  {
    JOptionPane pane = new JOptionPane(message, messageType, optionType, icon,
                                       options, initialValue);

    JDialog dialog = pane.createDialog(parentComponent, title);
    dialog.show();

    if (pane.getValue() instanceof Integer)
      return ((Integer) pane.getValue()).intValue();
    return -1;
  }

  /**
   * This method resets the UI to the Look and Feel default.
   */
  public void updateUI()
  {
    setUI((OptionPaneUI) UIManager.getUI(this));
  }

  /**
   * This method returns true if the key is a valid messageType.
   *
   * @param key The key to check.
   *
   * @return True if key is valid.
   */
  private boolean validMessageType(int key)
  {
    switch (key)
      {
      case ERROR_MESSAGE:
      case INFORMATION_MESSAGE:
      case PLAIN_MESSAGE:
      case QUESTION_MESSAGE:
      case WARNING_MESSAGE:
        return true;
      }
    return false;
  }

  /**
   * This method returns true if the key is a valid optionType.
   *
   * @param key The key to check.
   *
   * @return True if key is valid.
   */
  private boolean validOptionType(int key)
  {
    switch (key)
      {
      case DEFAULT_OPTION:
      case OK_CANCEL_OPTION:
      case YES_NO_CANCEL_OPTION:
      case YES_NO_OPTION:
        return true;
      }
    return false;
  }

  /**
   * This helper method makes the JInternalFrame wait until it is notified by
   * an InternalFrameClosing event. This method also adds the given
   * JOptionPane to the JInternalFrame and sizes it according to the
   * JInternalFrame's preferred size.
   *
   * @param f The JInternalFrame to make modal.
   */
  private static void startModal(JInternalFrame f)
  {
    // We need to add an additional glasspane-like component directly
    // below the frame, which intercepts all mouse events that are not
    // directed at the frame itself.
    JPanel modalInterceptor = new JPanel();
    modalInterceptor.setOpaque(false);
    JLayeredPane lp = JLayeredPane.getLayeredPaneAbove(f);
    lp.setLayer(modalInterceptor, JLayeredPane.MODAL_LAYER.intValue());
    modalInterceptor.setBounds(0, 0, lp.getWidth(), lp.getHeight());
    modalInterceptor.addMouseListener(new MouseAdapter(){});
    modalInterceptor.addMouseMotionListener(new MouseMotionAdapter(){});
    lp.add(modalInterceptor);
    f.toFront();

    // We need to explicitly dispatch events when we are blocking the event
    // dispatch thread.
    EventQueue queue = Toolkit.getDefaultToolkit().getSystemEventQueue();
    try
      {
        while (! f.isClosed())
          {
            if (EventQueue.isDispatchThread())
              {
                // The getNextEventMethod() issues wait() when no
                // event is available, so we don't need do explicitly wait().
                AWTEvent ev = queue.getNextEvent();
                // This mimics EventQueue.dispatchEvent(). We can't use
                // EventQueue.dispatchEvent() directly, because it is
                // protected, unfortunately.
                if (ev instanceof ActiveEvent)
                  ((ActiveEvent) ev).dispatch();
                else if (ev.getSource() instanceof Component)
                  ((Component) ev.getSource()).dispatchEvent(ev);
                else if (ev.getSource() instanceof MenuComponent)
                  ((MenuComponent) ev.getSource()).dispatchEvent(ev);
                // Other events are ignored as per spec in
                // EventQueue.dispatchEvent
              }
            else
              {
                // Give other threads a chance to become active.
                Thread.yield();
              }
          }
      }
    catch (InterruptedException ex)
      {
        // If we get interrupted, then leave the modal state.
      }
    finally
      {
        // Clean up the modal interceptor.
        lp.remove(modalInterceptor);

        // Remove the internal frame from its parent, so it is no longer
        // lurking around and clogging memory.
        Container parent = f.getParent();
        if (parent != null)
          parent.remove(f);
      }
  }
}
