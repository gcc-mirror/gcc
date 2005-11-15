/* BasicComboBoxUI.java --
   Copyright (C) 2004, 2005  Free Software Foundation, Inc.

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

import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.LayoutManager;
import java.awt.Rectangle;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.accessibility.Accessible;
import javax.swing.CellRendererPane;
import javax.swing.ComboBoxEditor;
import javax.swing.ComboBoxModel;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JList;
import javax.swing.ListCellRenderer;
import javax.swing.LookAndFeel;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.event.ListDataEvent;
import javax.swing.event.ListDataListener;
import javax.swing.plaf.ComboBoxUI;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.UIResource;

/**
 * A UI delegate for the {@link JComboBox} component.
 *
 * @author Olga Rodimina
 * @author Robert Schuster
 */
public class BasicComboBoxUI extends ComboBoxUI
{
  /**
   * The arrow button that is displayed in the right side of JComboBox. This
   * button is used to hide and show combo box's list of items.
   */
  protected JButton arrowButton;

  /**
   * The combo box represented by this UI delegate.
   */
  protected JComboBox comboBox;

  /**
   * The component that is responsible for displaying/editing the selected 
   * item of the combo box. 
   * 
   * @see BasicComboBoxEditor#getEditorComponent()
   */
  protected Component editor;

  /**
   * A listener listening to focus events occurring in the {@link JComboBox}.
   */
  protected FocusListener focusListener;

  /**
   * A flag indicating whether JComboBox currently has the focus.
   */
  protected boolean hasFocus;

  /**
   * A listener listening to item events fired by the {@link JComboBox}.
   */
  protected ItemListener itemListener;

  /**
   * A listener listening to key events that occur while {@link JComboBox} has
   * the focus.
   */
  protected KeyListener keyListener;

  /**
   * A listener listening to mouse events occuring in the {@link JComboBox}.
   */
  private MouseListener mouseListener;

  /**
   * List used when rendering selected item of the combo box. The selection
   * and foreground colors for combo box renderer are configured from this
   * list.
   */
  protected JList listBox;

  /**
   * ListDataListener listening to JComboBox model
   */
  protected ListDataListener listDataListener;

  /**
   * Popup list containing the combo box's menu items.
   */
  protected ComboPopup popup;
  
  protected KeyListener popupKeyListener;
  
  protected MouseListener popupMouseListener;
  
  protected MouseMotionListener popupMouseMotionListener;

  /**
   * Listener listening to changes in the bound properties of JComboBox
   */
  protected PropertyChangeListener propertyChangeListener;

  /** 
   * The button background. 
   * @see #installDefaults()
   */
  private Color buttonBackground;
  
  /** 
   * The button shadow. 
   * @see #installDefaults()
   */
  private Color buttonShadow;
  
  /**
   * The button dark shadow.
   * @see #installDefaults()
   */
  private Color buttonDarkShadow;

  /**
   * The button highlight.
   * @see #installDefaults()
   */
  private Color buttonHighlight;

  /* Size of the largest item in the comboBox
   * This is package-private to avoid an accessor method.
   */
  Dimension displaySize;

  // FIXME: This fields aren't used anywhere at this moment.
  protected Dimension cachedMinimumSize;
  protected CellRendererPane currentValuePane;
  protected boolean isMinimumSizeDirty;

  /**
   * Creates a new <code>BasicComboBoxUI</code> object.
   */
  public BasicComboBoxUI()
  {
    // Nothing to do here.
  }

  /**
   * A factory method to create a UI delegate for the given 
   * {@link JComponent}, which should be a {@link JComboBox}.
   *
   * @param c The {@link JComponent} a UI is being created for.
   *
   * @return A UI delegate for the {@link JComponent}.
   */
  public static ComponentUI createUI(JComponent c)
  {
    return new BasicComboBoxUI();
  }

  /**
   * Installs the UI for the given {@link JComponent}.
   *
   * @param c  the JComponent to install a UI for.
   * 
   * @see #uninstallUI(JComponent)
   */
  public void installUI(JComponent c)
  {
    super.installUI(c);

    if (c instanceof JComboBox)
      {
        comboBox = (JComboBox) c;
        comboBox.setOpaque(true);
        comboBox.setLayout(createLayoutManager());
        installDefaults();
        installComponents();
        installListeners();
        installKeyboardActions();
      }
  }

  /**
   * Uninstalls the UI for the given {@link JComponent}.
   *
   * @param c The JComponent that is having this UI removed.
   * 
   * @see #installUI(JComponent)
   */
  public void uninstallUI(JComponent c)
  {
    uninstallKeyboardActions();
    uninstallListeners();
    uninstallComponents();
    uninstallDefaults();
    comboBox = null;
  }

  /**
   * Installs the defaults that are defined in the {@link BasicLookAndFeel} 
   * for this {@link JComboBox}.
   * 
   * @see #uninstallDefaults()
   */
  protected void installDefaults()
  {
    LookAndFeel.installColorsAndFont(comboBox, "ComboBox.background",
                                     "ComboBox.foreground", "ComboBox.font");
    
    // fetch the button color scheme
    buttonBackground = UIManager.getColor("ComboBox.buttonBackground");
    buttonShadow = UIManager.getColor("ComboBox.buttonShadow");
    buttonDarkShadow = UIManager.getColor("ComboBox.buttonDarkShadow");
    buttonHighlight = UIManager.getColor("ComboBox.buttonHighlight");
  }

  /**
   * Creates and installs the listeners for this UI.
   * 
   * @see #uninstallListeners()
   */
  protected void installListeners()
  {
    // install combo box's listeners
    propertyChangeListener = createPropertyChangeListener();
    comboBox.addPropertyChangeListener(propertyChangeListener);

    focusListener = createFocusListener();
    comboBox.addFocusListener(focusListener);
    listBox.addFocusListener(focusListener);

    itemListener = createItemListener();
    comboBox.addItemListener(itemListener);

    keyListener = createKeyListener();
    comboBox.addKeyListener(keyListener);

    mouseListener = createMouseListener();
    arrowButton.addMouseListener(mouseListener);

    // install listeners that listen to combo box model
    listDataListener = createListDataListener();
    comboBox.getModel().addListDataListener(listDataListener);
  }

  /**
   * Uninstalls the defaults and sets any objects created during
   * install to <code>null</code>.
   * 
   * @see #installDefaults()
   */
  protected void uninstallDefaults()
  {
    if (comboBox.getFont() instanceof UIResource)
      comboBox.setFont(null);

    if (comboBox.getForeground() instanceof UIResource)
      comboBox.setForeground(null);
    
    if (comboBox.getBackground() instanceof UIResource)
      comboBox.setBackground(null);

    buttonBackground = null;
    buttonShadow = null;
    buttonDarkShadow = null;
    buttonHighlight = null;
  }

  /**
   * Detaches all the listeners we attached in {@link #installListeners}.
   * 
   * @see #installListeners()
   */
  protected void uninstallListeners()
  {
    comboBox.removePropertyChangeListener(propertyChangeListener);
    propertyChangeListener = null;

    comboBox.removeFocusListener(focusListener);
    listBox.removeFocusListener(focusListener);
    focusListener = null;

    comboBox.removeItemListener(itemListener);
    itemListener = null;

    comboBox.removeKeyListener(keyListener);
    keyListener = null;

    arrowButton.removeMouseListener(mouseListener);
    mouseListener = null;

    comboBox.getModel().removeListDataListener(listDataListener);
    listDataListener = null;
  }

  /**
   * Creates the popup that will contain list of combo box's items.
   *
   * @return popup containing list of combo box's items
   */
  protected ComboPopup createPopup()
  {
    return new BasicComboPopup(comboBox);
  }

  /**
   * Creates a {@link KeyListener} to listen to key events.
   *
   * @return KeyListener that listens to key events.
   */
  protected KeyListener createKeyListener()
  {
    return new KeyHandler();
  }

  /**
   * Creates a {@link MouseListener} that will listen to mouse events occurring
   * in the combo box.
   *
   * @return the MouseListener
   */
  private MouseListener createMouseListener()
  {
    return new MouseHandler();
  }

  /**
   * Creates the {@link FocusListener} that will listen to changes in this
   * JComboBox's focus.
   *
   * @return the FocusListener.
   */
  protected FocusListener createFocusListener()
  {
    return new FocusHandler();
  }

  /**
   * Creates a {@link ListDataListener} to listen to the combo box's data model.
   *
   * @return The new listener.
   */
  protected ListDataListener createListDataListener()
  {
    return new ListDataHandler();
  }

  /**
   * Creates an {@link ItemListener} that will listen to the changes in
   * the JComboBox's selection.
   *
   * @return The ItemListener
   */
  protected ItemListener createItemListener()
  {
    return new ItemHandler();
  }

  /**
   * Creates a {@link PropertyChangeListener} to listen to the changes in
   * the JComboBox's bound properties.
   *
   * @return The PropertyChangeListener
   */
  protected PropertyChangeListener createPropertyChangeListener()
  {
    return new PropertyChangeHandler();
  }

  /**
   * Creates and returns a layout manager for the combo box.  Subclasses can 
   * override this method to provide a different layout.
   *
   * @return a layout manager for the combo box.
   */
  protected LayoutManager createLayoutManager()
  {
    return new ComboBoxLayoutManager();
  }

  /**
   * Creates a component that will be responsible for rendering the
   * selected component in the combo box.
   *
   * @return A renderer for the combo box.
   */
  protected ListCellRenderer createRenderer()
  {
    return new BasicComboBoxRenderer();
  }

  /**
   * Creates the component that will be responsible for displaying/editing
   * the selected item in the combo box. This editor is used only when combo 
   * box is editable.
   *
   * @return A new component that will be responsible for displaying/editing
   *         the selected item in the combo box.
   */
  protected ComboBoxEditor createEditor()
  {
    return new BasicComboBoxEditor.UIResource();
  }

  /**
   * Installs the components for this JComboBox. ArrowButton, main
   * part of combo box (upper part) and popup list of items are created and
   * configured here.
   */
  protected void installComponents()
  {
    // create drop down list of items
    popup = createPopup();
    listBox = popup.getList();

    // set editor and renderer for the combo box. Editor is used
    // only if combo box becomes editable, otherwise renderer is used
    // to paint the selected item; combobox is not editable by default. 
    comboBox.setRenderer(createRenderer());

    // create and install arrow button
    arrowButton = createArrowButton();
    configureArrowButton();
    comboBox.add(arrowButton);

    ComboBoxEditor currentEditor = comboBox.getEditor();
    if (currentEditor == null || currentEditor instanceof UIResource)
      {
        currentEditor = createEditor();
        comboBox.setEditor(currentEditor);
      } 
    editor = currentEditor.getEditorComponent();

    comboBox.revalidate();
  }

  /**
   * Uninstalls components from this {@link JComboBox}.
   * 
   * @see #installComponents()
   */
  protected void uninstallComponents()
  {
    // uninstall arrow button
    unconfigureArrowButton();
    comboBox.remove(arrowButton);
    arrowButton = null;

    listBox = null;
    popup = null;

    comboBox.setRenderer(null);

    // if the editor is not an instanceof UIResource, it was not set by the
    // UI delegate, so don't clear it...
    ComboBoxEditor currentEditor = comboBox.getEditor();
    if (currentEditor instanceof UIResource)
      {
        comboBox.setEditor(null);
        editor = null;
      }
  }

  /**
   * Adds the current editor to the combo box.
   */
  public void addEditor()
  {
    comboBox.add(editor);
  }

  /**
   * Removes the current editor from the combo box.
   */
  public void removeEditor()
  {
    comboBox.remove(editor);
  }

  /**
   * Configures the editor for this combo box.
   */
  protected void configureEditor()
  {
    editor.setFont(comboBox.getFont());
    comboBox.getEditor().setItem(comboBox.getSelectedItem());
    // FIXME: Need to implement. Set font and add listeners.
  }

  /**
   * Unconfigures the editor for this combo nox.  This method is not implemented.
   */
  protected void unconfigureEditor()
  {
    // FIXME: Need to implement    
  }

  /**
   * Configures the arrow button.
   * 
   * @see #configureArrowButton()
   */
  public void configureArrowButton()
  {
    arrowButton.setEnabled(comboBox.isEnabled());
    arrowButton.setFont(comboBox.getFont());
  }

  /**
   * Unconfigures the arrow button.
   * 
   * @see #configureArrowButton()
   *
   * @specnote The specification says this method is implementation specific
   *           and should not be used or overridden.
   */
  public void unconfigureArrowButton()
  {
    // Nothing to do here yet.
  }

  /**
   * Creates an arrow button for this {@link JComboBox}.  The arrow button is
   * displayed at the right end of the combo box and is used to display/hide
   * the drop down list of items.
   *
   * @return A new button.
   */
  protected JButton createArrowButton()
  {
    return new BasicArrowButton(BasicArrowButton.SOUTH, buttonBackground, 
            buttonShadow, buttonDarkShadow, buttonHighlight);
  }

  /**
   * Returns <code>true</code> if the popup is visible, and <code>false</code>
   * otherwise.
   *
   * @param c The JComboBox to check
   *
   * @return <code>true</code> if popup part of the JComboBox is visible and 
   *         <code>false</code> otherwise.
   */
  public boolean isPopupVisible(JComboBox c)
  {
    return popup.isVisible();
  }

  /**
   * Displays/hides the {@link JComboBox}'s list of items on the screen.
   *
   * @param c The combo box, for which list of items should be
   *        displayed/hidden
   * @param v true if show popup part of the jcomboBox and false to hide.
   */
  public void setPopupVisible(JComboBox c, boolean v)
  {
    if (v)
      {
        popup.show();
        popup.getList().requestFocus();
      }
    else
      popup.hide();
  }

  /**
   * JComboBox is focus traversable if it is editable and not otherwise.
   *
   * @param c combo box for which to check whether it is focus traversable
   *
   * @return true if focus tranversable and false otherwise
   */
  public boolean isFocusTraversable(JComboBox c)
  {
    if (!comboBox.isEditable())
      return true;

    return false;
  }

  /**
   * Paints given menu item using specified graphics context
   *
   * @param g The graphics context used to paint this combo box
   * @param c comboBox which needs to be painted.
   */
  public void paint(Graphics g, JComponent c)
  {
    Rectangle rect = rectangleForCurrentValue();
    paintCurrentValueBackground(g, rect, hasFocus);
    paintCurrentValue(g, rect, hasFocus);
  }

  /**
   * Returns preferred size for the combo box.
   *
   * @param c comboBox for which to get preferred size
   *
   * @return The preferred size for the given combo box
   */
  public Dimension getPreferredSize(JComponent c)
  {
    // note:  overriding getMinimumSize() (for example in the MetalComboBoxUI 
    // class) affects the getPreferredSize() result, so it seems logical that
    // this method is implemented by delegating to the getMinimumSize() method
    return getMinimumSize(c);
  }

  /**
   * Returns the minimum size for this {@link JComboBox} for this
   * look and feel.
   *
   * @param c The {@link JComponent} to find the minimum size for.
   *
   * @return The dimensions of the minimum size.
   */
  public Dimension getMinimumSize(JComponent c)
  {
    Dimension d = getDisplaySize();
    int arrowButtonWidth = d.height;
    Dimension result = new Dimension(d.width + arrowButtonWidth, d.height);
    return result;
  }

  /** The value returned by the getMaximumSize() method. */
  private static final Dimension MAXIMUM_SIZE = new Dimension(32767, 32767);
  
  /**
   * Returns the maximum size for this {@link JComboBox} for this
   * look and feel.
   *
   * @param c The {@link JComponent} to find the maximum size for
   *
   * @return The maximum size (<code>Dimension(32767, 32767)</code>).
   */
  public Dimension getMaximumSize(JComponent c)
  {
    return MAXIMUM_SIZE;
  }

  public int getAccessibleChildrenCount(JComponent c)
  {
    // FIXME: Need to implement
    return 0;
  }

  public Accessible getAccessibleChild(JComponent c, int i)
  {
    // FIXME: Need to implement
    return null;
  }

  /**
   * Returns true if the specified key is a navigation key and false otherwise
   *
   * @param keyCode a key for which to check whether it is navigation key or
   *        not.
   *
   * @return true if the specified key is a navigation key and false otherwis
   */
  protected boolean isNavigationKey(int keyCode)
  {
    return false;
  }

  /**
   * Selects next possible item relative to the current selection
   * to be next selected item in the combo box.
   */
  protected void selectNextPossibleValue()
  {
    int index = comboBox.getSelectedIndex();
    if (index != comboBox.getItemCount() - 1)
      comboBox.setSelectedIndex(index + 1);
  }

  /**
   * Selects previous item relative to current selection to be
   * next selected item.
   */
  protected void selectPreviousPossibleValue()
  {
    int index = comboBox.getSelectedIndex();
    if (index != 0)
      comboBox.setSelectedIndex(index - 1);
  }

  /**
   * Displays combo box popup if the popup is not currently shown
   * on the screen and hides it if it is currently shown
   */
  protected void toggleOpenClose()
  {
    setPopupVisible(comboBox, ! isPopupVisible(comboBox));
  }

  /**
   * Returns the bounds in which comboBox's selected item will be
   * displayed.
   *
   * @return rectangle bounds in which comboBox's selected Item will be
   *         displayed
   */
  protected Rectangle rectangleForCurrentValue()
  {
    Rectangle cbBounds = SwingUtilities.getLocalBounds(comboBox);
    Rectangle abBounds = arrowButton.getBounds();   
    Rectangle rectForCurrentValue = new Rectangle(cbBounds.x, cbBounds.y,
      cbBounds.width - abBounds.width, cbBounds.height);
    return rectForCurrentValue;
  }

  /**
   * Returns the insets of the current border.
   *
   * @return Insets representing space between combo box and its border
   */
  protected Insets getInsets()
  {
    return new Insets(0, 0, 0, 0);
  }

  /**
   * Paints currently selected value in the main part of the combo
   * box (part without popup).
   *
   * @param g graphics context
   * @param bounds Rectangle representing the size of the area in which
   *        selected item should be drawn
   * @param hasFocus true if combo box has focus and false otherwise
   */
  public void paintCurrentValue(Graphics g, Rectangle bounds, boolean hasFocus)
  {
    if (! comboBox.isEditable())
      {
	Object currentValue = comboBox.getSelectedItem();
	boolean isPressed = arrowButton.getModel().isPressed();

	/* Gets the component to be drawn for the current value.
	 * If there is currently no selected item we will take an empty
	 * String as replacement.
	 */
        Component comp = comboBox.getRenderer().getListCellRendererComponent(
                listBox, (currentValue != null ? currentValue : ""), -1,
                isPressed, hasFocus);
        if (! comboBox.isEnabled())
          {
            comp.setBackground(UIManager.getLookAndFeelDefaults().getColor(
                "ComboBox.disabledBackground"));
            comp.setForeground(UIManager.getLookAndFeelDefaults().getColor(
                "ComboBox.disabledForeground"));
            comp.setEnabled(false);
          }
        comp.setBounds(0, 0, bounds.width, bounds.height);
        comp.setFont(comboBox.getFont());
        comp.paint(g);
        
        comboBox.revalidate();
      }
    else
      comboBox.getEditor().setItem(comboBox.getSelectedItem());
  }

  /**
   * Paints the background of part of the combo box, where currently
   * selected value is displayed. If the combo box has focus this method
   * should also paint focus rectangle around the combo box.
   *
   * @param g graphics context
   * @param bounds Rectangle representing the size of the largest item  in the
   *        comboBox
   * @param hasFocus true if combo box has fox and false otherwise
   */
  public void paintCurrentValueBackground(Graphics g, Rectangle bounds,
                                          boolean hasFocus)
  {
    // background is painted by renderer, so it seems that nothing
    // should be done here.
  }

  /**
   * Returns the default size for the display area of a combo box that does 
   * not contain any elements.  This method returns the width and height of
   * a single space in the current font, plus a margin of 1 pixel. 
   *
   * @return The default display size.
   * 
   * @see #getDisplaySize()
   */
  protected Dimension getDefaultSize()
  {
    // There is nothing in the spec to say how this method should be
    // implemented...so I've done some guessing, written some Mauve tests,
    // and written something that gives dimensions that are close to the 
    // reference implementation.
    FontMetrics fm = comboBox.getFontMetrics(comboBox.getFont());
    int w = fm.charWidth(' ') + 2;
    int h = fm.getHeight() + 2;
    return new Dimension(w, h);
  }

  /**
   * Returns the size of the display area for the combo box. This size will be 
   * the size of the combo box, not including the arrowButton.
   *
   * @return The size of the display area for the combo box.
   */
  protected Dimension getDisplaySize()
  {
    if (!comboBox.isEditable()) 
      {
        Object prototype = comboBox.getPrototypeDisplayValue();
        if (prototype != null)
          {
            // calculate result based on prototype
            ListCellRenderer renderer = comboBox.getRenderer();
            Component comp = renderer.getListCellRendererComponent(listBox, 
                prototype, -1, false, false);
            Dimension compSize = comp.getPreferredSize();
            compSize.width += 2;  // add 1 pixel margin around area
            compSize.height += 2;
            return compSize;
          }
        else
          {
            ComboBoxModel model = comboBox.getModel();
            int numItems = model.getSize();

            // if combo box doesn't have any items then simply
            // return its default size
            if (numItems == 0)
              {
                displaySize = getDefaultSize();
                return displaySize;
              }

            Dimension size = new Dimension(0, 0);

            // ComboBox's display size should be equal to the 
            // size of the largest item in the combo box. 
            ListCellRenderer renderer = comboBox.getRenderer();

            for (int i = 0; i < numItems; i++)
              {
                Object item = model.getElementAt(i);
                Component comp = renderer.getListCellRendererComponent(listBox, 
                    item, -1, false, false);

                Dimension compSize = comp.getPreferredSize();
                if (compSize.width + 2 > size.width)
                  size.width = compSize.width + 2;
                if (compSize.height + 2 > size.height)
                  size.height = compSize.height + 2;
              }
            displaySize = size;
            return displaySize;
          }
      }
    else // an editable combo,  
      {
        Component comp = comboBox.getEditor().getEditorComponent();
        Dimension prefSize = comp.getPreferredSize();
        int width = prefSize.width;
        int height = prefSize.height + 2;
        Object prototype = comboBox.getPrototypeDisplayValue();
        if (prototype != null)
          {
            FontMetrics fm = comboBox.getFontMetrics(comboBox.getFont());
            width = Math.max(width, fm.stringWidth(prototype.toString()) + 2);
          }
        displaySize = new Dimension(width, height);
        return displaySize;
      }
  }

  /**
   * Installs the keyboard actions for the {@link JComboBox} as specified
   * by the look and feel.
   */
  protected void installKeyboardActions()
  {
    // FIXME: Need to implement.
  }

  /**
   * Uninstalls the keyboard actions for the {@link JComboBox} there were
   * installed by in {@link #installListeners}.
   */
  protected void uninstallKeyboardActions()
  {
    // FIXME: Need to implement.
  }

  /**
   * A {@link LayoutManager} used to position the sub-components of the
   * {@link JComboBox}.
   * 
   * @see BasicComboBoxUI#createLayoutManager()
   */
  public class ComboBoxLayoutManager implements LayoutManager
  {
    /**
     * Creates a new ComboBoxLayoutManager object.
     */
    public ComboBoxLayoutManager()
    {
      // Nothing to do here.
    }

    /**
     * Adds a component to the layout.  This method does nothing, since the
     * layout manager doesn't need to track the components.
     * 
     * @param name  the name to associate the component with (ignored).
     * @param comp  the component (ignored).
     */
    public void addLayoutComponent(String name, Component comp)
    {
      // Do nothing
    }

    /**
     * Removes a component from the layout.  This method does nothing, since
     * the layout manager doesn't need to track the components.
     * 
     * @param comp  the component.
     */
    public void removeLayoutComponent(Component comp)
    {
      // Do nothing
    }

    /**
     * Returns preferred layout size of the JComboBox.
     *
     * @param parent  the Container for which the preferred size should be 
     *                calculated.
     *
     * @return The preferred size for the given container
     */
    public Dimension preferredLayoutSize(Container parent)
    {
      return getPreferredSize((JComponent) parent);
    }

    /**
     * Returns the minimum layout size.
     * 
     * @param parent  the container.
     * 
     * @return The minimum size.
     */
    public Dimension minimumLayoutSize(Container parent)
    {
      return preferredLayoutSize(parent);
    }

    /**
     * Arranges the components in the container.  It puts arrow
     * button right end part of the comboBox. If the comboBox is editable
     * then editor is placed to the left of arrow  button, starting from the
     * beginning.
     *
     * @param parent Container that should be layed out.
     */
    public void layoutContainer(Container parent)
    {
      // Position editor component to the left of arrow button if combo box is 
      // editable
      int arrowSize = comboBox.getHeight();
      int editorWidth = comboBox.getBounds().width - arrowSize;

      if (comboBox.isEditable())
        editor.setBounds(0, 0, editorWidth, comboBox.getBounds().height);
      
      arrowButton.setBounds(editorWidth, 0, arrowSize, arrowSize);
      comboBox.revalidate();
    }
  }

  /**
   * Handles focus changes occuring in the combo box. This class is
   * responsible for repainting combo box whenever focus is gained or lost
   * and also for hiding popup list of items whenever combo box loses its
   * focus.
   */
  public class FocusHandler extends Object implements FocusListener
  {
    /**
     * Creates a new FocusHandler object.
     */
    public FocusHandler()
    {
      // Nothing to do here.
    }

    /**
     * Invoked when combo box gains focus. It repaints main
     * part of combo box accordingly.
     *
     * @param e the FocusEvent
     */
    public void focusGained(FocusEvent e)
    {
      hasFocus = true;
      comboBox.repaint();
    }

    /**
     * Invoked when the combo box loses focus.  It repaints the main part
     * of the combo box accordingly and hides the popup list of items.
     *
     * @param e the FocusEvent
     */
    public void focusLost(FocusEvent e)
    {
      hasFocus = false;
      setPopupVisible(comboBox, false);
      comboBox.repaint();
    }
  }

  /**
   * Handles {@link ItemEvent}s fired by the {@link JComboBox} when its 
   * selected item changes.
   */
  public class ItemHandler extends Object implements ItemListener
  {
    /**
     * Creates a new ItemHandler object.
     */
    public ItemHandler()
    {
      // Nothing to do here.
    }

    /**
     * Invoked when selected item becomes deselected or when
     * new item becomes selected.
     *
     * @param e the ItemEvent representing item's state change.
     */
    public void itemStateChanged(ItemEvent e)
    {
      if (e.getStateChange() == ItemEvent.SELECTED && comboBox.isEditable())
        comboBox.getEditor().setItem(e.getItem());
      comboBox.repaint();
    }
  }

  /**
   * KeyHandler handles key events occuring while JComboBox has focus.
   */
  public class KeyHandler extends KeyAdapter
  {
    public KeyHandler()
    {
      // Nothing to do here.
    }

    /**
     * Invoked whenever key is pressed while JComboBox is in focus.
     */
    public void keyPressed(KeyEvent e)
    {
      // FIXME: This method calls JComboBox.selectWithKeyChar if the key that was 
      // pressed is not a navigation key. 
    }
  }

  /**
   * Handles the changes occurring in the JComboBox's data model.
   */
  public class ListDataHandler extends Object implements ListDataListener
  {
    /**
     * Creates a new ListDataHandler object.
     */
    public ListDataHandler()
    {
      // Nothing to do here.
    }

    /**
     * Invoked if the content's of JComboBox's data model are changed.
     *
     * @param e ListDataEvent describing the change.
     */
    public void contentsChanged(ListDataEvent e)
    {
      // if the item is selected or deselected
    }

    /**
     * Invoked when items are added to the JComboBox's data model.
     *
     * @param e ListDataEvent describing the change.
     */
    public void intervalAdded(ListDataEvent e)
    {
      ComboBoxModel model = comboBox.getModel();
      ListCellRenderer renderer = comboBox.getRenderer();

      if (displaySize == null)
        displaySize = getDisplaySize();
      if (displaySize.width < getDefaultSize().width)
        displaySize.width = getDefaultSize().width;
      if (displaySize.height < getDefaultSize().height)
        displaySize.height = getDefaultSize().height;

      comboBox.repaint();
    }

    /**
     * Invoked when items are removed from the JComboBox's
     * data model.
     *
     * @param e ListDataEvent describing the change.
     */
    public void intervalRemoved(ListDataEvent e)
    {
      // recalculate display size of the JComboBox.
      displaySize = getDisplaySize();
      comboBox.repaint();
    }
  }

  /**
   * Handles {@link PropertyChangeEvent}s fired by the {@link JComboBox}.
   */
  public class PropertyChangeHandler extends Object
    implements PropertyChangeListener
  {
    /**
     * Creates a new instance.
     */
    public PropertyChangeHandler()
    {
      // Nothing to do here.
    }

    /**
     * Invoked whenever bound property of JComboBox changes.
     * 
     * @param e  the event.
     */
    public void propertyChange(PropertyChangeEvent e)
    {
      if (e.getPropertyName().equals("enabled"))
        {
	  arrowButton.setEnabled(comboBox.isEnabled());

	  if (comboBox.isEditable())
	    comboBox.getEditor().getEditorComponent().setEnabled(comboBox
	                                                         .isEnabled());
        }
      else if (e.getPropertyName().equals("editable"))
        {
	  if (comboBox.isEditable())
	    {
	      configureEditor();
	      addEditor();
	    }
	  else
	    {
	      unconfigureEditor();
	      removeEditor();
	    }

	  comboBox.revalidate();
	  comboBox.repaint();
        }
      else if (e.getPropertyName().equals("dataModel"))
        {
	  // remove ListDataListener from old model and add it to new model
	  ComboBoxModel oldModel = (ComboBoxModel) e.getOldValue();
	  if (oldModel != null)
	    oldModel.removeListDataListener(listDataListener);

	  if ((ComboBoxModel) e.getNewValue() != null)
	    comboBox.getModel().addListDataListener(listDataListener);
        }
      else if (e.getPropertyName().equals("font"))
        {
          Font font = (Font) e.getNewValue();
          editor.setFont(font);
          listBox.setFont(font);
          arrowButton.setFont(font);
          comboBox.revalidate();
          comboBox.repaint();
        }

      // FIXME: Need to handle changes in other bound properties.	
    }
  }

  /**
   * A handler for mouse events occurring in the combo box.  An instance of 
   * this class is returned by the <code>createMouseListener()</code> method.
   */
  private class MouseHandler extends MouseAdapter
  {
    /**
     * Invoked when mouse is pressed over the combo box. It toggles the 
     * visibility of the popup list.
     *
     * @param e  the event
     */
    public void mousePressed(MouseEvent e)
    {
      if (comboBox.isEnabled())
        toggleOpenClose();
    }
  }
}
