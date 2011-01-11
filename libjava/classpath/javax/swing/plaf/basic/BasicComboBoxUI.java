/* BasicComboBoxUI.java --
   Copyright (C) 2004, 2005, 2006,  Free Software Foundation, Inc.

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
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.swing.CellRendererPane;
import javax.swing.ComboBoxEditor;
import javax.swing.ComboBoxModel;
import javax.swing.DefaultListCellRenderer;
import javax.swing.InputMap;
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

  /* Size of the largest item in the comboBox
   * This is package-private to avoid an accessor method.
   */
  Dimension displaySize = new Dimension();

  /**
   * Used to render the combo box values.
   */
  protected CellRendererPane currentValuePane;

  /**
   * The current minimum size if isMinimumSizeDirty is false.
   * Setup by getMinimumSize() and invalidated by the various listeners.
   */
  protected Dimension cachedMinimumSize;

  /**
   * Indicates whether or not the cachedMinimumSize field is valid or not.
   */
  protected boolean isMinimumSizeDirty = true;

  /**
   * Creates a new <code>BasicComboBoxUI</code> object.
   */
  public BasicComboBoxUI()
  {
    currentValuePane = new CellRendererPane();
    cachedMinimumSize = new Dimension();
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
        isMinimumSizeDirty = true;
        comboBox = (JComboBox) c;
        installDefaults();
        popup = createPopup();
        listBox = popup.getList();

        // Set editor and renderer for the combo box. Editor is used
        // only if combo box becomes editable, otherwise renderer is used
        // to paint the selected item; combobox is not editable by default.
        ListCellRenderer renderer = comboBox.getRenderer();
        if (renderer == null || renderer instanceof UIResource)
          comboBox.setRenderer(createRenderer());

        ComboBoxEditor currentEditor = comboBox.getEditor();
        if (currentEditor == null || currentEditor instanceof UIResource)
          {
            currentEditor = createEditor();
            comboBox.setEditor(currentEditor);
          }

        installComponents();
        installListeners();
        comboBox.setLayout(createLayoutManager());
        comboBox.setFocusable(true);
        installKeyboardActions();
        comboBox.putClientProperty(BasicLookAndFeel.DONT_CANCEL_POPUP,
                                   Boolean.TRUE);
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
    setPopupVisible(comboBox, false);
    popup.uninstallingUI();
    uninstallKeyboardActions();
    comboBox.setLayout(null);
    uninstallComponents();
    uninstallListeners();
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
    LookAndFeel.installBorder(comboBox, "ComboBox.border");
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

    itemListener = createItemListener();
    comboBox.addItemListener(itemListener);

    keyListener = createKeyListener();
    comboBox.addKeyListener(keyListener);

    // install listeners that listen to combo box model
    listDataListener = createListDataListener();
    comboBox.getModel().addListDataListener(listDataListener);

    // Install mouse and key listeners from the popup.
    popupMouseListener = popup.getMouseListener();
    comboBox.addMouseListener(popupMouseListener);

    popupMouseMotionListener = popup.getMouseMotionListener();
    comboBox.addMouseMotionListener(popupMouseMotionListener);

    popupKeyListener = popup.getKeyListener();
    comboBox.addKeyListener(popupKeyListener);
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

    LookAndFeel.uninstallBorder(comboBox);
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

    comboBox.getModel().removeListDataListener(listDataListener);
    listDataListener = null;

    if (popupMouseListener != null)
      comboBox.removeMouseListener(popupMouseListener);
    popupMouseListener = null;

    if (popupMouseMotionListener != null)
      comboBox.removeMouseMotionListener(popupMouseMotionListener);
    popupMouseMotionListener = null;

    if (popupKeyListener != null)
      comboBox.removeKeyListener(popupKeyListener);
    popupKeyListener = null;
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
    return new BasicComboBoxRenderer.UIResource();
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
    // create and install arrow button
    arrowButton = createArrowButton();
    comboBox.add(arrowButton);
    if (arrowButton != null)
      configureArrowButton();

    if (comboBox.isEditable())
      addEditor();

    comboBox.add(currentValuePane);
  }

  /**
   * Uninstalls components from this {@link JComboBox}.
   *
   * @see #installComponents()
   */
  protected void uninstallComponents()
  {
    // Unconfigure arrow button.
    if (arrowButton != null)
      {
        unconfigureArrowButton();
      }

    // Unconfigure editor.
    if (editor != null)
      {
        unconfigureEditor();
      }

    comboBox.removeAll();
    arrowButton = null;
  }

  /**
   * Adds the current editor to the combo box.
   */
  public void addEditor()
  {
    removeEditor();
    editor = comboBox.getEditor().getEditorComponent();
    if (editor != null)
      {
        configureEditor();
        comboBox.add(editor);
      }
  }

  /**
   * Removes the current editor from the combo box.
   */
  public void removeEditor()
  {
    if (editor != null)
      {
        unconfigureEditor();
        comboBox.remove(editor);
      }
  }

  /**
   * Configures the editor for this combo box.
   */
  protected void configureEditor()
  {
    editor.setFont(comboBox.getFont());
    if (popupKeyListener != null)
      editor.addKeyListener(popupKeyListener);
    if (keyListener != null)
      editor.addKeyListener(keyListener);
    comboBox.configureEditor(comboBox.getEditor(),
                             comboBox.getSelectedItem());
  }

  /**
   * Unconfigures the editor for this combo box.
   */
  protected void unconfigureEditor()
  {
    if (popupKeyListener != null)
      editor.removeKeyListener(popupKeyListener);
    if (keyListener != null)
      editor.removeKeyListener(keyListener);
  }

  /**
   * Configures the arrow button.
   *
   * @see #configureArrowButton()
   */
  public void configureArrowButton()
  {
    if (arrowButton != null)
      {
        arrowButton.setEnabled(comboBox.isEnabled());
        arrowButton.setFocusable(false);
        arrowButton.addMouseListener(popup.getMouseListener());
        arrowButton.addMouseMotionListener(popup.getMouseMotionListener());

        // Mark the button as not closing the popup, we handle this ourselves.
        arrowButton.putClientProperty(BasicLookAndFeel.DONT_CANCEL_POPUP,
                                      Boolean.TRUE);
      }
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
    if (arrowButton != null)
      {
        if (popupMouseListener != null)
          arrowButton.removeMouseListener(popupMouseListener);
        if (popupMouseMotionListener != null)
          arrowButton.removeMouseMotionListener(popupMouseMotionListener);
      }
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
    return new BasicArrowButton(BasicArrowButton.SOUTH);
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
      popup.show();
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
    hasFocus = comboBox.hasFocus();
    if (! comboBox.isEditable())
      {
        Rectangle rect = rectangleForCurrentValue();
        paintCurrentValueBackground(g, rect, hasFocus);
        paintCurrentValue(g, rect, hasFocus);
      }
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
    return getMinimumSize(c);
  }

  /**
   * Returns the minimum size for this {@link JComboBox} for this
   * look and feel. Also makes sure cachedMinimimSize is setup correctly.
   *
   * @param c The {@link JComponent} to find the minimum size for.
   *
   * @return The dimensions of the minimum size.
   */
  public Dimension getMinimumSize(JComponent c)
  {
    if (isMinimumSizeDirty)
      {
        Insets i = getInsets();
        Dimension d = getDisplaySize();
        d.width += i.left + i.right + d.height;
        cachedMinimumSize = new Dimension(d.width, d.height + i.top + i.bottom);
        isMinimumSizeDirty = false;
      }
    return new Dimension(cachedMinimumSize);
  }

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
    return new Dimension(32767, 32767);
  }

  /**
   * Returns the number of accessible children of the combobox.
   *
   * @param c the component (combobox) to check, ignored
   *
   * @return the number of accessible children of the combobox
   */
  public int getAccessibleChildrenCount(JComponent c)
  {
    int count = 1;
    if (comboBox.isEditable())
      count = 2;
    return count;
  }

  /**
   * Returns the accessible child with the specified index.
   *
   * @param c the component, this is ignored
   * @param i the index of the accessible child to return
   */
  public Accessible getAccessibleChild(JComponent c, int i)
  {
    Accessible child = null;
    switch (i)
    {
      case 0: // The popup.
        if (popup instanceof Accessible)
          {
            AccessibleContext ctx = ((Accessible) popup).getAccessibleContext();
            ctx.setAccessibleParent(comboBox);
            child = (Accessible) popup;
          }
        break;
      case 1: // The editor, if any.
        if (comboBox.isEditable() && editor instanceof Accessible)
          {
            AccessibleContext ctx =
              ((Accessible) editor).getAccessibleContext();
            ctx.setAccessibleParent(comboBox);
            child = (Accessible) editor;
          }
        break;
    }
    return child;
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
    return keyCode == KeyEvent.VK_UP || keyCode == KeyEvent.VK_DOWN
           || keyCode == KeyEvent.VK_LEFT || keyCode == KeyEvent.VK_RIGHT
           || keyCode == KeyEvent.VK_ENTER || keyCode == KeyEvent.VK_ESCAPE
           || keyCode == KeyEvent.VK_TAB;
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
    if (index > 0)
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
    int w = comboBox.getWidth();
    int h = comboBox.getHeight();
    Insets i = comboBox.getInsets();
    int arrowSize = h - (i.top + i.bottom);
    if (arrowButton != null)
      arrowSize = arrowButton.getWidth();
    return new Rectangle(i.left, i.top, w - (i.left + i.right + arrowSize),
                         h - (i.top + i.left));
  }

  /**
   * Returns the insets of the current border.
   *
   * @return Insets representing space between combo box and its border
   */
  protected Insets getInsets()
  {
    return comboBox.getInsets();
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
    /* Gets the component to be drawn for the current value.
     * If there is currently no selected item we will take an empty
     * String as replacement.
     */
    ListCellRenderer renderer = comboBox.getRenderer();
    if (comboBox.getSelectedIndex() != -1)
      {
        Component comp;
        if (hasFocus && ! isPopupVisible(comboBox))
          {
            comp = renderer.getListCellRendererComponent(listBox,
                comboBox.getSelectedItem(), -1, true, false);
          }
        else
          {
            comp = renderer.getListCellRendererComponent(listBox,
                comboBox.getSelectedItem(), -1, false, false);
            Color bg = UIManager.getColor("ComboBox.disabledForeground");
            comp.setBackground(bg);
          }
        comp.setFont(comboBox.getFont());
        if (hasFocus && ! isPopupVisible(comboBox))
          {
            comp.setForeground(listBox.getSelectionForeground());
            comp.setBackground(listBox.getSelectionBackground());
          }
        else if (comboBox.isEnabled())
          {
            comp.setForeground(comboBox.getForeground());
            comp.setBackground(comboBox.getBackground());
          }
        else
          {
            Color fg = UIManager.getColor("ComboBox.disabledForeground");
            comp.setForeground(fg);
            Color bg = UIManager.getColor("ComboBox.disabledBackground");
            comp.setBackground(bg);
          }
        currentValuePane.paintComponent(g, comp, comboBox, bounds.x, bounds.y,
                                        bounds.width, bounds.height);
      }
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
    Color saved = g.getColor();
    if (comboBox.isEnabled())
      g.setColor(UIManager.getColor("UIManager.background"));
    else
      g.setColor(UIManager.getColor("UIManager.disabledBackground"));
    g.fillRect(bounds.x, bounds.y, bounds.width, bounds.height);
    g.setColor(saved);
  }

  private static final ListCellRenderer DEFAULT_RENDERER
    = new DefaultListCellRenderer();

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
    Component comp = DEFAULT_RENDERER.getListCellRendererComponent(listBox,
        " ", -1, false, false);
    currentValuePane.add(comp);
    comp.setFont(comboBox.getFont());
    Dimension d = comp.getPreferredSize();
    currentValuePane.remove(comp);
    return d;
  }

  /**
   * Returns the size of the display area for the combo box. This size will be
   * the size of the combo box, not including the arrowButton.
   *
   * @return The size of the display area for the combo box.
   */
  protected Dimension getDisplaySize()
  {
    Dimension dim = new Dimension();
    ListCellRenderer renderer = comboBox.getRenderer();
    if (renderer == null)
      {
        renderer = DEFAULT_RENDERER;
      }

    Object prototype = comboBox.getPrototypeDisplayValue();
    if (prototype != null)
      {
        Component comp = renderer.getListCellRendererComponent(listBox,
            prototype, -1, false, false);
        currentValuePane.add(comp);
        comp.setFont(comboBox.getFont());
        Dimension renderSize = comp.getPreferredSize();
        currentValuePane.remove(comp);
        dim.height = renderSize.height;
        dim.width = renderSize.width;
      }
    else
      {
        ComboBoxModel model = comboBox.getModel();
        int size = model.getSize();
        if (size > 0)
          {
            for (int i = 0; i < size; ++i)
              {
                Component comp = renderer.getListCellRendererComponent(listBox,
                    model.getElementAt(i), -1, false, false);
                currentValuePane.add(comp);
                comp.setFont(comboBox.getFont());
                Dimension renderSize = comp.getPreferredSize();
                currentValuePane.remove(comp);
                dim.width = Math.max(dim.width, renderSize.width);
                dim.height = Math.max(dim.height, renderSize.height);
              }
          }
        else
          {
            dim = getDefaultSize();
            if (comboBox.isEditable())
              dim.width = 100;
          }
      }
    if (comboBox.isEditable())
      {
        Dimension editSize = editor.getPreferredSize();
        dim.width = Math.max(dim.width, editSize.width);
        dim.height = Math.max(dim.height, editSize.height);
      }
    displaySize.setSize(dim.width, dim.height);
    return dim;
  }

  /**
   * Installs the keyboard actions for the {@link JComboBox} as specified
   * by the look and feel.
   */
  protected void installKeyboardActions()
  {
    SwingUtilities.replaceUIInputMap(comboBox,
        JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT,
        (InputMap) UIManager.get("ComboBox.ancestorInputMap"));
    // Install any action maps here.
  }

  /**
   * Uninstalls the keyboard actions for the {@link JComboBox} there were
   * installed by in {@link #installListeners}.
   */
  protected void uninstallKeyboardActions()
  {
    SwingUtilities.replaceUIInputMap(comboBox,
        JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT, null);
    // Uninstall any action maps here.
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
      return parent.getPreferredSize();
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
      return parent.getMinimumSize();
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
      Insets i = getInsets();
      int arrowSize = comboBox.getHeight() - (i.top + i.bottom);

      if (arrowButton != null)
        arrowButton.setBounds(comboBox.getWidth() - (i.right + arrowSize),
                              i.top, arrowSize, arrowSize);
      if (editor != null)
        editor.setBounds(rectangleForCurrentValue());
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
      if (! e.isTemporary() && comboBox.isLightWeightPopupEnabled())
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
      ComboBoxModel model = comboBox.getModel();
      Object v = model.getSelectedItem();
      if (editor != null)
        comboBox.configureEditor(comboBox.getEditor(), v);
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
      if (comboBox.getModel().getSize() != 0 && comboBox.isEnabled())
        {
          if (! isNavigationKey(e.getKeyCode()))
            {
              if (! comboBox.isEditable())
                if (comboBox.selectWithKeyChar(e.getKeyChar()))
                  e.consume();
            }
          else
            {
              if (e.getKeyCode() == KeyEvent.VK_UP && comboBox.isPopupVisible())
                selectPreviousPossibleValue();
              else if (e.getKeyCode() == KeyEvent.VK_DOWN)
                {
                  if (comboBox.isPopupVisible())
                    selectNextPossibleValue();
                  else
                    comboBox.showPopup();
                }
              else if (e.getKeyCode() == KeyEvent.VK_ENTER
                       || e.getKeyCode() == KeyEvent.VK_ESCAPE)
                popup.hide();
            }
        }
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
      if (e.getIndex0() != -1 || e.getIndex1() != -1)
        {
          isMinimumSizeDirty = true;
          comboBox.revalidate();
        }
      if (editor != null)
        comboBox.configureEditor(comboBox.getEditor(),
            comboBox.getSelectedItem());
      comboBox.repaint();
    }

    /**
     * Invoked when items are added to the JComboBox's data model.
     *
     * @param e ListDataEvent describing the change.
     */
    public void intervalAdded(ListDataEvent e)
    {
      int start = e.getIndex0();
      int end = e.getIndex1();
      if (start == 0 && comboBox.getItemCount() - (end - start + 1) == 0)
        contentsChanged(e);
      else if (start != -1  || end != -1)
        {
          ListCellRenderer renderer = comboBox.getRenderer();
          ComboBoxModel model = comboBox.getModel();
          int w = displaySize.width;
          int h = displaySize.height;
          // TODO: Optimize using prototype here.
          for (int i = start; i <= end; ++i)
            {
              Component comp = renderer.getListCellRendererComponent(listBox,
                  model.getElementAt(i), -1, false, false);
              currentValuePane.add(comp);
              comp.setFont(comboBox.getFont());
              Dimension dim = comp.getPreferredSize();
              w = Math.max(w, dim.width);
              h = Math.max(h, dim.height);
              currentValuePane.remove(comp);
            }
          if (displaySize.width < w || displaySize.height < h)
            {
              if (displaySize.width < w)
                displaySize.width = w;
              if (displaySize.height < h)
                displaySize.height = h;
              comboBox.revalidate();
              if (editor != null)
                {
                  comboBox.configureEditor(comboBox.getEditor(),
                                           comboBox.getSelectedItem());
                }
            }
        }

    }

    /**
     * Invoked when items are removed from the JComboBox's
     * data model.
     *
     * @param e ListDataEvent describing the change.
     */
    public void intervalRemoved(ListDataEvent e)
    {
      contentsChanged(e);
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
      // Lets assume every change invalidates the minimumsize.
      String propName = e.getPropertyName();
      if (propName.equals("enabled"))
        {
          boolean enabled = comboBox.isEnabled();
          if (editor != null)
            editor.setEnabled(enabled);
          if (arrowButton != null)
            arrowButton.setEnabled(enabled);

          comboBox.repaint();
        }
      else if (propName.equals("editor") && comboBox.isEditable())
        {
          addEditor();
          comboBox.revalidate();
        }
      else if (e.getPropertyName().equals("editable"))
        {
          if (comboBox.isEditable())
            {
              addEditor();
            }
          else
            {
              removeEditor();
            }

          comboBox.revalidate();
        }
      else if (propName.equals("model"))
        {
          // remove ListDataListener from old model and add it to new model
          ComboBoxModel oldModel = (ComboBoxModel) e.getOldValue();
          if (oldModel != null && listDataListener != null)
            oldModel.removeListDataListener(listDataListener);

          ComboBoxModel newModel = (ComboBoxModel) e.getNewValue();
          if (newModel != null && listDataListener != null)
            comboBox.getModel().addListDataListener(listDataListener);

          if (editor != null)
            {
              comboBox.configureEditor(comboBox.getEditor(),
                                       comboBox.getSelectedItem());
            }
          isMinimumSizeDirty = true;
          comboBox.revalidate();
          comboBox.repaint();
        }
      else if (propName.equals("font"))
        {
          Font font = (Font) e.getNewValue();
          if (editor != null)
            {
              editor.setFont(font);
            }
          listBox.setFont(font);
          isMinimumSizeDirty = true;
          comboBox.revalidate();
        }
      else if (propName.equals("prototypeDisplayValue"))
        {
          isMinimumSizeDirty = true;
          comboBox.revalidate();
        }
      else if (propName.equals("renderer"))
        {
          isMinimumSizeDirty = true;
          comboBox.revalidate();
        }
      // FIXME: Need to handle changes in other bound properties.
    }
  }
}
