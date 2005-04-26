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


package javax.swing.plaf.basic;

import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
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
import javax.swing.UIDefaults;
import javax.swing.UIManager;
import javax.swing.event.ListDataEvent;
import javax.swing.event.ListDataListener;
import javax.swing.plaf.ComboBoxUI;
import javax.swing.plaf.ComponentUI;

/**
 * UI Delegate for JComboBox
 *
 * @author Olga Rodimina
 * @author Robert Schuster
 */
public class BasicComboBoxUI extends ComboBoxUI
{
  /**
   * This arrow button that is displayed in the rigth side of JComboBox. This
   * button is used to hide and show combo box's list of items
   */
  protected JButton arrowButton;

  /**
   * The combo box for which this UI delegate is for
   */
  protected JComboBox comboBox;

  /**
   * Component that is responsible for displaying/editting  selected item of
   * the combo box. By default JTextField is used as an editor for the
   * JComboBox
   */
  protected Component editor;

  /**
   * Listener listening to focus events occuring in the JComboBox
   */
  protected FocusListener focusListener;

  /**
   * tells whether JComboBox currently has focus
   */
  protected boolean hasFocus;

  /**
   * Listener listening to item events fired by the JComboBox
   */
  protected ItemListener itemListener;

  /**
   * KeyListener listening to key events that occur while JComboBox has focus
   */
  protected KeyListener keyListener;

  /**
   * MouseListener listening to mouse events occuring in the combo box
   */
  private MouseListener mouseListener;

  /**
   * List used when rendering selected item of the combo box. The selection
   * and foreground colors for combo box renderer  are configured from this
   * list
   */
  protected JList listBox;

  /**
   * ListDataListener listening to JComboBox model
   */
  protected ListDataListener listDataListener;

  /**
   * Popup list containing combo box's menu items
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
   * Colors that are used to render selected item in the combo box.
   */
  private Color shadow;
  private Color darkShadow;
  private Color highlight;
  private Color lightHighlight;

  /* Size of the largest item in the comboBox
   * This is package-private to avoid an accessor method.
   */
  Dimension largestItemSize;

  // It seems that JComboBox doesn't have a border set explicitely. So we just
  // paint the border everytime combo box is displayed. 

  /* border insets for this JComboBox
   * This is package-private to avoid an accessor method. */
  static final Insets borderInsets = new Insets(2, 2, 2, 2);

  // Width of the arrow button  
  // This is package-private to avoid an accessor method.
  // FIXME: has wrong name for a constant.
  static final int arrowButtonWidth = 15;

  // FIXME: This fields aren't used anywhere at this moment.
  protected Dimension cachedMinimumSize;
  protected CellRendererPane currentValuePane;
  protected boolean isMinimumSizeDirty;

  /**
   * Creates a new BasicComboBoxUI object.
   */
  public BasicComboBoxUI()
  {
  }

  /**
   * Factory method to create a BasicComboBoxUI for the given {@link
   * JComponent}, which should be a {@link JComboBox}.
   *
   * @param c The {@link JComponent} a UI is being created for.
   *
   * @return A BasicComboBoxUI for the {@link JComponent}.
   */
  public static ComponentUI createUI(JComponent c)
  {
    return new BasicComboBoxUI();
  }

  /**
   * This method installs the UI for the given JComponent.
   *
   * @param c The JComponent to install a UI for.
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
   * This method uninstalls the UI.
   *
   * @param c The JComponent that is having this UI removed.
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
   * This method installs the defaults that are defined in  the Basic look and
   * feel for this {@link JComboBox}.
   */
  protected void installDefaults()
  {
    UIDefaults defaults = UIManager.getLookAndFeelDefaults();

    comboBox.setBackground(defaults.getColor("ComboBox.background"));
    comboBox.setFont(defaults.getFont("ComboBox.font"));
    comboBox.setForeground(defaults.getColor("ComboBox.foreground"));

    // Set default color that should be used to to render selected item
    // of the combo box.
    shadow = defaults.getColor("Button.shadow");
    darkShadow = defaults.getColor("Button.darkShadow");
    lightHighlight = defaults.getColor("Button.light");
    highlight = defaults.getColor("Button.highlight");
  }

  /**
   * This method creates and installs the listeners for this UI.
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

    mouseListener = createMouseListener();
    comboBox.addMouseListener(mouseListener);

    // install listeners that listen to combo box model
    listDataListener = createListDataListener();
    comboBox.getModel().addListDataListener(listDataListener);

    configureArrowButton();
  }

  /**
   * This method uninstalls the defaults and sets any objects created during
   * install to null
   */
  protected void uninstallDefaults()
  {
    UIDefaults defaults = UIManager.getLookAndFeelDefaults();

    comboBox.setBackground(null);
    comboBox.setFont(null);
    comboBox.setForeground(null);

    shadow = null;
    darkShadow = null;
    lightHighlight = null;
    highlight = null;
  }

  /**
   * Detaches all the listeners we attached in {@link #installListeners}.
   */
  protected void uninstallListeners()
  {
    comboBox.removePropertyChangeListener(propertyChangeListener);
    propertyChangeListener = null;

    comboBox.removeFocusListener(focusListener);
    focusListener = null;

    comboBox.removeItemListener(itemListener);
    itemListener = null;

    comboBox.removeKeyListener(keyListener);
    keyListener = null;

    comboBox.removeMouseListener(mouseListener);
    mouseListener = null;

    comboBox.getModel().removeListDataListener(listDataListener);
    listDataListener = null;

    unconfigureArrowButton();
  }

  /**
   * This method creates popup that will contain list of combo box's items
   *
   * @return popup containing list of combo box's items
   */
  protected ComboPopup createPopup()
  {
    return new BasicComboPopup(comboBox);
  }

  /**
   * Creates KeyListener to listen to key events.
   *
   * @return KeyListener that listens to key events.
   */
  protected KeyListener createKeyListener()
  {
    return new KeyHandler();
  }

  /**
   * This method create MouseListener that will listen to mouse event occuring
   * in combo box.
   *
   * @return the MouseListener
   */
  private MouseListener createMouseListener()
  {
    return new MouseHandler();
  }

  /**
   * This method create FocusListener that will listen to changes in this
   * JComboBox's focus.
   *
   * @return theFocusListener
   */
  protected FocusListener createFocusListener()
  {
    return new FocusHandler();
  }

  /**
   * This method create ListDataListener to listen to ComboBox's  data model
   *
   * @return ListDataListener
   */
  protected ListDataListener createListDataListener()
  {
    return new ListDataHandler();
  }

  /**
   * This method creates ItemListener that will listen to to the changes in
   * the JComboBox's selection.
   *
   * @return the ItemListener
   */
  protected ItemListener createItemListener()
  {
    return new ItemHandler();
  }

  /**
   * This method creates PropertyChangeListener to listen to  the changes in
   * the JComboBox's bound properties.
   *
   * @return the PropertyChangeListener
   */
  protected PropertyChangeListener createPropertyChangeListener()
  {
    return new PropertyChangeHandler();
  }

  /**
   * This method returns layout manager for the combo box.
   *
   * @return layout manager for the combo box
   */
  protected LayoutManager createLayoutManager()
  {
    return new ComboBoxLayoutManager();
  }

  /**
   * This method creates component that will be responsible for rendering the
   * selected component in the combo box.
   *
   * @return render for the combo box
   */
  protected ListCellRenderer createRenderer()
  {
    return new BasicComboBoxRenderer();
  }

  /**
   * Creates component that will be responsible for displaying/editting
   * selected item in the combo box. This editor is used only when combo box
   * is editable.
   *
   * @return component that will be responsible for  displaying/editting
   *         selected item in the combo box.
   */
  protected ComboBoxEditor createEditor()
  {
    return new BasicComboBoxEditor();
  }

  /**
   * This method installs components for this JComboBox. ArrowButton, main
   * part of combo box (upper part) and  popup list of items are created and
   * configured here.
   */
  protected void installComponents()
  {
    // create and install arrow button
    arrowButton = createArrowButton();

    comboBox.add(arrowButton);

    // Set list that will be used by BasicComboBoxRender 
    // in order to determine the right colors when rendering
    listBox = new JList();

    Color background = arrowButton.getBackground();
    listBox.setBackground(background);
    listBox.setSelectionBackground(background.darker());

    Color foreground = arrowButton.getForeground();
    listBox.setForeground(foreground);
    listBox.setSelectionForeground(foreground);

    // set editor and renderer for the combo box. Editor is used
    // only if combo box becomes editable, otherwise renderer is used
    // to paint the selected item; combobox is not editable by default. 
    comboBox.setRenderer(createRenderer());

    comboBox.setEditor(createEditor());
    editor = comboBox.getEditor().getEditorComponent();

    // create drop down list of items
    popup = createPopup();

    comboBox.revalidate();
  }

  /**
   * This method uninstalls components from this JComboBox
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

    comboBox.setEditor(null);
    editor = null;
  }

  /**
   * This method adds editor to the combo box
   */
  public void addEditor()
  {
    comboBox.add(editor);
  }

  /**
   * This method removes editor from the combo box
   */
  public void removeEditor()
  {
    comboBox.remove(editor);
  }

  /**
   * This method configures editor for this combo box.
   */
  protected void configureEditor()
  {
    // FIXME: Need to implement. Set font and add listeners.
  }

  /**
   * This method removes all the listeners for the editor.
   */
  protected void unconfigureEditor()
  {
    // FIXME: Need to implement    
  }

  /**
   * This method adds listeners to the arrow button part of the combo box.
   */
  public void configureArrowButton()
  {
    arrowButton.addMouseListener(mouseListener);
  }

  /**
   * This method removes listeners from the arrow button part of the combo
   * box.
   */
  public void unconfigureArrowButton()
  {
    arrowButton.removeMouseListener(mouseListener);
  }

  /**
   * This method create arrow button for this JComboBox. Arrow button is
   * responsible for displaying / hiding drop down list of items  when it is
   * clicked.
   *
   * @return JButton arrow button for this JComboBox.
   */
  protected JButton createArrowButton()
  {
    return new BasicArrowButton(BasicArrowButton.SOUTH);
  }

  /**
   * This method checks if popup part of the combo box is visible on the
   * screen
   *
   * @param c The JComboBox to check
   *
   * @return true if popup part of the JComboBox is visible and false
   *         otherwise.
   */
  public boolean isPopupVisible(JComboBox c)
  {
    return popup.isVisible();
  }

  /**
   * Displays/Hides JComboBox's list of items on the screen.
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
    if (comboBox.isEditable())
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
    if (c instanceof JComboBox)
      {
	JComboBox cb = (JComboBox) c;

	paintBorder(g, comboBox.getBounds(), hasFocus);

	Rectangle rect = rectangleForCurrentValue();
	paintCurrentValueBackground(g, rect, hasFocus);
	paintCurrentValue(g, rect, hasFocus);
      }
  }

  private void paintBorder(Graphics g, Rectangle bounds, boolean hasFocus)
  {
    int x = 0;
    int y = 0;
    int width = bounds.width;
    int height = bounds.height;

    Color oldColor = g.getColor();

    if (! arrowButton.getModel().isPressed())
      BasicGraphicsUtils.drawEtchedRect(g, x, y, width, height, Color.gray,
                                        Color.white, Color.gray, Color.white);
    else
      {
	g.setColor(darkShadow);
	g.drawRect(x, y, width, height);
	g.setColor(shadow);
	g.drawRect(x + 1, y + 1, width - 3, height - 3);
      }
    g.setColor(oldColor);
  }

  /**
   * Returns preferred size for the given menu item.
   *
   * @param c comboBox for which to get preferred size
   *
   * @return $Dimension$ preferred size for the given combo box
   */
  public Dimension getPreferredSize(JComponent c)
  {
    // return null to indicate that combo box's layout will determin its
    // preferred size
    return null;
  }

  /**
   * This method returns the minimum size for this {@link JComboBox} for this
   * look and feel.
   *
   * @param c The {@link JComponent} to find the minimum size for.
   *
   * @return The dimensions of the minimum size.
   */
  public Dimension getMinimumSize(JComponent c)
  {
    return null;
  }

  /**
   * This method returns the maximum size for this {@link JComboBox} for this
   * look and feel.
   *
   * @param c The {@link JComponent} to find the maximum size for
   *
   * @return The dimensions of the minimum size.
   */
  public Dimension getMaximumSize(JComponent c)
  {
    return null;
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
   * This method selects next possible item relative to the current selection
   * to be next selected item in the combo box.
   */
  protected void selectNextPossibleValue()
  {
    int index = comboBox.getSelectedIndex();
    if (index != comboBox.getItemCount() - 1)
      comboBox.setSelectedIndex(index + 1);
  }

  /**
   * This method selects previous item relative to current selection to be
   * next selected item.
   */
  protected void selectPreviousPossibleValue()
  {
    int index = comboBox.getSelectedIndex();
    if (index != 0)
      comboBox.setSelectedIndex(index - 1);
  }

  /**
   * This method displays combo box popup if the popup is not currently shown
   * on the screen and hides it if it is  currently shown
   */
  protected void toggleOpenClose()
  {
    setPopupVisible(comboBox, ! isPopupVisible(comboBox));
  }

  /**
   * This method returns bounds in which comboBox's selected Item will be
   * displayed
   *
   * @return rectangle bounds in which comboBox's selected Item will be
   *         displayed
   */
  protected Rectangle rectangleForCurrentValue()
  {
    Rectangle cbBounds = comboBox.getBounds();

    // Subtract width or the arrow button and border insets	    
    Rectangle rectForCurrentValue = new Rectangle(cbBounds.x
                                                  + borderInsets.left,
                                                  cbBounds.y
                                                  + borderInsets.top,
                                                  cbBounds.width
                                                  - arrowButtonWidth
                                                  - borderInsets.left
                                                  - borderInsets.right,
                                                  cbBounds.height
                                                  - borderInsets.top
                                                  - borderInsets.bottom);

    return rectForCurrentValue;
  }

  /**
   * This method returns insets of the current border.
   *
   * @return Insets representing space between combo box and its border
   */
  protected Insets getInsets()
  {
    return new Insets(0, 0, 0, 0);
  }

  /**
   * This method paints currently selected value in the main part of the combo
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
	Component comp = comboBox.getRenderer()
	                             .getListCellRendererComponent(listBox,
	                                                           (currentValue != null ? currentValue : ""),
	                                                           -1,
	                                                           isPressed,
	                                                           hasFocus);
	if (! comboBox.isEnabled())
	      comp.setEnabled(false);

	g.translate(borderInsets.left, borderInsets.top);
	    comp.setBounds(0, 0, bounds.width, bounds.height);
	    comp.paint(g);
	    g.translate(-borderInsets.left, -borderInsets.top);
	    
	comboBox.revalidate();
      }
    else
      comboBox.getEditor().setItem(comboBox.getSelectedItem());
  }

  /**
   * This method paints background of part of the combo box, where currently
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
   * Returns default size for the combo box that doesn't contain any elements
   * in it
   *
   * @return Default size of the combo box with no elements in it.
   */
  protected Dimension getDefaultSize()
  {
    return new Dimension(6, 17);
  }

  /**
   * Returns size of the largest item in the combo box. This size will be the
   * size of the combo box, not including the arrowButton.
   *
   * @return dimensions of the largest item in the combo box.
   */
  protected Dimension getLargestItemSize()
  {
    ComboBoxModel model = comboBox.getModel();
    int numItems = model.getSize();

    // if combo box doesn't have any items then simply
    // return its default size
    if (numItems == 0)
      {
	largestItemSize = getDefaultSize();
	return largestItemSize;
      }

    Dimension size = new Dimension(0, 0);

    // ComboBox's display size should be equal to the 
    // size of the largest item in the combo box. 
    ListCellRenderer renderer = comboBox.getRenderer();

    for (int i = 0; i < numItems; i++)
      {
	Object item = model.getElementAt(i);
	String s = item.toString();
	Component comp = renderer.getListCellRendererComponent(listBox, item,
	                                                       -1, false, false);

	if (comp.getPreferredSize().getWidth() > size.getWidth())
	  size = comp.getPreferredSize();
      }

    largestItemSize = size;
    return largestItemSize;
  }

  /**
   * This method installs the keyboard actions for the JComboBox as specified
   * by the look and feel.
   */
  protected void installKeyboardActions()
  {
    // FIXME: Need to implement.
  }

  /**
   * This method uninstalls the keyboard actions for the JComboBox there were
   * installed by in {@link #installListeners}.
   */
  protected void uninstallKeyboardActions()
  {
    // FIXME: Need to implement.
  }

  /**
   * This class is Layout Manager for this combo box.
   */
  public class ComboBoxLayoutManager extends Object implements LayoutManager
  {
    /**
     * Creates a new ComboBoxLayoutManager object.
     */
    public ComboBoxLayoutManager()
    {
    }

    public void addLayoutComponent(String name, Component comp)
    {
      // Do nothing
    }

    public void removeLayoutComponent(Component comp)
    {
      // Do nothing
    }

    /**
     * Returns preferred layout size of the JComboBox.
     *
     * @param parent Container for which preferred size should be calculated
     *
     * @return preferred size for the given container
     */
    public Dimension preferredLayoutSize(Container parent)
    {
      Dimension d = new Dimension(0, 0);

      if (largestItemSize == null)
	largestItemSize = getLargestItemSize();

      // add size for the area that will display selected item
      d.width += largestItemSize.getWidth();
      d.height += largestItemSize.getHeight();

      // add size of the arrow button
      d.width += arrowButtonWidth;

      // add width and height of the border
      d.width += borderInsets.left + borderInsets.right;
      d.height += borderInsets.left + borderInsets.right;

      // Add combo box's insets 	
      Insets insets = parent.getInsets();
      d.width += insets.left + insets.right;
      d.width += insets.left + insets.right;

      return d;
    }

    public Dimension minimumLayoutSize(Container parent)
    {
      return preferredLayoutSize(parent);
    }

    /**
     * This method layouts out the components in the container.  It puts arrow
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
      int editorWidth = comboBox.getBounds().width - arrowButtonWidth - 2;

      if (comboBox.isEditable())
	editor.setBounds(borderInsets.left, borderInsets.top, editorWidth,
	                 comboBox.getBounds().height - borderInsets.left
	                 - borderInsets.top);

      arrowButton.setBounds(editorWidth, 2, arrowButtonWidth,
                            comboBox.getBounds().height - 4);
      comboBox.revalidate();
    }
  }

  /**
   * This class handles focus changes occuring in the combo box. This class is
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
    }

    /**
     * This mehtod is invoked when combo box gains focus. It repaints main
     * part of combo box  accordingally.
     *
     * @param e the FocusEvent
     */
    public void focusGained(FocusEvent e)
    {
      hasFocus = true;
      comboBox.repaint();
    }

    /**
     * This method is invoked when combo box loses focus It repaint main part
     * of combo box accordingally and  hides popup list of items.
     *
     * @param e the FocusEvent
     */
    public void focusLost(FocusEvent e)
    {
      hasFocus = false;
      comboBox.repaint();
      popup.hide();
    }
  }

  /**
   * This class handles ItemEvent fired by the JComboBox when its selected
   * item changes.
   */
  public class ItemHandler extends Object implements ItemListener
  {
    /**
     * Creates a new ItemHandler object.
     */
    public ItemHandler()
    {
    }

    /**
     * This method is invoked when selected item becomes deselected or when
     * new item becomes selected.
     *
     * @param e the ItemEvent representing item's state change.
     */
    public void itemStateChanged(ItemEvent e)
    {
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
    }

    /*
     * This method is invoked whenever key is pressed while JComboBox is in
     * focus.
     */
    public void keyPressed(KeyEvent e)
    {
      // FIXME: This method calls JComboBox.selectWithKeyChar if the key that was 
      // pressed is not a navigation key. 
    }
  }

  /**
   * This class handles to the changes occuring in the JComboBox's data model
   */
  public class ListDataHandler extends Object implements ListDataListener
  {
    /**
     * Creates a new ListDataHandler object.
     */
    public ListDataHandler()
    {
    }

    /**
     * This method is invoked content's of JComboBox's data model  are changed
     *
     * @param e ListDataEvent describing the change.
     */
    public void contentsChanged(ListDataEvent e)
    {
      // if the item is selected or deselected
    }

    /**
     * This method is invoked when items were added to the JComboBox's data
     * model.
     *
     * @param e ListDataEvent describing the change.
     */
    public void intervalAdded(ListDataEvent e)
    {
      // must determine if the size of the combo box should change
      int start = e.getIndex0();
      int end = e.getIndex1();

      ComboBoxModel model = comboBox.getModel();
      ListCellRenderer renderer = comboBox.getRenderer();

      if (largestItemSize == null)
	largestItemSize = new Dimension(0, 0);

      for (int i = start - 1; i < end; i++)
        {
	  Object item = model.getElementAt(i);
	  Component comp = renderer.getListCellRendererComponent(new JList(),
	                                                         item, -1,
	                                                         false, false);
	  if (comp.getPreferredSize().getWidth() > largestItemSize.getWidth())
	    largestItemSize = comp.getPreferredSize();
        }
    }

    /**
     * This method is invoked when items were removed from the JComboBox's
     * data model.
     *
     * @param e ListDataEvent describing the change.
     */
    public void intervalRemoved(ListDataEvent e)
    {
      // recalculate display size of the JComboBox.
      largestItemSize = getLargestItemSize();
      comboBox.repaint();
    }
  }

  /**
   * This class handles PropertyChangeEvents fired by JComboBox.
   */
  public class PropertyChangeHandler extends Object
    implements PropertyChangeListener
  {
    public PropertyChangeHandler()
    {
    }

    /**
     * This method is invoked whenever bound property of JComboBox changes.
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

      // FIXME: Need to handle changes in other bound properties.	
    }
  }

  /**
   * MouseHandler listens to mouse events occuring in the combo box. This
   * class is responsible for repainting this JComboBox whenever the mouse is
   * being pressed or released over it.
   */
  private class MouseHandler extends MouseAdapter
  {
    /**
     * This method is invoked when mouse is pressed over the combo box. It
     * repaints the combo box accordinglly
     *
     * @param e the MouseEvent
     */
    public void mousePressed(MouseEvent e)
    {
      if (comboBox.isEnabled())
        {
	  if (e.getSource() instanceof JComboBox)
	    {
	      arrowButton.getModel().setPressed(true);
	      arrowButton.getModel().setArmed(true);
	    }

	  comboBox.repaint();

	  if (e.getSource() instanceof BasicArrowButton)
	    toggleOpenClose();
        }
    }

    /**
     * This method is invoked when mouse is released over the combo box. It
     * repaints the combo box accordinglly
     *
     * @param e the MouseEvent
     */
    public void mouseReleased(MouseEvent e)
    {
      if (comboBox.isEnabled())
        {
	  if (e.getSource() instanceof JComboBox)
	    {
	      arrowButton.getModel().setPressed(false);
	      arrowButton.getModel().setArmed(false);
	    }

	  comboBox.repaint();
        }
    }
  }
}
