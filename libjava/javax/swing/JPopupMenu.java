/* JPopupMenu.java
   Copyright (C) 2002, 2004 Free Software Foundation, Inc.

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

package javax.swing;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.Graphics;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.LayoutManager;
import java.awt.Panel;
import java.awt.Point;
import java.awt.Window;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.EventListener;
import java.util.Vector;
import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;
import javax.swing.plaf.PopupMenuUI;


/**
 * DOCUMENT ME!
 */
public class JPopupMenu extends JComponent implements Accessible, MenuElement
{
  private static final String uiClassID = "PopupMenuUI";
  private static final Object defaultLWPopupEnabledKey = null;
  private static boolean defaultLWPopupEnabled = true;
  transient Component invoker;
  private int locationX;
  private int locationY;
  private String label;
  private boolean paintBorder;
  private Insets margin;
  private boolean lightWeightPopupEnabled;
  private SingleSelectionModel selectionModel;
  private transient Popup popup;
  private Point location;

  /**
   * Creates a new JPopupMenu object.
   */
  public JPopupMenu()
  {
    updateUI();
    
    lightWeightPopupEnabled = defaultLWPopupEnabled;
    selectionModel = new DefaultSingleSelectionModel();
  }

  /**
   * Creates a new JPopupMenu object.
   *
   * @param label DOCUMENT ME!
   */
  public JPopupMenu(String label)
  {
    this.label = label;
  }

  /**
   * DOCUMENT ME!
   *
   * @param stream DOCUMENT ME!
   *
   * @throws IOException DOCUMENT ME!
   * @throws ClassNotFoundException DOCUMENT ME!
   */
  private void readObject(ObjectInputStream stream)
                   throws IOException, ClassNotFoundException
  {
  }

  /**
   * DOCUMENT ME!
   *
   * @param stream DOCUMENT ME!
   *
   * @throws IOException DOCUMENT ME!
   */
  private void writeObject(ObjectOutputStream stream) throws IOException
  {
  }

  /**
   * DOCUMENT ME!
   *
   * @param item DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public JMenuItem add(JMenuItem item)
  {
    this.insert(item, -1);
    return item;
  }

  /**
   * DOCUMENT ME!
   *
   * @param text DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public JMenuItem add(String text)
  {
    JMenuItem item = new JMenuItem(text);
    return add(item);
  }

  /**
   * DOCUMENT ME!
   *
   * @param action DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public JMenuItem add(Action action)
  {
    JMenuItem item = new JMenuItem(action);
    return add(item);
  }

  /**
   * DOCUMENT ME!
   *
   * @param index DOCUMENT ME!
   */
  public void remove(int index)
  {
    super.remove(index);

    GridBagConstraints constraints = new GridBagConstraints();
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.weightx = 100.0;
    constraints.weighty = 100.0;

    Component[] items = getComponents();
    for (int i = index; i < items.length; i++)
      {
	constraints.gridy = i;
	super.add(items[i], constraints, i);
      }
  }

  /**
   * DOCUMENT ME!
   *
   * @param action DOCUMENT ME!
   * @param index DOCUMENT ME!
   */
  public void insert(Action action, int index)
  {
    JMenuItem item = new JMenuItem(action);
    this.insert(item, index);
  }

  /**
   * DOCUMENT ME!
   *
   * @param component DOCUMENT ME!
   * @param index DOCUMENT ME!
   */
  public void insert(Component component, int index)
  {
    GridBagConstraints constraints = new GridBagConstraints();
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.weightx = 100.0;
    constraints.weighty = 100.0;
    
    if (index == -1)
       index = getComponents().length;
       
    constraints.gridy = index;
    super.add(component, constraints, index);

    // need to change constraints for the components that were moved by 1
    // due to the insertion
    if (index != -1)
      {
	Component[] items = getComponents();

	for (int i = index + 1; i < items.length; i++)
	  {
	    constraints.gridy = i;
	    super.add(items[i], constraints, i);
	  }
      }
  }

  /**
   * DOCUMENT ME!
   *
   * @param graphics DOCUMENT ME!
   */
  protected void paintBorder(Graphics graphics)
  {
    if (paintBorder)
      getBorder().paintBorder(this, graphics, 0, 0, getSize(null).width,
                              getSize(null).height);
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public static boolean getDefaultLightWeightPopupEnabled()
  {
    return defaultLWPopupEnabled;
  }

  /**
   * DOCUMENT ME!
   *
   * @param enabled DOCUMENT ME!
   */
  public static void setDefaultLightWeightPopupEnabled(boolean enabled)
  {
    defaultLWPopupEnabled = enabled;
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public PopupMenuUI getUI()
  {
    return (PopupMenuUI) ui;
  }

  /**
   * DOCUMENT ME!
   *
   * @param ui DOCUMENT ME!
   */
  public void setUI(PopupMenuUI ui)
  {
    super.setUI(ui);
  }

  /**
   * DOCUMENT ME!
   */
  public void updateUI()
  {
    setUI((PopupMenuUI) UIManager.getUI(this));
    invalidate();
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public String getUIClassID()
  {
    return "PopupMenuUI";
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public SingleSelectionModel getSelectionModel()
  {
    return selectionModel;
  }

  /**
   * DOCUMENT ME!
   *
   * @param model DOCUMENT ME!
   */
  public void setSelectionModel(SingleSelectionModel model)
  {
    if (selectionModel != model)
      {
	SingleSelectionModel oldModel = this.selectionModel;
      }
  }

  /**
   * DOCUMENT ME!
   *
   * @param action DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  protected JMenuItem createActionComponent(Action action)
  {
    return null;
  }

  /**
   * DOCUMENT ME!
   *
   * @param item DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  protected PropertyChangeListener createActionChangeListener(JMenuItem item)
  {
    return null;
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public boolean isLightWeightPopupEnabled()
  {
    return lightWeightPopupEnabled;
  }

  /**
   * DOCUMENT ME!
   *
   * @param enabled DOCUMENT ME!
   */
  public void setLightWeightPopupEnabled(boolean enabled)
  {
    lightWeightPopupEnabled = enabled;
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public String getLabel()
  {
    return label;
  }

  /**
   * DOCUMENT ME!
   *
   * @param label DOCUMENT ME!
   */
  public void setLabel(String label)
  {
    this.label = label;
  }

  /**
   * DOCUMENT ME!
   */
  public void addSeparator()
  {
  }

  /**
   * DOCUMENT ME!
   *
   * @param listener DOCUMENT ME!
   */
  public void addPopupMenuListener(PopupMenuListener listener)
  {
    listenerList.add(PopupMenuListener.class, listener);
  }

  /**
   * DOCUMENT ME!
   *
   * @param listener DOCUMENT ME!
   */
  public void removePopupMenuListener(PopupMenuListener listener)
  {
    listenerList.remove(PopupMenuListener.class, listener);
  }

  /**
   * DOCUMENT ME!
   */
  protected void firePopupMenuWillBecomeVisible()
  {
    EventListener[] ll = listenerList.getListeners(PopupMenuListener.class);

    for (int i = 0; i < ll.length; i++)
      ((PopupMenuListener) ll[i]).popupMenuWillBecomeVisible(new PopupMenuEvent(this));
  }

  /**
   * DOCUMENT ME!
   */
  protected void firePopupMenuWillBecomeInvisible()
  {
    EventListener[] ll = listenerList.getListeners(PopupMenuListener.class);

    for (int i = 0; i < ll.length; i++)
      ((PopupMenuListener) ll[i]).popupMenuWillBecomeInvisible(new PopupMenuEvent(this));
  }

  /**
   * DOCUMENT ME!
   */
  protected void firePopupMenuCanceled()
  {
    EventListener[] ll = listenerList.getListeners(PopupMenuListener.class);

    for (int i = 0; i < ll.length; i++)
      ((PopupMenuListener) ll[i]).popupMenuCanceled(new PopupMenuEvent(this));
  }

  /**
   * DOCUMENT ME!
   */
  public void pack()
  {
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public boolean isVisible()
  {
    return super.isVisible();
  }

  /**
   * DOCUMENT ME!
   *
   * @param visible DOCUMENT ME!
   */
  public void setVisible(boolean visible)
  {
    super.setVisible(visible);

    firePopupMenuWillBecomeVisible();

    if (visible)
      {
	Container rootContainer = (Container) SwingUtilities.getRoot(invoker);

	boolean fit = true;
	Dimension size;

	// Determine the size of the popup menu
	if (this.getSize().width == 0 && this.getSize().width == 0)
	  size = this.getPreferredSize();
	else
	  size = this.getSize();

	if ((size.width > (rootContainer.getWidth() - locationX))
	    || (size.height > (rootContainer.getHeight() - locationY)))
	  fit = false;

	if (lightWeightPopupEnabled && fit)
	  popup = new LightWeightPopup(this);
	else
	  {
	    if (fit)
	      popup = new MediumWeightPopup(this);
	    else
	      popup = new HeavyWeightPopup(this);
	  }
	
        if (popup instanceof LightWeightPopup 
            || popup instanceof MediumWeightPopup)
          {
            JLayeredPane layeredPane;
            layeredPane = SwingUtilities.getRootPane(invoker).getLayeredPane();
            Point lp = layeredPane.getLocationOnScreen();
            Point r = SwingUtilities.getRoot(invoker).getLocationOnScreen();
            int px = locationX - (lp.x - r.x);
            int py = locationY - (lp.y - r.y);	  
            popup.show(px, py, size.width, size.height);	  		
          } 
        else
          popup.show(locationX, locationY, size.width, size.height);
      }
    else
      {
	firePopupMenuWillBecomeInvisible();
	popup.hide();
      }
  }

  /**
   * DOCUMENT ME!
   *
   * @param x DOCUMENT ME!
   * @param y DOCUMENT ME!
   */
  public void setLocation(int x, int y)
  {
    locationX = x;
    locationY = y;
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  private boolean isPopupMenu()
  {
    return true;
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public Component getInvoker()
  {
    return invoker;
  }

  /**
   * DOCUMENT ME!
   *
   * @param component DOCUMENT ME!
   */
  public void setInvoker(Component component)
  {
    invoker = component;
  }

  /**
   * DOCUMENT ME!
   *
   * @param component DOCUMENT ME!
   * @param x DOCUMENT ME!
   * @param y DOCUMENT ME!
   */
  public void show(Component component, int x, int y)
  {
    setInvoker(component);

    Point rootOnScreen;
    rootOnScreen = SwingUtilities.getRoot(invoker).getLocationOnScreen();
    Point invokerOnScreen = invoker.getLocationOnScreen();
    
    int popupX = (invokerOnScreen.x - rootOnScreen.x) + x;
    int popupY = (invokerOnScreen.y - rootOnScreen.y) + y;
    
    setLocation(popupX , popupY);
    setVisible(true);
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  JPopupMenu getRootPopupMenu()
  {
    return null;
  }

  /**
   * DOCUMENT ME!
   *
   * @param index DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public Component getComponentAtIndex(int index)
  {
    return getComponent(index);
  }

  /**
   * DOCUMENT ME!
   *
   * @param component DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public int getComponentIndex(Component component)
  {
    Component[] items = getComponents();

    for (int i = 0; i < items.length; i++)
      {
	if (items[i].equals(component))
	  return i;
      }

    return -1;
  }

  /**
   * DOCUMENT ME!
   *
   * @param size DOCUMENT ME!
   */
  public void setPopupSize(Dimension size)
  {
    super.setSize(size);
  }

  /**
   * DOCUMENT ME!
   *
   * @param x DOCUMENT ME!
   * @param y DOCUMENT ME!
   */
  public void setPopupSize(int x, int y)
  {
    super.setSize(x, y);
  }

  /**
   * DOCUMENT ME!
   *
   * @param selected DOCUMENT ME!
   */
  public void setSelected(Component selected)
  {
    int index = getComponentIndex(selected);
    selectionModel.setSelectedIndex(index);
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public boolean isBorderPainted()
  {
    return paintBorder;
  }

  /**
   * DOCUMENT ME!
   *
   * @param painted DOCUMENT ME!
   */
  public void setBorderPainted(boolean painted)
  {
    paintBorder = painted;
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public Insets getMargin()
  {
    return margin;
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  protected String paramString()
  {
    return "JPopupMenu";
  }

  /**
   * DOCUMENT ME!
   *
   * @param event DOCUMENT ME!
   * @param path DOCUMENT ME!
   * @param manager DOCUMENT ME!
   */
  public void processMouseEvent(MouseEvent event, MenuElement[] path,
                                MenuSelectionManager manager)
  {
  }

  /**
   * DOCUMENT ME!
   *
   * @param event DOCUMENT ME!
   * @param path DOCUMENT ME!
   * @param manager DOCUMENT ME!
   */
  public void processKeyEvent(KeyEvent event, MenuElement[] path,
                              MenuSelectionManager manager)
  {
  }

  /**
   * DOCUMENT ME!
   *
   * @param changed DOCUMENT ME!
   */
  public void menuSelectionChanged(boolean changed)
  {
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public MenuElement[] getSubElements()
  {
    Component[] items = getComponents();
    MenuElement[] subElements = new MenuElement[items.length];

    for (int i = 0; i < items.length; i++)
      subElements[i] = (MenuElement) items[i];

    return subElements;
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public Component getComponent()
  {
    return this;
  }

  /**
   * DOCUMENT ME!
   *
   * @param event DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public boolean isPopupTrigger(MouseEvent event)
  {
    return ((PopupMenuUI)getUI()).isPopupTrigger(event);
    
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public AccessibleContext getAccessibleContext()
  {
    if (accessibleContext == null)
      accessibleContext = new AccessibleJPopupMenu(this);

    return accessibleContext;
  }

  /**
   * DOCUMENT ME!
   */
  private interface Popup
  {
    /**
     * DOCUMENT ME!
     *
     * @param x DOCUMENT ME!
     * @param y DOCUMENT ME!
     * @param width DOCUMENT ME!
     * @param height DOCUMENT ME!
     */
    void show(int x, int y, int width, int height);

    /**
     * DOCUMENT ME!
     */
    void hide();
  }

  /**
   * DOCUMENT ME!
   */
  private class LightWeightPopup extends JPanel implements Popup
  {
    /**
     * Creates a new LightWeightPopup object.
     *
     * @param c DOCUMENT ME!
     */
    public LightWeightPopup(Container c)
    {
      this.add(c);
    }

    /**
     * DOCUMENT ME!
     *
     * @param x DOCUMENT ME!
     * @param y DOCUMENT ME!
     * @param width DOCUMENT ME!
     * @param height DOCUMENT ME!
     */
    public void show(int x, int y, int width, int height)
    {
      JLayeredPane layeredPane;
      layeredPane = SwingUtilities.getRootPane(invoker).getLayeredPane();
      this.setBounds(x, y, width, height);
      layeredPane.add(this, JLayeredPane.POPUP_LAYER, 0);
    }

    /**
     * DOCUMENT ME!
     */
    public void hide()
    {
      JLayeredPane layeredPane;
      layeredPane = SwingUtilities.getRootPane(invoker).getLayeredPane();
      int index = layeredPane.getIndexOf(this);
      layeredPane.remove(index);
    }
  }

  /**
   * DOCUMENT ME!
   */
  private class MediumWeightPopup extends Panel implements Popup
  {

    /**
     * Creates a new MediumWeightPopup object.
     *
     * @param c DOCUMENT ME!
     */
    public MediumWeightPopup(Container c)
    {
      this.add(c);      
    }

    /**
     * DOCUMENT ME!
     *
     * @param x DOCUMENT ME!
     * @param y DOCUMENT ME!
     * @param width DOCUMENT ME!
     * @param heigth DOCUMENT ME!
     */
    public void show(int x, int y, int width, int height)
    {
      JLayeredPane layeredPane;
      layeredPane = SwingUtilities.getRootPane(invoker).getLayeredPane();
      layeredPane.add(this, JLayeredPane.POPUP_LAYER, 0);
      this.setBounds(x, y, width, height);
    }

    /**
     * DOCUMENT ME!
     */
    public void hide()
    {
      JLayeredPane layeredPane;
      layeredPane = SwingUtilities.getRootPane(invoker).getLayeredPane(); 
      int index = layeredPane.getIndexOf(this);
      layeredPane.remove(index);
    }
  }

  /**
   * DOCUMENT ME!
   */
  private class HeavyWeightPopup extends JWindow implements Popup
  {
    /**
     * Creates a new HeavyWeightPopup object.
     *
     * @param c DOCUMENT ME!
     */
    public HeavyWeightPopup(Container c)
    {
      this.setContentPane(c);
    }

    /**
     * DOCUMENT ME!
     *
     * @param x DOCUMENT ME!
     * @param y DOCUMENT ME!
     * @param width DOCUMENT ME!
     * @param height DOCUMENT ME!
     */
    public void show(int x, int y, int width, int height)
    {
      this.setBounds(x, y, width, height);
      this.show();
    }

    /**
     * DOCUMENT ME!
     */
    public void hide()
    {
      this.hide();
    }
  }

  /**
   * DOCUMENT ME!
   */
  public static class Separator extends JSeparator
  {
    /**
     * Creates a new Separator object.
     */
    public Separator()
    {
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getUIClassID()
    {
      return null;
    }
  }

  /**
   * DOCUMENT ME!
   */
  protected class AccessibleJPopupMenu extends AccessibleJComponent
  {
    /**
     * Creates a new AccessibleJPopupMenu object.
     *
     * @param component DOCUMENT ME!
     */
    protected AccessibleJPopupMenu(JPopupMenu component)
    {
      super(component);
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public AccessibleRole getAccessibleRole()
    {
      return AccessibleRole.POPUP_MENU;
    }
  }
}
