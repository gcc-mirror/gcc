/* StandaloneAppletWindow.java -- an applet frame
   Copyright (C) 2003, 2004, 2006  Free Software Foundation, Inc.

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

package gnu.classpath.tools.appletviewer;

import java.applet.Applet;
import java.applet.AppletContext;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.Insets;
import java.awt.Label;
import java.awt.Menu;
import java.awt.MenuBar;
import java.awt.MenuItem;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.ContainerEvent;
import java.awt.event.ContainerListener;
import java.awt.event.HierarchyBoundsListener;
import java.awt.event.HierarchyEvent;
import java.awt.event.HierarchyListener;
import java.awt.event.InputMethodEvent;
import java.awt.event.InputMethodListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.HashMap;
import java.util.List;

class StandaloneAppletWindow
    extends Frame
    implements ActionListener, ContainerListener, ComponentListener,
    MouseListener, MouseMotionListener, InputMethodListener, HierarchyListener,
    HierarchyBoundsListener
{
  
  // This class implements various listeners because the author of an applet
  // may attach listeners to it, unaware of the applet's parent (this class). 
  // So, we must pass all listener events on this plugin applet window to the
  // actual applet.
  
  private static int testWindowCount;
  private static HashMap contexts = new HashMap();
  private Applet applet;
  private Label status = new Label();

  private MenuItem restartItem;
  private MenuItem reloadItem;
  private MenuItem cancelItem;
  private MenuItem saveItem;
  private MenuItem startItem;
  private MenuItem cloneItem;
  private MenuItem tagItem;
  private MenuItem infoItem;
  private MenuItem editItem;
  private MenuItem encodingItem;
  private MenuItem printItem;
  private MenuItem propertiesItem;
  private MenuItem closeItem;
  private MenuItem quitItem;

  StandaloneAppletWindow(AppletTag tag, List appletWindows)
  {
    appletWindows.add(this);
    applet = Main.createApplet(tag);

    if (contexts.get(tag.codebase) == null)
      contexts.put(tag.codebase, new StandaloneAppletContext(appletWindows));

    setLayout(new BorderLayout());
    add(applet, BorderLayout.CENTER);
    add(status, BorderLayout.SOUTH);

    addWindowListener(new WindowAdapter()
        {
	  public void windowClosing(WindowEvent event)
	  {
	    applet.stop();
	    StandaloneAppletWindow.this.hide();
	    System.exit(0);
	  }
        });
    
    addContainerListener(this);
    addComponentListener(this);
    addMouseListener(this);
    addMouseMotionListener(this);
    addInputMethodListener(this);
    addHierarchyListener(this);
    addHierarchyBoundsListener(this);
    
    restartItem = new MenuItem(Messages.getString("StandaloneAppletWindow.MenuRestart"));
    restartItem.setEnabled(false);
    restartItem.addActionListener(this);
    reloadItem = new MenuItem(Messages.getString("StandaloneAppletWindow.MenuReload"));
    reloadItem.setEnabled(false);
    reloadItem.addActionListener(this);
    cancelItem = new MenuItem(Messages.getString("StandaloneAppletWindow.MenuCancel"));
    cancelItem.setEnabled(false);
    cancelItem.addActionListener(this);
    saveItem = new MenuItem(Messages.getString("StandaloneAppletWindow.MenuSave"));
    saveItem.setEnabled(false);
    saveItem.addActionListener(this);
    startItem = new MenuItem(Messages.getString("StandaloneAppletWindow.MenuStart"));
    startItem.setEnabled(false);
    startItem.addActionListener(this);
    cloneItem = new MenuItem(Messages.getString("StandaloneAppletWindow.MenuClone"));
    cloneItem.setEnabled(false);
    cloneItem.addActionListener(this);
    closeItem = new MenuItem(Messages.getString("StandaloneAppletWindow.MenuClose"));
    closeItem.setEnabled(false);
    closeItem.addActionListener(this);
    tagItem =
      new MenuItem(Messages.getString("StandaloneAppletWindow.MenuTag"));
    tagItem.setEnabled(false);
    tagItem.addActionListener(this);
    infoItem =
      new MenuItem(Messages.getString("StandaloneAppletWindow.MenuInfo"));
    infoItem.setEnabled(false);
    infoItem.addActionListener(this);
    editItem =
      new MenuItem(Messages.getString("StandaloneAppletWindow.MenuEdit"));
    editItem.setEnabled(false);
    editItem.addActionListener(this);
    editItem.setEnabled(false);
    encodingItem =
      new MenuItem(Messages.getString("StandaloneAppletWindow.MenuEncoding"));
    encodingItem.setEnabled(false);
    encodingItem.addActionListener(this);
    printItem =
      new MenuItem(Messages.getString("StandaloneAppletWindow.MenuPrint"));
    printItem.setEnabled(false);
    printItem.addActionListener(this);
    propertiesItem =
      new MenuItem(Messages.getString("StandaloneAppletWindow.MenuProperties"));
    propertiesItem.setEnabled(false);
    propertiesItem.addActionListener(this);
    quitItem = new MenuItem(Messages.getString("StandaloneAppletWindow.MenuQuit"));
    quitItem.addActionListener(this);

    MenuBar menuBar = new MenuBar();
    Menu menuApplet = new Menu(Messages.getString("StandaloneAppletWindow.MenuTitle"));
    menuBar.add(menuApplet);
    menuApplet.add(restartItem);
    menuApplet.add(reloadItem);
    menuApplet.add(cancelItem);
    menuApplet.add(saveItem);
    menuApplet.add(startItem);
    menuApplet.add(cloneItem);
    menuApplet.addSeparator();
    menuApplet.add(tagItem);
    menuApplet.add(infoItem);
    menuApplet.add(editItem);
    menuApplet.add(encodingItem);
    menuApplet.addSeparator();
    menuApplet.add(printItem);
    menuApplet.addSeparator();
    menuApplet.add(propertiesItem);
    menuApplet.addSeparator();
    menuApplet.add(closeItem);
    menuApplet.add(quitItem);
    setMenuBar(menuBar);
    setTitle(Messages.getString("StandaloneAppletWindow.WindowTitle")
             + " " + tag.code);

    AppletContext context = (AppletContext) contexts.get(tag.codebase);
    ((StandaloneAppletContext) context).addApplet(applet);

    applet.setStub(new CommonAppletStub(tag, context, applet));

    // Create the frame's peer. Otherwise getPreferredSize will read
    // its insets as 0.
    addNotify();
    Insets i = getInsets();
    Dimension size = tag.getSize();
    setSize(i.left + size.width + i.right,
            i.top + size.height + status.getPreferredSize().height
            + i.bottom);
    applet.setSize(size);

    // Initialize the applet before showing this window so that the
    // applet doesn't receive events before it has been initialized.
    applet.init();
    applet.start();
    setVisible(true);
  }

  private void closeWindow()
  {
    applet.stop();
    StandaloneAppletViewer.appletWindows.remove(this);
    StandaloneAppletWindow.this.hide();
  }

  public void actionPerformed(ActionEvent e)
  {
    if (e.getSource() == quitItem)
      {
        closeWindow();
        System.exit(0);
      }
    else if (e.getSource() == closeItem)
      {
	// Close current window.
	closeWindow();
	
	// Exit if there are other windows left.
	if (StandaloneAppletViewer.appletWindows.isEmpty())
          System.exit(0);
      }
  }

  void showStatus(String status)
  {
    this.status.setText(status);
  }
  

  ///////////////////////////////////
  /// ContainerListener Methods /////
  ///////////////////////////////////
  
  /**
   * This method is called when a component is added to the container.
   * 
   * @param event the <code>ContainerEvent</code> indicating component
   *          addition
   */
  public void componentAdded(ContainerEvent event)
  {
    if (applet != null)
      {
        ContainerListener[] l = applet.getContainerListeners();
        for (int i = 0; i < l.length; i++)
          l[i].componentAdded(event);
      }
  }

  /**
   * This method is called when a component is removed from the container.
   * 
   * @param event the <code>ContainerEvent</code> indicating component removal
   */
  public void componentRemoved(ContainerEvent event)
  {
    if (applet != null)
      {
        ContainerListener[] l = applet.getContainerListeners();
        for (int i = 0; i < l.length; i++)
          l[i].componentRemoved(event);
      }
  }
  
  ///////////////////////////////////
  /// ComponentListener Methods /////
  ///////////////////////////////////
  
  /**
   * This method is called when the component is resized.
   *
   * @param event the <code>ComponentEvent</code> indicating the resize
   */
  public void componentResized(ComponentEvent event)
  {
    if (applet != null)
      {
        ComponentListener[] l = applet.getComponentListeners();
        for (int i = 0; i < l.length; i++)
          l[i].componentResized(event);
      }
  }

  /**
   * This method is called when the component is moved.
   *
   * @param event the <code>ComponentEvent</code> indicating the move
   */
  public void componentMoved(ComponentEvent event)
  {
    if (applet != null)
      {
        ComponentListener[] l = applet.getComponentListeners();
        for (int i = 0; i < l.length; i++)
          l[i].componentMoved(event);
      }
  }

  /**
   * This method is called when the component is made visible.
   *
   * @param event the <code>ComponentEvent</code> indicating the visibility
   */
  public void componentShown(ComponentEvent event)
  {
    if (applet != null)
      {
        ComponentListener[] l = applet.getComponentListeners();
        for (int i = 0; i < l.length; i++)
          l[i].componentShown(event);
      }
  }

  /**
   * This method is called when the component is hidden.
   *
   * @param event the <code>ComponentEvent</code> indicating the visibility
   */
  public void componentHidden(ComponentEvent event)
  {
    if (applet != null)
      {
        ComponentListener[] l = applet.getComponentListeners();
        for (int i = 0; i < l.length; i++)
          l[i].componentHidden(event);
      }
  }
     
  ///////////////////////////////////
  ////// MouseListener Methods //////
  ///////////////////////////////////
  
  /**
   * This method is called when the mouse is clicked (pressed and released
   * in short succession) on a component.
   *
   * @param event the <code>MouseEvent</code> indicating the click
   */
  public void mouseClicked(MouseEvent event)
  {
    if (applet != null)
      {
        MouseListener[] l = applet.getMouseListeners();
        for (int i = 0; i < l.length; i++)
          l[i].mouseClicked(event);
      }
  }

  /**
   * This method is called when the mouse is pressed over a component.
   *
   * @param event the <code>MouseEvent</code> for the press
   */
  public void mousePressed(MouseEvent event)
  {
    if (applet != null)
      {
        MouseListener[] l = applet.getMouseListeners();
        for (int i = 0; i < l.length; i++)
          l[i].mousePressed(event);
      }
  }

  /**
   * This method is called when the mouse is released over a component.
   *
   * @param event the <code>MouseEvent</code> for the release
   */
  public void mouseReleased(MouseEvent event)
  {
    if (applet != null)
      {
        MouseListener[] l = applet.getMouseListeners();
        for (int i = 0; i < l.length; i++)
          l[i].mouseReleased(event);
      }
  }

  /**
   * This method is called when the mouse enters a component.
   *
   * @param event the <code>MouseEvent</code> for the entry
   */
  public void mouseEntered(MouseEvent event)
  {
    if (applet != null)
      {
        MouseListener[] l = applet.getMouseListeners();
        for (int i = 0; i < l.length; i++)
          l[i].mouseEntered(event);
      }
  }

  /** 
   * This method is called when the mouse exits a component.
   *
   * @param event the <code>MouseEvent</code> for the exit
   */
  public void mouseExited(MouseEvent event)
  {
    if (applet != null)
      {
        MouseListener[] l = applet.getMouseListeners();
        for (int i = 0; i < l.length; i++)
          l[i].mouseExited(event);
      }
  }
  
  ///////////////////////////////////
  /// MouseMotionListener Methods ///
  ///////////////////////////////////
  
  /**
   * This method is called when the mouse is moved over a component
   * while a button has been pressed.
   *
   * @param event the <code>MouseEvent</code> indicating the motion
   */
  public void mouseDragged(MouseEvent event)
  {
    if (applet != null)
      {
        MouseMotionListener[] l = applet.getMouseMotionListeners();
        for (int i = 0; i < l.length; i++)
          l[i].mouseDragged(event);
      }
  }

  /**
   * This method is called when the mouse is moved over a component
   * while no button is pressed.
   *
   * @param event the <code>MouseEvent</code> indicating the motion
   */
  public void mouseMoved(MouseEvent event)
  {
    if (applet != null)
      {
        MouseMotionListener[] l = applet.getMouseMotionListeners();
        for (int i = 0; i < l.length; i++)
          l[i].mouseMoved(event);
      }
  }
  
  ///////////////////////////////////
  /// InputMethodListener Methods ///
  ///////////////////////////////////
  
  /**
   * This method is called when the text is changed.
   *
   * @param event the <code>InputMethodEvent</code> indicating the text change
   */
  public void inputMethodTextChanged(InputMethodEvent event)
  {
    if (applet != null)
      {
        InputMethodListener[] l = applet.getInputMethodListeners();
        for (int i = 0; i < l.length; i++)
          l[i].inputMethodTextChanged(event);
      }
  }

  /**
   * This method is called when the cursor position within the text is changed.
   *
   * @param event the <code>InputMethodEvent</code> indicating the change
   */
  public void caretPositionChanged(InputMethodEvent event)
  {
    if (applet != null)
      {
        InputMethodListener[] l = applet.getInputMethodListeners();
        for (int i = 0; i < l.length; i++)
          l[i].caretPositionChanged(event);
      }
  }
  
  ///////////////////////////////////
  //// HierarchyListener Methods ////
  ///////////////////////////////////
  
  /**
   * Called when the hierarchy of this component changes. Use
   * <code>getChangeFlags()</code> on the event to see what exactly changed.
   *
   * @param e the event describing the change
   */
  public void hierarchyChanged(HierarchyEvent event)
  {
    if (applet != null)
      {
        HierarchyListener[] l = applet.getHierarchyListeners();
        for (int i = 0; i < l.length; i++)
          l[i].hierarchyChanged(event);
      }
  }
  
  /////////////////////////////////////////
  //// HierarchyBoundsListener Methods ////
  /////////////////////////////////////////
  
  /**
   * Called when an ancestor component of the source is moved.
   *
   * @param e the event describing the ancestor's motion
   */
  public void ancestorMoved(HierarchyEvent e)
  {
    if (applet != null)
      {
        HierarchyBoundsListener[] l = applet.getHierarchyBoundsListeners();
        for (int i = 0; i < l.length; i++)
          l[i].ancestorMoved(e);
      }
  }

  /**
   * Called when an ancestor component is resized.
   *
   * @param e the event describing the ancestor's resizing
   */
  public void ancestorResized(HierarchyEvent e)
  {
    if (applet != null)
      {
        HierarchyBoundsListener[] l = applet.getHierarchyBoundsListeners();
        for (int i = 0; i < l.length; i++)
          l[i].ancestorResized(e);
      }
  }
}
