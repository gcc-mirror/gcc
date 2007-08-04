/* HeadlessToolkit.java -- A toolkit for headless mode
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package gnu.java.awt.peer.headless;

import gnu.java.awt.ClasspathToolkit;
import gnu.java.awt.EmbeddedWindow;
import gnu.java.awt.peer.ClasspathFontPeer;
import gnu.java.awt.peer.EmbeddedWindowPeer;

import java.awt.AWTException;
import java.awt.Button;
import java.awt.Canvas;
import java.awt.Checkbox;
import java.awt.CheckboxMenuItem;
import java.awt.Choice;
import java.awt.Dialog;
import java.awt.Dimension;
import java.awt.EventQueue;
import java.awt.FileDialog;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Frame;
import java.awt.GraphicsDevice;
import java.awt.GraphicsEnvironment;
import java.awt.HeadlessException;
import java.awt.Image;
import java.awt.Label;
import java.awt.List;
import java.awt.Menu;
import java.awt.MenuBar;
import java.awt.MenuItem;
import java.awt.Panel;
import java.awt.PopupMenu;
import java.awt.PrintJob;
import java.awt.ScrollPane;
import java.awt.Scrollbar;
import java.awt.TextArea;
import java.awt.TextField;
import java.awt.Window;
import java.awt.datatransfer.Clipboard;
import java.awt.dnd.DragGestureEvent;
import java.awt.dnd.peer.DragSourceContextPeer;
import java.awt.im.InputMethodHighlight;
import java.awt.image.ColorModel;
import java.awt.image.ImageObserver;
import java.awt.image.ImageProducer;
import java.awt.peer.ButtonPeer;
import java.awt.peer.CanvasPeer;
import java.awt.peer.CheckboxMenuItemPeer;
import java.awt.peer.CheckboxPeer;
import java.awt.peer.ChoicePeer;
import java.awt.peer.DialogPeer;
import java.awt.peer.FileDialogPeer;
import java.awt.peer.FontPeer;
import java.awt.peer.FramePeer;
import java.awt.peer.LabelPeer;
import java.awt.peer.ListPeer;
import java.awt.peer.MenuBarPeer;
import java.awt.peer.MenuItemPeer;
import java.awt.peer.MenuPeer;
import java.awt.peer.PanelPeer;
import java.awt.peer.PopupMenuPeer;
import java.awt.peer.RobotPeer;
import java.awt.peer.ScrollPanePeer;
import java.awt.peer.ScrollbarPeer;
import java.awt.peer.TextAreaPeer;
import java.awt.peer.TextFieldPeer;
import java.awt.peer.WindowPeer;
import java.io.InputStream;
import java.net.URL;
import java.util.Map;
import java.util.Properties;

public class HeadlessToolkit
  extends ClasspathToolkit
{

  /**
   * The graphics environment for headless graphics.
   */
  private HeadlessGraphicsEnvironment graphicsEnv;

  public void beep()
  {
    // TODO Auto-generated method stub

  }

  public int checkImage(Image image, int width, int height,
                        ImageObserver observer)
  {
    // TODO Auto-generated method stub
    return 0;
  }

  protected ButtonPeer createButton(Button target)
  {
    throw new HeadlessException();
  }

  protected CanvasPeer createCanvas(Canvas target)
  {
    throw new HeadlessException();
  }

  protected CheckboxPeer createCheckbox(Checkbox target)
  {
    throw new HeadlessException();
  }

  protected CheckboxMenuItemPeer createCheckboxMenuItem(CheckboxMenuItem target)
  {
    throw new HeadlessException();
  }

  protected ChoicePeer createChoice(Choice target)
  {
    throw new HeadlessException();
  }

  protected DialogPeer createDialog(Dialog target)
  {
    throw new HeadlessException();
  }

  public DragSourceContextPeer createDragSourceContextPeer(DragGestureEvent e)
  {
    throw new HeadlessException();
  }

  protected FileDialogPeer createFileDialog(FileDialog target)
  {
    throw new HeadlessException();
  }

  protected FramePeer createFrame(Frame target)
  {
    throw new HeadlessException();
  }

  public Image createImage(String filename)
  {
    // FIXME: Implement.
    return null;
  }

  public Image createImage(URL url)
  {
    // FIXME: Implement.
    return null;
  }

  public Image createImage(ImageProducer producer)
  {
    // FIXME: Implement.
    return null;
  }

  public Image createImage(byte[] data, int offset, int len)
  {
    // TODO Auto-generated method stub
    return null;
  }

  protected LabelPeer createLabel(Label target)
  {
    throw new HeadlessException();
  }

  protected ListPeer createList(List target)
  {
    throw new HeadlessException();
  }

  protected MenuPeer createMenu(Menu target)
  {
    throw new HeadlessException();
  }

  protected MenuBarPeer createMenuBar(MenuBar target)
  {
    throw new HeadlessException();
  }

  protected MenuItemPeer createMenuItem(MenuItem target)
  {
    throw new HeadlessException();
  }

  protected PanelPeer createPanel(Panel target)
  {
    throw new HeadlessException();
  }

  protected PopupMenuPeer createPopupMenu(PopupMenu target)
  {
    throw new HeadlessException();
  }

  protected ScrollPanePeer createScrollPane(ScrollPane target)
  {
    throw new HeadlessException();
  }

  protected ScrollbarPeer createScrollbar(Scrollbar target)
  {
    throw new HeadlessException();
  }

  protected TextAreaPeer createTextArea(TextArea target)
  {
    throw new HeadlessException();
  }

  protected TextFieldPeer createTextField(TextField target)
  {
    throw new HeadlessException();
  }

  protected WindowPeer createWindow(Window target)
  {
    throw new HeadlessException();
  }

  public ColorModel getColorModel()
  {
    // TODO Auto-generated method stub
    return null;
  }

  public String[] getFontList()
  {
    // TODO Auto-generated method stub
    return null;
  }

  public FontMetrics getFontMetrics(Font name)
  {
    // TODO Auto-generated method stub
    return null;
  }

  protected FontPeer getFontPeer(String name, int style)
  {
    // TODO Auto-generated method stub
    return null;
  }

  public Image getImage(String name)
  {
    // TODO Auto-generated method stub
    return null;
  }

  public Image getImage(URL url)
  {
    // TODO Auto-generated method stub
    return null;
  }

  public PrintJob getPrintJob(Frame frame, String title, Properties props)
  {
    // TODO Auto-generated method stub
    return null;
  }

  public int getScreenResolution()
  {
    throw new HeadlessException();
  }

  public Dimension getScreenSize()
  {
    throw new HeadlessException();
  }

  public Clipboard getSystemClipboard()
  {
    throw new HeadlessException();
  }

  protected EventQueue getSystemEventQueueImpl()
  {
    throw new HeadlessException();
  }

  public Map mapInputMethodHighlight(InputMethodHighlight highlight)
  {
    // TODO Auto-generated method stub
    return null;
  }

  public boolean prepareImage(Image image, int width, int height,
                              ImageObserver observer)
  {
    // TODO Auto-generated method stub
    return false;
  }

  public void sync()
  {
    // TODO Auto-generated method stub

  }

  public EmbeddedWindowPeer createEmbeddedWindow(EmbeddedWindow w)
  {
    throw new HeadlessException();
  }

  public Font createFont(int format, InputStream stream)
  {
    // TODO Auto-generated method stub
    return null;
  }

  public RobotPeer createRobot(GraphicsDevice screen) throws AWTException
  {
    throw new HeadlessException();
  }

  public ClasspathFontPeer getClasspathFontPeer(String name, Map attrs)
  {
    // TODO Auto-generated method stub
    return null;
  }

  public GraphicsEnvironment getLocalGraphicsEnvironment()
  {
    if (graphicsEnv == null)
      graphicsEnv = new HeadlessGraphicsEnvironment();
    return graphicsEnv;
  }

  @Override
  public boolean isModalExclusionTypeSupported
  (Dialog.ModalExclusionType modalExclusionType)
  {
    return false;
  }

  @Override
  public boolean isModalityTypeSupported(Dialog.ModalityType modalityType)
  {
    return false;
  }


}
