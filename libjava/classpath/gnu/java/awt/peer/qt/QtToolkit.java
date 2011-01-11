/* QtToolkit.java --
   Copyright (C)  2005, 2006, 2007  Free Software Foundation, Inc.

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

package gnu.java.awt.peer.qt;

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
import java.awt.event.AWTEventListener;
import java.awt.im.InputMethodHighlight;
import java.awt.image.ColorModel;
import java.awt.image.DirectColorModel;
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
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

public class QtToolkit extends ClasspathToolkit
{
  public static EventQueue eventQueue = null; // the native event queue
  public static QtRepaintThread repaintThread = null;
  public static MainQtThread guiThread = null;
  public static QtGraphicsEnvironment graphicsEnv = null;

  private static void initToolkit()
  {
    eventQueue = new EventQueue();
    repaintThread = new QtRepaintThread();
    System.loadLibrary("qtpeer");

    String theme = null;
    try
      {
        String style = System.getProperty("qtoptions.style");
        if(style != null)
          theme = style;
      }
    catch(SecurityException e)
      {
      }
    catch(IllegalArgumentException e)
      {
      }

    boolean doublebuffer = true;
    try
      {
        String style = System.getProperty("qtoptions.nodoublebuffer");
        if(style != null)
          doublebuffer = false;
      }
    catch(SecurityException e)
      {
      }
    catch(IllegalArgumentException e)
      {
      }

    guiThread = new MainQtThread( theme, doublebuffer );
    guiThread.start();
    repaintThread.start();
  }

  /**
   * Construct the toolkit!
   */
  public QtToolkit()
  {
    if( guiThread == null )
      initToolkit();

    // make sure the GUI thread has started.
    while (!guiThread.isRunning())
      ;

    if( graphicsEnv == null )
      graphicsEnv = new QtGraphicsEnvironment( this );
  }

  native String[] nativeFontFamilies();

  native int numScreens();

  native int defaultScreen();

  // ************ Public methods *********************

  public synchronized native void beep();

  public int checkImage(Image image, int w, int h, ImageObserver observer)
  {
    if(image instanceof QtImage)
      return ((QtImage)image).checkImage(observer);

    return ImageObserver.ERROR; // FIXME
  }

  protected ButtonPeer createButton( Button target )
  {
    return new QtButtonPeer( this, target );
  }

  protected CanvasPeer createCanvas(Canvas target)
  {
    return new QtCanvasPeer( this, target );
  }

  protected CheckboxPeer createCheckbox(Checkbox target)
  {
    return new QtCheckboxPeer( this, target );
  }

  protected  ChoicePeer createChoice(Choice target)
  {
    return new QtChoicePeer( this, target );
  }

  protected CheckboxMenuItemPeer createCheckboxMenuItem(CheckboxMenuItem target)
  {
    return new QtMenuItemPeer( this, target );
  }

  public DragSourceContextPeer createDragSourceContextPeer(DragGestureEvent dge)
  {
    throw new RuntimeException("Not implemented");
  }

  protected FramePeer createFrame(Frame target)
  {
    return new QtFramePeer( this, target );
  }

  protected FileDialogPeer createFileDialog(FileDialog target)
  {
    return new QtFileDialogPeer( this, target );
  }

  public Image createImage(ImageProducer producer)
  {
    return new QtImage( producer );
  }

  public Image createImage(byte[] imageData,
                           int imageOffset,
                           int imageLength)
  {
    byte[] dataCopy = new byte[imageLength];
    System.arraycopy(imageData, imageOffset, dataCopy, 0, imageLength);
    return new QtImage( dataCopy );
  }

  public Image createImage(String filename)
  {
    return new QtImage( filename );
  }

  public Image createImage(URL url)
  {
    return new QtImage( url );
  }

  protected TextFieldPeer createTextField(TextField target)
  {
    return new QtTextFieldPeer(this,target);
  }

  protected LabelPeer createLabel(Label target)
  {
    return new QtLabelPeer( this, target );
  }

  protected ListPeer createList(List target)
  {
    return new QtListPeer( this, target );
  }

  protected ScrollbarPeer createScrollbar(Scrollbar target)
  {
    return new QtScrollbarPeer( this, target );
  }

  protected ScrollPanePeer createScrollPane(ScrollPane target)
  {
    return new QtScrollPanePeer( this, target );
  }

  protected TextAreaPeer createTextArea(TextArea target)
  {
    return new QtTextAreaPeer( this, target );
  }

  protected PanelPeer createPanel(Panel target)
  {
    return new QtPanelPeer( this, target);
  }

  protected WindowPeer createWindow(Window target)
  {
    return new QtWindowPeer( this, target );
  }

  protected DialogPeer createDialog(Dialog target)
  {
    return new QtDialogPeer( this, target );
  }

  protected MenuBarPeer createMenuBar(MenuBar target)
  {
    return new QtMenuBarPeer( this, target );
  }

  protected MenuPeer createMenu(Menu target)
  {
    return new QtMenuPeer( this, target );
  }

  protected PopupMenuPeer createPopupMenu(PopupMenu target)
  {
    return new QtPopupMenuPeer( this, target );
  }

  protected MenuItemPeer createMenuItem(MenuItem target)
  {
    return new QtMenuItemPeer( this, target );
  }

  /**
   * @since 1.4
   */
  public AWTEventListener[] getAWTEventListeners()
  {
    return null; // FIXME
  }

  /**
   * @since 1.4
   */
  public AWTEventListener[] getAWTEventListeners(long mask)
  {
    return null; // FIXME
  }

  public ColorModel getColorModel()
  {
    return new DirectColorModel(32,
                                0x00FF0000,
                                0x0000FF00,
                                0x000000FF,
                                0xFF000000);
  }

  /**
   * Just return the defaults.
   */
  public String[] getFontList()
  {
    String[] builtIn = new String[] { "Dialog",
                                      "DialogInput",
                                      "Monospaced",
                                      "Serif",
                                      "SansSerif" };
    String[] nat = nativeFontFamilies();
    String[] allFonts = new String[ nat.length + 5 ];
    System.arraycopy(builtIn, 0, allFonts, 0, 5);
    System.arraycopy(nat, 0, allFonts, 5, nat.length);
    return allFonts;
  }

  public FontMetrics getFontMetrics(Font font)
  {
    return new QtFontMetrics(font);
  }

  protected FontPeer getFontPeer(String name,
                                 int style)
  {
    Map attrs = new HashMap ();
    ClasspathFontPeer.copyStyleToAttrs(style, attrs);
    ClasspathFontPeer.copySizeToAttrs(12, attrs); // Default size is 12.
    return getClasspathFontPeer (name, attrs);
  }

  public Image getImage(String filename)
  {
    return new QtImage(filename);
  }

  public Image getImage(URL url)
  {
    return createImage( url );
  }

  public PrintJob getPrintJob(Frame frame,
                              String jobtitle,
                              Properties props)
  {
    SecurityManager sm;
    sm = System.getSecurityManager();
    if (sm != null)
      sm.checkPrintJobAccess();

    throw new RuntimeException("Not implemented");
  }

  public Clipboard getSystemClipboard()
  {
    throw new RuntimeException("Not implemented");
  }

  protected EventQueue getSystemEventQueueImpl()
  {
    return eventQueue;
  }

  public native Dimension getScreenSize();

  public native int getScreenResolution();

  public Map mapInputMethodHighlight(InputMethodHighlight highlight)
  {
    return null; // FIXME
  }

  public boolean prepareImage(Image image, int w, int h, ImageObserver observer)
  {
    if(image instanceof QtImage)
      return true;
    return false; // FIXME?
  }

  public native void sync();

  // ********************** ClasspathToolkit methods

  public GraphicsEnvironment getLocalGraphicsEnvironment()
  {
    return graphicsEnv;
  }

  public ClasspathFontPeer getClasspathFontPeer (String name, Map attrs)
  {
    return new QtFontPeer (name, attrs);
  }

  // FIXME
  public Font createFont(int format, InputStream stream)
  {
    throw new UnsupportedOperationException();
  }

  // FIXME
  public RobotPeer createRobot (GraphicsDevice screen) throws AWTException
  {
    throw new UnsupportedOperationException();
  }

  public EmbeddedWindowPeer createEmbeddedWindow(EmbeddedWindow w)
  {
    //    return new QtEmbeddedWindowPeer( this, w );
    return null;
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
