/* Toolkit.java -- AWT Toolkit superclass
   Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004
   Free Software Foundation, Inc.

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


package java.awt;

import java.awt.datatransfer.Clipboard;
import java.awt.dnd.DragGestureEvent;
import java.awt.dnd.DragGestureListener;
import java.awt.dnd.DragGestureRecognizer;
import java.awt.dnd.DragSource;
import java.awt.dnd.peer.DragSourceContextPeer;
import java.awt.event.AWTEventListener;
import java.awt.event.KeyEvent;
import java.awt.im.InputMethodHighlight;
import java.awt.image.ColorModel;
import java.awt.image.ImageObserver;
import java.awt.image.ImageProducer;
import java.awt.peer.ButtonPeer;
import java.awt.peer.CanvasPeer;
import java.awt.peer.CheckboxPeer;
import java.awt.peer.CheckboxMenuItemPeer;
import java.awt.peer.ChoicePeer;
import java.awt.peer.DialogPeer;
import java.awt.peer.FileDialogPeer;
import java.awt.peer.FontPeer;
import java.awt.peer.FramePeer;
import java.awt.peer.LabelPeer;
import java.awt.peer.LightweightPeer;
import java.awt.peer.ListPeer;
import java.awt.peer.MenuPeer;
import java.awt.peer.MenuBarPeer;
import java.awt.peer.MenuItemPeer;
import java.awt.peer.PanelPeer;
import java.awt.peer.PopupMenuPeer;
import java.awt.peer.ScrollbarPeer;
import java.awt.peer.ScrollPanePeer;
import java.awt.peer.TextAreaPeer;
import java.awt.peer.TextFieldPeer;
import java.awt.peer.WindowPeer;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.net.URL;
import java.util.Map;
import java.util.Properties;

/**
 * The AWT system uses a set of native peer objects to implement its
 * widgets.  These peers are provided by a peer toolkit, that is accessed
 * via a subclass of this superclass.  The system toolkit is retrieved
 * by the static methods <code>getDefaultToolkit</code>.  This method
 * determines the system toolkit by examining the system property
 * <code>awt.toolkit</code>.  That property is set to the name of the
 * <code>Toolkit</code> subclass for the specified peer set.  If the
 * <code>awt.toolkit</code> property is not set, then the default
 * toolkit <code>gnu.java.awt.peer.gtk.GtkToolkit</code> is used.  This
 * toolkit creates its peers using the GTK+ toolkit.
 *
 * @author Aaron M. Renn <arenn@urbanophile.com>
 */
public abstract class Toolkit
{
  /** The default toolkit name. */
  private static String default_toolkit_name
    = gnu.classpath.Configuration.default_awt_peer_toolkit;

  /**
   * The toolkit in use.  Once we load it, we don't ever change it
   * if the awt.toolkit property is set.
   */
  private static Toolkit toolkit;

  /** The toolkit properties. */
  private static Properties props = new Properties();

  protected final Map desktopProperties = new Properties();

  protected final PropertyChangeSupport desktopPropsSupport
    = new PropertyChangeSupport(this);

  /**
   * Default constructor for subclasses.
   */
  public Toolkit()
  {
  }

  /**
   * Creates a peer object for the specified <code>Button</code>.
   *
   * @param target The <code>Button</code> to create the peer for.
   * 
   * @return The peer for the specified <code>Button</code> object.
   *
   * @exception HeadlessException If GraphicsEnvironment.isHeadless() is true.
   */
  protected abstract ButtonPeer createButton(Button target);

  /**
   * Creates a peer object for the specified <code>TextField</code>.
   *
   * @param target The <code>TextField</code> to create the peer for.
   *
   * @return The peer for the specified <code>TextField</code> object.
   *
   * @exception HeadlessException If GraphicsEnvironment.isHeadless() is true.
   */
  protected abstract TextFieldPeer createTextField(TextField target);

  /**
   * Creates a peer object for the specified <code>Label</code>.
   *
   * @param target The <code>Label</code> to create the peer for.
   *
   * @return The peer for the specified <code>Label</code> object.
   *
   * @exception HeadlessException If GraphicsEnvironment.isHeadless() is true.
   */
  protected abstract LabelPeer createLabel(Label target);

  /**
   * Creates a peer object for the specified <code>List</code>.
   *
   * @param target The <code>List</code> to create the peer for.
   *
   * @return The peer for the specified <code>List</code> object.
   *
   * @exception HeadlessException If GraphicsEnvironment.isHeadless() is true.
   */
  protected abstract ListPeer createList(List target);

  /**
   * Creates a peer object for the specified <code>Checkbox</code>.
   *
   * @param target The <code>Checkbox</code> to create the peer for.
   *
   * @return The peer for the specified <code>Checkbox</code> object.
   *
   * @exception HeadlessException If GraphicsEnvironment.isHeadless() is true.
   */
  protected abstract CheckboxPeer createCheckbox(Checkbox target);

  /**
   * Creates a peer object for the specified <code>Scrollbar</code>.
   *
   * @param target The <code>Scrollbar</code> to create the peer for.
   *
   * @return The peer for the specified <code>Scrollbar</code> object.
   *
   * @exception HeadlessException If GraphicsEnvironment.isHeadless() is true.
   */
  protected abstract ScrollbarPeer createScrollbar(Scrollbar target);

  /**
   * Creates a peer object for the specified <code>ScrollPane</code>.
   *
   * @param target The <code>ScrollPane</code> to create the peer for.
   *
   * @return The peer for the specified <code>ScrollPane</code> object.
   *
   * @exception HeadlessException If GraphicsEnvironment.isHeadless() is true.
   */
  protected abstract ScrollPanePeer createScrollPane(ScrollPane target);

  /**
   * Creates a peer object for the specified <code>TextArea</code>.
   *
   * @param target The <code>TextArea</code> to create the peer for.
   *
   * @return The peer for the specified <code>TextArea</code> object.
   *
   * @exception HeadlessException If GraphicsEnvironment.isHeadless() is true.
   */
  protected abstract TextAreaPeer createTextArea(TextArea target);

  /**
   * Creates a peer object for the specified <code>Choice</code>.
   *
   * @param target The <code>Choice</code> to create the peer for.
   *
   * @return The peer for the specified <code>Choice</code> object.
   *
   * @exception HeadlessException If GraphicsEnvironment.isHeadless() is true.
   */
  protected abstract ChoicePeer createChoice(Choice target);

  /**
   * Creates a peer object for the specified <code>Frame</code>.
   *
   * @param target The <code>Frame</code> to create the peer for.
   *
   * @return The peer for the specified <code>Frame</code> object.
   *
   * @exception HeadlessException If GraphicsEnvironment.isHeadless() is true.
   */
  protected abstract FramePeer createFrame(Frame target);

  /**
   * Creates a peer object for the specified <code>Canvas</code>.
   *
   * @param target The <code>Canvas</code> to create the peer for.
   *
   * @return The peer for the specified <code>Canvas</code> object.
   */
  protected abstract CanvasPeer createCanvas(Canvas target);

  /**
   * Creates a peer object for the specified <code>Panel</code>.
   *
   * @param target The <code>Panel</code> to create the peer for.
   *
   * @return The peer for the specified <code>Panel</code> object.
   */
  protected abstract PanelPeer createPanel(Panel target);

  /**
   * Creates a peer object for the specified <code>Window</code>.
   *
   * @param target The <code>Window</code> to create the peer for.
   *
   * @return The peer for the specified <code>Window</code> object.
   *
   * @exception HeadlessException If GraphicsEnvironment.isHeadless() is true.
   */
  protected abstract WindowPeer createWindow(Window target);

  /**
   * Creates a peer object for the specified <code>Dialog</code>.
   *
   * @param target The dialog to create the peer for
   *
   * @return The peer for the specified font name.
   *
   * @exception HeadlessException If GraphicsEnvironment.isHeadless() is true.
   */
  protected abstract DialogPeer createDialog(Dialog target);

  /**
   * Creates a peer object for the specified <code>MenuBar</code>.
   *
   * @param target The <code>MenuBar</code> to create the peer for.
   *
   * @return The peer for the specified <code>MenuBar</code> object.
   *
   * @exception HeadlessException If GraphicsEnvironment.isHeadless() is true.
   */
  protected abstract MenuBarPeer createMenuBar(MenuBar target);

  /**
   * Creates a peer object for the specified <code>Menu</code>.
   *
   * @param target The <code>Menu</code> to create the peer for.
   *
   * @return The peer for the specified <code>Menu</code> object.
   *
   * @exception HeadlessException If GraphicsEnvironment.isHeadless() is true.
   */
  protected abstract MenuPeer createMenu(Menu target);

  /**
   * Creates a peer object for the specified <code>PopupMenu</code>.
   *
   * @param target The <code>PopupMenu</code> to create the peer for.
   *
   * @return The peer for the specified <code>PopupMenu</code> object.
   *
   * @exception HeadlessException If GraphicsEnvironment.isHeadless() is true.
   */
  protected abstract PopupMenuPeer createPopupMenu(PopupMenu target);

  /**
   * Creates a peer object for the specified <code>MenuItem</code>.
   *
   * @param target The <code>MenuItem</code> to create the peer for.
   *
   * @return The peer for the specified <code>MenuItem</code> object.
   *
   * @exception HeadlessException If GraphicsEnvironment.isHeadless() is true.
   */
  protected abstract MenuItemPeer createMenuItem(MenuItem target);

  /**
   * Creates a peer object for the specified <code>FileDialog</code>.
   *
   * @param target The <code>FileDialog</code> to create the peer for.
   *
   * @return The peer for the specified <code>FileDialog</code> object.
   *
   * @exception HeadlessException If GraphicsEnvironment.isHeadless() is true.
   */
  protected abstract FileDialogPeer createFileDialog(FileDialog target);

  /**
   * Creates a peer object for the specified <code>CheckboxMenuItem</code>.
   *
   * @param target The <code>CheckboxMenuItem</code> to create the peer for.
   *
   * @return The peer for the specified <code>CheckboxMenuItem</code> object.
   *
   * @exception HeadlessException If GraphicsEnvironment.isHeadless() is true.
   */
  protected abstract CheckboxMenuItemPeer
    createCheckboxMenuItem(CheckboxMenuItem target);

  /**
   * Creates a peer object for the specified <code>Component</code>.  The
   * peer returned by this method is not a native windowing system peer
   * with its own native window.  Instead, this method allows the component
   * to draw on its parent window as a "lightweight" widget.
   *
   * @param target The <code>Component</code> to create the peer for.
   *
   * @return The peer for the specified <code>Component</code> object.
   */
  protected LightweightPeer createComponent(Component target)
  {
    return new gnu.java.awt.peer.GLightweightPeer (target);
  }

  /**
   * Creates a peer object for the specified font name.
   *
   * @param name The font to create the peer for.
   * @param style The font style to create the peer for.
   *
   * @return The peer for the specified font name.
   *
   * @deprecated
   */
  protected abstract FontPeer getFontPeer(String name, int style);

  /**
   * Copies the current system colors into the specified array.  This is
   * the interface used by the <code>SystemColor</code> class.  Although
   * this method fills in the array with some default colors a real Toolkit
   * should override this method and provide real system colors for the
   * native GUI platform.
   *
   * @param colors The array to copy the system colors into.
   *               It must be at least 26 elements.
   *
   * @exception HeadlessException If GraphicsEnvironment.isHeadless() is true.
   *
   * @see java.awt.SystemColor
   */
  protected void loadSystemColors(int systemColors[])
  {
    systemColors[SystemColor.DESKTOP]                 = 0xFF005C5C;
    systemColors[SystemColor.ACTIVE_CAPTION]          = 0xFF000080;
    systemColors[SystemColor.ACTIVE_CAPTION_TEXT]     = 0xFFFFFFFF;
    systemColors[SystemColor.ACTIVE_CAPTION_BORDER]   = 0xFFC0C0C0;
    systemColors[SystemColor.INACTIVE_CAPTION]        = 0xFF808080;
    systemColors[SystemColor.INACTIVE_CAPTION_TEXT]   = 0xFFC0C0C0;
    systemColors[SystemColor.INACTIVE_CAPTION_BORDER] = 0xFFC0C0C0;
    systemColors[SystemColor.WINDOW]                  = 0xFFFFFFFF;
    systemColors[SystemColor.WINDOW_BORDER]           = 0xFF000000;
    systemColors[SystemColor.WINDOW_TEXT]             = 0xFF000000;
    systemColors[SystemColor.MENU]                    = 0xFFC0C0C0;
    systemColors[SystemColor.MENU_TEXT]               = 0xFF000000;
    systemColors[SystemColor.TEXT]                    = 0xFFC0C0C0;
    systemColors[SystemColor.TEXT_TEXT]               = 0xFF000000;
    systemColors[SystemColor.TEXT_HIGHLIGHT]          = 0xFF000090;
    systemColors[SystemColor.TEXT_HIGHLIGHT_TEXT]     = 0xFFFFFFFF;
    systemColors[SystemColor.TEXT_INACTIVE_TEXT]      = 0xFF808080;
    systemColors[SystemColor.CONTROL]                 = 0xFFC0C0C0;
    systemColors[SystemColor.CONTROL_TEXT]            = 0xFF000000;
    systemColors[SystemColor.CONTROL_HIGHLIGHT]       = 0xFFFFFFFF;
    systemColors[SystemColor.CONTROL_LT_HIGHLIGHT]    = 0xFFE0E0E0;
    systemColors[SystemColor.CONTROL_SHADOW]          = 0xFF808080;
    systemColors[SystemColor.CONTROL_DK_SHADOW]       = 0xFF000000;
    systemColors[SystemColor.SCROLLBAR]               = 0xFFE0E0E0;
    systemColors[SystemColor.INFO]                    = 0xFFE0E000;
    systemColors[SystemColor.INFO_TEXT]               = 0xFF000000;
  }

  /**
   * @since 1.4
   *
   * @exception HeadlessException If GraphicsEnvironment.isHeadless() is true.
   */
  public void setDynamicLayout(boolean dynamic)
  {
  }

  /**
   * @since 1.4
   *
   * @exception HeadlessException If GraphicsEnvironment.isHeadless() is true.
   */
  protected boolean isDynamicLayoutSet()
  {
    return false;
  }

  /**
   * @since 1.4
   *
   * @exception HeadlessException If GraphicsEnvironment.isHeadless() is true.
   */
  public boolean isDynamicLayoutActive()
  {
    return false;
  }

  /**
   * Returns the dimensions of the screen in pixels.
   *
   * @return The dimensions of the screen in pixels.
   * 
   * @exception HeadlessException If GraphicsEnvironment.isHeadless() is true.
   */
  public abstract Dimension getScreenSize();

  /**
   * Returns the screen resolution in dots per square inch.
   *
   * @return The screen resolution in dots per square inch.
   *
   * @exception HeadlessException If GraphicsEnvironment.isHeadless() is true.
   */
  public abstract int getScreenResolution();

  /**
   * @exception HeadlessException If GraphicsEnvironment.isHeadless() is true.
   *
   * @since 1.4
   */
  public Insets getScreenInsets(GraphicsConfiguration gc)
  {
    return null;
  }

  /**
   * Returns the color model of the screen.
   *
   * @return The color model of the screen.
   * 
   * @exception HeadlessException If GraphicsEnvironment.isHeadless() is true.
   */
  public abstract ColorModel getColorModel();

  /**
   * Returns the names of the available fonts.
   *
   * @return The names of the available fonts.
   *
   * @deprecated
   */
  public abstract String[] getFontList();

  /**
   * Return the font metrics for the specified font
   *
   * @param name The name of the font to return metrics for.
   *
   * @return The requested font metrics.
   *
   * @deprecated
   */
  public abstract FontMetrics getFontMetrics(Font name);

  /**
   * Flushes any buffered data to the screen so that it is in sync with
   * what the AWT system has drawn to it.
   */
  public abstract void sync();

  /**
   * Returns an instance of the default toolkit.  The default toolkit is
   * the subclass of <code>Toolkit</code> specified in the system property
   * <code>awt.toolkit</code>, or <code>gnu.java.awt.peer.gtk.GtkToolkit</code>
   * if the property is not set.
   *
   * @return An instance of the system default toolkit.
   *
   * @throws AWTError If the toolkit cannot be loaded.
   */
  public static Toolkit getDefaultToolkit()
  {
    if (toolkit != null)
      return toolkit;
    String toolkit_name = System.getProperty("awt.toolkit",
                                             default_toolkit_name);
    try
      {
        Class cls = Class.forName(toolkit_name);
        Object obj = cls.newInstance();
        if (!(obj instanceof Toolkit))
          throw new AWTError(toolkit_name + " is not a subclass of " +
                             "java.awt.Toolkit");
        toolkit = (Toolkit) obj;
        return toolkit;
      }
    catch (Throwable t)
      {
	AWTError e = new AWTError("Cannot load AWT toolkit: " + toolkit_name);
	throw (AWTError) e.initCause(t);
      }
  }

  /**
   * Returns an image from the specified file, which must be in a
   * recognized format.  Supported formats vary from toolkit to toolkit.
   *
   * @return name The name of the file to read the image from.
   */
  public abstract Image getImage(String name);

  /**
   * Returns an image from the specified URL, which must be in a
   * recognized format.  Supported formats vary from toolkit to toolkit.
   *
   * @return url The URl to read the image from.
   */
  public abstract Image getImage(URL url);

  public abstract Image createImage(String filename);

  public abstract Image createImage(URL url);

  /**
   * Readies an image to be rendered on the screen.  The width and height
   * values can be set to the default sizes for the image by passing -1
   * in those parameters.
   *
   * @param image The image to prepare for rendering.
   * @param width The width of the image.
   * @param height The height of the image.
   * @param observer The observer to receive events about the preparation
   * process.
   *
   * @return <code>true</code> if the image is already prepared for rendering,
   * <code>false</code> otherwise.
   */
  public abstract boolean prepareImage(Image image, int width, int height,
                                       ImageObserver observer);

  /**
   * Checks the status of specified image as it is being readied for
   * rendering.
   *
   * @param image The image to prepare for rendering.
   * @param width The width of the image.
   * @param height The height of the image.
   * @param observer The observer to receive events about the preparation
   * process.
   *
   * @return A union of the bitmasks from
   * <code>java.awt.image.ImageObserver</code> that indicates the current
   * state of the imaging readying process.
   */
  public abstract int checkImage(Image image, int width, int height,
                                 ImageObserver observer);

  /**
   * Creates an image using the specified <code>ImageProducer</code>
   *
   * @param producer The <code>ImageProducer</code> to create the image from.
   *
   * @return The created image.
   */
  public abstract Image createImage(ImageProducer producer);

  /**
   * Creates an image from the specified byte array. The array must be in
   * a recognized format.  Supported formats vary from toolkit to toolkit.
   *
   * @param data The raw image data.
   *
   * @return The created image.
   */
  public Image createImage(byte[] data)
  {
    return createImage(data, 0, data.length);
  }

  /**
   * Creates an image from the specified portion of the byte array passed.
   * The array must be in a recognized format.  Supported formats vary from
   * toolkit to toolkit.
   *
   * @param data The raw image data.
   * @param offset The offset into the data where the image data starts.
   * @param len The length of the image data.
   *
   * @return The created image.
   */
  public abstract Image createImage(byte[] data, int offset, int len);

  /**
   * Returns a instance of <code>PrintJob</code> for the specified
   * arguments.
   *
   * @param frame The window initiating the print job.
   * @param title The print job title.
   * @param props The print job properties.
   *
   * @return The requested print job, or <code>null</code> if the job
   * was cancelled.
   *
   * @exception NullPointerException If frame is null,
   * or GraphicsEnvironment.isHeadless() returns true.
   * @exception SecurityException If this thread is not allowed to initiate
   * a print job request.
   */
  public abstract PrintJob getPrintJob(Frame frame, String title,
                                       Properties props);

  /**
   * Returns a instance of <code>PrintJob</code> for the specified
   * arguments.
   *
   * @param frame The window initiating the print job.
   * @param title The print job title.
   * @param jobAttr A set of job attributes which will control the print job.
   * @param pageAttr A set of page attributes which will control the print job.
   *
   * @exception NullPointerException If frame is null, and either jobAttr is null
   * or jobAttr.getDialog() returns JobAttributes.DialogType.NATIVE.
   * @exception IllegalArgumentException If pageAttrspecifies differing cross
   * feed and feed resolutions, or when GraphicsEnvironment.isHeadless() returns
   * true.
   * @exception SecurityException If this thread is not allowed to initiate
   * a print job request.
   *
   * @since 1.3
   */
  public PrintJob getPrintJob(Frame frame, String title,
                              JobAttributes jobAttr, PageAttributes pageAttr)
  {
    return null;
  }

  /**
   * Causes a "beep" tone to be generated.
   */
  public abstract void beep();

  /**
   * Returns the system clipboard.
   *
   * @return THe system clipboard.
   *
   * @exception HeadlessException If GraphicsEnvironment.isHeadless() is true.
   */
  public abstract Clipboard getSystemClipboard();

  /**
   * Gets the singleton instance of the system selection as a Clipboard object.
   *
   * @exception HeadlessException If GraphicsEnvironment.isHeadless() is true.
   *
   * @since 1.4
   */
  public Clipboard getSystemSelection()
  {
    return null;
  }

  /**
   * Returns the accelerator key mask for menu shortcuts. The default is
   * <code>Event.CTRL_MASK</code>.  A toolkit must override this method
   * to change the default.
   *
   * @return The key mask for the menu accelerator key.
   *
   * @exception HeadlessException If GraphicsEnvironment.isHeadless() is true.
   */
  public int getMenuShortcutKeyMask()
  {
    return Event.CTRL_MASK;
  }

  /**
   * Returns whether the given locking key on the keyboard is currently in its
   * "on" state.
   *
   * @exception HeadlessException If GraphicsEnvironment.isHeadless() is true.
   * @exception IllegalArgumentException If keyCode is not one of the valid keys.
   * @exception UnsupportedOperationException If the host system doesn't allow
   * getting the state of this key programmatically, or if the keyboard doesn't
   * have this key.
   */
  public boolean getLockingKeyState(int keyCode)
  {
    if (keyCode != KeyEvent.VK_CAPS_LOCK
        && keyCode != KeyEvent.VK_NUM_LOCK
        && keyCode != KeyEvent.VK_SCROLL_LOCK)
      throw new IllegalArgumentException();
    
    throw new UnsupportedOperationException();
  }

  /**
   * Sets the state of the given locking key on the keyboard.
   *
   * @exception HeadlessException If GraphicsEnvironment.isHeadless() is true.
   * @exception IllegalArgumentException If keyCode is not one of the valid keys.
   * @exception UnsupportedOperationException If the host system doesn't allow
   * getting the state of this key programmatically, or if the keyboard doesn't
   * have this key.
   */
  public void setLockingKeyState(int keyCode, boolean on)
  {
    if (keyCode != KeyEvent.VK_CAPS_LOCK
        && keyCode != KeyEvent.VK_NUM_LOCK
        && keyCode != KeyEvent.VK_SCROLL_LOCK)
      throw new IllegalArgumentException();
    
    throw new UnsupportedOperationException();
  }

  /**
   * Returns the native container object of the specified component.  This
   * method is necessary because the parent component might be a lightweight
   * component.
   *
   * @param component The component to fetch the native container for.
   *
   * @return The native container object for this component.
   */
  protected static Container getNativeContainer(Component component)
  {
    component = component.getParent();
    while (true)
      {
        if (component == null)
          return null;
        if (! (component instanceof Container))
          {
            component = component.getParent();
            continue;
          }
        if (component.getPeer() instanceof LightweightPeer)
          {
            component = component.getParent();
            continue;
          }
        return (Container) component;
      }
  }

  /**
   * Creates a new custom cursor object.
   *
   * @exception IndexOutOfBoundsException If the hotSpot values are outside
   * the bounds of the cursor.
   * @exception HeadlessException If GraphicsEnvironment.isHeadless() is true.
   */
  public Cursor createCustomCursor(Image cursor, Point hotSpot, String name)
  {
    // Presumably the only reason this isn't abstract is for backwards
    // compatibility? FIXME?
    return null;
  }

  /**
   * Returns the supported cursor dimension which is closest to the
   * desired sizes.
   *
   * @exception HeadlessException If GraphicsEnvironment.isHeadless() is true.
   */
  public Dimension getBestCursorSize(int preferredWidth, int preferredHeight)
  {
    return new Dimension (0,0);
  }

  /**
   * Returns the maximum number of colors the Toolkit supports in a custom
   * cursor palette.
   *
   * @exception HeadlessException If GraphicsEnvironment.isHeadless() is true.
   */
  public int getMaximumCursorColors()
  {
    return 0;
  }

  /**
   * Returns whether Toolkit supports this state for Frames.
   *
   * @exception HeadlessException If GraphicsEnvironment.isHeadless() is true.
   * 
   * @since 1.4
   */
  public boolean isFrameStateSupported(int state)
  {
    return false;
  }

  /**
   * Returns the value of the property with the specified name, or the
   * default value if the property does not exist.
   *
   * @param key The name of the property to retrieve.
   * @param def The default value of the property.
   */
  public static String getProperty(String key, String def)
  {
    return props.getProperty(key, def);
  }


  /**
   * Returns the event queue that is suitable for the calling context.
   *
   * <p>Despite the word &#x201c;System&#x201d; in the name of this
   * method, a toolkit may provide different event queues for each
   * applet. There is no guarantee that the same queue is shared
   * system-wide.
   *
   * <p>The implementation first checks whether a
   * SecurityManager has been installed. If so, its {@link
   * java.lang.SecurityManager#checkAwtEventQueueAccess()} method gets
   * called. The security manager will throw a SecurityException if it
   * does not grant the permission to access the event queue.
   *
   * <p>Next, the call is delegated to {@link
   * #getSystemEventQueueImpl()}.
   *
   * @return The event queue for this applet (or application).
   *
   * @throws SecurityException if a security manager has been
   * installed, and it does not grant the permission to access the
   * event queue.
   */
  public final EventQueue getSystemEventQueue()
  {
    SecurityManager sm;

    sm = System.getSecurityManager();
    if (sm != null)
      sm.checkAwtEventQueueAccess();

    return getSystemEventQueueImpl();
  }


  /**
   * Returns the event queue that is suitable for the calling context.
   *
   * <p>Despite the word &#x201c;System&#x201d; in the name of this
   * method, a toolkit may provide different event queues for each
   * applet. There is no guarantee that the same queue is shared
   * system-wide.
   *
   * <p>No security checks are performed, which is why this method
   * may only be called by Toolkits.
   *
   * @see #getSystemEventQueue()
   */
  protected abstract EventQueue getSystemEventQueueImpl();


  /**
   * @since 1.3
   */
  public abstract DragSourceContextPeer
    createDragSourceContextPeer(DragGestureEvent e);

  /**
   * @since 1.3
   */
  public DragGestureRecognizer
    createDragGestureRecognizer(Class recognizer, DragSource ds,
                                Component comp, int actions,
                                DragGestureListener l)
  {
    return null;
  }

  public final Object getDesktopProperty(String propertyName)
  {
    return desktopProperties.get(propertyName);
  }

  protected final void setDesktopProperty(String name, Object newValue)
  {
    Object oldValue = getDesktopProperty(name);
    desktopProperties.put(name, newValue);
    desktopPropsSupport.firePropertyChange(name, oldValue, newValue);
  }

  protected Object lazilyLoadDesktopProperty(String name)
  {
    // FIXME - what is this??
    return null;
  }

  protected void initializeDesktopProperties()
  {
    // Overridden by toolkit implementation?
  }

  public void addPropertyChangeListener(String name,
                                        PropertyChangeListener pcl)
  {
    desktopPropsSupport.addPropertyChangeListener(name, pcl);
  }

  public void removePropertyChangeListener(String name,
                                           PropertyChangeListener pcl)
  {
    desktopPropsSupport.removePropertyChangeListener(name, pcl);
  }

  /**
   * @since 1.4
   */
  public PropertyChangeListener[] getPropertyChangeListeners()
  {
    return desktopPropsSupport.getPropertyChangeListeners();
  }

  /**
   * @since 1.4
   */
  public PropertyChangeListener[] getPropertyChangeListeners(String name)
  {
    return desktopPropsSupport.getPropertyChangeListeners(name);
  }

  public void addAWTEventListener(AWTEventListener listener, long eventMask)
  {
    // SecurityManager s = System.getSecurityManager();
    // if (s != null)
    //  s.checkPermission(AWTPermission("listenToAllAWTEvents"));
    // FIXME
  }

  public void removeAWTEventListener(AWTEventListener listener)
  {
    // FIXME
  }

  /**
   * @since 1.4
   */
  public AWTEventListener[] getAWTEventListeners()
  {
    return null;
  }

  /**
   * @since 1.4
   */
  public AWTEventListener[] getAWTEventListeners(long mask)
  {
    return null;
  }

  /**
   * @since 1.3
   */
  public abstract Map mapInputMethodHighlight(InputMethodHighlight highlight);
} // class Toolkit
