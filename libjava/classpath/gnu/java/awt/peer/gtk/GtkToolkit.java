/* GtkToolkit.java -- Implements an AWT Toolkit using GTK for peers
   Copyright (C) 1998, 1999, 2002, 2003, 2004, 2005, 2006, 2007
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


package gnu.java.awt.peer.gtk;

import gnu.classpath.Configuration;

import gnu.java.awt.AWTUtilities;
import gnu.java.awt.EmbeddedWindow;
import gnu.java.awt.dnd.GtkMouseDragGestureRecognizer;
import gnu.java.awt.dnd.peer.gtk.GtkDragSourceContextPeer;
import gnu.java.awt.peer.ClasspathFontPeer;
import gnu.java.awt.peer.EmbeddedWindowPeer;

import java.awt.AWTException;
import java.awt.Button;
import java.awt.Canvas;
import java.awt.Checkbox;
import java.awt.CheckboxMenuItem;
import java.awt.Choice;
import java.awt.Component;
import java.awt.Cursor;
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
import java.awt.Point;
import java.awt.PopupMenu;
import java.awt.PrintJob;
import java.awt.Rectangle;
import java.awt.ScrollPane;
import java.awt.Scrollbar;
import java.awt.TextArea;
import java.awt.TextField;
import java.awt.Window;
import java.awt.datatransfer.Clipboard;
import java.awt.dnd.DragGestureEvent;
import java.awt.dnd.DragGestureListener;
import java.awt.dnd.DragGestureRecognizer;
import java.awt.dnd.DragSource;
import java.awt.dnd.InvalidDnDOperationException;
import java.awt.dnd.peer.DragSourceContextPeer;
import java.awt.font.TextAttribute;
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
import java.awt.peer.MouseInfoPeer;
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
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Properties;

import javax.imageio.spi.IIORegistry;

/* This class uses a deprecated method java.awt.peer.ComponentPeer.getPeer().
   This merits comment.  We are basically calling Sun's bluff on this one.
   We think Sun has deprecated it simply to discourage its use as it is
   bad programming style.  However, we need to get at a component's peer in
   this class.  If getPeer() ever goes away, we can implement a hash table
   that will keep up with every window's peer, but for now this is faster. */

public class GtkToolkit extends gnu.java.awt.ClasspathToolkit
{
  static final Object GTK_LOCK;

  private static EventQueue q;

  static native void gtkInit(int portableNativeSync, Object lock);

  static native void gtkMain();

  static native void gtkQuit();

  /**
   * Initializes field IDs that are used by native code.
   */
  private static native void initIDs();

  /**
   * True when the field IDs are already initialized, false otherwise.
   */
  private static boolean initializedGlobalIDs = false;

  /**
   * Initializes some global fieldIDs for use in the native code. This is
   * called by a couple of classes in the GTK peers to ensure that
   * some necessary stuff is loaded.
   */
  static synchronized void initializeGlobalIDs()
  {
    if (! initializedGlobalIDs)
      {
        initIDs();
        initializedGlobalIDs = true;
      }
  }

  static
  {
    if (true) // GCJ LOCAL
      {
        System.loadLibrary("gtkpeer");
      }

    /**
     * Gotta do that first.
     */
    initializeGlobalIDs();

    int portableNativeSync;     
    String portNatSyncProp = 
      System.getProperty("gnu.classpath.awt.gtk.portable.native.sync");
      
    if (portNatSyncProp == null)
      portableNativeSync = -1;  // unset
    else if (Boolean.valueOf(portNatSyncProp).booleanValue())
      portableNativeSync = 1;   // true
    else
      portableNativeSync = 0;   // false

    GTK_LOCK = new String("GTK LOCK");
    gtkInit(portableNativeSync, GTK_LOCK);
  }

  public GtkToolkit ()
  {
  }

  public native void beep();

  private native void getScreenSizeDimensions(int[] xy);
  
  public int checkImage (Image image, int width, int height, 
                         ImageObserver observer) 
  {
    int status = ImageObserver.ALLBITS 
      | ImageObserver.WIDTH 
      | ImageObserver.HEIGHT;

    if (image instanceof GtkImage)
      return ((GtkImage) image).checkImage (observer);

    if (image instanceof AsyncImage)
      return ((AsyncImage) image).checkImage(observer);

    if (observer != null)
      observer.imageUpdate (image, status,
                            -1, -1,
                            image.getWidth (observer),
                            image.getHeight (observer));
    
    return status;
  }

  /** 
   * Helper to return either a Image -- the argument -- or a
   * GtkImage with the errorLoading flag set if the argument is null.
   */
  static Image imageOrError(Image b)
  {
    if (b == null) 
      return GtkImage.getErrorImage();
    else
      return b;
  }

  public Image createImage (String filename)
  {
    if (filename.length() == 0)
      return new GtkImage ();
    
    Image image;
    try
      {
        image = CairoSurface.getBufferedImage( new GtkImage( filename ) );
      }
    catch (IllegalArgumentException iae)
      {
        image = null;
      }
    return imageOrError(image);
  }

  public Image createImage (URL url)
  {
    return new AsyncImage(url);
  }

  public Image createImage (ImageProducer producer) 
  {
    if (producer == null)
      return null;
      
    Image image;
    try
      {
        image = CairoSurface.getBufferedImage( new GtkImage( producer ) );
      }
    catch (IllegalArgumentException iae)
      {
        image = null;
      }
    return imageOrError(image);
  }

  public Image createImage (byte[] imagedata, int imageoffset,
			    int imagelength)
  {
    Image image;
    try
      {
        byte[] data = new byte[ imagelength ];
        System.arraycopy(imagedata, imageoffset, data, 0, imagelength);
        image = CairoSurface.getBufferedImage( new GtkImage( data ) );
      }
    catch (IllegalArgumentException iae)
      {
        image = null;
      }
    return imageOrError(image);
  }
  
  /**
   * Creates an ImageProducer from the specified URL. The image is assumed
   * to be in a recognised format. 
   *
   * @param url URL to read image data from.
   */  
  public ImageProducer createImageProducer(URL url)
  {
    return createImage( url ).getSource();
  }

  /**
   * Returns the native color model (which isn't the same as the default
   * ARGB color model, but doesn't have to be). 
   */
  public ColorModel getColorModel () 
  {
    /* Return the GDK-native ABGR format */
    return new DirectColorModel(32, 
                                0x000000FF,
                                0x0000FF00,
                                0x00FF0000,
                                0xFF000000);
  }

  public String[] getFontList () 
  {
    return (new String[] { "Dialog", 
                           "DialogInput", 
                           "Monospaced", 
                           "Serif",
                           "SansSerif" });
  }

  static class LRUCache<K,V> extends LinkedHashMap<K,V>
  {    
    int max_entries;
    public LRUCache(int max)
    {
      super(max, 0.75f, true);
      max_entries = max;
    }
    protected boolean removeEldestEntry(Map.Entry eldest)
    {
      return size() > max_entries;
    }
  }

  private LRUCache<Map,ClasspathFontPeer> fontCache =
    new LRUCache<Map,ClasspathFontPeer>(50);
  private LRUCache<Object,Image> imageCache = new LRUCache<Object,Image>(50);

  public FontMetrics getFontMetrics (Font font) 
  {
    return ((GdkFontPeer) font.getPeer()).getFontMetrics(font);
  }

  public Image getImage (String filename) 
  {
    if (imageCache.containsKey(filename))
      return imageCache.get(filename);
    else
      {
        Image im = createImage(filename);
        imageCache.put(filename, im);
        return im;
      }
  }

  public Image getImage (URL url) 
  {
    if (imageCache.containsKey(url))
      return imageCache.get(url);
    else
      {
        Image im = createImage(url);
        imageCache.put(url, im);
        return im;
      }
  }

  public PrintJob getPrintJob (Frame frame, String jobtitle, Properties props) 
  {
    SecurityManager sm;
    sm = System.getSecurityManager();
    if (sm != null)
      sm.checkPrintJobAccess();

    return null;
  }

  public native int getScreenResolution();

  public Dimension getScreenSize ()
  {
    int dim[] = new int[2];
    getScreenSizeDimensions(dim);
    return new Dimension(dim[0], dim[1]);
  }

  public Clipboard getSystemClipboard() 
  {
    SecurityManager secman = System.getSecurityManager();
    if (secman != null)
      secman.checkSystemClipboardAccess();

    return GtkClipboard.getClipboardInstance();
  }

  public Clipboard getSystemSelection()
  {
    SecurityManager secman = System.getSecurityManager();
    if (secman != null)
      secman.checkSystemClipboardAccess();
    
    return GtkClipboard.getSelectionInstance();
  }

  /**
   * Prepares a GtkImage. For every other kind of Image it just
   * assumes the image is already prepared for rendering.
   */
  public boolean prepareImage (Image image, int width, int height, 
                               ImageObserver observer) 
  {
    /* GtkImages are always prepared, as long as they're loaded. */
    if (image instanceof GtkImage)
      return ((((GtkImage)image).checkImage (observer)
               & ImageObserver.ALLBITS) != 0);

    if (image instanceof AsyncImage)
      {
        AsyncImage aImg = (AsyncImage) image;
        aImg.addObserver(observer);
        return aImg.realImage != null;
      }

    /* Assume anything else is too */
    return true;
  }

  public native void sync();

  protected void setComponentState (Component c, GtkComponentPeer cp)
  {
    /* Make the Component reflect Peer defaults */
    if (c.getForeground () == null)
      c.setForeground (cp.getForeground ());
    if (c.getBackground () == null)
      c.setBackground (cp.getBackground ());
    //        if (c.getFont () == null)
    //  	c.setFont (cp.getFont ());
      
    /* Make the Peer reflect the state of the Component */
    if (! (c instanceof Window))
      {
        cp.setCursor (c.getCursor ());
	
        Rectangle bounds = c.getBounds ();
        cp.setBounds (bounds.x, bounds.y, bounds.width, bounds.height);
        cp.setVisible (c.isVisible ());
      }
  }

  protected ButtonPeer createButton (Button b)
  {
    checkHeadless();
    return new GtkButtonPeer (b);
  }

  protected CanvasPeer createCanvas (Canvas c) 
  {
    checkHeadless();
    return new GtkCanvasPeer (c);
  }

  protected CheckboxPeer createCheckbox (Checkbox cb) 
  {
    checkHeadless();
    return new GtkCheckboxPeer (cb);
  }

  protected CheckboxMenuItemPeer createCheckboxMenuItem (CheckboxMenuItem cmi)
  {
    checkHeadless();
    return new GtkCheckboxMenuItemPeer (cmi);
  }

  protected ChoicePeer createChoice (Choice c) 
  {
    checkHeadless();
    return new GtkChoicePeer (c);
  }

  protected DialogPeer createDialog (Dialog d)
  {
    checkHeadless();
    GtkMainThread.createWindow();
    return new GtkDialogPeer (d);
  }

  protected FileDialogPeer createFileDialog (FileDialog fd)
  {
    checkHeadless();
    return new GtkFileDialogPeer (fd);
  }

  protected FramePeer createFrame (Frame f)
  {
    checkHeadless();
    GtkMainThread.createWindow();
    return new GtkFramePeer (f);
  }

  protected LabelPeer createLabel (Label label) 
  {
    checkHeadless();
    return new GtkLabelPeer (label);
  }

  protected ListPeer createList (List list)
  {
    checkHeadless();
    return new GtkListPeer (list);
  }

  protected MenuPeer createMenu (Menu m) 
  {
    checkHeadless();
    return new GtkMenuPeer (m);
  }

  protected MenuBarPeer createMenuBar (MenuBar mb) 
  {
    checkHeadless();
    return new GtkMenuBarPeer (mb);
  }

  protected MenuItemPeer createMenuItem (MenuItem mi) 
  {
    checkHeadless();
    return new GtkMenuItemPeer (mi);
  }

  protected PanelPeer createPanel (Panel p) 
  {
    checkHeadless();
    return new GtkPanelPeer (p);
  }

  protected PopupMenuPeer createPopupMenu (PopupMenu target) 
  {
    checkHeadless();
    return new GtkPopupMenuPeer (target);
  }

  protected ScrollPanePeer createScrollPane (ScrollPane sp) 
  {
    checkHeadless();
    return new GtkScrollPanePeer (sp);
  }

  protected ScrollbarPeer createScrollbar (Scrollbar sb) 
  {
    checkHeadless();
    return new GtkScrollbarPeer (sb);
  }

  protected TextAreaPeer createTextArea (TextArea ta) 
  {
    checkHeadless();
    return new GtkTextAreaPeer (ta);
  }

  protected TextFieldPeer createTextField (TextField tf) 
  {
    checkHeadless();
    return new GtkTextFieldPeer (tf);
  }

  protected WindowPeer createWindow (Window w)
  {
    checkHeadless();
    GtkMainThread.createWindow();
    return new GtkWindowPeer (w);
  }

  public EmbeddedWindowPeer createEmbeddedWindow (EmbeddedWindow w)
  {
    checkHeadless();
    GtkMainThread.createWindow();
    return new GtkEmbeddedWindowPeer (w);
  }

  /** 
   * @deprecated part of the older "logical font" system in earlier AWT
   * implementations. Our newer Font class uses getClasspathFontPeer.
   */
  protected FontPeer getFontPeer (String name, int style) {
    // All fonts get a default size of 12 if size is not specified.
    return getFontPeer(name, style, 12);
  }

  /**
   * Private method that allows size to be set at initialization time.
   */
  private FontPeer getFontPeer (String name, int style, int size) 
  {
    Map<TextAttribute,Object> attrs = new HashMap<TextAttribute,Object>();
    ClasspathFontPeer.copyStyleToAttrs (style, attrs);
    ClasspathFontPeer.copySizeToAttrs (size, attrs);
    return getClasspathFontPeer (name, attrs);
  }

  /**
   * Newer method to produce a peer for a Font object, even though Sun's
   * design claims Font should now be peerless, we do not agree with this
   * model, hence "ClasspathFontPeer". 
   */

  public ClasspathFontPeer getClasspathFontPeer (String name,
                                                 Map<?,?> attrs)
  {
    Map<Object,Object> keyMap = new HashMap<Object,Object>(attrs);
    // We don't know what kind of "name" the user requested (logical, face,
    // family), and we don't actually *need* to know here. The worst case
    // involves failure to consolidate fonts with the same backend in our
    // cache. This is harmless.
    keyMap.put ("GtkToolkit.RequestedFontName", name);
    if (fontCache.containsKey (keyMap))
      return fontCache.get (keyMap);
    else
      {
        ClasspathFontPeer newPeer = new GdkFontPeer (name, attrs);
        fontCache.put (keyMap, newPeer);
        return newPeer;
      }
  }

  protected EventQueue getSystemEventQueueImpl() 
  {
    synchronized (GtkToolkit.class)
      {
        if (q == null)
          {
            q = new EventQueue();
          }
      }    
    return q;
  }

  public Cursor createCustomCursor(Image image, Point hotspot, String name)
  {
    return new GtkCursor(image, hotspot, name);
  }

  protected native void loadSystemColors (int[] systemColors);

  public DragSourceContextPeer createDragSourceContextPeer(DragGestureEvent e)
  {
    if (GraphicsEnvironment.isHeadless())
      throw new InvalidDnDOperationException();
    return new GtkDragSourceContextPeer(e);
  }
  
  public <T extends DragGestureRecognizer> T
  createDragGestureRecognizer(Class<T> recognizer, DragSource ds, 
                              Component comp, int actions,
                              DragGestureListener l)
  {
    if (recognizer.getName().equals("java.awt.dnd.MouseDragGestureRecognizer")
        && ! GraphicsEnvironment.isHeadless())
      {
        GtkMouseDragGestureRecognizer gestureRecognizer
          = new GtkMouseDragGestureRecognizer(ds, comp, actions, l);
        gestureRecognizer.registerListeners();
        return recognizer.cast(gestureRecognizer);
      }
    else
      {
        return null;
      }
  }

  public Map<TextAttribute,?> mapInputMethodHighlight(InputMethodHighlight highlight)
  {
    throw new Error("not implemented");
  }

  public Rectangle getBounds()
  {
    int[] dims = new int[2];
    getScreenSizeDimensions(dims);
    return new Rectangle(0, 0, dims[0], dims[1]);
  }
  
  // ClasspathToolkit methods

  public GraphicsEnvironment getLocalGraphicsEnvironment()
  {
    return new GdkGraphicsEnvironment();
  }

  public Font createFont(int format, InputStream stream)
  {
    throw new UnsupportedOperationException();
  }

  public RobotPeer createRobot (GraphicsDevice screen) throws AWTException
  {
    return new GdkRobotPeer (screen);
  }

  public boolean getLockingKeyState(int keyCode)
  {
    int state = getLockState(keyCode);
    
    if (state != -1)
      return state == 1;
    
    if (AWTUtilities.isValidKey(keyCode))
      throw new UnsupportedOperationException
        ("cannot get locking state of key code " + keyCode);
    
    throw new IllegalArgumentException("invalid key code " + keyCode);
  }

  protected native int getLockState(int keyCode);

  public void registerImageIOSpis(IIORegistry reg)
  {
    GdkPixbufDecoder.registerSpis(reg);
  }

  protected MouseInfoPeer getMouseInfoPeer()
  {
    return new GtkMouseInfoPeer();
  }

  public boolean isFrameStateSupported(int state)
  {
    // GTK supports ICONFIED, NORMAL and MAXIMIZE_BOTH, but
    // not (yet?) MAXIMIZE_VERT and MAXIMIZE_HORIZ.
    return state == Frame.NORMAL || state == Frame.ICONIFIED
           || state == Frame.MAXIMIZED_BOTH;
  }

  private void checkHeadless()
  {
    if (GraphicsEnvironment.isHeadless())
      throw new HeadlessException();
  }

  public native int getMouseNumberOfButtons();

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

} // class GtkToolkit
