/* Copyright (C) 2000, 2002, 2003, 2005  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.awt.xlib;

import java.awt.*;
import java.awt.dnd.*;
import java.awt.dnd.peer.*;
import java.awt.font.*;
import java.awt.im.*;
import java.awt.peer.*;
import java.awt.image.ImageProducer;
import java.awt.image.ImageObserver;
import java.net.*;
import java.awt.datatransfer.Clipboard;
import java.io.InputStream;
import java.text.AttributedString;
import java.util.Map;
import java.util.Properties;
import gnu.gcj.xlib.Display;
import gnu.gcj.xlib.Screen;
import gnu.gcj.xlib.Visual;
import gnu.java.awt.ClasspathToolkit;
import gnu.java.awt.EmbeddedWindow;
import gnu.java.awt.peer.ClasspathFontPeer;
import gnu.java.awt.peer.EmbeddedWindowPeer;

public class XToolkit extends ClasspathToolkit
{
  static XToolkit INSTANCE;
  
  Display display;

  EventQueue queue;
  XEventLoop eventLoop;

  XGraphicsConfiguration defaultConfig;

  public XToolkit()
  {
    INSTANCE = this;
    display = new Display();
    synchronized (display)
      {
	queue = new XEventQueue(display);
	eventLoop = new XEventLoop(display, queue);
      }
  }

  public void flushIfIdle()
  {
    eventLoop.flushIfIdle();
  }

  protected ButtonPeer createButton(Button frontend)
  {
    // FIXME: Stubbed out, needs Swing:
    /*
    XCanvasPeer realPeer = new XCanvasPeer(frontend);
    SButtonPeer sbPeer = new SButtonPeer(frontend, realPeer);
    return sbPeer;
    */
    return null;
  }
    
  protected TextFieldPeer createTextField(TextField frontend)
  {
    return null; // FIXME
  }
    
  protected LabelPeer createLabel(Label frontend) 
  {
    return null; // FIXME
  }
    
  protected ListPeer createList(List frontend) 
  {
    return null; // FIXME
  }
  
  protected CheckboxPeer createCheckbox(Checkbox frontend) 
  {
    return null; // FIXME
  }
    
  protected ScrollbarPeer createScrollbar(Scrollbar frontend) 
  {
    return null; // FIXME
  }
  
  protected ScrollPanePeer createScrollPane(ScrollPane frontend) 
  {
    return null; // FIXME
  }
  
  protected TextAreaPeer createTextArea(TextArea frontend) 
  {
    return null; // FIXME
  }
  
  protected ChoicePeer createChoice(Choice frontend) 
  {
    return null; // FIXME
  }
  
  protected FramePeer createFrame(Frame frontend) {
    return new XFramePeer(frontend);
  }

  protected CanvasPeer createCanvas(Canvas frontend) {
    XCanvasPeer peer = new XCanvasPeer(frontend);
    return peer;
  }
  
  protected PanelPeer createPanel(Panel frontend) {
    return new XPanelPeer(frontend);
  }

  protected WindowPeer createWindow(Window frontend) 
  {
    return null; // FIXME
  }
    
  protected DialogPeer createDialog(Dialog frontend) 
  {
    return null; // FIXME
  }
  
  protected MenuBarPeer createMenuBar(MenuBar frontend) 
  {
    return null; // FIXME
  }
  
  protected MenuPeer createMenu(Menu frontend) 
  {
    return null; // FIXME
  }
  
  protected PopupMenuPeer createPopupMenu(PopupMenu frontend) 
  {
    return null; // FIXME
  }
  
  protected MenuItemPeer createMenuItem(MenuItem frontend) 
  {
    return null; // FIXME
  }
  
  protected FileDialogPeer createFileDialog(FileDialog frontend) 
  {
    return null; // FIXME
  }
  
  protected CheckboxMenuItemPeer
      createCheckboxMenuItem(CheckboxMenuItem frontend) 
  {
    return null; // FIXME
  }

  protected java.awt.peer.FontPeer getFontPeer(String name, int style) 
  {
    return new XFontPeer (name,style);
  }

  public Dimension getScreenSize()
  { 
    throw new UnsupportedOperationException("not implemented yet");
  }

  public int getScreenResolution()
  {
    throw new UnsupportedOperationException("not implemented yet");
  }
  
  public java.awt.image.ColorModel getColorModel()
  {
    return getDefaultXGraphicsConfiguration().getColorModel();
  }

  public String[] getFontList()
  {
    throw new UnsupportedOperationException("not implemented yet");
  }

  public FontMetrics getFontMetrics(Font font)
  {
    return getDefaultXGraphicsConfiguration().getXFontMetrics(font);
  }

  public void sync()
  {
    flushIfIdle ();
    // FIXME: should instead wait for eventLoop to go idle
    // (perhaps send a dummy event there and block till it makes
    // it through the queue)
  }
    
  public Image getImage(String filename)
  {
    return createImage(filename);
  }
  
  public Image getImage(URL url)
  {
    throw new UnsupportedOperationException("not implemented yet");
  }

  public Image createImage(String filename)
  {
    // FIXME: Stubbed out. We need a proper image I/O API.

    /*
    BufferedImage jpeg;
    FileInputStream fis = openFile(filename);
    if (fis == null)
      return null;
    
    BasicRasterImageConsumer consumer = new BasicRasterImageConsumer();
    JPEGImageDecoder jid = new JPEGImageDecoder(fis);

    jid.startProduction(consumer);
    jpeg = consumer.getImage();
    
    int w = jpeg.getWidth();
    int h = jpeg.getHeight();
    
    BufferedImage img =
      getDefaultXGraphicsConfiguration().createCompatibleImage(w, h);
    
    Renderers renderers = Renderers.getInstance();
    
    RasterOp renderer = renderers.createRenderer(jpeg.getColorModel(),
						 jpeg.getSampleModel(),
						 img.getColorModel(),
						 img.getSampleModel());
	
    if (renderer == null)
      {
	throw new UnsupportedOperationException("couldn't find renderer");
      }

    renderer.filter(jpeg.getRaster(), img.getRaster());
    
    return img;
    */

    return null;
  }

  public Image createImage(URL url)
  {
    throw new UnsupportedOperationException("not implemented yet");
  }

  public boolean prepareImage(Image image,
			      int width,
			      int height,
			      ImageObserver observer)
  {
    throw new UnsupportedOperationException("not implemented yet");
  }
  
  public int checkImage(Image image,
			int width,
			int height,
			ImageObserver observer)
  {
    throw new UnsupportedOperationException("not implemented yet");
  }
  
  public Image createImage(ImageProducer producer)
  {
    throw new UnsupportedOperationException("not implemented yet");
  }

  public Image createImage(byte[] imagedata,
			   int imageoffset,
			   int imagelength)
  {
    throw new UnsupportedOperationException("not implemented yet");
  }

  /*
    public PrintJob getPrintJob(Frame frame,
				String jobtitle,
				Properties props);
  */

  public void beep()
  {
    throw new UnsupportedOperationException("not implemented yet");
  }

  public Clipboard getSystemClipboard() 
  {
    return null; // FIXME
  }
    
  protected EventQueue getSystemEventQueueImpl()
  {
    return queue;
  }
    
  public PrintJob getPrintJob (Frame frame, String title, Properties props)
  {
    return null;		// FIXME
  }

  XGraphicsConfiguration getDefaultXGraphicsConfiguration()
  {
    if (defaultConfig == null)
      {
	Screen screen = display.getDefaultScreen();
	Visual visual = screen.getRootVisual();
	defaultConfig = new XGraphicsConfiguration(visual);

	// ASSERT:
	if (!defaultConfig.getVisual().getScreen().equals(screen))
	  {
	    String msg = "screen of graphics configuration is not " +
	      "default screen";
	    throw new Error(msg);
	  }
      }
    
    return defaultConfig;
  }

  public DragSourceContextPeer
    createDragSourceContextPeer(DragGestureEvent dge)
    throws InvalidDnDOperationException
  {
    throw new UnsupportedOperationException("not implemented");  
  }

  public DragGestureRecognizer
    createDragGestureRecognizer(Class abstractRecognizerClass,
				DragSource ds, Component c,
				int srcActions, DragGestureListener dgl)
  {
    throw new UnsupportedOperationException("not implemented");
  }

    
  public Map mapInputMethodHighlight(InputMethodHighlight highlight)
  {
    throw new UnsupportedOperationException("not implemented");
  }
  
  /** Returns a shared instance of the local, platform-specific
   * graphics environment.
   *
   * <p>This method is specific to GNU Classpath. It gets called by
   * the Classpath implementation of {@link
   * GraphicsEnvironment.getLocalGraphcisEnvironment()}.
   */
  public GraphicsEnvironment getLocalGraphicsEnvironment ()
  {
    return new XGraphicsEnvironment (this);
  }
  
  /** Acquires an appropriate {@link ClasspathFontPeer}, for use in
   * classpath's implementation of {@link java.awt.Font}.
   *
   * @param name The logical name of the font. This may be either a face
   * name or a logical font name, or may even be null. A default
   * implementation of name decoding is provided in
   * {@link ClasspathFontPeer}, but may be overridden in other toolkits.
   *
   * @param attrs Any extra {@link java.awt.font.TextAttribute} attributes
   * this font peer should have, such as size, weight, family name, or
   * transformation.
   */
  public ClasspathFontPeer getClasspathFontPeer (String name, Map attrs)
  {
    int style = Font.PLAIN;
    float size = 12;

    if (attrs.containsKey (TextAttribute.WEIGHT))
      {
        Float weight = (Float) attrs.get (TextAttribute.WEIGHT);
        if (weight.floatValue () >= TextAttribute.WEIGHT_BOLD.floatValue ())
          style += Font.BOLD;
      }

    if (attrs.containsKey (TextAttribute.POSTURE))
      {
        Float posture = (Float) attrs.get (TextAttribute.POSTURE);
        if (posture.floatValue () >= TextAttribute.POSTURE_OBLIQUE.floatValue ())
          style += Font.ITALIC;
      }

    if (attrs.containsKey (TextAttribute.SIZE))
      {
        Float fsize = (Float) attrs.get (TextAttribute.SIZE);
        size = fsize.floatValue ();
      }

    return new XFontPeer (name,style,size);
  }

  /** Creates a font, reading the glyph definitions from a stream.
   *
   * <p>This method provides the platform-specific implementation for
   * the static factory method {@link Font#createFont(int,
   * java.io.InputStream)}.
   *
   * @param format the format of the font data, such as {@link
   * Font#TRUETYPE_FONT}. An implementation may ignore this argument
   * if it is able to automatically recognize the font format from the
   * provided data.
   *
   * @param stream an input stream from where the font data is read
   * in. The stream will be advanced to the position after the font
   * data, but not closed.
   *
   * @throws IllegalArgumentException if <code>format</code> is
   * not supported.
   *
   * @throws FontFormatException if <code>stream</code> does not
   * contain data in the expected format, or if required tables are
   * missing from a font.
   *
   * @throws IOException if a problem occurs while reading in the
   * contents of <code>stream</code>.
   */
  public Font createFont (int format, InputStream stream)
  {
    throw new java.lang.UnsupportedOperationException ();
  }

  public RobotPeer createRobot (GraphicsDevice screen) throws AWTException
  {
    throw new java.lang.UnsupportedOperationException ();
  }

  public EmbeddedWindowPeer createEmbeddedWindow (EmbeddedWindow w)
  {
    throw new java.lang.UnsupportedOperationException ();
  }

  public boolean nativeQueueEmpty() 
  {
    // Tell EventQueue the native queue is empty, because XEventLoop
    // separately ensures that native events are posted to AWT.
    return true;
  }

  public void wakeNativeQueue() 
  {
    // Not implemented, because the native queue is always awake.
    // (i.e. it's polled in a thread separate from the AWT dispatch thread)
  }

  /** Checks the native event queue for events.  If blocking, waits until an
   * event is available before returning, unless interrupted by
   * wakeNativeQueue.  If non-blocking, returns immediately even if no
   * event is available.
   *
   * @param locked The calling EventQueue
   * @param block If true, waits for a native event before returning
   */
  public void iterateNativeQueue(java.awt.EventQueue locked, boolean block) 
  {
    // There is nothing to do here except block, because XEventLoop 
    // iterates the queue in a dedicated thread.
    if (block)
    {
      try
      {
        queue.wait ();
      }
      catch (InterruptedException ie)
      {
        // InterruptedException intentionally ignored
      }
    }
  }

  public void setAlwaysOnTop(boolean b)
  {
    // TODO: Implement properly.
  }

  public boolean isModalExclusionTypeSupported
    (Dialog.ModalExclusionType modalExclusionType)
  {
    // TODO: Implement properly.
    return false;
  }

  public boolean isModalityTypeSupported(Dialog.ModalityType modalityType)
  {
    // TODO: Implement properly.
    return false;
  }
}
