/* Copyright (C) 2000, 2002, 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.awt.xlib;

import java.awt.*;
import java.awt.dnd.*;
import java.awt.dnd.peer.*;
import java.awt.im.*;
import java.awt.peer.*;
import java.awt.image.ImageProducer;
import java.awt.image.ImageObserver;
import java.net.*;
import java.awt.datatransfer.Clipboard;
import java.util.Properties;
import java.util.Map;

import gnu.gcj.xlib.Display;
import gnu.gcj.xlib.Screen;
import gnu.gcj.xlib.Visual;

public class XToolkit extends Toolkit
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
    return null;
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
}
