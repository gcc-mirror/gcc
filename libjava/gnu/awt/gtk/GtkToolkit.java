/* GtkToolkit.java -- Implements an AWT Toolkit using GTK for peers
   Copyright (C) 1998, 1999, 2000 Free Software Foundation, Inc.

This file is part of the peer AWT libraries of GNU Classpath.

This library is free software; you can redistribute it and/or modify
it under the terms of the GNU Library General Public License as published 
by the Free Software Foundation, either version 2 of the License, or
(at your option) any later verion.

This library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Library General Public License for more details.

You should have received a copy of the GNU Library General Public License
along with this library; if not, write to the Free Software Foundation
Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307 USA. */


package gnu.awt.gtk;

import java.awt.*;
import java.net.*;
import java.util.Hashtable;
import java.util.Properties;
import java.util.MissingResourceException;
import java.awt.datatransfer.*;
import java.awt.image.*;
import java.awt.peer.*;

public class GtkToolkit extends java.awt.Toolkit
{
  static GtkMainThread gtkthread;
  static EventQueue evtqueue;
  static Hashtable containers = new Hashtable();
  static Clipboard systemClipboard;
  static GtkToolkit instance = null;

  public GtkToolkit ()
  {
    gtkInit();
    instance = this;
    //systemClipboard = new GtkClipboard ();
  }
  
  // Start the thread to run the GTK event loop. This is called from 
  // a GtkComponentPeer static initializer.
  void init ()
  {
    gtkthread = new GtkMainThread ();
    gtkthread.start();
  }

  static native void gtkInit();
  
  native public void beep ();
  
  public int checkImage (Image image, int width, int height, 
			 ImageObserver observer) 
  {
    return ImageObserver.ALLBITS;

//      GtkImage i = (GtkImage) image;
//      return i.checkImage ();
  }

  public Image createImage(String filename)
  {
    return null;
  }

  public Image createImage(URL url)
  {
    return null;
  }

  public Image createImage (ImageProducer producer) 
  {
//    return new GtkImage (producer, null);
    return null;
  }

  public Image createImage (byte[] imagedata, int imageoffset,
			    int imagelength) 
  {
    System.out.println ("createImage byte[] NOT SUPPORTED");
    return null;
  }

  public ColorModel getColorModel () 
  {
    return ColorModel.getRGBdefault ();
  }

  public String[] getFontList () 
  {
    return (new String[] { "Dialog", 
			   "DialogInput", 
			   "Monospaced", 
			   "Serif", 
			   "SansSerif" });
  }

  public FontMetrics getFontMetrics (Font font) 
  {
//    return new GdkFontMetrics (font);
    return null;
  }

  public Image getImage (String filename) 
  {
//    return new GtkImage (new GdkPixbufDecoder (filename), null);
    return null;
  }

  public Image getImage (URL url) 
  {
//    return new GtkImage (new GdkPixbufDecoder (url), null);
    return null;
  }

  /*
  public PrintJob getPrintJob (Frame frame, String jobtitle, Properties props) 
  {
    return null;
  }
  */
  native public int getScreenResolution();

  native public Dimension getScreenSize ();

  public Clipboard getSystemClipboard() 
  {
    return systemClipboard;
  }

  public boolean prepareImage (Image image, int width, int height, 
			       ImageObserver observer) 
  {
    return false;
  }

  native public void sync ();

  protected void setComponentState (Component c, GtkComponentPeer cp)
  {      
    /* Make the Peer reflect the state of the Component */
    if (! (c instanceof Window))
      {
	cp.setCursor (c.getCursor ());
	
	Rectangle bounds = c.getBounds ();
	cp.setBounds (bounds.x, bounds.y, bounds.width, bounds.height);
	if (c instanceof Canvas)
	  System.out.println ("width " + bounds.width + " height " + bounds.height);
	
	cp.setVisible (c.isVisible ());
      }
  }

  protected ButtonPeer createButton (Button b)
  {
    return null;
    /*    
    GtkButtonPeer bp = new GtkButtonPeer (b);
    Rectangle bounds = b.getBounds ();
    bp.setBounds (bounds.x, bounds.y, bounds.width, bounds.height);
    return bp;
    */
  }

  protected CanvasPeer createCanvas (Canvas c) 
  {
//    return new GtkCanvasPeer (c);
    return null;
  }

  protected CheckboxPeer createCheckbox (Checkbox cb) 
  {
    return null;
    /*
    if (cb.getCheckboxGroup () != null)
      return new GtkRadioButtonPeer (cb);
    else
      return new GtkCheckButtonPeer (cb);
    */
  }

  protected CheckboxMenuItemPeer createCheckboxMenuItem (CheckboxMenuItem cmi)
  {
    return null;
    //return new GtkCheckboxMenuItemPeer (cmi);
  }

  protected ChoicePeer createChoice (Choice c) 
  {
    return null;
    //return new GtkChoicePeer (c);
  }

  protected DialogPeer createDialog (Dialog d)
  {
    return null;  
    //return new GtkDialogPeer (d);
  }

  protected FileDialogPeer createFileDialog (FileDialog fd)
  {
    return null;  
    //return new GtkFileDialogPeer (fd);
  }

  protected FramePeer createFrame (Frame f)
  {  
    return new GtkFramePeer (f);
  }

  protected LabelPeer createLabel (Label label) 
  {
    return null;
    //return new GtkLabelPeer (label);
  }

  protected ListPeer createList (List list)
  {
    return null;
    //return new GtkListPeer (list);
  }

  protected MenuPeer createMenu (Menu m) 
  {
    return null;
    //return new GtkMenuPeer (m);
  }

  protected MenuBarPeer createMenuBar (MenuBar mb) 
  {
    return null;
    //return new GtkMenuBarPeer (mb);
  }

  protected MenuItemPeer createMenuItem (MenuItem mi) 
  {
    return null;
    //return new GtkMenuItemPeer (mi);
  }

  protected PanelPeer createPanel (Panel p) 
  {
    return null;
    //return new GtkPanelPeer (p);
  }

  protected PopupMenuPeer createPopupMenu (PopupMenu target) 
  {
    return null;
    //return new GtkPopupMenuPeer (target);
  }

  protected ScrollPanePeer createScrollPane (ScrollPane sp) 
  {
    return null;
    //return new GtkScrollPanePeer (sp);
  }

  protected ScrollbarPeer createScrollbar (Scrollbar sb) 
  {
    return null;
    //return new GtkScrollbarPeer (sb);
  }

  protected TextAreaPeer createTextArea (TextArea ta) 
  {
    return null;
    //return new GtkTextAreaPeer (ta);
  }

  protected TextFieldPeer createTextField (TextField tf) 
  {
    return null;
    //return new GtkTextFieldPeer (tf);
  }

  protected WindowPeer createWindow (Window w)
  {
    return new GtkWindowPeer (w);
  }

  protected FontPeer getFontPeer (String name, int style) 
  {
    return null;
    /*
    try 
    {
      GtkFontPeer fp = new GtkFontPeer (name, style);
      return fp;
    } 
    catch (MissingResourceException ex) 
    {
      return null;
    }
    */
  }

  protected EventQueue getSystemEventQueueImpl() 
  {
    return GtkComponentPeer.eventQueue;
  }

  protected void loadSystemColors (int[] systemColors) 
  {
  }
}
