/* GtkToolkit.java -- Implements an AWT Toolkit using GTK for peers
   Copyright (C) 1998, 1999, 2002, 2003 Free Software Foundation, Inc.

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


package gnu.java.awt.peer.gtk;

import java.awt.*;
import java.awt.datatransfer.*;
import java.awt.dnd.DragGestureEvent;
import java.awt.dnd.peer.DragSourceContextPeer;
import java.awt.im.InputMethodHighlight;
import java.awt.image.*;
import java.awt.peer.*;
import java.net.*;
import java.util.Hashtable;
import java.util.Map;
import java.util.MissingResourceException;
import java.util.Properties;
import gnu.java.awt.image.*;
import gnu.classpath.Configuration;

/* This class uses a deprecated method java.awt.peer.ComponentPeer.getPeer().
   This merits comment.  We are basically calling Sun's bluff on this one.
   We think Sun has deprecated it simply to discourage its use as it is 
   bad programming style.  However, we need to get at a component's peer in
   this class.  If getPeer() ever goes away, we can implement a hash table
   that will keep up with every window's peer, but for now this is faster. */

public class GtkToolkit extends java.awt.Toolkit
{
  GtkMainThread main;
  Hashtable containers = new Hashtable();
  static EventQueue q = new EventQueue();
  static Clipboard systemClipboard;

  static 
  {
    if (Configuration.INIT_LOAD_LIBRARY)
      System.loadLibrary("gtkpeer");
  }

  public GtkToolkit ()
  {
    main = new GtkMainThread ();
    systemClipboard = new GtkClipboard ();
    GtkGenericPeer.enableQueue (q);
  }
  
  native public void beep ();
  native private void getScreenSizeDimensions (int[] xy);
  
  public int checkImage (Image image, int width, int height, 
			 ImageObserver observer) 
  {
    return ImageObserver.ALLBITS;

//      GtkImage i = (GtkImage) image;
//      return i.checkImage ();
  }

  public Image createImage (String filename)
  {
    // FIXME - gcj local: GdkPixbufDecoder doesn't work.
    // return new GtkImage (new GdkPixbufDecoder (filename), null);
    return null;
  }

  public Image createImage (URL url)
  {
    // FIXME - gcj local: GdkPixbufDecoder doesn't work.
    // return new GtkImage (new GdkPixbufDecoder (url), null);
    return null;
  }

  public Image createImage (ImageProducer producer) 
  {
    return new GtkImage (producer, null);
  }

  public Image createImage (byte[] imagedata, int imageoffset,
			    int imagelength) 
  {
    // System.out.println ("createImage byte[] NOT SUPPORTED");
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
    return new GdkFontMetrics (font);
  }

  public Image getImage (String filename) 
  {
    // FIXME - gcj local: GdkPixbufDecoder doesn't work.
    // return new GtkImage (new GdkPixbufDecoder (filename), null);
    return null;
  }

  public Image getImage (URL url) 
  {
    // FIXME - gcj local: GdkPixbufDecoder doesn't work.
    // return new GtkImage (new GdkPixbufDecoder (url), null);
    return null;
  }

  public PrintJob getPrintJob (Frame frame, String jobtitle, Properties props) 
  {
    return null;
  }

  native public int getScreenResolution();

  public Dimension getScreenSize () {
    int dim[] = new int[2];
    getScreenSizeDimensions(dim);
    return new Dimension(dim[0], dim[1]);
  }

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
    return new GtkButtonPeer (b);
  }

  protected CanvasPeer createCanvas (Canvas c) 
  {
    return new GtkCanvasPeer (c);
  }

  protected CheckboxPeer createCheckbox (Checkbox cb) 
  {
    return new GtkCheckboxPeer (cb);
  }

  protected CheckboxMenuItemPeer createCheckboxMenuItem (CheckboxMenuItem cmi)
  {
    return new GtkCheckboxMenuItemPeer (cmi);
  }

  protected ChoicePeer createChoice (Choice c) 
  {
    return new GtkChoicePeer (c);
  }

  protected DialogPeer createDialog (Dialog d)
  {
    return new GtkDialogPeer (d);
  }

  protected FileDialogPeer createFileDialog (FileDialog fd)
  {
    return new GtkFileDialogPeer (fd);
  }

  protected FramePeer createFrame (Frame f)
  {
    return new GtkFramePeer (f);
  }

  protected LabelPeer createLabel (Label label) 
  {
    return new GtkLabelPeer (label);
  }

  protected ListPeer createList (List list)
  {
    return new GtkListPeer (list);
  }

  protected MenuPeer createMenu (Menu m) 
  {
    return new GtkMenuPeer (m);
  }

  protected MenuBarPeer createMenuBar (MenuBar mb) 
  {
    return new GtkMenuBarPeer (mb);
  }

  protected MenuItemPeer createMenuItem (MenuItem mi) 
  {
    return new GtkMenuItemPeer (mi);
  }

  protected PanelPeer createPanel (Panel p) 
  {
    return new GtkPanelPeer (p);
  }

  protected PopupMenuPeer createPopupMenu (PopupMenu target) 
  {
    return new GtkPopupMenuPeer (target);
  }

  protected ScrollPanePeer createScrollPane (ScrollPane sp) 
  {
    return new GtkScrollPanePeer (sp);
  }

  protected ScrollbarPeer createScrollbar (Scrollbar sb) 
  {
    return new GtkScrollbarPeer (sb);
  }

  protected TextAreaPeer createTextArea (TextArea ta) 
  {
    return new GtkTextAreaPeer (ta);
  }

  protected TextFieldPeer createTextField (TextField tf) 
  {
    return new GtkTextFieldPeer (tf);
  }

  protected WindowPeer createWindow (Window w)
  {
    return new GtkWindowPeer (w);
  }

  protected FontPeer getFontPeer (String name, int style) 
  {
    try {
      GtkFontPeer fp = new GtkFontPeer (name, style);
      return fp;
    } catch (MissingResourceException ex) {
      return null;
    }
  }

  protected EventQueue getSystemEventQueueImpl() 
  {
    return q;
  }

  protected void loadSystemColors (int[] systemColors) 
  {
  }

  public DragSourceContextPeer createDragSourceContextPeer(DragGestureEvent e)
  {
    throw new Error("not implemented");
  }

  public Map mapInputMethodHighlight(InputMethodHighlight highlight)
  {
    throw new Error("not implemented");
  }
} // class GtkToolkit
