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
import java.awt.datatransfer.Clipboard;
import java.awt.dnd.DragGestureEvent;
import java.awt.dnd.peer.DragSourceContextPeer;
import java.awt.font.TextAttribute;
import java.awt.im.InputMethodHighlight;
import java.awt.image.ColorModel;
import java.awt.image.ImageObserver;
import java.awt.image.ImageProducer;
import java.awt.peer.*;
import java.net.URL;
import java.util.Hashtable;
import java.util.Map;
import java.util.MissingResourceException;
import java.util.Properties;
import gnu.java.awt.EmbeddedWindow;
import gnu.java.awt.EmbeddedWindowSupport;
import gnu.java.awt.peer.EmbeddedWindowPeer;
import gnu.java.awt.peer.ClasspathFontPeer;
import gnu.classpath.Configuration;
import gnu.java.awt.peer.gtk.GdkPixbufDecoder;

/* This class uses a deprecated method java.awt.peer.ComponentPeer.getPeer().
   This merits comment.  We are basically calling Sun's bluff on this one.
   We think Sun has deprecated it simply to discourage its use as it is 
   bad programming style.  However, we need to get at a component's peer in
   this class.  If getPeer() ever goes away, we can implement a hash table
   that will keep up with every window's peer, but for now this is faster. */

/**
 * This class accesses a system property called
 * <tt>gnu.java.awt.peer.gtk.Graphics</tt>.  If the property is defined and
 * equal to "Graphics2D", the cairo-based GdkGraphics2D will be used in
 * drawing contexts. Any other value will cause the older GdkGraphics
 * object to be used.
 */

public class GtkToolkit extends gnu.java.awt.ClasspathToolkit
  implements EmbeddedWindowSupport
{
  GtkMainThread main;
  Hashtable containers = new Hashtable();
  static EventQueue q = new EventQueue();
  static Clipboard systemClipboard;

  static boolean useGraphics2dSet;
  static boolean useGraphics2d;

  public static boolean useGraphics2D()
  {
    if (useGraphics2dSet)
      return useGraphics2d;
    useGraphics2d = System.getProperty("gnu.java.awt.peer.gtk.Graphics", 
                                       "Graphics").equals("Graphics2D");
    useGraphics2dSet = true;
    return useGraphics2d;
  }

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
    return ((GtkImage) image).checkImage ();
  }

  public Image createImage (String filename)
  {
    return new GtkImage (new GdkPixbufDecoder (filename), null);
  }

  public Image createImage (URL url)
  {
    return new GtkImage (new GdkPixbufDecoder (url), null);
  }

  public Image createImage (ImageProducer producer) 
  {
    return new GtkImage (producer, null);
  }

  public Image createImage (byte[] imagedata, int imageoffset,
			    int imagelength)
  {
    return new GtkImage (new GdkPixbufDecoder (imagedata,
					       imageoffset,
					       imagelength),
			 null);
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
    if (useGraphics2D())
      return new GdkClasspathFontPeerMetrics (font);
    else
      return new GdkFontMetrics (font);
  }

  public Image getImage (String filename) 
  {
    return new GtkImage (new GdkPixbufDecoder (filename), null);
  }

  public Image getImage (URL url) 
  {
    return new GtkImage (new GdkPixbufDecoder (url), null);
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
    GtkImage i = (GtkImage) image;

    if (i.isLoaded ()) return true;

    class PrepareImage extends Thread
    {
      GtkImage image;
      ImageObserver observer;

      PrepareImage (GtkImage image, ImageObserver observer)
      {
	this.image = image;
	image.setObserver (observer);
      }
      
      public void run ()
      {
	image.source.startProduction (image);
      }
    }

    new PrepareImage (i, observer).start ();
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

  public EmbeddedWindowPeer createEmbeddedWindow (EmbeddedWindow w)
  {
    return new GtkEmbeddedWindowPeer (w);
  }

  /** 
   * @deprecated part of the older "logical font" system in earlier AWT
   * implementations. Our newer Font class uses getClasspathFontPeer.
   */
  protected FontPeer getFontPeer (String name, int style) 
  {
    try {
      GtkFontPeer fp = new GtkFontPeer (name, style);
      return fp;
    } catch (MissingResourceException ex) {
      return null;
    }
  }

  /**
   * Newer method to produce a peer for a Font object, even though Sun's
   * design claims Font should now be peerless, we do not agree with this
   * model, hence "ClasspathFontPeer". 
   */

  public ClasspathFontPeer getClasspathFontPeer (String name, Map attrs)
  {
    if (useGraphics2D())
      return new GdkClasspathFontPeer (name, attrs);
    else
      {
        int style = Font.PLAIN;

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
        
        return (ClasspathFontPeer) this.getFontPeer (name, style);
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

  // ClasspathToolkit methods

  public GraphicsEnvironment getLocalGraphicsEnvironment()
  {
    throw new java.lang.UnsupportedOperationException ();
  }

  public Font createFont(int format, java.io.InputStream stream)
  {
    throw new java.lang.UnsupportedOperationException ();
  }


} // class GtkToolkit
