/* GtkComponentPeer.java -- Implements ComponentPeer with GTK
   Copyright (C) 1998, 1999, 2002 Free Software Foundation, Inc.

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

import java.awt.AWTEvent;
import java.awt.BufferCapabilities;
import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Frame;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GraphicsConfiguration;
import java.awt.Image;
import java.awt.Insets;
import java.awt.ItemSelectable;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.Window;
import java.awt.event.FocusEvent;
import java.awt.event.ItemEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.event.PaintEvent;
import java.awt.image.ColorModel;
import java.awt.image.ImageObserver;
import java.awt.image.ImageProducer;
import java.awt.image.VolatileImage;
import java.awt.peer.ComponentPeer;

public class GtkComponentPeer extends GtkGenericPeer
  implements ComponentPeer
{
  Component awtComponent;

  Insets insets;

  /* this isEnabled differs from Component.isEnabled, in that it
     knows if a parent is disabled.  In that case Component.isEnabled 
     may return true, but our isEnabled will always return false */
  native boolean isEnabled ();
  native static boolean modalHasGrab ();

  native int[] gtkWidgetGetForeground ();
  native int[] gtkWidgetGetBackground ();
  native void gtkWidgetSetVisible (boolean b);
  native void gtkWidgetGetDimensions (int[] dim);
  native void gtkWidgetGetPreferredDimensions (int[] dim);
  native void gtkWidgetGetLocationOnScreen (int[] point);
  native void gtkWidgetSetCursor (int type);
  native void gtkWidgetSetBackground (int red, int green, int blue);
  native void gtkWidgetSetForeground (int red, int green, int blue);

  void create ()
  {
    throw new RuntimeException ();
  }

  native void connectJObject ();
  native void connectSignals ();

  protected GtkComponentPeer (Component awtComponent)
  {
    super (awtComponent);
    this.awtComponent = awtComponent;
    insets = new Insets (0, 0, 0, 0);

    /* temporary try/catch block until all peers use this creation method */
    try {
      create ();
      
      GtkArgList args = new GtkArgList ();
      getArgs (awtComponent, args);
      args.setArgs (this);

      connectJObject ();
      connectSignals ();

      if (awtComponent.getForeground () != null)
	setForeground (awtComponent.getForeground ());
      if (awtComponent.getBackground () != null)
	setBackground (awtComponent.getBackground ());
      if (awtComponent.getFont() != null)
	setFont(awtComponent.getFont());

      setCursor (awtComponent.getCursor ());
      Rectangle bounds = awtComponent.getBounds ();
      setBounds (bounds.x, bounds.y, bounds.width, bounds.height);

    } catch (RuntimeException ex) { ; }
  }

  public int checkImage (Image image, int width, int height, 
			 ImageObserver observer) 
  {
    GtkImage i = (GtkImage) image;
    return i.checkImage ();
  }

  public Image createImage (ImageProducer producer) 
  {
    return new GtkImage (producer, null);
  }

  public Image createImage (int width, int height)
  {
    Graphics g;
    if (GtkToolkit.useGraphics2D ())
      {
        Graphics2D g2 = new GdkGraphics2D (width, height);
        g2.setBackground (getBackground ());
        g = g2;
      }
    else
      g = new GdkGraphics (width, height);

    return new GtkOffScreenImage (null, g, width, height);
  }

  public void disable () 
  {
    setEnabled (false);
  }

  public void enable () 
  {
    setEnabled (true);
  }

  public ColorModel getColorModel () 
  {
    return ColorModel.getRGBdefault ();
  }

  public FontMetrics getFontMetrics (Font font)
  {
    return new GdkFontMetrics (font);
  }

  public Graphics getGraphics ()
  {
    return null;
  }

  public Point getLocationOnScreen () 
  { 
    int point[] = new int[2];
    gtkWidgetGetLocationOnScreen (point);
    return new Point (point[0], point[1]);
  }

  public Dimension getMinimumSize () 
  {
    return minimumSize ();
  }

  public Dimension getPreferredSize ()
  {
    return preferredSize ();
  }

  public Toolkit getToolkit ()
  {
    return Toolkit.getDefaultToolkit();
  }
  
  public void handleEvent (AWTEvent event)
  {
  }
  
  public boolean isFocusTraversable () 
  {
    return true;
  }

  public Dimension minimumSize () 
  {
    int dim[] = new int[2];

    gtkWidgetGetPreferredDimensions (dim);

    return new Dimension (dim[0], dim[1]);
  }

  public void paint (Graphics g)
  {
    awtComponent.paint (g);
  }

  public Dimension preferredSize ()
  {
    int dim[] = new int[2];

    gtkWidgetGetPreferredDimensions (dim);

    return new Dimension (dim[0], dim[1]);
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

  public void print (Graphics g) 
  {
    throw new RuntimeException ();
  }

  public void repaint (long tm, int x, int y, int width, int height)
  {
    q.postEvent (new PaintEvent (awtComponent, PaintEvent.UPDATE,
				 new Rectangle (x, y, width, height)));
  }

  native public void requestFocus ();

  public void reshape (int x, int y, int width, int height) 
  {
    setBounds (x, y, width, height);
  }

  public void setBackground (Color c) 
  {
    gtkWidgetSetBackground (c.getRed(), c.getGreen(), c.getBlue());
  }

  native public void setNativeBounds (int x, int y, int width, int height);

  public void setBounds (int x, int y, int width, int height)
  {
    Component parent = awtComponent.getParent ();
    
    if (parent instanceof Window)
      {
	Insets insets = ((Window) parent).getInsets ();
	// Convert from Java coordinates to GTK coordinates.
	setNativeBounds (x - insets.left, y - insets.top, width, height);
      }
    else
      setNativeBounds (x, y, width, height);
  }

  public void setCursor (Cursor cursor) 
  {
    gtkWidgetSetCursor (cursor.getType ());
  }

  public void setEnabled (boolean b)
  {
    set ("sensitive", b);
  }

  public void setFont (Font f)
  {
    // FIXME: This should really affect the widget tree below me.
    // Currently this is only handled if the call is made directly on
    // a text widget, which implements setFont() itself.
  }

  public void setForeground (Color c) 
  {
    gtkWidgetSetForeground (c.getRed(), c.getGreen(), c.getBlue());
  }

  public Color getForeground ()
  {
    int rgb[] = gtkWidgetGetForeground ();
    return new Color (rgb[0], rgb[1], rgb[2]);
  }

  public Color getBackground ()
  {
    int rgb[] = gtkWidgetGetBackground ();
    return new Color (rgb[0], rgb[1], rgb[2]);
  }

  public void setVisible (boolean b)
  {
    set ("visible", b);
  }
  
  public void hide () 
  {
    setVisible (false);
  }

  public void show () 
  {
    setVisible (true);
  }

  protected void postMouseEvent(int id, long when, int mods, int x, int y, 
				int clickCount, boolean popupTrigger) 
  {
    q.postEvent(new MouseEvent(awtComponent, id, when, mods, x, y, 
			       clickCount, popupTrigger));
  }

  protected void postExposeEvent (int x, int y, int width, int height)
  {
    q.postEvent (new PaintEvent (awtComponent, PaintEvent.PAINT,
				 new Rectangle (x, y, width, height)));
  }

  protected void postKeyEvent (int id, long when, int mods,
			       int keyCode, char keyChar, int keyLocation)
  {
    q.postEvent (new KeyEvent (awtComponent, id, when, mods,
			       keyCode, keyChar, keyLocation));
  }

  protected void postFocusEvent (int id, boolean temporary)
  {
    q.postEvent (new FocusEvent (awtComponent, id, temporary));
  }

  protected void postItemEvent (Object item, int stateChange)
  {
    q.postEvent (new ItemEvent ((ItemSelectable)awtComponent, 
				ItemEvent.ITEM_STATE_CHANGED,
				item, stateChange));
  }

  public void getArgs (Component component, GtkArgList args)
  {
    args.add ("visible", component.isVisible ());
    args.add ("sensitive", component.isEnabled ());

    ComponentPeer p;

    do
      {
	component = component.getParent ();
	p = component.getPeer ();
      } while (p instanceof java.awt.peer.LightweightPeer);
    
    if (p != null)
      args.add ("parent", p);
  }

  native void set (String name, String value);
  native void set (String name, boolean value);
  native void set (String name, int value);
  native void set (String name, float value);
  native void set (String name, Object value);

  void set (GtkArg arg)
  {
    String name = arg.getName ();
    Object value = arg.getValue ();

    if (value instanceof Boolean)
      set (name, ((Boolean)value).booleanValue ());
    else if (value instanceof Integer)
      set (name, ((Integer)value).intValue ());
    else if (value instanceof Float)
      set (name, ((Float)value).floatValue ());
    else if (value instanceof String)
      set (name, ((String) value));
    else
      set (name, value);
  }

  public GraphicsConfiguration getGraphicsConfiguration ()
  {
    // FIXME: just a stub for now.
    return null;
  }

  public void setEventMask (long mask)
  {
    // FIXME: just a stub for now.
  }

  public boolean isFocusable ()
  {
    return false;
  }

  public boolean requestFocus (Component source, boolean b1, 
                               boolean b2, long x)
  {
    return false;
  }

  public boolean isObscured ()
  {
    return false;
  }

  public boolean canDetermineObscurity ()
  {
    return false;
  }

  public void coalescePaintEvent (PaintEvent e)
  {
    
  }

  public void updateCursorImmediately ()
  {
    
  }

  public VolatileImage createVolatileImage (int width, int height)
  {
    return null;
  }

  public boolean handlesWheelScrolling ()
  {
    return false;
  }

  public void createBuffers (int x, BufferCapabilities capabilities)
    throws java.awt.AWTException

  {
    
  }

  public Image getBackBuffer ()
  {
    return null;
  }

  public void flip (BufferCapabilities.FlipContents contents)
  {
    
  }

  public void destroyBuffers ()
  {
    
  }
}
