/* GtkComponentPeer.java -- Implements ComponentPeer with GTK
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
import java.awt.event.*;
import java.awt.image.*;
import java.awt.peer.ComponentPeer;

public abstract class GtkComponentPeer implements ComponentPeer
{
  // We need to put a reference to the Event Queue somewhere. This seems like 
  // a convenient place.
  static EventQueue eventQueue = new EventQueue();

  Component awtComponent;
  gnu.gcj.RawData ptr;  // Actual gtk object.

  static
  {
    // This will start the main toolkit thread.
    GtkToolkit.instance.init();
  }

  public int checkImage (Image image, int width, int height, 
			 ImageObserver observer) 
  {
    return -1;
    /*
    GtkImage i = (GtkImage) image;
    return i.checkImage ();
    */
  }

  public Image createImage (ImageProducer producer)
  {
    return null;
    //return new GtkImage (producer, null);
  }

  public Image createImage (int width, int height)
  {
    return null;
    /*
    GdkGraphics g = new GdkGraphics (width, height);
    return new GtkOffScreenImage (null, g, width, height);
    */
  }

  public void disable () 
  {
    setEnabled (false);
  }

  native public void dispose ();

  public void enable () 
  {
    setEnabled (true);
  }

  /** 
   * Get the graphics configuration of the component. The color model
   * of the component can be derived from the configuration.
   */
  public GraphicsConfiguration getGraphicsConfiguration ()
  {
    return null;
  }

  public FontMetrics getFontMetrics (Font font)
  {
    return null;
    //return new GdkFontMetrics (font);
  }

  public Graphics getGraphics ()
  {
    throw new InternalError ();
  }

  public native Point getLocationOnScreen ();  
  public native Dimension getMinimumSize();
  public native Dimension getPreferredSize();
  
  public Toolkit getToolkit ()
  {
    return GtkToolkit.instance;
  }
  
  public void handleEvent(AWTEvent e)
  {
  }
  
  public void hide () 
  {
    setVisible (false);
  }

  public void show () 
  {
    setVisible (true);
  }
  
  public boolean isFocusTraversable () 
  {
    return true;
  }

  public Dimension minimumSize () 
  {
    return getMinimumSize();
  }
  
  public Dimension preferredSize()
  {
    return getPreferredSize();
  }

  public void paint (Graphics g)
  {
    awtComponent.paint (g); // ???
  }

  public boolean prepareImage (Image image, int width, int height,
			       ImageObserver observer) 
  {
    /*
    GtkImage i = (GtkImage) image;

    if (i.isLoaded ()) return true;

    class PrepareImage extends Thread
    {
      GtkImage image;
      ImageObserver observer;

      PrepareImage (GtkImage image, ImageObserver observer)
      {
	this.image = image;
	this.observer = observer;
      }
      
      public void run ()
      {
	// XXX: need to return data to image observer
	image.source.startProduction (null);
      }
    }

    new PrepareImage (i, observer).start ();
    */
    return false;
  }
  
  public void print (Graphics g) 
  {
    throw new RuntimeException ();
  }
  
  native public void requestFocus ();

  public void repaint (long tm, int x, int y, int width, int height)
  {
    // ???
    eventQueue.postEvent (new PaintEvent (
      awtComponent, PaintEvent.UPDATE, new Rectangle (x, y, width, height)));
  }

  
  public void reshape (int x, int y, int width, int height) 
  {
    setBounds (x, y, width, height);
  }

  public native void setBounds (int x, int y, int width, int height);
  public native void setCursor (Cursor cursor);

  public native void setEnabled (boolean b);
  
  public native void setEventMask(long eventMask);
  public native void setFont(Font font);
  public native void setForeground(Color color);
  public native void setBackground (Color c);
  public native void setVisible(boolean visible);

  native void realize();

  protected GtkComponentPeer (Component awtComponent)
  {
    this.awtComponent = awtComponent;
    create();
    
    // TODO: Each of these calls will currently perform a separate native lock.
    // It may be desirable to use our own, recusive mutex implementation by
    // passing our threads implementation to g_threads_init().
    // This would greatly reduce locking calls in the peer code, and allow us
    // to aquire the lock from java code.
    Rectangle r = awtComponent.getBounds();
    setBounds (r.x, r.y, r.width, r.height);
    
    Color c = awtComponent.getForeground();
    if (c != null)
      setForeground (c);
    c = awtComponent.getBackground();
    if (c != null)
      setBackground (c);
    setEnabled (awtComponent.isEnabled());
    Font f = awtComponent.getFont();
    if (f != null)
      setFont (awtComponent.getFont());
      
    realize();
  }
    
  protected native void create ();

  // FIXME: It may make sense to do the following directly from the native
  // code.
  protected void postMouseEvent(int id, long when, int mods, int x, int y, 
				int clickCount, boolean popupTrigger) 
  {
    eventQueue.postEvent(new MouseEvent(awtComponent, id, when, mods, x, y, 
					clickCount, popupTrigger));
  }

  protected void postExposeEvent (int x, int y, int width, int height)
  {
    eventQueue.postEvent (new PaintEvent (awtComponent, PaintEvent.PAINT,
				      new Rectangle (x, y, width, height)));
  }

  protected void postKeyEvent (int id, long when, int mods, 
			       int keyCode, char keyChar)
  {
    eventQueue.postEvent (new KeyEvent (awtComponent, id, when, mods, 
			       keyCode, keyChar));
  }
  
  protected void postFocusEvent (int id, boolean temporary)
  {
    eventQueue.postEvent (new FocusEvent (awtComponent, id, temporary));
  }

  protected void postItemEvent (Object item, int stateChange)
  {
    eventQueue.postEvent (new ItemEvent ((ItemSelectable)awtComponent, 
				ItemEvent.ITEM_STATE_CHANGED,
				item, stateChange));
  }
}
