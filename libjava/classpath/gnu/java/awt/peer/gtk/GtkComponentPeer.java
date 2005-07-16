/* GtkComponentPeer.java -- Implements ComponentPeer with GTK
   Copyright (C) 1998, 1999, 2002, 2004, 2005  Free Software Foundation, Inc.

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

import java.awt.AWTEvent;
import java.awt.AWTException;
import java.awt.BufferCapabilities;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
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
import java.awt.event.TextEvent;
import java.awt.image.BufferedImage;
import java.awt.image.ColorModel;
import java.awt.image.ImageObserver;
import java.awt.image.ImageProducer;
import java.awt.image.VolatileImage;
import java.awt.peer.ComponentPeer;

public class GtkComponentPeer extends GtkGenericPeer
  implements ComponentPeer
{
  VolatileImage backBuffer;
  BufferCapabilities caps;

  Component awtComponent;

  Insets insets;

  boolean isInRepaint;

  /* this isEnabled differs from Component.isEnabled, in that it
     knows if a parent is disabled.  In that case Component.isEnabled 
     may return true, but our isEnabled will always return false */
  native boolean isEnabled ();
  static native boolean modalHasGrab();

  native int[] gtkWidgetGetForeground ();
  native int[] gtkWidgetGetBackground ();
  native void gtkWidgetGetDimensions (int[] dim);
  native void gtkWidgetGetPreferredDimensions (int[] dim);
  native void gtkWidgetGetLocationOnScreen (int[] point);
  native void gtkWidgetSetCursor (int type);
  native void gtkWidgetSetBackground (int red, int green, int blue);
  native void gtkWidgetSetForeground (int red, int green, int blue);
  native void gtkWidgetSetSensitive (boolean sensitive);
  native void gtkWidgetSetParent (ComponentPeer parent);
  native void gtkWidgetRequestFocus ();
  native void gtkWidgetDispatchKeyEvent (int id, long when, int mods,
                                         int keyCode, int keyLocation);

  native boolean isRealized ();

  void realize ()
  {
    // Default implementation does nothing
  }

  native void setNativeEventMask ();

  void create ()
  {
    throw new RuntimeException ();
  }

  native void connectSignals ();

  protected GtkComponentPeer (Component awtComponent)
  {
    super (awtComponent);
    this.awtComponent = awtComponent;
    insets = new Insets (0, 0, 0, 0);

    create ();

    connectSignals ();

    if (awtComponent.getForeground () != null)
      setForeground (awtComponent.getForeground ());
    if (awtComponent.getBackground () != null)
      setBackground (awtComponent.getBackground ());
    if (awtComponent.getFont() != null)
      setFont(awtComponent.getFont());

    Component parent = awtComponent.getParent ();

    // Only set our parent on the GTK side if our parent on the AWT
    // side is not showing.  Otherwise the gtk peer will be shown
    // before we've had a chance to position and size it properly.
    if (awtComponent instanceof Window
        || (parent != null && ! parent.isShowing ()))
      setParentAndBounds ();

    setNativeEventMask ();

    realize ();
  }

  void setParentAndBounds ()
  {
    setParent ();

    setComponentBounds ();

    setVisibleAndEnabled ();
  }

  void setParent ()
  {
    ComponentPeer p;
    Component component = awtComponent;
    do
      {
        component = component.getParent ();
        p = component.getPeer ();
      }
    while (p instanceof java.awt.peer.LightweightPeer);

    if (p != null)
      gtkWidgetSetParent (p);
  }

  void beginNativeRepaint ()
  {
    isInRepaint = true;
  }

  void endNativeRepaint ()
  {
    isInRepaint = false;
  }

  /*
   * Set the bounds of this peer's AWT Component based on dimensions
   * returned by the native windowing system.  Most Components impose
   * their dimensions on the peers which is what the default
   * implementation does.  However some peers, like GtkFileDialogPeer,
   * need to pass their size back to the AWT Component.
   */
  void setComponentBounds ()
  {
    Rectangle bounds = awtComponent.getBounds ();

    if (bounds.x == 0 && bounds.y == 0
        && bounds.width == 0 && bounds.height == 0)
      return;

    setBounds (bounds.x, bounds.y, bounds.width, bounds.height);
  }

  void setVisibleAndEnabled ()
  {
    setVisible (awtComponent.isVisible ());
    setEnabled (awtComponent.isEnabled ());
  }

  public int checkImage (Image image, int width, int height, 
			 ImageObserver observer) 
  {
    return getToolkit().checkImage(image, width, height, observer);
  }

  public Image createImage (ImageProducer producer) 
  {
    return new GtkImage (producer);
  }

  public Image createImage (int width, int height)
  {
    Image image;
    if (GtkToolkit.useGraphics2D ())
      image = new BufferedImage (width, height, BufferedImage.TYPE_INT_RGB);
    else
      image = new GtkImage (width, height);

    Graphics g = image.getGraphics();
    g.setColor(getBackground());
    g.fillRect(0, 0, width, height);
    return image;
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
    return getToolkit().getFontMetrics(font);
  }

  public Graphics getGraphics ()
  {
    if (GtkToolkit.useGraphics2D ())
        return new GdkGraphics2D (this);
    else
        return new GdkGraphics (this);
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
    int id = event.getID();
    KeyEvent ke = null;

    switch (id)
      {
      case PaintEvent.PAINT:
      case PaintEvent.UPDATE:
        {
          try 
            {
              Graphics g = getGraphics ();
          
              // Some peers like GtkFileDialogPeer are repainted by Gtk itself
              if (g == null)
                break;

              g.setClip (((PaintEvent) event).getUpdateRect());

              if (id == PaintEvent.PAINT)
                awtComponent.paint (g);
              else
                awtComponent.update (g);

              g.dispose ();
            }
          catch (InternalError e)
            {
              System.err.println (e);
            }
        }
        break;
      case KeyEvent.KEY_PRESSED:
        ke = (KeyEvent) event;
        gtkWidgetDispatchKeyEvent (ke.getID (), ke.getWhen (), ke.getModifiersEx (),
                                   ke.getKeyCode (), ke.getKeyLocation ());
        break;
      case KeyEvent.KEY_RELEASED:
        ke = (KeyEvent) event;
        gtkWidgetDispatchKeyEvent (ke.getID (), ke.getWhen (), ke.getModifiersEx (),
                                   ke.getKeyCode (), ke.getKeyLocation ());
        break;
      }
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
    return getToolkit().prepareImage(image, width, height, observer);
  }

  public void print (Graphics g) 
  {
    throw new RuntimeException ();
  }

  public void repaint (long tm, int x, int y, int width, int height)
  {
    if (x == 0 && y == 0 && width == 0 && height == 0)
      return;

    q().postEvent (new PaintEvent (awtComponent, PaintEvent.UPDATE,
                                 new Rectangle (x, y, width, height)));
  }

  public void requestFocus ()
  {
    gtkWidgetRequestFocus();
    postFocusEvent(FocusEvent.FOCUS_GAINED, false);
  }

  public void reshape (int x, int y, int width, int height) 
  {
    setBounds (x, y, width, height);
  }

  public void setBackground (Color c) 
  {
    gtkWidgetSetBackground (c.getRed(), c.getGreen(), c.getBlue());
  }

  native void setNativeBounds (int x, int y, int width, int height);

  public void setBounds (int x, int y, int width, int height)
  {
    Component parent = awtComponent.getParent ();

    // Heavyweight components that are children of one or more
    // lightweight containers have to be handled specially.  Because
    // calls to GLightweightPeer.setBounds do nothing, GTK has no
    // knowledge of the lightweight containers' positions.  So we have
    // to add the offsets manually when placing a heavyweight
    // component within a lightweight container.  The lightweight
    // container may itself be in a lightweight container and so on,
    // so we need to continue adding offsets until we reach a
    // container whose position GTK knows -- that is, the first
    // non-lightweight.
    boolean lightweightChild = false;
    Insets i;
    while (parent.isLightweight ())
      {
	lightweightChild = true;

	i = ((Container) parent).getInsets ();

	x += parent.getX () + i.left;
	y += parent.getY () + i.top;

	parent = parent.getParent ();
      }

    // We only need to convert from Java to GTK coordinates if we're
    // placing a heavyweight component in a Window.
    if (parent instanceof Window && !lightweightChild)
      {
	Insets insets = ((Window) parent).getInsets ();
        GtkWindowPeer peer = (GtkWindowPeer) parent.getPeer ();
        int menuBarHeight = 0;
        if (peer instanceof GtkFramePeer)
          menuBarHeight = ((GtkFramePeer) peer).getMenuBarHeight ();

        // Convert from Java coordinates to GTK coordinates.
        setNativeBounds (x - insets.left, y - insets.top + menuBarHeight,
                         width, height);
      }
    else
      setNativeBounds (x, y, width, height);
  }

  void setCursor ()
  {
    setCursor (awtComponent.getCursor ());
  }

  public void setCursor (Cursor cursor) 
  {
    gtkWidgetSetCursor (cursor.getType ());
  }

  public void setEnabled (boolean b)
  {
    gtkWidgetSetSensitive (b);
  }

  public void setFont (Font f)
  {
    // FIXME: This should really affect the widget tree below me.
    // Currently this is only handled if the call is made directly on
    // a text widget, which implements setFont() itself.
    gtkWidgetModifyFont(f.getName(), f.getStyle(), f.getSize());
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
    if (b)
      show ();
    else
      hide ();
  }

  public native void hide ();
  public native void show ();

  protected void postMouseEvent(int id, long when, int mods, int x, int y, 
				int clickCount, boolean popupTrigger) 
  {
    q().postEvent(new MouseEvent(awtComponent, id, when, mods, x, y, 
			       clickCount, popupTrigger));
  }

  protected void postExposeEvent (int x, int y, int width, int height)
  {
    if (!isInRepaint)
      q().postEvent (new PaintEvent (awtComponent, PaintEvent.PAINT,
                                   new Rectangle (x, y, width, height)));
  }

  protected void postKeyEvent (int id, long when, int mods,
                               int keyCode, char keyChar, int keyLocation)
  {
    KeyEvent keyEvent = new KeyEvent (awtComponent, id, when, mods,
                                      keyCode, keyChar, keyLocation);

    // Also post a KEY_TYPED event if keyEvent is a key press that
    // doesn't represent an action or modifier key.
    if (keyEvent.getID () == KeyEvent.KEY_PRESSED
        && (!keyEvent.isActionKey ()
            && keyCode != KeyEvent.VK_SHIFT
            && keyCode != KeyEvent.VK_CONTROL
            && keyCode != KeyEvent.VK_ALT))
      {
        synchronized (q)
          {
            q().postEvent (keyEvent);
            q().postEvent (new KeyEvent (awtComponent, KeyEvent.KEY_TYPED, when, mods,
                                        KeyEvent.VK_UNDEFINED, keyChar, keyLocation));
          }
      }
    else
      q().postEvent (keyEvent);
  }

  protected void postFocusEvent (int id, boolean temporary)
  {
    q().postEvent (new FocusEvent (awtComponent, id, temporary));
  }

  protected void postItemEvent (Object item, int stateChange)
  {
    q().postEvent (new ItemEvent ((ItemSelectable)awtComponent, 
				ItemEvent.ITEM_STATE_CHANGED,
				item, stateChange));
  }

  protected void postTextEvent ()
  {
    q().postEvent (new TextEvent (awtComponent, TextEvent.TEXT_VALUE_CHANGED));
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

  public boolean handlesWheelScrolling ()
  {
    return false;
  }

  // Convenience method to create a new volatile image on the screen
  // on which this component is displayed.
  public VolatileImage createVolatileImage (int width, int height)
  {
    return new GtkVolatileImage (width, height);
  }

  // Creates buffers used in a buffering strategy.
  public void createBuffers (int numBuffers, BufferCapabilities caps)
    throws AWTException
  {
    // numBuffers == 2 implies double-buffering, meaning one back
    // buffer and one front buffer.
    if (numBuffers == 2)
      backBuffer = new GtkVolatileImage(awtComponent.getWidth(),
					awtComponent.getHeight(),
					caps.getBackBufferCapabilities());
    else
      throw new AWTException("GtkComponentPeer.createBuffers:"
			     + " multi-buffering not supported");
    this.caps = caps;
  }

  // Return the back buffer.
  public Image getBackBuffer ()
  {
    return backBuffer;
  }

  // FIXME: flip should be implemented as a fast native operation
  public void flip (BufferCapabilities.FlipContents contents)
  {
    getGraphics().drawImage(backBuffer,
			    awtComponent.getWidth(),
			    awtComponent.getHeight(),
			    null);

    // create new back buffer and clear it to the background color.
    if (contents == BufferCapabilities.FlipContents.BACKGROUND)
	{
	  backBuffer = createVolatileImage(awtComponent.getWidth(),
					   awtComponent.getHeight());
	  backBuffer.getGraphics().clearRect(0, 0,
					     awtComponent.getWidth(),
					     awtComponent.getHeight());
	}
    // FIXME: support BufferCapabilities.FlipContents.PRIOR
  }

  // Release the resources allocated to back buffers.
  public void destroyBuffers ()
  {
    backBuffer.flush();
  }
}
