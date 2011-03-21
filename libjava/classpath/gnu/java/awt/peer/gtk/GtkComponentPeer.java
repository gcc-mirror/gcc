/* GtkComponentPeer.java -- Implements ComponentPeer with GTK
   Copyright (C) 1998, 1999, 2002, 2004, 2005, 2006
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

import java.awt.AWTEvent;
import java.awt.AWTException;
import java.awt.BufferCapabilities;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.EventQueue;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.GraphicsConfiguration;
import java.awt.GraphicsDevice;
import java.awt.GraphicsEnvironment;
import java.awt.Image;
import java.awt.Insets;
import java.awt.ItemSelectable;
import java.awt.KeyboardFocusManager;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.Window;
import java.awt.event.FocusEvent;
import java.awt.event.ItemEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseWheelEvent;
import java.awt.event.PaintEvent;
import java.awt.event.TextEvent;
import java.awt.image.ColorModel;
import java.awt.image.ImageObserver;
import java.awt.image.ImageProducer;
import java.awt.image.VolatileImage;
import java.awt.peer.ComponentPeer;
import java.awt.peer.ContainerPeer;
import java.awt.peer.LightweightPeer;
import java.util.Timer;
import java.util.TimerTask;

public class GtkComponentPeer extends GtkGenericPeer
  implements ComponentPeer
{
  VolatileImage backBuffer;
  BufferCapabilities caps;

  Component awtComponent;

  Insets insets;

  /**
   * The current repaint area. Use should be guarded by synchronizing on this.
   */
  private Rectangle currentPaintArea;

  /* this isEnabled differs from Component.isEnabled, in that it
     knows if a parent is disabled.  In that case Component.isEnabled
     may return true, but our isEnabled will always return false */
  native boolean isEnabled ();
  static native boolean modalHasGrab();

  native int[] gtkWidgetGetForeground ();
  native int[] gtkWidgetGetBackground ();
  native void gtkWidgetGetDimensions (int[] dim);
  native void gtkWidgetGetPreferredDimensions (int[] dim);
  native void gtkWindowGetLocationOnScreen (int[] point);
  native void gtkWindowGetLocationOnScreenUnlocked (int[] point);
  native void gtkWidgetGetLocationOnScreen (int[] point);
  native void gtkWidgetGetLocationOnScreenUnlocked (int[] point);
  native void gtkWidgetSetCursor (int type, GtkImage image, int x, int y);
  native void gtkWidgetSetCursorUnlocked (int type, GtkImage image,
                                          int x, int y);
  native void gtkWidgetSetBackground (int red, int green, int blue);
  native void gtkWidgetSetForeground (int red, int green, int blue);
  native void gtkWidgetSetSensitive (boolean sensitive);
  native void gtkWidgetSetParent (ComponentPeer parent);
  native void gtkWidgetRequestFocus ();
  native void gtkWidgetDispatchKeyEvent (int id, long when, int mods,
                                         int keyCode, int keyLocation);
  native boolean gtkWidgetHasFocus();
  native boolean gtkWidgetCanFocus();

  native void realize();
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

    setParentAndBounds ();

    setNativeEventMask ();

    // This peer is guaranteed to have an X window upon construction.
    // That is, native methods such as those in GdkGraphics can rely
    // on this component's widget->window field being non-null.
    realize ();

    if (awtComponent.isCursorSet())
      setCursor ();
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
    return CairoSurface.getBufferedImage(width, height);
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

  // getGraphics may be overridden by derived classes but it should
  // never return null.
  public Graphics getGraphics ()
  {
    return ComponentGraphics.getComponentGraphics(this);
  }

  public Point getLocationOnScreen ()
  {
    int point[] = new int[2];
    if (Thread.currentThread() == GtkMainThread.mainThread)
        gtkWidgetGetLocationOnScreenUnlocked (point);
    else
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
        paintComponent((PaintEvent) event);
        break;
      case PaintEvent.UPDATE:
        updateComponent((PaintEvent) event);
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

  // This method and its overrides are the only methods in the peers
  // that should call awtComponent.paint.
  protected void paintComponent (PaintEvent event)
  {
    // Do not call Component.paint if the component is not showing or
    // if its bounds form a degenerate rectangle.
    if (!awtComponent.isShowing()
        || (awtComponent.getWidth() < 1 || awtComponent.getHeight() < 1))
      return;

    // Creating and disposing a GdkGraphics every time paint is called
    // seems expensive.  However, the graphics state does not carry
    // over between calls to paint, and resetting the graphics object
    // may even be more costly than simply creating a new one.

    // Make sure that the paintArea includes the area from the event
    // in the case when an application sends PaintEvents directly.
    coalescePaintEvent(event);
    Rectangle paintArea;
    synchronized (this)
      {
        paintArea = currentPaintArea;
        currentPaintArea = null;
      }

    if (paintArea != null)
      {
        Graphics g = getGraphics();
        try
          {
            g.setClip(paintArea);
            awtComponent.paint(g);
          }
        finally
          {
            g.dispose();
          }
      }
  }

  // This method and its overrides are the only methods in the peers
  // that should call awtComponent.update.
  protected void updateComponent (PaintEvent event)
  {
    // Do not call Component.update if the component is not showing or
    // if its bounds form a degenerate rectangle.
    if (!awtComponent.isShowing()
        || (awtComponent.getWidth() < 1 || awtComponent.getHeight() < 1))
      return;

    // Make sure that the paintArea includes the area from the event
    // in the case when an application sends PaintEvents directly.
    coalescePaintEvent(event);
    Rectangle paintArea;
    synchronized (this)
      {
        paintArea = currentPaintArea;
        currentPaintArea = null;
      }

    if (paintArea != null)
    {
      Graphics g = getGraphics();
      try
        {
          g.setClip(paintArea);
          awtComponent.update(g);
        }
      finally
        {
          g.dispose();
        }
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
    g.drawImage( ComponentGraphics.grab( this ), 0, 0, null );
  }

  public void repaint (long tm, int x, int y, int width, int height)
  {
    if (width < 1 || height < 1)
      return;

    if (tm <= 0)
      q().postEvent(new PaintEvent(awtComponent, PaintEvent.UPDATE,
                                   new Rectangle(x, y, width, height)));
    else
      RepaintTimerTask.schedule(tm, x, y, width, height, awtComponent);
  }

  /**
   * Used for scheduling delayed paint updates on the event queue.
   */
  private static class RepaintTimerTask extends TimerTask
  {
    private static final Timer repaintTimer = new Timer(true);

    private int x, y, width, height;
    private Component awtComponent;

    RepaintTimerTask(Component c, int x, int y, int width, int height)
    {
      this.x = x;
      this.y = y;
      this.width = width;
      this.height = height;
      this.awtComponent = c;
    }

    public void run()
    {
      q().postEvent (new PaintEvent (awtComponent, PaintEvent.UPDATE,
                                     new Rectangle (x, y, width, height)));
    }

    static void schedule(long tm, int x, int y, int width, int height,
                         Component c)
    {
      repaintTimer.schedule(new RepaintTimerTask(c, x, y, width, height), tm);
    }
  }

  public void requestFocus ()
  {
    assert false: "Call new requestFocus() method instead";
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
    int new_x = x;
    int new_y = y;

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
    Insets i;
    while (parent.isLightweight())
      {
        i = ((Container) parent).getInsets();

        new_x += parent.getX() + i.left;
        new_y += parent.getY() + i.top;

        parent = parent.getParent();
      }
    // We only need to convert from Java to GTK coordinates if we're
    // placing a heavyweight component in a Window.
    if (parent instanceof Window)
      {
        GtkWindowPeer peer = (GtkWindowPeer) parent.getPeer ();
        // important: we want the window peer's insets here, not the
        // window's, since user sub-classes of Window can override
        // getInset and we only want to correct for the frame borders,
        // not for any user-defined inset values
        Insets insets = peer.getInsets ();

        int menuBarHeight = 0;
        if (peer instanceof GtkFramePeer)
          menuBarHeight = ((GtkFramePeer) peer).getMenuBarHeight ();

        new_x -= insets.left;
        new_y -= insets.top;
        new_y += menuBarHeight;
      }

    setNativeBounds (new_x, new_y, width, height);

    // If the height or width were (or are now) smaller than zero
    // then we want to adjust the visibility.
    setVisible(awtComponent.isVisible());
  }

  void setCursor ()
  {
    setCursor (awtComponent.getCursor ());
  }

  public void setCursor (Cursor cursor)
  {
    int x, y;
    GtkImage image;
    int type = cursor.getType();
    if (cursor instanceof GtkCursor)
      {
        GtkCursor gtkCursor = (GtkCursor) cursor;
        image = gtkCursor.getGtkImage();
        Point hotspot = gtkCursor.getHotspot();
        x = hotspot.x;
        y = hotspot.y;
      }
    else
      {
        image = null;
        x = 0;
        y = 0;
      }

    if (Thread.currentThread() == GtkMainThread.mainThread)
      gtkWidgetSetCursorUnlocked(cursor.getType(), image, x, y);
    else
      gtkWidgetSetCursor(cursor.getType(), image, x, y);
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

  public native void setVisibleNative (boolean b);
  public native void setVisibleNativeUnlocked (boolean b);

  public void setVisible (boolean b)
  {
    // Only really set visible when component is bigger than zero pixels.
    if (b && ! (awtComponent instanceof Window))
      {
        Rectangle bounds = awtComponent.getBounds();
        b = (bounds.width > 0) && (bounds.height > 0);
      }

    if (Thread.currentThread() == GtkMainThread.mainThread)
      setVisibleNativeUnlocked (b);
    else
      setVisibleNative (b);
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
    // It is important to do the getLocationOnScreen() here, instead
    // of using the old MouseEvent constructors, because
    // Component.getLocationOnScreen() locks on the AWT lock, which can
    // trigger a deadlock. You don't want this.
    Point locOnScreen = getLocationOnScreen();
    q().postEvent(new MouseEvent(awtComponent, id, when, mods, x, y,
                                 locOnScreen.x + x, locOnScreen.y + y,
                                 clickCount, popupTrigger,
                                 MouseEvent.NOBUTTON));
  }

  /**
   * Callback for component_scroll_cb.
   */
  protected void postMouseWheelEvent(int id, long when, int mods,
                                     int x, int y, int clickCount,
                                     boolean popupTrigger,
                                     int type, int amount, int rotation)
  {
    q().postEvent(new MouseWheelEvent(awtComponent, id, when, mods,
                                      x, y, clickCount, popupTrigger,
                                      type, amount, rotation));
  }

  protected void postExposeEvent (int x, int y, int width, int height)
  {
    q().postEvent (new PaintEvent (awtComponent, PaintEvent.PAINT,
                                   new Rectangle (x, y, width, height)));
  }

  protected void postKeyEvent (int id, long when, int mods,
                               int keyCode, char keyChar, int keyLocation)
  {
    KeyEvent keyEvent = new KeyEvent (awtComponent, id, when, mods,
                                      keyCode, keyChar, keyLocation);

    EventQueue q = q();

    // Also post a KEY_TYPED event if keyEvent is a key press that
    // doesn't represent an action or modifier key.
    if (keyEvent.getID () == KeyEvent.KEY_PRESSED
        && (!keyEvent.isActionKey ()
            && keyCode != KeyEvent.VK_SHIFT
            && keyCode != KeyEvent.VK_CONTROL
            && keyCode != KeyEvent.VK_ALT))
      {
        synchronized(q)
          {
            q.postEvent(keyEvent);
            keyEvent = new KeyEvent(awtComponent, KeyEvent.KEY_TYPED, when,
                                    mods, KeyEvent.VK_UNDEFINED, keyChar,
                                    keyLocation);
            q.postEvent(keyEvent);
          }
      }
    else
      q.postEvent(keyEvent);
  }

  /**
   * Referenced from native code.
   *
   * @param id
   * @param temporary
   */
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
    // FIXME: The component might be showing on a non-default screen.
    GraphicsEnvironment env = GraphicsEnvironment.getLocalGraphicsEnvironment();
    GraphicsDevice dev = env.getDefaultScreenDevice();
    return dev.getDefaultConfiguration();
  }

  public void setEventMask (long mask)
  {
    // FIXME: just a stub for now.
  }

  public boolean isFocusable ()
  {
    return false;
  }

  public boolean requestFocus (Component request, boolean temporary,
                               boolean allowWindowFocus, long time)
  {
    assert request == awtComponent || isLightweightDescendant(request);
    boolean retval = false;
    if (gtkWidgetHasFocus())
      {
        KeyboardFocusManager kfm =
          KeyboardFocusManager.getCurrentKeyboardFocusManager();
        Component currentFocus = kfm.getFocusOwner();
        if (currentFocus == request)
          // Nothing to do in this trivial case.
          retval = true;
        else
          {
            // Requested component is a lightweight descendant of this one
            // or the actual heavyweight.
            // Since this (native) component is already focused, we simply
            // change the actual focus and be done.
            postFocusEvent(FocusEvent.FOCUS_GAINED, temporary);
            retval = true;
          }
      }
    else
      {
        if (gtkWidgetCanFocus())
          {
            if (allowWindowFocus)
              {
                Window window = getWindowFor(request);
                GtkWindowPeer wPeer = (GtkWindowPeer) window.getPeer();
                if (! wPeer.gtkWindowHasFocus())
                  wPeer.requestWindowFocus();
              }
            // Store requested focus component so that the corresponding
            // event is dispatched correctly.
            gtkWidgetRequestFocus();
            retval = true;
          }
      }
    return retval;
  }

  private Window getWindowFor(Component c)
  {
    Component comp = c;
    while (! (comp instanceof Window))
      comp = comp.getParent();
    return (Window) comp;
  }

  /**
   * Returns <code>true</code> if the component is a direct (== no intermediate
   * heavyweights) lightweight descendant of this peer's component.
   *
   * @param c the component to check
   *
   * @return <code>true</code> if the component is a direct (== no intermediate
   *         heavyweights) lightweight descendant of this peer's component
   */
  protected boolean isLightweightDescendant(Component c)
  {
    Component comp = c;
    while (comp.getPeer() instanceof LightweightPeer)
      comp = comp.getParent();
    return comp == awtComponent;
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
    synchronized (this)
    {
      Rectangle newRect = e.getUpdateRect();
      if (currentPaintArea == null)
        currentPaintArea = newRect;
      else
        Rectangle.union(currentPaintArea, newRect, currentPaintArea);
    }
  }

  public void updateCursorImmediately ()
  {
    if (awtComponent.getCursor() != null)
      setCursor(awtComponent.getCursor());
  }

  public boolean handlesWheelScrolling ()
  {
    return false;
  }

  // Convenience method to create a new volatile image on the screen
  // on which this component is displayed.
  public VolatileImage createVolatileImage (int width, int height)
  {
    return new GtkVolatileImage (this, width, height, null);
  }

  // Creates buffers used in a buffering strategy.
  public void createBuffers (int numBuffers, BufferCapabilities caps)
    throws AWTException
  {
    // numBuffers == 2 implies double-buffering, meaning one back
    // buffer and one front buffer.
    if (numBuffers == 2)
      backBuffer = new GtkVolatileImage(this, awtComponent.getWidth(),
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

  public String toString ()
  {
    return "peer of " + awtComponent.toString();
  }
  public Rectangle getBounds()
  {
      // FIXME: implement
    return null;
  }
  public void reparent(ContainerPeer parent)
  {
    // FIXME: implement

  }
  public void setBounds(int x, int y, int width, int height, int z)
  {
    // FIXME: implement
      setBounds (x, y, width, height);

  }
  public boolean isReparentSupported()
  {
    // FIXME: implement

    return false;
  }
  public void layout()
  {
    // FIXME: implement

  }

  public boolean requestFocus(Component lightweightChild, boolean temporary,
                              boolean focusedWindowChangeAllowed,
                              long time, sun.awt.CausedFocusEvent.Cause cause)
  {
    // TODO: Implement this properly and remove the other requestFocus()
    // methods.
    return true;
  }

}
