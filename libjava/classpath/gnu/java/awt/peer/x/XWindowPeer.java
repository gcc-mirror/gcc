/* XWindowPeer.java -- Window peer for X
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package gnu.java.awt.peer.x;

import java.awt.Component;
import java.awt.Dialog;
import java.awt.EventQueue;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Frame;
import java.awt.Graphics;
import java.awt.GraphicsConfiguration;
import java.awt.GraphicsDevice;
import java.awt.GraphicsEnvironment;
import java.awt.Image;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.PaintEvent;
import java.awt.event.WindowEvent;
import java.awt.image.VolatileImage;

import gnu.x11.Atom;
import gnu.x11.Window;
import gnu.x11.event.Event;

import gnu.java.awt.font.OpenTypeFontPeer;
import gnu.java.awt.peer.ClasspathFontPeer;
import gnu.java.awt.peer.swing.SwingWindowPeer;

public class XWindowPeer
    extends SwingWindowPeer
{

  private static int standardSelect = Event.BUTTON_PRESS_MASK
                                      | Event.BUTTON_RELEASE_MASK
                                      | Event.POINTER_MOTION_MASK
                                     // | Event.RESIZE_REDIRECT_MASK //
                                      | Event.EXPOSURE_MASK
                                      | Event.PROPERTY_CHANGE_MASK
                                      //| Event.STRUCTURE_NOTIFY_MASK
                                      //| Event.SUBSTRUCTURE_NOTIFY_MASK
                                      | Event.KEY_PRESS_MASK
                                      | Event.KEY_RELEASE_MASK
                                      //| Event.VISIBILITY_CHANGE_MASK //
                                      ;
  
  /**
   * The X window.
   */
  protected Window xwindow;

  /**
   * The frame insets. These get updated in {@link #show()}.
   */
  private Insets insets;

  XWindowPeer(java.awt.Window window)
  {
    super(window);
    XGraphicsDevice dev = XToolkit.getDefaultDevice();

    // TODO: Maybe initialize lazily in show().
    Window.Attributes atts = new Window.Attributes();
    // FIXME: Howto generate a Window without decorations?
    int x = Math.max(window.getX(), 0);
    int y = Math.max(window.getY(), 0);
    int w = Math.max(window.getWidth(), 1);
    int h = Math.max(window.getHeight(), 1);
    xwindow = new Window(dev.getDisplay().default_root, x, y, w, h, 0, atts);
    xwindow.select_input(standardSelect);
    
    dev.getEventPump().registerWindow(xwindow, window);
    xwindow.set_wm_delete_window();
    
    boolean undecorated;
    if (awtComponent instanceof Frame)
      {
        Frame f = (Frame) awtComponent;
        undecorated = f.isUndecorated();
      }
    else if (awtComponent instanceof Dialog)
      {
        Dialog d = (Dialog) awtComponent;
        undecorated = d.isUndecorated();
      }
    else
      {
        undecorated = true;
      }
    if (undecorated)
      {
        // First try the Motif implementation of undecorated frames. This
        // is semantically closest and supported by all major window
        // managers.
        // TODO: At the time of writing this, there's no freedesktop.org
        // standard extension that matches the required semantic. Maybe
        // undecorated frames are added in the future, if so, then use these.
        Atom at = Atom.intern(dev.getDisplay(), "_MOTIF_WM_HINTS");
        if (at != null)
          {
            xwindow.change_property(Window.REPLACE, at, at, 32,
                                    new int[]{1 << 1, 0, 0, 0, 0}, 0, 5);
          }
      }
    insets = new Insets(0, 0, 0, 0);
  }

  public void toBack()
  {
    // TODO Auto-generated method stub

  }

  public void toFront()
  {
    // TODO Auto-generated method stub

  }

  public void updateAlwaysOnTop()
  {
    // TODO Auto-generated method stub

  }

  public boolean requestWindowFocus()
  {
    // TODO Auto-generated method stub
    return false;
  }

  public Point getLocationOnScreen()
  {
    return new Point(xwindow.x, xwindow.y);
  }

  /**
   * Returns a XGraphics suitable for drawing on this frame.
   *
   * @return a XGraphics suitable for drawing on this frame
   */
  public Graphics getGraphics()
  {
	XGraphics2D xg2d = new XGraphics2D(xwindow);
	xg2d.setColor(awtComponent.getForeground());
	xg2d.setBackground(awtComponent.getBackground());
	xg2d.setFont(awtComponent.getFont());
	return xg2d;
  }

  public Image createImage(int w, int h)
  {
    // FIXME: Should return a buffered image.
    return createVolatileImage(w, h);
  }

  @Override
  public VolatileImage createVolatileImage(int width, int height)
  {
    GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
    GraphicsDevice gd = ge.getDefaultScreenDevice();
    GraphicsConfiguration gc = gd.getDefaultConfiguration();
    return gc.createCompatibleVolatileImage(width, height);
  }

  /**
   * Makes the component visible. This is called by {@link Component#show()}.
   *
   * This is implemented to call setVisible(true) on the Swing component.
   */
  public void show()
  {
    // Prevent ResizeRedirect events.
    //xwindow.select_input(Event.NO_EVENT_MASK);
    //xwindow.select_input(noResizeRedirectSelect);

    XGraphicsDevice dev = XToolkit.getDefaultDevice();
    xwindow.map();
    EventQueue eq = XToolkit.getDefaultToolkit().getSystemEventQueue();
    java.awt.Window w = (java.awt.Window) super.awtComponent;
    eq.postEvent(new WindowEvent(w, WindowEvent.WINDOW_OPENED));
    eq.postEvent(new PaintEvent(w, PaintEvent.PAINT,
                                new Rectangle(0, 0, w.getWidth(),
                                              w.getHeight())));

    Graphics g = getGraphics();
    g.clearRect(0, 0, awtComponent.getWidth(), awtComponent.getHeight());
    g.dispose();
//    // Reset input selection.
//    atts.set_override_redirect(false);
//    xwindow.change_attributes(atts);
    
    // Determine the frame insets.
    Atom atom = (Atom) Atom.intern(dev.getDisplay(), "_NET_FRAME_EXTENTS");
    Window.Property p = xwindow.get_property(false, atom, Atom.CARDINAL, 0,
                                             Window.MAX_WM_LENGTH);
    if (p.format() != 0)
      {
        insets = new Insets(p.value(0), p.value(1), p.value(2), p.value(3));
        Window.Changes ch = new Window.Changes();
        ch.width(awtComponent.getWidth() - insets.left - insets.top);
        ch.height(awtComponent.getHeight() - insets.top - insets.bottom);
        xwindow.configure(ch);
      }

  }

  /**
   * Makes the component invisible. This is called from
   * {@link Component#hide()}.
   *
   * This is implemented to call setVisible(false) on the Swing component.
   */
  public void hide()
  {
    xwindow.unmap();
  }

  /**
   * Notifies the peer that the bounds of this component have changed. This
   * is called by {@link Component#reshape(int, int, int, int)}.
   *
   * This is implemented to call setBounds() on the Swing component.
   *
   * @param x the X coordinate of the upper left corner of the component
   * @param y the Y coordinate of the upper left corner of the component
   * @param width the width of the component
   * @param height the height of the component
   */
  public void reshape(int x, int y, int width, int height)
  {
    Insets i = insets;
    xwindow.move_resize(x - i.left, y - i.right, width - i.left - i.right,
                        height - i.top - i.bottom);
  }

  public Insets insets()
  {
    return (Insets) insets.clone();
  }

  /**
   * Returns the font metrics for the specified font.
   *
   * @return the font metrics for the specified font
   */
  public FontMetrics getFontMetrics(Font font)
  {
    ClasspathFontPeer fontPeer = (ClasspathFontPeer) font.getPeer();
    return fontPeer.getFontMetrics(font);
  }

  /**
   * Unregisters the window in the event pump when it is closed.
   */
  protected void finalize()
  {
    XGraphicsDevice dev = XToolkit.getDefaultDevice();
    dev.getEventPump().unregisterWindow(xwindow);
  }
  
  public Window getXwindow()
  {
    return xwindow;
  }
}
