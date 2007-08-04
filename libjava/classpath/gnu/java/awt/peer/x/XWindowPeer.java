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
import java.awt.EventQueue;
import java.awt.Font;
import java.awt.FontMetrics;
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

import gnu.x11.Window;
import gnu.x11.event.Event;

import gnu.java.awt.peer.swing.SwingWindowPeer;

public class XWindowPeer
    extends SwingWindowPeer
{

  private static int standardSelect = Event.BUTTON_PRESS_MASK
                                      | Event.BUTTON_RELEASE_MASK
                                      | Event.POINTER_MOTION_MASK
                                      //| Event.RESIZE_REDIRECT_MASK
                                      | Event.EXPOSURE_MASK
                                      //| Event.PROPERTY_CHANGE_MASK
                                      | Event.STRUCTURE_NOTIFY_MASK
                                      | Event.KEY_PRESS_MASK
                                      | Event.KEY_RELEASE_MASK
                                      ;

  /**
   * Indicates if we are in callback mode, that is when a property (like size)
   * is changed in reponse to a request from the X server and doesn't need
   * to be propagated back to the X server.
   */
  boolean callback = false;

  /**
   * The X window.
   */
  private Window xwindow;

  XWindowPeer(java.awt.Window window)
  {
    super(window);
    XGraphicsDevice dev = XToolkit.getDefaultDevice();

    // TODO: Maybe initialize lazily in show().
    // FIXME: Howto generate a Window without decorations?
    int x = Math.max(window.getX(), 0);
    int y = Math.max(window.getY(), 0);
    int w = Math.max(window.getWidth(), 1);
    int h = Math.max(window.getHeight(), 1);
    xwindow = new Window(dev.getDisplay().default_root, x, y, w, h);
    xwindow.create();
    xwindow.select_input(standardSelect);
    dev.getEventPump().registerWindow(xwindow, window);
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
    return new XGraphics2D(xwindow);
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
//    // Prevent ResizeRedirect events.
//    //xwindow.select_input(noResizeRedirectSelect);
//    Window.Attributes atts = new Window.Attributes();
//    atts.set_override_redirect(true);
//    xwindow.change_attributes(atts);

    // Prevent ResizeRedirect events.
    //xwindow.select_input(Event.NO_EVENT_MASK);
    //xwindow.select_input(noResizeRedirectSelect);

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
    // Prevent ResizeRedirect events.
//    //xwindow.select_input(noResizeRedirectSelect);
//    Window.Attributes atts = new Window.Attributes();
//    atts.set_override_redirect(true);
//    xwindow.change_attributes(atts);

    // Need to substract insets because AWT size is including insets,
    // and X size is excuding insets.
    Insets i = insets();
    xwindow.move_resize(x - i.left, y - i.right, width - i.left - i.right,
                        height - i.top - i.bottom);

    // Reset input selection.
//    atts = new Window.Attributes();
//    atts.set_override_redirect(false);
//    xwindow.change_attributes(atts);
  }

  public Insets insets()
  {
    Insets i = new Insets(0, 0, 0, 0);
//    Window.GeometryReply g = xwindow.geometry();
//    int b = g.border_width();
//    Insets i = new Insets(b, b, b, b);
//    Window.WMSizeHints wmSize = xwindow.wm_normal_hints();
//    if (wmSize != null)
//      {
//        i.left = wmSize.x() - g.x();
//        i.right = wmSize.width() - g.width() - i.left ;
//        i.top = wmSize.y() - g.y();
//        i.bottom = wmSize.height() - g.height() - i.top;
//      }
//    System.err.println("insets: " + i);
    return i;
  }

  /**
   * Returns the font metrics for the specified font.
   *
   * @return the font metrics for the specified font
   */
  public FontMetrics getFontMetrics(Font font)
  {
    XFontPeer2 fontPeer = (XFontPeer2) font.getPeer();
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
}
