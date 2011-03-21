/* GtkFramePeer.java -- Implements FramePeer with GTK
   Copyright (C) 1999, 2002, 2004, 2006, 2007 Free Software Foundation, Inc.

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

import java.awt.Frame;
import java.awt.Image;
import java.awt.MenuBar;
import java.awt.Rectangle;
import java.awt.peer.FramePeer;
import java.awt.peer.MenuBarPeer;

public class GtkFramePeer extends GtkWindowPeer
    implements FramePeer
{
  private int menuBarHeight;
  private MenuBarPeer menuBar;
  native int getMenuBarHeight (MenuBarPeer bar);
  native void setMenuBarWidthUnlocked (MenuBarPeer bar, int width);
  native void setMenuBarWidth (MenuBarPeer bar, int width);
  native void setMenuBarPeer (MenuBarPeer bar);
  native void removeMenuBarPeer ();
  native void gtkFixedSetVisible (boolean visible);

  private native void maximize();
  private native void unmaximize();
  private native void iconify();
  private native void deiconify();

  int getMenuBarHeight ()
  {
    return menuBar == null ? 0 : getMenuBarHeight (menuBar);
  }

  public void setMenuBar (MenuBar bar)
  {
    if (bar == null && menuBar != null)
      {
        // We're removing the menubar.
        gtkFixedSetVisible (false);
        menuBar = null;
        removeMenuBarPeer ();
        insets.top -= menuBarHeight;
        menuBarHeight = 0;
        // if component has already been validated, we need to revalidate.
        // otherwise, it will be validated when it is shown.
        if (awtComponent.isValid())
          awtComponent.validate ();
        gtkFixedSetVisible (true);
      }
    else if (bar != null && menuBar == null)
      {
        // We're adding a menubar where there was no menubar before.
        gtkFixedSetVisible (false);
        menuBar = (MenuBarPeer) bar.getPeer();
        setMenuBarPeer (menuBar);
        int menuBarWidth =
          awtComponent.getWidth () - insets.left - insets.right;
        if (menuBarWidth > 0)
          setMenuBarWidth (menuBar, menuBarWidth);
        menuBarHeight = getMenuBarHeight ();
        insets.top += menuBarHeight;
        // if component has already been validated, we need to revalidate.
        // otherwise, it will be validated when it is shown.
        if (awtComponent.isValid())
          awtComponent.validate ();
        gtkFixedSetVisible (true);
      }
    else if (bar != null && menuBar != null)
      {
        // We're swapping the menubar.
        gtkFixedSetVisible (false);
        removeMenuBarPeer();
        int oldHeight = menuBarHeight;
        int menuBarWidth =
          awtComponent.getWidth () - insets.left - insets.right;
        menuBar = (MenuBarPeer) bar.getPeer ();
        setMenuBarPeer (menuBar);
        if (menuBarWidth > 0)
          setMenuBarWidth (menuBar, menuBarWidth);
        menuBarHeight = getMenuBarHeight ();
        if (oldHeight != menuBarHeight)
          {
            insets.top += (menuBarHeight - oldHeight);
            awtComponent.validate ();
          }
        gtkFixedSetVisible (true);
      }
  }

  public void setBounds (int x, int y, int width, int height)
  {
    int menuBarWidth = width - insets.left - insets.right;
    if (menuBar != null && menuBarWidth > 0)
      setMenuBarWidth (menuBar, menuBarWidth);

    super.setBounds(x, y, width, height + menuBarHeight);
  }

  public void setResizable (boolean resizable)
  {
    // Call setSize; otherwise when resizable is changed from true to
    // false the frame will shrink to the dimensions it had before it
    // was resizable.
    setSize (awtComponent.getWidth() - insets.left - insets.right,
             awtComponent.getHeight() - insets.top - insets.bottom
             + menuBarHeight);
    gtkWindowSetResizable (resizable);
  }

  protected void postInsetsChangedEvent (int top, int left,
                                         int bottom, int right)
  {
    insets.top = top + menuBarHeight;
    insets.left = left;
    insets.bottom = bottom;
    insets.right = right;
  }

  public GtkFramePeer (Frame frame)
  {
    super (frame);
  }

  void create ()
  {
    // Create a normal decorated window.
    create (GDK_WINDOW_TYPE_HINT_NORMAL,
            !((Frame) awtComponent).isUndecorated ());

    Frame frame = (Frame) awtComponent;

    setMenuBar (frame.getMenuBar ());

    setTitle (frame.getTitle ());
    gtkWindowSetResizable (frame.isResizable ());
    setIconImage(frame.getIconImage());
  }

  native void nativeSetIconImage (GtkImage image);

  public void setIconImage (Image image)
  {
    if (image != null)
      {
        GtkImage gtkImage;
        if (image instanceof GtkImage)
          gtkImage = (GtkImage) image;
        else
          gtkImage = new GtkImage(image.getSource());

        if (gtkImage.isLoaded && ! gtkImage.errorLoading)
          nativeSetIconImage(gtkImage);
      }
  }

  protected void postConfigureEvent (int x, int y, int width, int height)
  {
    if (menuBar != null && width > 0)
      setMenuBarWidthUnlocked (menuBar, width);

    // Since insets.top already includes the MenuBar's height, we need
    // to subtract the MenuBar's height from the top inset.
    int frame_height = height - menuBarHeight;

    // Likewise, since insets.top includes the MenuBar height, we need
    // to add back the MenuBar height to the frame's y position.  If
    // no MenuBar exists in this frame, the MenuBar height will be 0.
    int frame_y = y + menuBarHeight;

    super.postConfigureEvent(x, frame_y, width, frame_height);
  }

  public int getState ()
  {
    return windowState;
  }

  public void setState (int state)
  {
    switch (state)
      {
        case Frame.NORMAL:
          if ((windowState & Frame.ICONIFIED) != 0)
            deiconify();
          if ((windowState & Frame.MAXIMIZED_BOTH) != 0)
            unmaximize();
          break;
        case Frame.ICONIFIED:
          iconify();
          break;
        case Frame.MAXIMIZED_BOTH:
          maximize();
      }
  }

  public void setMaximizedBounds (Rectangle r)
  {

  }
  public void setBoundsPrivate(int x, int y, int width, int height)
  {
    // TODO Auto-generated method stub

  }

  public boolean requestWindowFocus()
  {
    // TODO Auto-generated method stub
    return false;
  }

  public Rectangle getBoundsPrivate()
  {
    // TODO: Implement this properly.
    throw new InternalError("Not yet implemented");
  }

}
