/* GtkFramePeer.java -- Implements FramePeer with GTK
   Copyright (C) 1999, 2002, 2004 Free Software Foundation, Inc.

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

import java.awt.Component;
import java.awt.Frame;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.MenuBar;
import java.awt.Rectangle;
import java.awt.Window;
import java.awt.event.PaintEvent;
import java.awt.image.ColorModel;
import java.awt.peer.FramePeer;
import java.awt.peer.MenuBarPeer;

public class GtkFramePeer extends GtkWindowPeer
    implements FramePeer
{
  private int menuBarHeight;
  private MenuBarPeer menuBar;
  native int getMenuBarHeight (MenuBarPeer bar);

  native void setMenuBarPeer (MenuBarPeer bar);
  native void removeMenuBarPeer ();
  native void moveLayout (int offset);
  native void gtkLayoutSetVisible (boolean vis);

  public void setMenuBar (MenuBar bar)
  {
    if (bar == null)
    {    
      if (menuBar != null)
      {
        gtkLayoutSetVisible(false);
        removeMenuBarPeer(); 
        menuBar = null;
        moveLayout(menuBarHeight);
        insets.top -= menuBarHeight;
        menuBarHeight = 0;      
        awtComponent.doLayout();
        gtkLayoutSetVisible(true);
      }
    }
    else
    {
      gtkLayoutSetVisible(false);
      int oldHeight = 0;
      if (menuBar != null)
      {
        removeMenuBarPeer();
        oldHeight = menuBarHeight;
        insets.top -= menuBarHeight;
      }
      menuBar = (MenuBarPeer) ((MenuBar) bar).getPeer();
      setMenuBarPeer(menuBar);
      menuBarHeight = getMenuBarHeight (menuBar);
      if (oldHeight != menuBarHeight)
        moveLayout(oldHeight - menuBarHeight);
      insets.top += menuBarHeight;
      awtComponent.doLayout();
      gtkLayoutSetVisible(true);
    }
  }

  public void setBounds (int x, int y, int width, int height)
  {
    nativeSetBounds (x, y,
		     width - insets.left - insets.right,
		     height - insets.top - insets.bottom
		     + menuBarHeight);
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
    create (GDK_WINDOW_TYPE_HINT_NORMAL, true);

    Frame frame = (Frame) awtComponent;

    setMenuBar (frame.getMenuBar ());

    setTitle (frame.getTitle ());
    setResizable (frame.isResizable ());
    setIconImage(frame.getIconImage());
  }

  native void nativeSetIconImageFromDecoder (GdkPixbufDecoder decoder);
  native void nativeSetIconImageFromData (int[] pixels, int width, int height);
  public void setIconImage (Image image) 
  {
      if (image != null)
        {
          GtkImage img = (GtkImage) image;
          // FIXME: Image should be loaded, but if not, do image loading here.
          if (img.isLoaded())
            {
              if (img.getSource() instanceof GdkPixbufDecoder)
                {
                  nativeSetIconImageFromDecoder((GdkPixbufDecoder) img.getSource());
                }
              else
                {
                  int[] pixels = img.getPixelCache();
                  ColorModel model = img.getColorModel();
                  int[] data = new int[pixels.length * 4];
                  for (int i = 0; i < pixels.length; i++)
                    {
                      data[i * 4] = model.getRed(pixels[i]);
                      data[i * 4 + 1] = model.getGreen(pixels[i]);
                      data[i * 4 + 2] = model.getBlue(pixels[i]);
                      data[i * 4 + 3] = model.getAlpha(pixels[i]);
                    }
                  nativeSetIconImageFromData(data, img.getWidth(null), img.getHeight(null));
                }
            }
        }
  }

  public Graphics getGraphics ()
  {
    Graphics g;
    if (GtkToolkit.useGraphics2D ())
      g = new GdkGraphics2D (this);
    else
      g = new GdkGraphics (this);
    g.translate (-insets.left, -insets.top);
    return g;
  }
  
  protected void postConfigureEvent (int x, int y, int width, int height)
  {
    int frame_x = x - insets.left;
    // Since insets.top includes the MenuBar height, we need to add back
    // the MenuBar height to the frame's y position.
    // If no MenuBar exists in this frame, the MenuBar height will be 0.
    int frame_y = y - insets.top + menuBarHeight;
    int frame_width = width + insets.left + insets.right;
    // Ditto as above. Since insets.top already includes the MenuBar's height,
    // we need to subtract the MenuBar's height from the top inset.
    int frame_height = height + insets.top + insets.bottom - menuBarHeight;
    if (frame_x != awtComponent.getX()
        || frame_y != awtComponent.getY()
        || frame_width != awtComponent.getWidth()
        || frame_height != awtComponent.getHeight())
      {
        setBoundsCallback ((Window) awtComponent,
                           frame_x,
                           frame_y,
                           frame_width,
                           frame_height);
      }
    awtComponent.validate();
  }
  
  protected void postMouseEvent(int id, long when, int mods, int x, int y, 
				int clickCount, boolean popupTrigger)
  {
    super.postMouseEvent (id, when, mods, 
			  x + insets.left, y + insets.top, 
			  clickCount, popupTrigger);
  }

  protected void postExposeEvent (int x, int y, int width, int height)
  {
    q.postEvent (new PaintEvent (awtComponent, PaintEvent.PAINT,
				 new Rectangle (x + insets.left, 
						y + insets.top, 
						width, height)));
  }

  public int getState ()
  {
    return 0;
  }

  public void setState (int state)
  {

  }

  public void setMaximizedBounds (Rectangle r)
  {

  }
}


