/* GLightweightPeer.java --
   Copyright (C) 2003 Free Software Foundation, Inc.

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

package gnu.java.awt.peer;

import java.awt.AWTEvent;
import java.awt.AWTException;
import java.awt.BufferCapabilities;
import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.GraphicsConfiguration;
import java.awt.Image;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.event.PaintEvent;
import java.awt.image.ColorModel;
import java.awt.image.ImageObserver;
import java.awt.image.ImageProducer;
import java.awt.image.VolatileImage;
import java.awt.peer.ContainerPeer;
import java.awt.peer.LightweightPeer;

/*
 * Another possible implementation strategy for lightweight peers is
 * to make GLightweightPeer a placeholder class that implements
 * LightweightPeer.  Then the Component and Container classes could
 * identify a peer as lightweight and handle it specially.  The
 * current approach is probably more clear but less efficient.
 */

/**
 * A stub class that implements the ComponentPeer and ContainerPeer
 * interfaces using callbacks into the Component and Container
 * classes.  GLightweightPeer allows the Component and Container
 * classes to treat lightweight and heavyweight peers in the same way.
 *
 * Lightweight components are painted directly onto their parent
 * containers through an Image object provided by the toolkit.
 */
public class GLightweightPeer
  implements LightweightPeer, ContainerPeer
{
  private Component comp;

  private Insets containerInsets;

  public GLightweightPeer(Component comp)
  {
    this.comp = comp;
  }

  // -------- java.awt.peer.ContainerPeer implementation:
  
  public Insets insets()
  {
    return getInsets ();
  }
  
  public Insets getInsets()
  {
    if (containerInsets == null)
      containerInsets = new Insets (0,0,0,0);
    return containerInsets;
  }
  
  public void beginValidate()
  {
  }
  
  public void endValidate()
  {
  }
  
  public void beginLayout()
  {
  }
  
  public void endLayout()
  {
  }
  
  public boolean isPaintPending()
  {
    return false;
  }

  // -------- java.awt.peer.ComponentPeer implementation:

  public int checkImage(Image img, int width, int height, ImageObserver o)
  {
    return comp.getToolkit().checkImage(img, width, height, o);
  }

  public Image createImage(ImageProducer prod)
  {
    return comp.getToolkit().createImage(prod);
  }

  /* This method is not called. */
  public Image createImage(int width, int height)
  {
    return null;
  }

  public void disable() {}

  public void dispose() {}

  public void enable() {}

  public GraphicsConfiguration getGraphicsConfiguration()
  {
    return null;
  }

  public FontMetrics getFontMetrics(Font f)
  {
    return comp.getToolkit().getFontMetrics(f);
  }

  /* Returning null here tells the Component object that called us to
   * use its parent's Graphics. */
  public Graphics getGraphics()
  {
    return null;
  }

  public Point getLocationOnScreen()
  {
    Point parentLocation = comp.getParent().getLocationOnScreen();
    return new Point (parentLocation.x + comp.getX(),
                      parentLocation.y + comp.getY());
  }

  public Dimension getMinimumSize()
  {
    return new Dimension(comp.getWidth(), comp.getHeight());
  }

  /* A lightweight component's preferred size is equivalent to its
   * Component width and height values. */
  public Dimension getPreferredSize()
  {
    return new Dimension(comp.getWidth(), comp.getHeight());
  }

  /* Returning null here tells the Component object that called us to
   * use its parent's Toolkit. */
  public Toolkit getToolkit()
  {
    return null;
  }

  public void handleEvent(AWTEvent e) {}

  public void hide() {}

  public boolean isFocusable() 
  {
    return false;
  }

  public boolean isFocusTraversable()
  {
    return false;
  }

  public Dimension minimumSize()
  {
    return getMinimumSize();
  }

  public Dimension preferredSize()
  {
    return getPreferredSize();
  }

  public void paint(Graphics graphics) {}

  public boolean prepareImage(Image img, int width, int height,
			      ImageObserver o)
  {
    return comp.getToolkit().prepareImage(img, width, height, o);
  }

  public void print(Graphics graphics) {}

  public void repaint(long tm, int x, int y, int width, int height) {}

  public void requestFocus() {}

  public boolean requestFocus(Component source, boolean bool1, boolean bool2, long x)
  {
    return false;
  }

  public void reshape(int x, int y, int width, int height) {}

  public void setBackground(Color color) {}

  public void setBounds(int x, int y, int width, int height) {}

  public void setCursor(Cursor cursor) {}

  public void setEnabled(boolean enabled) {}

  public void setEventMask(long eventMask) {}

  public void setFont(Font font) {}

  public void setForeground(Color color) {}

  public void setVisible(boolean visible) {}

  public void show() {}

  public ColorModel getColorModel ()
  {
    return comp.getColorModel ();
  }

  public boolean isObscured()
  {
    return false;
  }

  public boolean canDetermineObscurity()
  {
    return false;
  }

  public void coalescePaintEvent(PaintEvent e) { }

  public void updateCursorImmediately() { }

  public VolatileImage createVolatileImage(int width, int height) 
  { 
    return null; 
  }

  public boolean handlesWheelScrolling()
  {
    return false;
  }

  public void createBuffers(int x, BufferCapabilities capabilities) 
    throws AWTException { }

  public Image getBackBuffer()
  {
    return null;
  }

  public void flip(BufferCapabilities.FlipContents contents) { }

  public void destroyBuffers() { }
}
