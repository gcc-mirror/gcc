/* GLightweightPeer.java --
   Copyright (C) 2003, 2004, 2006 Free Software Foundation, Inc.

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


package gnu.java.awt.peer;

import java.awt.AWTEvent;
import java.awt.AWTException;
import java.awt.BufferCapabilities;
import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.GraphicsConfiguration;
import java.awt.Image;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.event.PaintEvent;
import java.awt.image.ColorModel;
import java.awt.image.ImageObserver;
import java.awt.image.ImageProducer;
import java.awt.image.VolatileImage;
import java.awt.peer.ContainerPeer;
import java.awt.peer.LightweightPeer;

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
  public GLightweightPeer()
  {
    // Nothing to do here.
  }

  // -------- java.awt.peer.ContainerPeer implementation:
  
  public Insets insets()
  {
    // Nothing to do here for lightweights.
    return null;
  }
  
  public Insets getInsets()
  {
    // Nothing to do here for lightweights.
    return null;
  }
  
  public void beginValidate()
  {
    // Nothing to do here for lightweights.
  }
  
  public void endValidate()
  {
    // Nothing to do here for lightweights.
  }
  
  public void beginLayout()
  {
    // Nothing to do here for lightweights.
  }
  
  public void endLayout()
  {
    // Nothing to do here for lightweights.
  }
  
  public boolean isPaintPending()
  {
    // Nothing to do here for lightweights.
    return false;
  }

  // -------- java.awt.peer.ComponentPeer implementation:

  public int checkImage(Image img, int width, int height, ImageObserver o)
  {
    // Nothing to do here for lightweights.
    return -1;
  }

  public Image createImage(ImageProducer prod)
  {
    // Nothing to do here for lightweights.
    return null;
  }

  /* This method is not called. */
  public Image createImage(int width, int height)
  {
    // Nothing to do here for lightweights.
    return null;
  }

  public void disable()
  {
    // Nothing to do here for lightweights.
  }

  public void dispose()
  {
    // Nothing to do here for lightweights.
  }

  public void enable()
  {
    // Nothing to do here for lightweights.
  }

  public GraphicsConfiguration getGraphicsConfiguration()
  {
    // Nothing to do here for lightweights.
    return null;
  }

  public FontMetrics getFontMetrics(Font f)
  {
    // We shouldn't end up here, but if we do we can still try do something
    // reasonable.
    Toolkit tk = Toolkit.getDefaultToolkit();
    return tk.getFontMetrics(f);
  }

  /* Returning null here tells the Component object that called us to
   * use its parent's Graphics. */
  public Graphics getGraphics()
  {
    // Nothing to do here for lightweights.
    return null;
  }

  public Point getLocationOnScreen()
  {
    // Nothing to do here for lightweights.
    return null;
  }

  public Dimension getMinimumSize()
  {
    return minimumSize();
  }

  public Dimension getPreferredSize()
  {
    return preferredSize();
  }

  /* Returning null here tells the Component object that called us to
   * use its parent's Toolkit. */
  public Toolkit getToolkit()
  {
    // Nothing to do here for lightweights.
    return null;
  }

  public void handleEvent(AWTEvent e)
  {
    // This can only happen when an application posts a PaintEvent for
    // a lightweight component directly. We still support painting for
    // this case.
    if (e instanceof PaintEvent)
      {
        PaintEvent pe = (PaintEvent) e;
        Component target = (Component) e.getSource();
        if (target != null && target.isShowing())
          {
            Graphics g = target.getGraphics();
            if (g != null)
              {
                try
                  {
                    Rectangle clip = pe.getUpdateRect();
                    g.setClip(clip);
                    target.paint(g);
                  }
                finally
                  {
                    g.dispose();
                  }
              }
          }
      }
  }

  public void hide()
  {
    // Nothing to do here for lightweights.
  }

  public boolean isFocusable() 
  {
    // Nothing to do here for lightweights.
    return false;
  }

  public boolean isFocusTraversable()
  {
    // Nothing to do here for lightweights.
    return false;
  }

  public Dimension minimumSize()
  {
    return new Dimension(0, 0);
  }

  public Dimension preferredSize()
  {
    return new Dimension(0, 0);
  }

  public void paint(Graphics graphics)
  {
    // Nothing to do here for lightweights.
  }

  public boolean prepareImage(Image img, int width, int height,
			      ImageObserver o)
  {
    // Nothing to do here for lightweights.
    return false;
  }

  public void print(Graphics graphics)
  {
    // Nothing to do here for lightweights.
  }

  public void repaint(long tm, int x, int y, int width, int height)
  {
    // Nothing to do here for lightweights.
  }

  public void requestFocus()
  {
    // Nothing to do here for lightweights.
  }

  public boolean requestFocus(Component source, boolean bool1, boolean bool2,
                              long x)
  {
    // Nothing to do here for lightweights.
    return false;
  }

  public void reshape(int x, int y, int width, int height)
  {
    // Nothing to do here for lightweights.
  }

  public void setBackground(Color color)
  {
    // Nothing to do here for lightweights.
  }

  public void setBounds(int x, int y, int width, int height)
  {
    // Nothing to do here for lightweights.
  }

  /**
   * Sets the cursor on the heavy-weight parent peer.
   * Called by the MouseListener on mouse enter.
   */
  public void setCursor(Cursor cursor)
  {
    // Nothing to do here for lightweights.
  }

  public void setEnabled(boolean enabled)
  {
    // Nothing to do here for lightweights.
  }

  public void setEventMask(long eventMask)
  {
    // Nothing to do here for lightweights.
  }

  public void setFont(Font font)
  {
    // Nothing to do here for lightweights.
  }

  public void setForeground(Color color)
  {
    // Nothing to do here for lightweights.
  }

  public void setVisible(boolean visible)
  {
    // Nothing to do here for lightweights.
  }

  public void show()
  {
    // Nothing to do here for lightweights.
  }

  public ColorModel getColorModel()
  {
    // Nothing to do here for lightweights.
    return null;
  }

  public boolean isObscured()
  {
    // Nothing to do here for lightweights.
    return false;
  }

  public boolean canDetermineObscurity()
  {
    // Nothing to do here for lightweights.
    return false;
  }

  public void coalescePaintEvent(PaintEvent e)
  {
    // Nothing to do here for lightweights.
  }

  public void updateCursorImmediately()
  {
    // Nothing to do here for lightweights.
  }

  public VolatileImage createVolatileImage(int width, int height) 
  { 
    // Nothing to do here for lightweights.
    return null; 
  }

  public boolean handlesWheelScrolling()
  {
    // Nothing to do here for lightweights.
    return false;
  }

  public void createBuffers(int x, BufferCapabilities capabilities) 
    throws AWTException
  {
    // Nothing to do here for lightweights.
  }

  public Image getBackBuffer()
  {
    // Nothing to do here for lightweights.
    return null;
  }

  public void flip(BufferCapabilities.FlipContents contents)
  {
    // Nothing to do here for lightweights.
  }

  public void destroyBuffers()
  {
    // Nothing to do here for lightweights.
  }

  public boolean isRestackSupported()
  {
    // Nothing to do here for lightweights.
    return false;
  }

  public void cancelPendingPaint(int x, int y, int width, int height)
  {
    // Nothing to do here for lightweights.
  }

  public void restack()
  {
    // Nothing to do here for lightweights.
  }

  public Rectangle getBounds()
  {
    // Nothing to do here for lightweights.
    return null;
  }

  public void reparent(ContainerPeer parent)
  {
    // Nothing to do here for lightweights.
  }

  public void setBounds(int x, int y, int z, int width, int height)
  {
    // Nothing to do here for lightweights.
  }

  public boolean isReparentSupported()
  {
    // Nothing to do here for lightweights.
    return true;
  }

  public void layout()
  {
    // Nothing to do here for lightweights.
  }

  public boolean requestFocus(Component lightweightChild, boolean temporary,
                              boolean focusedWindowChangeAllowed,
                              long time, sun.awt.CausedFocusEvent.Cause cause)
  {
    // Always grant focus request.
    return true;
  }

}
