/* Copyright (C) 2000, 2002  Free Software Foundation

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

package gnu.java.awt;

import java.awt.*;
import java.awt.peer.*;
import java.awt.image.*;

/**
 * @author Rolf W. Rasmussen <rolfwr@ii.uib.no>
 */
public class GLightweightPeer implements LightweightPeer
{
  public static final GLightweightPeer INSTANCE = new GLightweightPeer();

  public GLightweightPeer() {}

  // -------- java.awt.peer.ComponentPeer implementation:
  
  public int checkImage(Image img, int width, int height, ImageObserver o)
  {
    return 0;
  }

  public Image createImage(ImageProducer prod)
  {
    return null;
  }

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
    return null;
  }

  public Graphics getGraphics()
  {
    return null;
  }

  public Point getLocationOnScreen()
  {
    return null;
  }

  public Dimension getMinimumSize()
  {
    return null;
  }

  public Dimension getPreferredSize()
  {
    return null;
  }

  public Toolkit getToolkit()
  {
    return null;
  }

  public void handleEvent(AWTEvent e) {}

  public void hide() {}

  public boolean isFocusTraversable()
  {
    return false;
  }

  public Dimension minimumSize()
  {
    return null;
  }

  public Dimension preferredSize()
  {
    return null;
  }

  public void paint(Graphics graphics) {}

  public boolean prepareImage(Image img, int width, int height,
			      ImageObserver o)
  {
    return false;
  }

  public void print(Graphics graphics) {}

  public void repaint(long tm, int x, int y, int width, int height) {}

  public void requestFocus() {}

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
    return null;
  }
}
