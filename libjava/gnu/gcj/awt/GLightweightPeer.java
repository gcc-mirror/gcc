/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.awt;

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
}
