/* ComponentPeer.java -- Toplevel component peer
   Copyright (C) 1999, 2000, 2002 Free Software Foundation, Inc.

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

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */


package java.awt.peer;

import java.awt.*;
import java.awt.image.*;

public interface ComponentPeer
{
  public int checkImage(Image img, int width, int height, 
			ImageObserver ob);
  public Image createImage(ImageProducer prod);
  public Image createImage(int width, int height);
  public void disable();
  public void dispose();
  public void enable();
  public ColorModel getColorModel();
  public FontMetrics getFontMetrics(Font f);
  public Graphics getGraphics();
  public Point getLocationOnScreen();
  public Dimension getMinimumSize();
  public Dimension getPreferredSize();
  public Toolkit getToolkit();
  // The JCL says that handleEvent returns boolean.  However, we've
  // experimentally determined that it in fact actually returns void.
  public void handleEvent(AWTEvent e);
  public void hide();
  public boolean isFocusTraversable();
  public Dimension minimumSize();
  public Dimension preferredSize();
  public void paint(Graphics graphics);
  public boolean prepareImage(Image img, int width, int height,
				       ImageObserver ob);
  public void print(Graphics graphics);
  public void repaint(long tm, int x, int y, int width, int height);
  public void requestFocus();
  public void reshape(int x, int y, int width, int height);
  public void setBackground(Color color);
  public void setBounds(int x, int y, int width, int height);
  public void setCursor(Cursor cursor);
  public void setEnabled(boolean enabled);
  public void setFont(Font font);
  public void setForeground(Color color);
  public void setVisible(boolean visible);
  public void show();

  /** 
   * Get the graphics configuration of the component. The color model
   * of the component can be derived from the configuration.
   */
  GraphicsConfiguration getGraphicsConfiguration();

  public void setEventMask (long mask);
}
