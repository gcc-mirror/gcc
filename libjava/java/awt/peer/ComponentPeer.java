/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.awt.peer;

import java.awt.*;
import java.awt.image.*;

public interface ComponentPeer
{
  int checkImage(Image img, int width, int height, ImageObserver o);
  Image createImage(ImageProducer prod);
  Image createImage(int width, int height);
  void dispose();

  /** 
   * Get the graphics configuration of the component. The color model
   * of the component can be derived from the configuration.
   */
  GraphicsConfiguration getGraphicsConfiguration();

  FontMetrics getFontMetrics(Font f);
  Graphics getGraphics();
  Point getLocationOnScreen();
  Dimension getMinimumSize();
  Dimension getPreferredSize();
  Toolkit getToolkit();
  // The JCL says that handleEvent returns boolean.  However, we've
  // experimentally determined that it in fact actually returns void.
  void handleEvent(AWTEvent e);
  boolean isFocusTraversable();
  void paint(Graphics graphics);
  boolean prepareImage(Image img, int width, int height, ImageObserver o);
  void print(Graphics graphics);
  void repaint(long tm, int x, int y, int width, int height);
  void requestFocus();
  void setBackground(Color color);
  void setBounds(int x, int y, int width, int height);
  void setCursor(Cursor cursor);
  void setEnabled(boolean enabled);
  void setEventMask(long eventMask);
  void setFont(Font font);
  void setForeground(Color color);
  void setVisible(boolean visible);
}
