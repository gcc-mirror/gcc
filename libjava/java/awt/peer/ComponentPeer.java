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


package java.awt.peer;

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
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.event.PaintEvent;
import java.awt.image.ColorModel;
import java.awt.image.ImageObserver;
import java.awt.image.ImageProducer;
import java.awt.image.VolatileImage;

public interface ComponentPeer
{
  int checkImage(Image img, int width, int height, 
		 ImageObserver ob);
  Image createImage(ImageProducer prod);
  Image createImage(int width, int height);
  void disable();
  void dispose();
  void enable();
  ColorModel getColorModel();
  FontMetrics getFontMetrics(Font f);
  Graphics getGraphics();
  Point getLocationOnScreen();
  Dimension getMinimumSize();
  Dimension getPreferredSize();
  Toolkit getToolkit();
  void handleEvent(AWTEvent e);
  void hide();

  /**
   * Part of the earlier 1.1 API, replaced by isFocusable().
   */
  boolean isFocusTraversable();
  boolean isFocusable();
  Dimension minimumSize();
  Dimension preferredSize();
  void paint(Graphics graphics);
  boolean prepareImage(Image img, int width, int height,
			      ImageObserver ob);
  void print(Graphics graphics);
  void repaint(long tm, int x, int y, int width, int height);

  /**
   * Part of the earlier 1.1 API, apparently replaced by argument 
   * form of the same method.
   */
  void requestFocus();
  boolean requestFocus (Component source, boolean bool1, boolean bool2, long x);

  void reshape(int x, int y, int width, int height);
  void setBackground(Color color);
  void setBounds(int x, int y, int width, int height);

  /**
   * Part of the earlier 1.1 API, apparently no longer needed.
   */
  void setCursor(Cursor cursor);

  void setEnabled(boolean enabled);
  void setFont(Font font);
  void setForeground(Color color);
  void setVisible(boolean visible);
  void show();

  /** 
   * Get the graphics configuration of the component. The color model
   * of the component can be derived from the configuration.
   */
  GraphicsConfiguration getGraphicsConfiguration();

  /**
   * Part of an older API, no longer needed.
   */
  void setEventMask (long mask);

  // Methods below are introduced since 1.1
  boolean isObscured();
  boolean canDetermineObscurity();
  void coalescePaintEvent(PaintEvent e);
  void updateCursorImmediately();
  boolean handlesWheelScrolling();

  /**
   * A convenience method that creates a volatile image.  The volatile
   * image is created on the screen device on which this component is
   * displayed, in the device's current graphics configuration.
   *
   * @param width width of the image
   * @param height height of the image
   *
   * @see VolatileImage
   *
   * @since 1.2
   */
  VolatileImage createVolatileImage(int width, int height);

  /**
   * Create a number of image buffers that implement a buffering
   * strategy according to the given capabilities.
   *
   * @param numBuffers the number of buffers
   * @param caps the buffering capabilities
   *
   * @throws AWTException if the specified buffering strategy is not
   * implemented
   *
   * @since 1.2
   */
  void createBuffers(int numBuffers, BufferCapabilities caps)
    throws AWTException;

  /**
   * Return the back buffer of this component.
   *
   * @return the back buffer of this component.
   *
   * @since 1.2
   */
  Image getBackBuffer();

  /**
   * Perform a page flip, leaving the contents of the back buffer in
   * the specified state.
   *
   * @param contents the state in which to leave the back buffer
   *
   * @since 1.2
   */
  void flip(BufferCapabilities.FlipContents contents);

  /**
   * Destroy the resources created by createBuffers.
   *
   * @since 1.2
   */
  void destroyBuffers();
}
