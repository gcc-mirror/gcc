/* Copyright (C) 1999, 2000, 2002  Free Software Foundation

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


package java.awt;

import java.awt.peer.ComponentPeer;

public class Canvas extends Component implements java.io.Serializable
{
  transient GraphicsConfiguration graphicsConfiguration;

  /**
   * Initializes a new instance of <code>Canvas</code>.
   */
  public Canvas() { }

  public Canvas(GraphicsConfiguration graphicsConfiguration)
  {
    this.graphicsConfiguration = graphicsConfiguration;
  }

  GraphicsConfiguration getGraphicsConfigurationImpl()
  {
    if (graphicsConfiguration != null)
      return graphicsConfiguration;
    return super.getGraphicsConfigurationImpl();
  }

  /**
   * Creates the native peer for this object.
   */
  public void addNotify()
  {
    if (peer == null)
      peer = (ComponentPeer) getToolkit().createCanvas(this);
    super.addNotify();
  }

  /**
   * Repaints the canvas window.  This method should be overriden by 
   * a subclass to do something useful, as this method simply paints
   * the window with the background color.
   */
  public void paint(Graphics gfx)
  {
    /* This implementation doesn't make much sense since the filling
      of background color is guaranteed for heavyweight components
      such as this.  But there's no need to worry, since paint() is
      usually overridden anyway.  */
    gfx.setColor(getBackground());
    Dimension size = getSize();
    gfx.fillRect(0, 0, size.width, size.height);
  }

  // Serialization constant
  private static final long serialVersionUID = -2284879212465893870L;
}
