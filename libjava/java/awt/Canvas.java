/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.awt;

import java.awt.peer.ComponentPeer;

public class Canvas extends Component
{
  transient GraphicsConfiguration graphicsConfiguration;
  
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

  public void addNotify()
  {
    if (peer == null)
      {
	peer = (ComponentPeer) getToolkit().createCanvas(this);
      }
    super.addNotify();
  }

  /** Override this to create components with custom painting.
      Defaults to filling the component with the background color. */
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
}
