/* ImageViewer.java -- Simple image display component.
   Copyright (C) 2004 Free Software Foundation, Inc.

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

package gnu.javax.activation.viewers;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Image;
import java.awt.Graphics;
import java.awt.MediaTracker;
import java.awt.Toolkit;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.IOException;

import javax.activation.CommandObject;
import javax.activation.DataHandler;

/**
 * Simple image display component.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 * @version 1.0.2
 */
public class ImageViewer extends Component
  implements CommandObject
{

  private Image image;

  /**
   * Returns the preferred size for this component (the image size).
   */
  public Dimension getPreferredSize()
    {
      Dimension ps = new Dimension(0, 0);
      if (image != null)
        {
          ps.width = image.getWidth(this);
          ps.height = image.getHeight(this);
        }
      return ps;
    }

  public void setCommandContext(String verb, DataHandler dh)
    throws IOException
  {
    // Read image into a byte array
    InputStream in = dh.getInputStream();
    ByteArrayOutputStream bytes = new ByteArrayOutputStream();
    byte[] buf = new byte[4096];
    for (int len = in.read(buf); len != -1; len = in.read(buf))
      bytes.write(buf, 0, len);
    in.close();
    // Create and prepare the image
    Toolkit toolkit = getToolkit();
    Image img = toolkit.createImage(bytes.toByteArray());
    try
      {
        MediaTracker tracker = new MediaTracker(this);
        tracker.addImage(img, 0);
        tracker.waitForID(0);
      }
    catch (InterruptedException e)
      {
      }
    toolkit.prepareImage(img, -1, -1, this);
  }

  /**
   * Image bits arrive.
   */
  public boolean imageUpdate(Image image, int flags, int x, int y,
                             int width, int height)
  {
    if ((flags & ALLBITS) != 0)
      {
        this.image = image;
        invalidate();
        repaint();
        return false;
      }
    return ((flags & ERROR) == 0);
  }

  /**
   * Scale the image into this component's bounds.
   */
  public void paint(Graphics g)
  {
    if (image != null)
      {
        Dimension is = new Dimension(image.getWidth(this),
                                     image.getHeight(this));
        if (is.width > -1 && is.height > -1)
          {
            Dimension cs = getSize();
            g.drawImage(image, 0, 0, cs.width, cs.height,
                        0, 0, is.width, is.height, this);
          }
      }
  }

}
