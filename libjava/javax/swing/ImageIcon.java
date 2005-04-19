/* ImageIcon.java --
   Copyright (C) 2002, 2004, 2005  Free Software Foundation, Inc.

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

package javax.swing;

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.MediaTracker;
import java.awt.Toolkit;
import java.awt.image.ImageObserver;
import java.io.Serializable;
import java.net.URL;


public class ImageIcon
  implements Icon, Serializable
{
  private static final long serialVersionUID = 532615968316031794L;

  /** A dummy Component that is used in the MediaTracker. */
  protected static Component component = new Component(){};

  /** The MediaTracker used to monitor the loading of images. */
  protected static MediaTracker tracker = new MediaTracker(component);

  /** The ID that is used in the tracker. */
  private static int id;

  Image image;
  String description;
  ImageObserver observer;

  /** The image loading status. */
  private int loadStatus;

  public ImageIcon()
  {
  }
  
  public ImageIcon(String file)
  {
    this(file, file);
  }

  public ImageIcon(String file, String description)
  {
    this(Toolkit.getDefaultToolkit().getImage(file), description);
  }

  public ImageIcon(byte[] imageData)
  {
    this(imageData, null);
  }
  
  public ImageIcon(byte[] imageData, String description)
  {
    this(Toolkit.getDefaultToolkit().createImage(imageData), description);
  }

  public ImageIcon(URL url)
  {
    this(url, null);
  }

  public ImageIcon(URL url, String description)
  {
    this(Toolkit.getDefaultToolkit().getImage(url), description);
  }

  public ImageIcon(Image image)
  {
    this(image, null);
  }

  public ImageIcon(Image image, String description)
  {
    setImage(image);
    setDescription(description);
  }
    
  public ImageObserver getImageObserver()
  {
    return observer;
  }
  
  public void setImageObserver(ImageObserver newObserver)
  {
    observer = newObserver;
  }

  public Image getImage()
  {
    return image;
  }

  public void setImage(Image image)
  {
    loadImage(image);
    this.image = image;
  }

  public String getDescription()
  {
    return description;
  }

  public void setDescription(String description)
  {
    this.description = description;
  }

  public int getIconHeight()
  {
    return image.getHeight(observer);
  }

  public int getIconWidth()
  {
    return image.getWidth(observer);
  }

  public void paintIcon(Component c, Graphics g, int x, int y)
  {
    g.drawImage(image, x, y, observer != null ? observer : c);
  }

  /**
   * Loads the image and blocks until the loading operation is finished.
   *
   * @param image the image to be loaded
   */
  protected void loadImage(Image image)
  {
    try
      {
	tracker.addImage(image, id);
	id++;
	tracker.waitForID(id - 1);
      }
    catch (InterruptedException ex)
      {
	; // ignore this for now
      }
    finally
      {
	loadStatus = tracker.statusID(id - 1, false);
      }
  }

  /**
   * Returns the load status of the icon image.
   *
   * @return the load status of the icon image
   *
   * @see {@link MediaTracker.COMPLETE}
   * @see {@link MediaTracker.ABORTED}
   * @see {@link MediaTracker.ERRORED}
   */
  public int getImageLoadStatus()
  {
    return loadStatus;
  }
}
