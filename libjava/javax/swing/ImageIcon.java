/* ImageIcon.java --
   Copyright (C) 2002, 2004  Free Software Foundation, Inc.

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
import java.awt.Toolkit;
import java.awt.image.ImageObserver;
import java.io.Serializable;
import java.net.URL;


public class ImageIcon
  implements Icon, Serializable
{
  private static final long serialVersionUID = 532615968316031794L;
  Image image;
  String description;
  ImageObserver observer;

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
    this.image = Toolkit.getDefaultToolkit().createImage(image.getSource());
    this.description = description;
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
    this.image = Toolkit.getDefaultToolkit().createImage(image.getSource());
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
}
