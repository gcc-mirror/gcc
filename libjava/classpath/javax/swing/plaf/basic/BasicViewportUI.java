/* BasicViewportUI.java --
   Copyright (C) 2002, 2004 Free Software Foundation, Inc.

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


package javax.swing.plaf.basic;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.image.ImageObserver;

import javax.swing.JComponent;
import javax.swing.JViewport;
import javax.swing.ViewportLayout;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.ViewportUI;

public class BasicViewportUI extends ViewportUI 
{

  ChangeListener changeListener;
  Image backingStoreImage;
  int backingStoreWidth = -1;
  int backingStoreHeight = -1;
  
  class ChangeHandler implements ChangeListener
  {
    public void stateChanged(ChangeEvent event)
    {
      JViewport v = (JViewport) event.getSource();
      v.repaint();
    }
  }

  void installDefaults(JComponent c)
  {    
    c.setOpaque(true);
  }

  void uninstallDefaults(JComponent c)
  {
  }

  void installListeners(JComponent c)
  {
    ((JViewport)c).addChangeListener(changeListener);
  }

  void uninstallListeners(JComponent c)
  {
    ((JViewport)c).removeChangeListener(changeListener);
  }

  public BasicViewportUI()
  {
    changeListener = new ChangeHandler();
  }

  public static ComponentUI createUI(JComponent c)
  {
    return new BasicViewportUI();
  }

  public void installUI(JComponent c) 
  {
    super.installUI(c);
    installListeners(c);
  }

  public void uninstallUI(JComponent c) 
  {
    uninstallListeners(c);
  }
    

  public Dimension getPreferredSize(JComponent c) 
  {
    // let the ViewportLayout decide
    return null;
  }

  public void paint(Graphics g, JComponent c)
  {
    JViewport port = (JViewport)c;
    Component view = port.getView();

    if (view == null)
      return;

    Point pos = port.getViewPosition();
    Rectangle viewBounds = view.getBounds();
    Rectangle portBounds = port.getBounds();

    if (viewBounds.width == 0 
        || viewBounds.height == 0
        || portBounds.width == 0
        || portBounds.height == 0)
      return;

    switch (port.getScrollMode())
      {

      case JViewport.BACKINGSTORE_SCROLL_MODE:
        paintBackingStore(g, port, view, pos, viewBounds, portBounds);
        break;

      case JViewport.BLIT_SCROLL_MODE:
        // FIXME: implement separate blit mode

      case JViewport.SIMPLE_SCROLL_MODE:
      default:
        paintSimple(g, port, view, pos, viewBounds, portBounds);
        break;
      }
  }

  private void paintSimple(Graphics g, 
                           JViewport v, 
                           Component view, 
                           Point pos, 
                           Rectangle viewBounds, 
                           Rectangle portBounds)
  {
    Rectangle oldClip = g.getClipBounds ();
    g.setClip (oldClip.intersection (viewBounds));
    g.translate (-pos.x, -pos.y);
    try
      {   
        view.paint(g);
      } 
    finally 
      {
        g.translate (pos.x, pos.y);
        g.setClip (oldClip);
      }        
  }

  private void paintBackingStore(Graphics g, 
                                 JViewport v, 
                                 Component view, 
                                 Point pos, 
                                 Rectangle viewBounds, 
                                 Rectangle portBounds)
  {      
    if (backingStoreImage == null 
        || backingStoreWidth != viewBounds.width
        || backingStoreHeight != viewBounds.height)
      {
        backingStoreImage = v.createImage(viewBounds.width, viewBounds.height);
        backingStoreWidth = viewBounds.width;
        backingStoreHeight = viewBounds.height;
      }

    Graphics g2 = backingStoreImage.getGraphics();

    if (v.getBackground() != null)
      {
        // fill the backing store background
        java.awt.Color save = g2.getColor();
        g2.setColor(v.getBackground());
        g2.fillRect (0, 0, backingStoreWidth, backingStoreHeight);
        g2.setColor(save);

        // fill the viewport background
        save = g.getColor();
        g.setColor(v.getBackground());
        g.fillRect (0, 0, portBounds.width, portBounds.height);
        g.setColor(save);

      }
    else
      {
        // clear the backing store background
        g2.clearRect(0, 0, backingStoreWidth, backingStoreHeight);

        // clear the viewport background
        g.clearRect(0, 0, portBounds.width, portBounds.height);
      }

    g2.setClip(g.getClipBounds());
    g2.translate(-pos.x, -pos.y);
    try 
      {
        view.paint(g2);
      }
    finally
      {
        g2.translate(pos.x, pos.y);
      }
    g2 = null;
    g.drawImage(backingStoreImage, 
                0, 0, 
                (ImageObserver)null);
  }
}
