/* GtkContainerPeer.java -- Implements ContainerPeer with GTK
   Copyright (C) 1998, 1999 Free Software Foundation, Inc.

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


package gnu.java.awt.peer.gtk;

import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.Window;
import java.awt.peer.ComponentPeer;
import java.awt.peer.ContainerPeer;

public class GtkContainerPeer extends GtkComponentPeer
  implements ContainerPeer
{
  Container c;
  boolean isValidating;

  public GtkContainerPeer(Container c)
  {
    super (c);
    this.c = c;
  }

  public void beginValidate ()
  {
    isValidating = true;
  }

  public void endValidate ()
  {
    Component parent = awtComponent.getParent ();

    // Only set our parent on the GTK side if our parent on the AWT
    // side is not showing.  Otherwise the gtk peer will be shown
    // before we've had a chance to position and size it properly.
    if (parent != null && parent.isShowing ())
      {
        Component[] components = ((Container) awtComponent).getComponents ();
        int ncomponents = components.length;

        for (int i = 0; i < ncomponents; i++)
          {
            ComponentPeer peer = components[i].getPeer ();

            // Skip lightweight peers.
            if (peer instanceof GtkComponentPeer)
              ((GtkComponentPeer) peer).setParentAndBounds ();
          }

        // GTK windows don't have parents.
        if (!(awtComponent instanceof Window))
          setParentAndBounds ();
      }

    isValidating = false;
  }

  public Insets getInsets() 
  {
    return insets;
  }

  public Insets insets() 
  {
    return getInsets ();
  }

  public void setBounds (int x, int y, int width, int height)
  {
    super.setBounds (x, y, width, height);
  }

  public void setFont(Font f)
  {
    super.setFont(f);
    Component[] components = ((Container) awtComponent).getComponents();
    for (int i = 0; i < components.length; i++)
      {
        GtkComponentPeer peer = (GtkComponentPeer) components[i].getPeer();
        if (peer != null && ! peer.awtComponent.isFontSet())
          peer.setFont(f);
      }
  }

  public Graphics getGraphics ()
  {
    return super.getGraphics();
  }

  public void beginLayout () { }
  public void endLayout () { }
  public boolean isPaintPending () { return false; }

  public void setBackground (Color c)
  {
    super.setBackground(c);
  
    Object components[] = ((Container) awtComponent).getComponents();
    for (int i = 0; i < components.length; i++)
      {
        Component comp = (Component) components[i];

        // If the child's background has not been explicitly set yet,
        // it should inherit this container's background. This makes the
        // child component appear as if it has a transparent background.
        // Note that we do not alter the background property of the child,
        // but only repaint the child with the parent's background color.
        if (!comp.isBackgroundSet() && comp.getPeer() != null)
          comp.getPeer().setBackground(c);
      }
  }
}
