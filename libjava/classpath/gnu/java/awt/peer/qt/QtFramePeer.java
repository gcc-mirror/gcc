/* QtFramePeer.java --
   Copyright (C)  2005  Free Software Foundation, Inc.

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

package gnu.java.awt.peer.qt;

import java.awt.Component;
import java.awt.Frame;
import java.awt.Image;
import java.awt.Insets;
import java.awt.MenuBar;
import java.awt.Rectangle;
import java.awt.peer.FramePeer;

public class QtFramePeer extends QtWindowPeer implements FramePeer
{
  private int theState; // FIXME

  long frameObject;

  public QtFramePeer( QtToolkit kit, Component owner )
  {
    super( kit, owner );
  }

  protected native void init();

  protected void setup()
  {
    super.setup();
    setTitle( ((Frame)owner).getTitle() );
    if( ((Frame)owner).getMenuBar() != null )
      setMenuBar( ((Frame)owner).getMenuBar() );
  }

  private native void setIcon(QtImage image);

  private native void setMaximizedBounds(int w, int h);

  private native void setMenu(QtMenuBarPeer mb);

  private native int menuBarHeight();

  // ************ Public methods *********************
  
  public void destroy()
  {
    dispose();
  }

  public int getState()
  {
    // FIXME 
    return theState;
  }

  public Insets getInsets()
  {
    int mbHeight = ( ((Frame)owner).getMenuBar() != null ) ? 
      menuBarHeight() : 0;
    return new Insets(mbHeight, 0, 0, 0);
  }

  public void setIconImage(Image im)
  {
    if (im instanceof QtImage)
      setIcon( (QtImage)im );
    else 
      setIcon( new QtImage( im.getSource() ) );
  }

  public void setMaximizedBounds(Rectangle rect)
  {
    // FIXME
  }  

  public void setMenuBar(MenuBar mb)
  {
    if( mb != null )
      {
	QtMenuBarPeer mbpeer = (QtMenuBarPeer)mb.getPeer();
	if( mbpeer == null )
	  {
	    mb.addNotify();
	    mbpeer = (QtMenuBarPeer)mb.getPeer();
	    if( mbpeer == null )
	      throw new IllegalStateException("No menu bar peer.");
	  }
	mbpeer.addMenus();
	setMenu( mbpeer );
      } 
    else
      setMenu( null );
  }

  public void setResizable(boolean resizeable)
  {
    // FIXME
  }

  public void setState(int s)
  {
    theState = s;
    // FIXME
  }

  public void setBoundsPrivate(int x, int y, int width, int height)
  {
    // TODO Auto-generated method stub
    
  }

  public void updateAlwaysOnTop()
  {
    // TODO Auto-generated method stub
    
  }

  public boolean requestWindowFocus()
  {
    // TODO Auto-generated method stub
    return false;
  }

  public Rectangle getBoundsPrivate()
  {
    // TODO: Implement this properly.
    throw new InternalError("Not yet implemented");
  }

}
