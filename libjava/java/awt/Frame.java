/* Copyright (C) 1999, 2000  Free Software Foundation

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt;
import java.awt.peer.FramePeer;

/* A very incomplete placeholder. */

public class Frame extends Window implements MenuContainer
{
  public static final int NORMAL = 0;
  public static final int ICONIFIED = 1;

  MenuBar menuBar = null;
  String title;

  private transient Image iconImage;
  private transient boolean isResizable = true;
  private transient int state = NORMAL;

  public Frame ()
  {
    super();
  }

  public Frame(GraphicsConfiguration gc)
  {
    super(gc);
  }

  public Frame (String title)
  {
    super();
    setTitle(title);
  }

  public Frame(String title, GraphicsConfiguration gc)
  {
    super(gc);
    setTitle(title);
  }

  public String getTitle()
  {
    return (title != null) ? title : "";
  }
    
  public void setTitle (String title)
  {
    this.title = title;
    if (peer != null)
      ((FramePeer)peer).setTitle(title);
  }

  public Image getIconImage()
  {
    return iconImage;
  }
  
  public void setIconImage(Image image)
  {
    iconImage = image;
    if (peer != null)
      ((FramePeer) peer).setIconImage(iconImage);
  }

  protected void finalize() throws Throwable
  {
    //frames.remove(this);
    /* FIXME: This won't work. Finalize will never be called if frames
       has a reference to the object. We need weak references to
       implement this correctly. */

    super.finalize();
  }

  public synchronized void setMenuBar (MenuBar menuBar)
  { 
    if (this.menuBar != menuBar)
      {
	//this.menuBar.removeNotify();
	this.menuBar = menuBar;
	//this.menuBar.addNotify();
      }	

    if (peer != null)
      ((FramePeer) peer).setMenuBar(menuBar);
  }
  
  public boolean isResizable()
  {
    return isResizable;
  }

  public void setResizable(boolean resizable)
  {
    isResizable = resizable;
    if (peer != null)
      ((FramePeer) peer).setResizable(isResizable);
  }

  public int getState()
  {
    /* FIXME: State might have changed in the peer... Must check. */
    
    return state;
  }


  public synchronized void addNotify ()
  {
    if (peer == null)
      peer = getToolkit ().createFrame (this);
    super.addNotify();
  }

  public boolean postEvent(Event evt) { return false; } // FIXME
 
  public void remove(MenuComponent m)
  {
    if (m == menuBar)
      {
	setMenuBar(null);
	return;
      }
	
    super.remove(m);
  }
  
  public void removeNotify()
  {
    //if ((peer != null) && (menuBar != null)) menuBar.removeNotify();
    super.removeNotify();
  }
    
  public static Frame[] getFrames()
  {
    //Frame[] array = new Frames[frames.size()];
    //return frames.toArray(array);
    
    // see finalize() comment
    String msg = "FIXME: can't be implemented without weak references";
    throw new UnsupportedOperationException(msg);
  }
}
