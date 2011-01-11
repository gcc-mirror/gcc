/* QtComponentPeer.java --
   Copyright (C)  2005, 2006  Free Software Foundation, Inc.

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

import java.awt.AWTEvent;
import java.awt.AWTException;
import java.awt.BufferCapabilities;
import java.awt.Component;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Image;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GraphicsConfiguration;
import java.awt.GraphicsDevice;
import java.awt.KeyboardFocusManager;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.Window;
import java.awt.peer.ComponentPeer;
import java.awt.peer.ContainerPeer;
import java.awt.image.ColorModel;
import java.awt.image.VolatileImage;
import java.awt.image.ImageObserver;
import java.awt.image.ImageProducer;
import java.awt.event.ComponentEvent; // 100%
import java.awt.event.FocusEvent; // 100%
import java.awt.event.InputEvent; // (abstract)
import java.awt.event.KeyEvent; // 2/3
import java.awt.event.MouseEvent; // 70%?
import java.awt.event.PaintEvent; // Yup.
import java.awt.event.WindowEvent; // 2/ 12
import java.util.Timer;
import java.util.TimerTask;

public class QtComponentPeer extends NativeWrapper implements ComponentPeer
{

  /**
   * Popup trigger button, may differ between platforms
   */
  protected static final int POPUP_TRIGGER = 3;

  /**
   * The toolkit which manufactured this peer.
   */
  protected QtToolkit toolkit;

  /**
   * The component which owns this peer.
   */
  Component owner;

  /**
   * Classpath updates our eventMask.
   */
  private long eventMask;

  /**
   * if the thing has mouse motion listeners or not.
   */
  private boolean hasMotionListeners;

  /**
   * The component's double buffer for off-screen drawing.
   */
  protected QtImage backBuffer;

  protected long qtApp;

  private boolean settingUp;

  private boolean ignoreResize = false;

  QtComponentPeer( QtToolkit kit, Component owner )
  {
    this.owner = owner;
    this.toolkit = kit;
    qtApp = QtToolkit.guiThread.QApplicationPointer;
    nativeObject = 0;
    synchronized(this)
      {
        callInit(); // Calls the init method FROM THE MAIN THREAD.
        try
          {
            wait(); // Wait for the thing to be created.
          }
        catch(InterruptedException e)
          {
          }
      }
    setup();
    hasMotionListeners = false;
  }

  protected native void callInit();

  /**
   * Init does the creation of native widgets, it is therefore
   * called from the main thread. (the constructor waits for this to happen.)
   */
  protected void init()
  {
  }

  protected void setup()
  {
    settingUp = true;
    if (owner != null)
      {
        if (owner instanceof javax.swing.JComponent)
          setBackground(owner.getBackground());
        else
          owner.setBackground(getNativeBackground());

        if (owner.getForeground() != null)
          setForeground(owner.getForeground());
        else
          setForeground( Color.black );

        if (owner.getCursor() != null)
          if (owner.getCursor().getType() != Cursor.DEFAULT_CURSOR)
            setCursor(owner.getCursor());

        if (owner.getFont() != null)
          setFont(owner.getFont());

        setEnabled( owner.isEnabled() );

        backBuffer = null;
        updateBounds();

        setVisible( owner.isVisible() );
        QtToolkit.repaintThread.queueComponent(this);
      }
    settingUp = false;
  }

  native void QtUpdate();
  native void QtUpdateArea( int x, int y, int w, int h );
  private synchronized native void disposeNative();
  private native void setGround( int r, int g, int b, boolean isForeground );
  private native void setBoundsNative( int x, int y, int width, int height );
  private native void setCursor( int ctype );
  private native Color getNativeBackground();
  private native void setFontNative( QtFontPeer fp );
  private native int whichScreen();
  private native void reparentNative( QtContainerPeer parent );
  private native void getLocationOnScreenNative( Point p );

  private boolean drawableComponent()
  {
    return ((this instanceof QtContainerPeer &&
             !(this instanceof QtScrollPanePeer)) ||
            (this instanceof QtCanvasPeer));
  }

  void updateBounds()
  {
    Rectangle r = owner.getBounds();
    setBounds( r.x, r.y, r.width, r.height );
  }

  synchronized void updateBackBuffer(int width, int height)
  {
    if(width <= 0 || height <= 0)
      return;

    if( !drawableComponent() && backBuffer == null)
      return;

    if( backBuffer != null )
      {
        if( width < backBuffer.width && height < backBuffer.height )
          return;
        backBuffer.dispose();
      }
    backBuffer = new QtImage(width, height);
  }


  // ************ Event methods *********************

  /**
   * Window closing event
   */
  protected void closeEvent()
  {
    if (owner instanceof Window)
      {
        WindowEvent e = new WindowEvent((Window)owner,
                                        WindowEvent.WINDOW_CLOSING);
        QtToolkit.eventQueue.postEvent(e);
      }
  }

  protected void enterEvent(int modifiers, int x, int y, int dummy)
  {
    MouseEvent e = new MouseEvent(owner,
                                  MouseEvent.MOUSE_ENTERED,
                                  System.currentTimeMillis(),
                                  (modifiers & 0x2FF), x, y, 0, false);
    QtToolkit.eventQueue.postEvent(e);
  }

  protected void focusInEvent()
  {
    FocusEvent e = new FocusEvent(owner, FocusEvent.FOCUS_GAINED);
    QtToolkit.eventQueue.postEvent(e);
   }

  protected void focusOutEvent()
  {
    FocusEvent e = new FocusEvent(owner, FocusEvent.FOCUS_LOST);
    QtToolkit.eventQueue.postEvent(e);
  }

  protected void keyPressEvent(int modifiers, int code, int unicode, int dummy)
  {
    KeyboardFocusManager manager = KeyboardFocusManager.getCurrentKeyboardFocusManager ();
    KeyEvent e = new KeyEvent(owner,
                              KeyEvent.KEY_PRESSED,
                              System.currentTimeMillis(),
                              modifiers, code, (char)(unicode & 0xFFFF),
                              KeyEvent.KEY_LOCATION_UNKNOWN);
    if (!manager.dispatchEvent (e))
      QtToolkit.eventQueue.postEvent(e);
  }

  protected void keyReleaseEvent(int modifiers, int code, int unicode, int dummy)
  {
    KeyEvent e = new KeyEvent(owner,
                              KeyEvent.KEY_RELEASED,
                              System.currentTimeMillis(),
                              modifiers, code, (char)(unicode & 0xFFFF),
                              KeyEvent.KEY_LOCATION_UNKNOWN);
    QtToolkit.eventQueue.postEvent(e);
  }

  protected void leaveEvent(int modifiers, int x, int y, int dummy)
  {
    MouseEvent e = new MouseEvent(owner,
                                  MouseEvent.MOUSE_EXITED,
                                  System.currentTimeMillis(),
                                  (modifiers & 0x2FF), x, y, 0, false);
    QtToolkit.eventQueue.postEvent(e);
  }

  // FIXME: Coalesce press-release events into clicks.
  protected void mouseDoubleClickEvent( int modifiers, int x, int y, int clickCount)
  {
    if( (eventMask & AWTEvent.MOUSE_EVENT_MASK) == 0 )
      return;
    int button = 0;
    if((modifiers & InputEvent.BUTTON1_DOWN_MASK) ==
       InputEvent.BUTTON1_DOWN_MASK) button = 1;
    if((modifiers & InputEvent.BUTTON2_DOWN_MASK) ==
       InputEvent.BUTTON2_DOWN_MASK) button = 2;
    if((modifiers & InputEvent.BUTTON3_DOWN_MASK) ==
       InputEvent.BUTTON3_DOWN_MASK) button = 3;
    MouseEvent e = new MouseEvent(owner,
                                  MouseEvent.MOUSE_CLICKED,
                                  System.currentTimeMillis(),
                                  (modifiers & 0x2FF), x, y, clickCount,
                                  false, button);
    QtToolkit.eventQueue.postEvent(e);
  }

  protected void mouseMoveEvent( int modifiers, int x, int y, int clickCount)
  {
    if( (eventMask & AWTEvent.MOUSE_EVENT_MASK) == 0 )
      return;

    int button = 0;
    if((modifiers & InputEvent.BUTTON1_DOWN_MASK) ==
       InputEvent.BUTTON1_DOWN_MASK) button = 1;
    if((modifiers & InputEvent.BUTTON2_DOWN_MASK) ==
       InputEvent.BUTTON2_DOWN_MASK) button = 2;
    if((modifiers & InputEvent.BUTTON3_DOWN_MASK) ==
           InputEvent.BUTTON3_DOWN_MASK) button = 3;

    int type = (button != 0) ?
      MouseEvent.MOUSE_DRAGGED :MouseEvent.MOUSE_MOVED;

    MouseEvent e = new MouseEvent(owner,
                                  type,
                                  System.currentTimeMillis(),
                                  (modifiers & 0x2FF), x, y, clickCount,
                                  false, button);
    QtToolkit.eventQueue.postEvent(e);
  }

  protected void mousePressEvent( int modifiers, int x, int y, int clickCount)
  {
    if( (eventMask & AWTEvent.MOUSE_EVENT_MASK) == 0 )
      return;
    int button = 0;
    if((modifiers & InputEvent.BUTTON1_DOWN_MASK) ==
       InputEvent.BUTTON1_DOWN_MASK) button = 1;
    if((modifiers & InputEvent.BUTTON2_DOWN_MASK) ==
       InputEvent.BUTTON2_DOWN_MASK) button = 2;
    if((modifiers & InputEvent.BUTTON3_DOWN_MASK) ==
       InputEvent.BUTTON3_DOWN_MASK) button = 3;
    MouseEvent e = new MouseEvent(owner,
                                  MouseEvent.MOUSE_PRESSED,
                                  System.currentTimeMillis(),
                                  (modifiers & 0x2FF), x, y, clickCount,
                                  ( button == POPUP_TRIGGER ),
                                   button);
    QtToolkit.eventQueue.postEvent(e);
  }

  protected void mouseReleaseEvent( int modifiers, int x, int y, int clickCount)
  {
    if( (eventMask & AWTEvent.MOUSE_EVENT_MASK) == 0 )
      return;
    int button = 0;
    if((modifiers & InputEvent.BUTTON1_DOWN_MASK) ==
       InputEvent.BUTTON1_DOWN_MASK) button = 1;
    if((modifiers & InputEvent.BUTTON2_DOWN_MASK) ==
       InputEvent.BUTTON2_DOWN_MASK) button = 2;
    if((modifiers & InputEvent.BUTTON3_DOWN_MASK) ==
       InputEvent.BUTTON3_DOWN_MASK) button = 3;

    MouseEvent e = new MouseEvent(owner,
                                  MouseEvent.MOUSE_RELEASED,
                                  System.currentTimeMillis(),
                                  (modifiers & 0x2FF), x, y, clickCount,
                                  false, button);
    QtToolkit.eventQueue.postEvent(e);
  }

  protected void moveEvent(int x, int y, int oldx, int oldy)
  {
    if( !ignoreResize )
      {
        // Since Component.setLocation calls back to setBounds,
        // we need to ignore that.
        ignoreResize = true;
        owner.setLocation( x, y );
        ignoreResize = false;
      }
  }

  protected void resizeEvent(int oldWidth, int oldHeight,
                             int width, int height)
  {
    if(!(owner instanceof Window))
      return;
    updateBackBuffer(width, height);
    ignoreResize = true;
    owner.setSize(width, height);
    ignoreResize = false;
    ComponentEvent e = new ComponentEvent(owner,
                                          ComponentEvent.COMPONENT_RESIZED);
    QtToolkit.eventQueue.postEvent(e);
    QtToolkit.repaintThread.queueComponent(this);
  }

  protected void showEvent()
  {
    if (owner instanceof Window)
      {
        WindowEvent e = new WindowEvent((Window)owner,
                                        WindowEvent.WINDOW_OPENED);
        QtToolkit.eventQueue.postEvent(e);
      }
    else
      {
        ComponentEvent e = new ComponentEvent(owner,
                                              ComponentEvent.COMPONENT_SHOWN);
        QtToolkit.eventQueue.postEvent(e);
      }
  }

  protected void hideEvent()
  {
    ComponentEvent e = new ComponentEvent(owner,
                                          ComponentEvent.COMPONENT_HIDDEN);
    QtToolkit.eventQueue.postEvent(e);
  }

  // ************ Public methods *********************

  /** Classpath-specific method */
  public void setEventMask(long x)
  {
    eventMask = x;
  }


  public boolean canDetermineObscurity()
  {
    return true;
  }

  public int checkImage(Image img,
                        int w,
                        int h,
                        ImageObserver o)
  {
    return toolkit.checkImage(img, w, h, o);
  }

  public void createBuffers(int numBuffers, BufferCapabilities caps)
    throws AWTException
  {
    // FIXME
  }

  public Image createImage(ImageProducer producer)
  {
    return toolkit.createImage(producer);
  }

  public Image createImage(int width, int height)
  {
    return new QtImage(width, height);
  }

  public void coalescePaintEvent(PaintEvent e)
  {
    // FIXME
  }

  public VolatileImage createVolatileImage(int w, int h)
  {
    return new QtVolatileImage( w, h );
  }

  public void destroyBuffers()
  {
    // FIXME
  }

  public void disable()
  {
    setEnabled(false);
  }

  public void dispose()
  {
    disposeNative();
    if( backBuffer != null )
      backBuffer.dispose();
  }

  public void enable()
  {
    setEnabled(true);
  }

  public void finalize()
  {
    dispose();
  }

  public void flip(BufferCapabilities.FlipContents contents)
  {
  }

  public Image getBackBuffer()
  {
    return backBuffer;
  }

  public ColorModel getColorModel()
  {
    return toolkit.getColorModel();
  }

  public FontMetrics getFontMetrics(Font font)
  {
    return new QtFontMetrics( font, getGraphics() );
  }

  public Graphics getGraphics()
  {
    if( backBuffer == null )
      {
        Rectangle r = owner.getBounds();
        backBuffer = new QtImage( r.width, r.height );
      }
    return backBuffer.getDirectGraphics( this );
  }

  public GraphicsConfiguration getGraphicsConfiguration()
  {
    int id = whichScreen(); // get the ID of the screen the widget is on.
    GraphicsDevice[] devs = QtToolkit.graphicsEnv.getScreenDevices();
    return devs[id].getDefaultConfiguration();
  }

  public Point getLocationOnScreen()
  {
    Point p = new Point();
    synchronized( p )
      {
        getLocationOnScreenNative( p );
        try
          {
            p.wait(); // Wait for the thing to be created.
          }
        catch(InterruptedException e)
          {
          }
      }
    return p;
  }

  private native void getSizeNative(Dimension d, boolean preferred);

  private Dimension getSize(boolean preferred)
  {
    Dimension d = new Dimension();
    synchronized( d )
      {
        getSizeNative(d, preferred);
        try
          {
            d.wait(); // Wait for the thing to be created.
          }
        catch(InterruptedException e)
          {
          }
      }
    return d;
  }

  public Dimension getMinimumSize()
  {
    return getSize( false );
  }

  public Dimension getPreferredSize()
  {
    return getSize( true );
  }

  public Toolkit getToolkit()
  {
    return toolkit;
  }

  public native boolean handlesWheelScrolling();

  public void hide()
  {
    setVisible(false);
  }

  public native boolean isFocusable();

  public boolean isFocusTraversable()
  {
    // FIXME
    return false;
  }

  public native boolean isObscured();

  public Dimension minimumSize()
  {
    return getMinimumSize();
  }

  public Dimension preferredSize()
  {
    return getPreferredSize();
  }

  public native void requestFocus();

  public boolean requestFocus (Component source, boolean bool1,
                               boolean bool2, long x)
  {
    // FIXME
    return true;
  }

  public void reshape(int x,
                      int y,
                      int width,
                      int height)
  {
    setBounds( x, y, width, height );
  }

  public void setBackground(Color c)
  {
    if(c == null && !settingUp)
      return;
    setGround(c.getRed(), c.getGreen(), c.getBlue(), false);
  }

  public void setBounds(int x, int y, int width, int height)
  {
    if( ignoreResize )
      return;
    updateBackBuffer(width, height);
    QtToolkit.repaintThread.queueComponent(this);
    setBoundsNative(x, y, width, height);
  }

  public void setCursor(Cursor cursor)
  {
    if (cursor != null)
      setCursor(cursor.getType());
  }

  public native void setEnabled(boolean b);

  public void setFont(Font f)
  {
    if( f == null || f.getPeer() == null)
      throw new IllegalArgumentException("Null font.");
    setFontNative( (QtFontPeer)f.getPeer() );
  }

  public void setForeground(Color c)
  {
    if(c == null && !settingUp)
      return;
    setGround(c.getRed(), c.getGreen(), c.getBlue(), true);
  }

  public native void setVisible(boolean b);

  public void show()
  {
    setVisible(true);
  }

  public void handleEvent (AWTEvent e)
  {
    int eventID = e.getID();
    Rectangle r;

    switch (eventID)
      {
      case ComponentEvent.COMPONENT_SHOWN:
        QtToolkit.repaintThread.queueComponent(this);
        break;
      case PaintEvent.PAINT:
      case PaintEvent.UPDATE:
        r = ((PaintEvent)e).getUpdateRect();
        QtToolkit.repaintThread.queueComponent(this, r.x, r.y,
                                               r.width, r.height);
        break;
      case KeyEvent.KEY_PRESSED:
        break;
      case KeyEvent.KEY_RELEASED:
        break;
      }
  }

  /**
   * paint() is called back from the native side in response to a native
   * repaint event.
   */
  public void paint(Graphics g)
  {
    Rectangle r = g.getClipBounds();

    if (backBuffer != null)
      backBuffer.drawPixelsScaledFlipped ((QtGraphics) g,
                                          0, 0, 0, /* bg colors */
                                          false, false, /* no flipping */
                                          r.x, r.y, r.width, r.height,
                                          r.x, r.y, r.width, r.height,
                                          false ); /* no compositing */
  }

  public void paintBackBuffer() throws InterruptedException
  {
    if( backBuffer != null )
      {
        backBuffer.clear();
        Graphics2D bbg = (Graphics2D)backBuffer.getGraphics();
        owner.paint(bbg);
        bbg.dispose();
      }
  }

  public void paintBackBuffer(int x, int y, int w, int h)
    throws InterruptedException
  {
    if( backBuffer != null )
      {
        Graphics2D bbg = (Graphics2D)backBuffer.getGraphics();
        bbg.setBackground( getNativeBackground() );
        bbg.clearRect(x, y, w, h);
        bbg.setClip(x, y, w, h);
        owner.paint(bbg);
        bbg.dispose();
      }
  }

  public boolean prepareImage(Image img,
                              int w,
                              int h,
                              ImageObserver o)
  {
    return toolkit.prepareImage(img, w, h, o);
  }

  public void print(Graphics g)
  {
    // FIXME
  }

  /**
   * Schedules a timed repaint.
   */
  public void repaint(long tm,
                      int x,
                      int y,
                      int w,
                      int h)
  {
    if( tm <= 0 )
      {
        QtToolkit.repaintThread.queueComponent(this, x, y, w, h);
        return;
      }
    Timer t = new Timer();
    t.schedule(new RepaintTimerTask(this, x, y, w, h), tm);
  }

  /**
   * Update the cursor (note that setCursor is usually not called)
   */
  public void updateCursorImmediately()
  {
    if (owner.getCursor() != null)
      setCursor(owner.getCursor().getType());
  }

  /**
   * Timed repainter
   */
  private class RepaintTimerTask extends TimerTask
  {
    private int x, y, w, h;
    private QtComponentPeer peer;
    RepaintTimerTask(QtComponentPeer peer, int x, int y, int w, int h)
    {
      this.x=x;
      this.y=y;
      this.w=w;
      this.h=h;
      this.peer=peer;
    }
    public void run()
    {
      QtToolkit.repaintThread.queueComponent(peer, x, y, w, h);
    }
  }

  public native Rectangle getBounds();

  public void reparent(ContainerPeer parent)
  {
    if(!(parent instanceof QtContainerPeer))
      throw new IllegalArgumentException("Illegal peer.");
    reparentNative((QtContainerPeer)parent);
  }

  public void setBounds(int x, int y, int width, int height, int z)
  {
    // TODO Auto-generated method stub

  }

  public boolean isReparentSupported()
  {
    return true;
  }

  // What does this do, anyway?
  public void layout()
  {
    // TODO Auto-generated method stub
  }

  public boolean requestFocus(Component lightweightChild, boolean temporary,
                              boolean focusedWindowChangeAllowed,
                              long time, sun.awt.CausedFocusEvent.Cause cause)
  {
    // TODO: Implement this properly and remove the other requestFocus()
    // methods.
    return true;
  }

}
