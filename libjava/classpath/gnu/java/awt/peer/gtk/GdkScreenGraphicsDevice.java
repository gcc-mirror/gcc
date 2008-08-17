/* GdkScreenGraphicsDevice.java -- information about a screen device
   Copyright (C) 2004, 2005, 2006  Free Software Foundation, Inc.

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


package gnu.java.awt.peer.gtk;

import java.awt.DisplayMode;
import java.awt.Frame;
import java.awt.GraphicsConfiguration;
import java.awt.GraphicsDevice;
import java.awt.Rectangle;
import java.awt.Window;
import java.util.ArrayList;

import gnu.classpath.Configuration;
import gnu.classpath.Pointer;

class GdkScreenGraphicsDevice extends GraphicsDevice
{
  private final int native_state = GtkGenericPeer.getUniqueInteger ();
  
  private Window fullscreenWindow;
  
  private boolean oldWindowDecorationState;
  
  private Rectangle oldWindowBounds;
  
  private Rectangle bounds;
  
  private GdkGraphicsConfiguration[] configurations;
  
  /** The <code>GdkGraphicsEnvironment</code> instance that created this
   * <code>GdkScreenGraphicsDevice</code>. This is only needed for native
   * methods which need to access the 'native_state' field storing a pointer
   * to a GdkDisplay object.
   */ 
  GdkGraphicsEnvironment env;
  
  /** An identifier that is created by Gdk
   */
  String idString;
  
  /** The display modes supported by this <code>GdkScreenGraphicsDevice</code>.
   * If the array is <code>null</code> <code>nativeGetDisplayModes</code> has
   * to be called.
   */
  X11DisplayMode[] displayModes;

  /** The non-changeable display mode of this <code>GdkScreenGraphicsDevice
   * </code>. This field gets initialized by the {@link #init()} method. If it
   * is still <code>null</code> afterwards, the XRandR extension is available
   * and display mode changes are possible. If it is non-null XRandR is not
   * available, no display mode changes are possible and no other native
   * method must be called. 
   */
  DisplayMode fixedDisplayMode;

  /**
   * The pointer to the native screen resource.
   *
   * This field is manipulated by native code. Don't change or remove
   * without adjusting the native code.
   */
  private Pointer screen;

  static
  {
    if (true) // GCJ LOCAL
      {
        System.loadLibrary("gtkpeer");
      }

    GtkToolkit.initializeGlobalIDs();
    initIDs();
  }
  
  static native void initIDs();
  
  GdkScreenGraphicsDevice (GdkGraphicsEnvironment e)
  {
    super();
    env = e;
    
    configurations = new GdkGraphicsConfiguration[1];
    configurations[0] = new GdkGraphicsConfiguration(this);
  }

  /** This method is called from the native side immediately after
   * the constructor is run.
   */
  void init()
  {
    fixedDisplayMode = nativeGetFixedDisplayMode(env);
  }
  
  /** Depending on the availability of the XRandR extension the method returns
   * the screens' non-changeable display mode or null, meaning that XRandR can
   * handle display mode changes.
   */
  native DisplayMode nativeGetFixedDisplayMode(GdkGraphicsEnvironment env);
  
  public int getType ()
  {
    // Gdk manages only raster screens.
    return GraphicsDevice.TYPE_RASTER_SCREEN;
  }

  public String getIDstring ()
  {
    if (idString == null)
      idString = nativeGetIDString();
    
    return idString;
  }
  
  private native String nativeGetIDString(); 

  public GraphicsConfiguration[] getConfigurations ()
  {
    return (GraphicsConfiguration[]) configurations.clone();
  }
  
  public GraphicsConfiguration getDefaultConfiguration ()
  {
    return configurations[0];
  }


  /**
   * Returns the current display mode of this device, or null if unknown.
   *
   * @return the current display mode
   * @see #setDisplayMode(DisplayMode)
   * @see #getDisplayModes()
   * @since 1.4
   */
  public DisplayMode getDisplayMode()
  {
    if (fixedDisplayMode != null)
      return fixedDisplayMode;
    
    synchronized (this)
      {
        if (displayModes == null)
          displayModes = nativeGetDisplayModes(env);
      }

    int index = nativeGetDisplayModeIndex(env);
    int rate = nativeGetDisplayModeRate(env);
    
    return new DisplayMode(displayModes[index].width,
                           displayModes[index].height,
                           DisplayMode.BIT_DEPTH_MULTI,
                           rate);
  }
  
  native int nativeGetDisplayModeIndex(GdkGraphicsEnvironment env);
  
  native int nativeGetDisplayModeRate(GdkGraphicsEnvironment env);
  
  public DisplayMode[] getDisplayModes()
  {
    if (fixedDisplayMode != null)
      return new DisplayMode[] { fixedDisplayMode };
    
    synchronized (this)
      {
        if (displayModes == null)
          displayModes = nativeGetDisplayModes(env);
      }
    
    ArrayList<DisplayMode> list = new ArrayList<DisplayMode>();
    for(int i=0;i<displayModes.length;i++)
      for(int j=0;j<displayModes[i].rates.length;j++)
        list.add(new DisplayMode(displayModes[i].width,
                                 displayModes[i].height,
                                 DisplayMode.BIT_DEPTH_MULTI,
                                 displayModes[i].rates[j]));
    
    return list.toArray(new DisplayMode[list.size()]);
  }
  
  native X11DisplayMode[] nativeGetDisplayModes(GdkGraphicsEnvironment env);

  /**
   * Real fullscreen exclusive mode is not supported.
   *
   * @return <code>false</code>
   * @since 1.4
   */
  public boolean isFullScreenSupported()
  {
    return true;
  }
  
  public boolean isDisplayChangeSupported()
  {
    return fixedDisplayMode == null;
  }

  public void setDisplayMode(DisplayMode dm)
  {
    if (fixedDisplayMode != null)
      throw new UnsupportedOperationException("Cannnot change display mode.");
    
    if (dm == null)
      throw new IllegalArgumentException("DisplayMode must not be null.");
    
    synchronized (this)
      {
        if (displayModes == null)
          displayModes = nativeGetDisplayModes(env);
      }
    
    for (int i=0; i<displayModes.length; i++)
      if (displayModes[i].width == dm.getWidth()
          && displayModes[i].height == dm.getHeight())
        {
          synchronized (this)
          {
            nativeSetDisplayMode(env,
                                 i,
                                 (short) dm.getRefreshRate());
          
            bounds = null;
          }
          
          return;
        }
    
    throw new IllegalArgumentException("Mode not supported by this device.");
  }
  
  native void nativeSetDisplayMode(GdkGraphicsEnvironment env,
                                int index, short rate);
  
  /** A class that simply encapsulates the X11 display mode data.
   */
  static class X11DisplayMode
  {
    short[] rates;
    int width;
    int height;
    
    X11DisplayMode(int width, int height, short[] rates)
    {
      this.width = width;
      this.height = height;
      this.rates = rates;
    }
    
  }
  
  public void setFullScreenWindow(Window w)
  {
    // Bring old fullscreen window back into its original state.
    if (fullscreenWindow != null && w != fullscreenWindow)
      {
        if (fullscreenWindow instanceof Frame)
          {
            // Decoration state can only be switched when the peer is
            // non-existent. That means we have to dispose the 
            // Frame.
            Frame f = (Frame) fullscreenWindow;
            if (oldWindowDecorationState != f.isUndecorated())
              {
                f.dispose();
                f.setUndecorated(oldWindowDecorationState);
              }
          }
        
        fullscreenWindow.setBounds(oldWindowBounds);

        if (!fullscreenWindow.isVisible())
          fullscreenWindow.setVisible(true);
      }
    
    // If applicable remove decoration, then maximize the window and
    // bring it to the foreground.
    if (w != null)
      {
        if (w instanceof Frame)
          {
            Frame f = (Frame) w;
            oldWindowDecorationState = f.isUndecorated();
            if (!oldWindowDecorationState)
              {
                f.dispose();
                f.setUndecorated(true);
              }
          }
        
        oldWindowBounds = w.getBounds();
    
        DisplayMode dm = getDisplayMode();
    
        w.setBounds(0, 0, dm.getWidth(), dm.getHeight());
        
        if (!w.isVisible())
          w.setVisible(true);
        
        w.requestFocus();
        w.toFront();
        
      }
    
    fullscreenWindow = w;
  }
  
  public Window getFullScreenWindow()
  {
   return fullscreenWindow; 
  }

  Rectangle getBounds()
  {
   synchronized(this)
     {
       if (bounds == null)
         bounds = nativeGetBounds();
     }
   
   return bounds;
  }
  
  native Rectangle nativeGetBounds();

}
