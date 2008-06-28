/* XGraphicsEnvironment.java -- Represents the X environment
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package gnu.java.awt.peer.x;

import gnu.java.awt.font.OpenTypeFontPeer;
import gnu.java.awt.java2d.RasterGraphics;
import gnu.x11.Display;

import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.GraphicsDevice;
import java.awt.GraphicsEnvironment;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Locale;
import java.util.Properties;

/**
 * Represents the X environment for AWT.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class XGraphicsEnvironment
  extends GraphicsEnvironment
{

  /**
   * The default graphics device. This is normally the local main X
   * Display, but can be configured to be any X connection.
   */
  private XGraphicsDevice defaultDevice;

  /**
   * All configured devices.
   */
  private XGraphicsDevice[] devices;

  /**
   * Creates a new XGraphicsEnvironment. This loads the configuration if
   * there is one present and initializes the XGraphicsDevices in the
   * environment. If there is no configuration, then there is one
   * default device initialized with the local main X device.
   */
  public XGraphicsEnvironment()
  {
    // Initiliaze the devices.
    Properties props = new Properties();
    File config = new File(System.getProperty("user.home"),
                           ".xawt.properties");

    try
      {
        FileInputStream configIn = new FileInputStream(config);
        props.load(configIn);
        int dev = 1;
        ArrayList deviceList = new ArrayList();
        while (true)
          {
            String propName = "display." + dev;
            String propValue = props.getProperty(propName);
            if (propValue != null)
              {
                Display.Name displayName = new Display.Name(propValue);
                XGraphicsDevice device = new XGraphicsDevice(displayName);
                if (dev == 1)
                  defaultDevice = device;
                deviceList.add(device);
                dev++;
              }
            else
              {
                if (dev == 1)
                  {
                    defaultDevice = initDefaultDevice();
                    deviceList.add(defaultDevice);
                  }
                break;
              }
          }
        devices = (XGraphicsDevice[]) deviceList.toArray
                                      (new XGraphicsDevice[deviceList.size()]);
      }
    catch (FileNotFoundException ex)
      {
        defaultDevice = initDefaultDevice();
        devices = new XGraphicsDevice[]{ defaultDevice };
      }
    catch (IOException ex)
      {
        defaultDevice = initDefaultDevice();
        devices = new XGraphicsDevice[]{ defaultDevice };
      }
    
  }

  /**
   * Helper method that initializes the default device in the case when there
   * is no configuration for the default.
   */
  private XGraphicsDevice initDefaultDevice()
  {
    String display = System.getenv("DISPLAY");
    if (display == null)
      display = ":0.0";
    Display.Name displayName = new Display.Name(display);
    return new XGraphicsDevice(displayName);
  }

  /**
   * Returns all configured screen devices.
   *
   * @return all configured screen devices
   */
  public GraphicsDevice[] getScreenDevices()
  {
    // We return a copy so that nobody can fiddle with our devices.
    XGraphicsDevice[] copy = new XGraphicsDevice[devices.length];
    System.arraycopy(devices, 0, copy, 0, devices.length);
    return copy;
  }

  /**
   * Returns the default screen device.
   *
   * @return the default screen device
   */
  public GraphicsDevice getDefaultScreenDevice()
  {
    return defaultDevice;
  }

  /**
   * Returns a Graphics instance suitable for drawing on top of the
   * BufferedImage.
   *
   * @param image the buffered image to create a graphics for
   *
   * @return a Graphics2D instance for drawing on the BufferedImage
   */
  public Graphics2D createGraphics(BufferedImage image)
  {
    return new RasterGraphics(image.getRaster(), image.getColorModel());
  }

  public Font[] getAllFonts()
  {
    // TODO: Implement this.
    throw new UnsupportedOperationException("Not yet implemented.");
  }

  public String[] getAvailableFontFamilyNames()
  {
    return getAvailableFontFamilyNames(Locale.getDefault());
  }

  public String[] getAvailableFontFamilyNames(Locale l)
  {
    // TODO: This doesn't work when we are using X fonts.
    // Fix this.
    return OpenTypeFontPeer.getAvailableFontFamilyNames(l);
  }

}
