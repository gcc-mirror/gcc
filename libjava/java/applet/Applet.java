/* Applet.java -- Java base applet class
   Copyright (C) 1999 Free Software Foundation, Inc.
 
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


package java.applet;

import java.awt.Dimension;
import java.awt.Image;
import java.net.URL;
import java.util.Locale;

/**
  * This is the base applet class.  An applet is a Java program that
  * runs inside a web browser or other applet viewer in a restricted
  * environment.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public class Applet extends java.awt.Panel implements java.io.Serializable
{
  // The applet stub for this applet
  private AppletStub stub;

  /**
    * Default constructor for subclasses.
    */
  public Applet() {}

  /**
    * Returns the URL of the document this applet is embedded in.
    *
    * @return The URL of the document this applet is embedded in.
    */
  public URL getDocumentBase()
  {
    return (stub.getDocumentBase ());
  }

  /**
    * Returns the URL of the code base for this applet.
    *
    * @return The URL of the code base for this applet.
    */
  public URL getCodeBase()
  {
    return (stub.getCodeBase ());
  }

  /**
    * Returns the value of the specified parameter that was specified in 
    * the &lt;APPLET&gt; tag for this applet.
    *
    * @param name The parameter name.
    *
    * @param value The parameter value, or <code>null</code> if the parameter
    * does not exist.
    */
  public String getParameter(String name)
  {
    return (stub.getParameter (name));
  }

  /**
    * Returns the applet context for this applet.
    *
    * @return The applet context for this applet.
    */
  public AppletContext getAppletContext()
  {
    return (stub.getAppletContext ());
  }

  /**
    * Tests whether or not this applet is currently active.
    *
    * @return <code>true</code> if this applet is active, <code>false</code>
    * otherwise.
    */
  public boolean isActive()
  {
    return (stub.isActive ());
  }

  /**
    * Requests that the applet window for this applet be resized.
    *
    * @param width The new width in pixels.
    * @param height The new height in pixels.
    */
  public void resize(int width, int height)
  {
    stub.appletResize (width, height);
  }

  /**
    * Requests that the applet window for this applet be resized.
    *
    * @param dim The <code>Dimension</code> object with the requested
    * width and height.
    */
  public void resize(Dimension dim)
  {
    resize (dim.width, dim.height);
  }

  /**
    * Returns an audio clip from the specified URL.
    *
    * @param url The URL of the audio clip.
    *
    * @return The retrieved audio clip. 
    */
  public AudioClip getAudioClip(URL url)
  {
    return (getAppletContext ().getAudioClip (url));
  }

  /**
    * Returns an audio clip from the specified URL and name
    *
    * @param url The base URL of the audio clip.
    * @param name The name of the clip relative to the URL.
    *
    * @return The retrieved audio clip. 
    */
  public AudioClip getAudioClip(URL url, String name)
  {
    try
      {
        return (getAppletContext ().getAudioClip (new URL (url.toExternalForm()
                                                	   + name)));
      }
    catch(Exception e)
      {
        return (getAudioClip (url));
      }
  }

  /**
    * Loads and plays the audio clip pointed to by the specified URL.
    *
    * @param The URL of the audio clip.
    */
  public void play (URL url)
  {
    getAudioClip (url).play ();
  }

  /**
    * Loads and plays the audio clip pointed to by the specified URL.
    *
    * @param The base URL of the audio clip.
    * @param name The name of the audio clip relative to the URL.
    */
  public void play (URL url, String name)
  {
    getAudioClip (url, name).play ();
  }

  /**
    * Returns an image from the specified URL.  Note that the image is not
    * actually retrieved until the applet attempts to display it, so this
    * method returns immediately.
    *
    * @param url The URL of the image.
    *
    * @return The retrieved image.
    */
  public Image getImage(URL url)
  {
    return (getAppletContext ().getImage (url));
  }

  /**
    * Returns an image from the specified URL.  Note that the image is not
    * actually retrieved until the applet attempts to display it, so this
    * method returns immediately.
    *
    * @param url The base URL of the image.
    * @param name The name of the image relative to the URL.
    *
    * @return The retrieved image.
    */
  public Image getImage(URL url, String name)
  {
    try
      {
        return (getAppletContext ().getImage (new URL (url.toExternalForm()
                                        	       + name)));
      }
    catch(Exception e)
      {
        return (getImage (url));
      }
  }

  /**
    * Returns the locale for this applet, if it has been set.  If no applet
    * specific locale has been set, the default locale is returned.
    *
    * @return The locale for this applet.
    */
  public Locale getLocale()
  {
    return (super.getLocale ());
  }

  /**
    * Returns a descriptive string with applet defined information.  The
    * implementation in this class returns <code>null</code>.  Applets who
    * wish to return this information should override.
    *
    * @return A string describing the applet.
    */
  public String getAppletInfo()
  {
    return (null);
  }

  /**
    * Returns a list of parameters this applet supports.  Each element of
    * the array is a list of three strings with the name of the parameter,
    * the data type or valid values, and a description.  This method is
    * optional and the default implementation returns <code>null</code>.
    *
    * @return The list of parameters supported by this applet.
    */
  public String[][] getParameterInfo()
  {
    return (null);
  }

  /**
    * This method is called when the applet is first loaded.  The default
    * implementation does nothing.  Applets that wish to do one time
    * initialization should override.
    */
  public void init() {}

  /**
    * This method is called when the applet is being unloaded.  The default
    * implementation does nothing.  Applets that need to clean up resources
    * on exit should override.
    */
  public void destroy() {}

  /**
    * This method is called when the applet should start running.  This is
    * normally each time a web page containing it is loaded.  The default
    * implemention does nothing.  Subclasses should override.
    */
  public void start() {}

  /**
    * This method is called when the applet should stop running.  This is
    * normally when the next web page is loaded.  The default implementation
    * does nothing.
    */
  public void stop() {}

  /**
    * The browser calls this method to set the applet's stub, which is the
    * low level interface to the browser.
    *
    * @param stub The applet stub for this applet.
    */
  public final void setStub (AppletStub stub)
  {
    this.stub = stub;
  }

} // class Applet

