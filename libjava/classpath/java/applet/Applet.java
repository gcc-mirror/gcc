/* Applet.java -- Java base applet class
   Copyright (C) 1999, 2002, 2004, 2005  Free Software Foundation, Inc.

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


package java.applet;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.GraphicsEnvironment;
import java.awt.HeadlessException;
import java.awt.Image;
import java.awt.Panel;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Locale;

import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;
import javax.accessibility.AccessibleState;
import javax.accessibility.AccessibleStateSet;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.Clip;
import javax.sound.sampled.LineUnavailableException;
import javax.sound.sampled.UnsupportedAudioFileException;

/**
 * This is the base applet class.  An applet is a Java program that
 * runs inside a web browser or other applet viewer in a restricted
 * environment.
 *
 * <p>To be useful, a subclass should override at least start(). Also useful
 * are init, stop, and destroy for control purposes, and getAppletInfo and
 * getParameterInfo for descriptive purposes.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @author Eric Blake (ebb9@email.byu.edu)
 * @since 1.0
 * @status updated to 1.4
 */
public class Applet extends Panel
{
  /**
   * Compatible with JDK 1.0+.
   */
  private static final long serialVersionUID = -5836846270535785031L;

  /** The applet stub for this applet. */
  private transient AppletStub stub;

  /** Some applets call setSize in their constructors.  In that case,
      these fields are used to store width and height values until a
      stub is set. */
  private transient int width;
  private transient int height;

  /**
   * The accessibility context for this applet.
   *
   * @serial the accessibleContext for this
   * @since 1.2
   */
  private AccessibleContext accessibleContext;

  /**
   * Default constructor for subclasses.
   *
   * @throws HeadlessException if in a headless environment
   */
  public Applet()
  {
    if (GraphicsEnvironment.isHeadless())
      throw new HeadlessException();
  }

  /**
   * The browser calls this method to set the applet's stub, which is the
   * low level interface to the browser. Manually setting this to null is
   * asking for problems down the road.
   *
   * @param stub the applet stub for this applet
   */
  public final void setStub(AppletStub stub)
  {
    this.stub = stub;

    if (width != 0 && height != 0)
      stub.appletResize (width, height);
  }

  /**
   * Tests whether or not this applet is currently active. An applet is active
   * just before the browser invokes start(), and becomes inactive just
   * before the browser invokes stop().
   *
   * @return <code>true</code> if this applet is active
   */
  public boolean isActive()
  {
    return stub.isActive();
  }

  /**
   * Returns the basename URL of the document this applet is embedded in. This
   * is everything up to the final '/'.
   *
   * @return the URL of the document this applet is embedded in
   * @see #getCodeBase()
   */
  public URL getDocumentBase()
  {
    return stub.getDocumentBase();
  }

  /**
   * Returns the URL of the code base for this applet.
   *
   * @return the URL of the code base for this applet
   */
  public URL getCodeBase()
  {
    return stub.getCodeBase();
  }

  /**
   * Returns the value of the specified parameter that was specified in
   * the <code>&lt;APPLET&gt;</code> tag for this applet.
   *
   * @param name the parameter name
   * @return the parameter value, or null if the parameter does not exist
   * @throws NullPointerException if name is null
   */
  public String getParameter(String name)
  {
    return stub.getParameter(name);
  }

  /**
   * Returns the applet context for this applet.
   *
   * @return the applet context for this applet
   */
  public AppletContext getAppletContext()
  {
    return stub.getAppletContext();
  }

  /**
   * Requests that the applet window for this applet be resized.
   *
   * @param width the new width in pixels
   * @param height the new height in pixels
   */
  public void resize(int width, int height)
  {
    if (stub == null)
      {
        this.width = width;
        this.height = height;
      }
    else
      stub.appletResize(width, height);
  }

  /**
   * Requests that the applet window for this applet be resized.
   *
   * @param dim the requested dimensions
   * @throws NullPointerException if dim is null
   */
  public void resize(Dimension dim)
  {
    resize(dim.width, dim.height);
  }

  /**
   * Displays the specified message in the status window if that window
   * exists.
   *
   * @param message the status message, may be null
   */
  public void showStatus(String message)
  {
    getAppletContext().showStatus(message);
  }

  /**
   * Returns an image from the specified URL.  Note that the image is not
   * actually retrieved until the applet attempts to display it, so this
   * method returns immediately.
   *
   * @param url the URL of the image
   * @return the retrieved image
   * @throws NullPointerException if url is null
   */
  public Image getImage(URL url)
  {
    return getAppletContext().getImage(url);
  }

  /**
   * Returns an image from the specified absolute URL, and relative path
   * from that URL.  Note that the image is not actually retrieved until the
   * applet attempts to display it, so this method returns immediately.
   * This calls <code>getImage(new URL(url, name))</code>, but if building
   * the new URL fails, this returns null.
   *
   * @param url the base URL of the image
   * @param name the name of the image relative to the URL
   * @return the retrieved image, or null on failure
   * @see #getImage(URL)
   */
  public Image getImage(URL url, String name)
  {
    try
      {
        return getImage(new URL(url, name));
      }
    catch (MalformedURLException e)
      {
        return null;
      }
  }

  /**
   * Returns an audio clip from the specified URL. This clip is not tied to
   * any particular applet.
   *
   * @param url the URL of the audio clip
   * @return the retrieved audio clip
   * @throws NullPointerException if url is null
   * @see #getAudioClip(URL)
   * @since 1.2
   */
  public static final AudioClip newAudioClip(URL url)
  {
    return new URLAudioClip(url);
  }

  /**
   * Returns an audio clip from the specified URL. Note that the clip is not
   * actually retrieved until the applet attempts to play it, so this method
   * returns immediately.
   *
   * @param url the URL of the audio clip
   * @return the retrieved audio clip
   * @throws NullPointerException if url is null
   */
  public AudioClip getAudioClip(URL url)
  {
    return getAppletContext().getAudioClip(url);
  }

  /**
   * Returns an audio clip from the specified absolute URL, and relative path
   * from that URL.  Note that the clip is not actually retrieved until the
   * applet attempts to play it, so this method returns immediately. This
   * calls <code>getAudioClip(new URL(url, name))</code>, but if building
   * the new URL fails, this returns null.
   *
   * @param url the base URL of the audio clip
   * @param name the name of the clip relative to the URL
   * @return the retrieved audio clip, or null on failure
   * @see #getAudioClip(URL)
   */
  public AudioClip getAudioClip(URL url, String name)
  {
    try
      {
        return getAudioClip(new URL(url, name));
      }
    catch (MalformedURLException e)
      {
        return null;
      }
  }

  /**
   * Returns a descriptive string with applet defined information.  The
   * implementation in this class returns <code>null</code>, so subclasses
   * must override to return information.
   *
   * @return a string describing the author, version, and applet copyright
   */
  public String getAppletInfo()
  {
    return null;
  }

  /**
   * Returns the locale for this applet, if it has been set.  If no applet
   * specific locale has been set, the default locale is returned.
   *
   * @return the locale for this applet
   * @see Component#setLocale(Locale)
   * @since 1.1
   */
  public Locale getLocale()
  {
    return super.getLocale();
  }

  /**
   * Returns a list of parameters this applet supports.  Each element of
   * the outer array is an array of three strings with the name of the
   * parameter, the data type or valid values, and a description.  This
   * method is optional and the default implementation returns null.
   *
   * @return the list of parameters supported by this applet
   */
  public String[][] getParameterInfo()
  {
    return null;
  }

  /**
   * Loads and plays the audio clip pointed to by the specified URL. This does
   * nothing if the URL does not point to a valid audio clip.
   *
   * @param url the URL of the audio clip
   * @throws NullPointerException if url is null
   * @see #getAudioClip(URL)
   */
  public void play(URL url)
  {
    AudioClip ac = getAudioClip(url);
    try
      {
        ac.play();
      }
    catch (Exception ignored)
      {
      }
  }

  /**
   * Loads and plays the audio clip pointed to by the specified absolute URL,
   * and relative path from that URL. This does nothing if the URL cannot be
   * constructed, or if it does not point to a valid audio clip.
   *
   * @param url the base URL of the audio clip
   * @param name the name of the audio clip relative to the URL
   * @see #getAudioClip(URL, String)
   * @see #play(URL)
   */
  public void play(URL url, String name)
  {
    try
      {
        getAudioClip(url, name).play();
      }
    catch (Exception ignored)
      {
      }
  }

  /**
   * This method is called when the applet is first loaded, before start().
   * The default implementation does nothing; override to do any one-time
   * initialization.
   *
   * @see #start()
   * @see #stop()
   * @see #destroy()
   */
  public void init()
  {
  }

  /**
   * This method is called when the applet should start running.  This is
   * normally each time a web page containing it is loaded.  The default
   * implemention does nothing; override for your applet to be useful.
   *
   * @see #init()
   * @see #stop()
   * @see #destroy()
   */
  public void start()
  {
  }

  /**
   * This method is called when the applet should stop running.  This is
   * normally when the next web page is loaded.  The default implementation
   * does nothing; override for your applet to stop using resources when
   * it is no longer visible, but may be restarted soon.
   *
   * @see #init()
   * @see #start()
   * @see #destroy()
   */
  public void stop()
  {
  }

  /**
   * This method is called when the applet is being unloaded.  The default
   * implementation does nothing; override for your applet to clean up
   * resources on exit.
   *
   * @see #init()
   * @see #start()
   * @see #stop()
   */
  public void destroy()
  {
  }

  /**
   * Gets the AccessibleContext associated with this applet, creating one if
   * necessary. This always returns an instance of {@link AccessibleApplet}.
   *
   * @return the accessibility context of this applet
   * @since 1.3
   */
  public AccessibleContext getAccessibleContext()
  {
    if (accessibleContext == null)
      accessibleContext = new AccessibleApplet();
    return accessibleContext;
  }

  /**
   * Read an applet from an object stream. This checks for a headless
   * environment, then does the normal read.
   *
   * @param s the stream to read from
   * @throws ClassNotFoundException if a class is not found
   * @throws IOException if deserialization fails
   * @throws HeadlessException if this is a headless environment
   * @see GraphicsEnvironment#isHeadless()
   * @since 1.4
   */
  private void readObject(ObjectInputStream s)
    throws ClassNotFoundException, IOException
  {
    if (GraphicsEnvironment.isHeadless())
      throw new HeadlessException();
    s.defaultReadObject();
  }

  /**
   * This class provides accessibility support for Applets, and is the
   * runtime type returned by {@link #getAccessibleContext()}.
   *
   * @author Eric Blake (ebb9@email.byu.edu)
   * @since 1.3
   */
  protected class AccessibleApplet extends AccessibleAWTPanel
  {
    /**
     * Compatible with JDK 1.4+.
     */
    private static final long serialVersionUID = 8127374778187708896L;

    /**
     * The default constructor.
     */
    protected AccessibleApplet()
    {
    }

    /**
     * Get the role of this accessible object, a frame.
     *
     * @return the role of the object
     * @see AccessibleRole#FRAME
     */
    public AccessibleRole getAccessibleRole()
    {
      return AccessibleRole.FRAME;
    }

    /**
     * Get the state set of this accessible object. In addition to the default
     * states of a Component, the applet can also be active.
     *
     * @return the role of the object
     * @see AccessibleState
     */
    public AccessibleStateSet getAccessibleStateSet()
    {
      AccessibleStateSet s = super.getAccessibleStateSet();
      if (isActive())
        s.add(AccessibleState.ACTIVE);
      return s;
    }
  } // class AccessibleApplet

  private static class URLAudioClip implements AudioClip
  {
    // The URL we will try to play.
    // This is null if we have already tried to create an
    // audio input stream from this URL.
    private URL url;

    // The real audio clip.  This is null before the URL is read,
    // and might be null afterward if we were unable to read the URL
    // for some reason.
    private Clip clip;

    public URLAudioClip(URL url)
    {
      this.url = url;
    }

    private synchronized Clip getClip()
    {
      if (url == null)
        return clip;
      try
        {
          clip = AudioSystem.getClip();
          clip.open(AudioSystem.getAudioInputStream(url));
        }
      catch (LineUnavailableException _)
        {
          // Ignore.
        }
      catch (IOException _)
        {
          // Ignore.
        }
      catch (UnsupportedAudioFileException _)
        {
          // Ignore.
        }
      url = null;
      return clip;
    }

    public void loop()
    {
      Clip myclip = getClip();
      if (myclip != null)
        myclip.loop(Clip.LOOP_CONTINUOUSLY);
    }

    public void play()
    {
      Clip myclip = getClip();
      if (myclip != null)
        myclip.start();
    }

    public void stop()
    {
      Clip myclip = getClip();
      if (myclip != null)
        {
          myclip.stop();
          myclip.setFramePosition(0);
        }
    }
  }
} // class Applet
