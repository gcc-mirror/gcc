/* gnu.java.beans.DummyAppletContext
   Copyright (C) 2004, 2005 Free Software Foundation, Inc.

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


package gnu.java.beans;

import java.applet.Applet;
import java.applet.AppletContext;
import java.applet.AudioClip;
import java.awt.Image;
import java.awt.Toolkit;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.Collections;
import java.util.Enumeration;
import java.util.Iterator;

/** A placeholder <code>AppletContext</code> implementation that does nothing.
 *
 * <p>This is the default implementation for GNU Classpath and is used for <code>Applet</code>
 * beans being created with {@link java.beans.Beans.instantiate}.</p>
 *
 * <p>It has no functionality in order to allow it to be used without any dependencies
 * (e.g. sound, network access, ...).</p>
 *
 * @author Robert Schuster
 */
class DummyAppletContext implements AppletContext
{
  private static final Enumeration EMPTY_ENUMERATION = Collections.enumeration(Collections.EMPTY_SET);
  private static final AudioClip DUMMY_CLIP = new DummyAudioClip();

  DummyAppletContext()
  {
  }

  /** Implementation is VM neutral and returns a dummy {@link AudioClip} instance
   * for every URL that returns a non-<code>null</code> object on
   * <code>URL.openConnection()</code>.
   *
   * @see java.applet.AppletContext#getAudioClip(java.net.URL)
   *
   * FIXME: When Java Sound API (javax.sound) is included in Classpath or URL is able to handle
   * sampled sound this should be adjusted.
   */
  public AudioClip getAudioClip(URL url)
  {
    try
      {
	return (url.openConnection() != null ? DUMMY_CLIP : null);
      }
    catch (IOException ioe)
      {
	return null;
      }
  }

  /** Loads the <code>Image</code> instance by delegating to
   * {@link java.awt.Toolkit.createImage(URL) }.
   *
   * @see java.applet.AppletContext#getImage(java.net.URL)
   * @see java.awt.Toolkit#createImage(java.net.URL)
   */
  public Image getImage(URL url)
  {
    return Toolkit.getDefaultToolkit().createImage(url);
  }

  /** Returns <code>null</code> for every argument.
   *
   * @see java.applet.AppletContext#getApplet(java.lang.String)
   */
  public Applet getApplet(String name)
  {
    return null;
  }

  /** Returns always an empty <code>Enumeration</code>.
   *
   * @see java.applet.AppletContext#getApplets()
   */
  public Enumeration getApplets()
  {
    return EMPTY_ENUMERATION;
  }

  /** Does nothing.
   *
   * @see java.applet.AppletContext#showDocument(java.net.URL)
   */
  public void showDocument(URL url)
  {
  }

  /** Does nothing.
   *
   * @see java.applet.AppletContext#showDocument(java.net.URL, java.lang.String)
   */
  public void showDocument(URL url, String target)
  {
  }

  /** Does nothing.
   *
   * @see java.applet.AppletContext#showStatus(java.lang.String)
   */
  public void showStatus(String message)
  {
  }

  /** Does nothing.
   *
   * @see java.applet.AppletContext#setStream(java.lang.String, java.io.InputStream)
   */
  public void setStream(String key, InputStream stream)
    throws IOException
  {
    throw new IOException("Dummy implementation imposes zero InputStream associations.");
  }

  /** Returns <code>null</code> for every argument.
   *
   * @see java.applet.AppletContext#getStream(java.lang.String)
   */
  public InputStream getStream(String key)
  {
    return null;
  }

  /** Returns always an empty iterator.
   *
   * @see java.applet.AppletContext#getStreamKeys()
   */
  public Iterator getStreamKeys()
  {
    return Collections.EMPTY_SET.iterator();
  }

  /** Dummy <code>AudioClip</code> implementation that does nothing but
   * preventing <code>NullPointerException</code>S being thrown in programs
   * that expect a valid <code>AudioClip</code> instance to be returned by
   * their Applet.
   *
   * @author Robert Schuster
   */
  static class DummyAudioClip implements AudioClip
  {
    public void play()
    {
    }

    public void stop()
    {
    }

    public void loop()
    {
    }

    public String toString()
    {
      return "DummyAudioClip never plays anything - implement javax.sound and make us happy :)";
    }
  }
}
