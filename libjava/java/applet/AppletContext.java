/* AppletContext.java -- access the applet's runtime environment
   Copyright (C) 1999, 2002, 2004  Free Software Foundation, Inc.

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

import java.awt.Image;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.Enumeration;
import java.util.Iterator;

/**
 * This interface allows an applet access to the browser to retrieve
 * additional data files and display documents.  It also allows the
 * applet to find out other applets in the same document.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @since 1.0
 * @status updated to 1.4
 */
public interface AppletContext
{
  /**
   * Returns an audio clip from the specified URL.
   *
   * @param url the URL of the audio clip
   * @return the retrieved audio clip
   * @throws NullPointerException if url is null
   */
  AudioClip getAudioClip(URL url);

  /**
   * Returns an image from the specified URL.  Note that the image is not
   * actually retrieved until the applet attempts to display it, so this
   * method returns immediately.
   *
   * @param url the absolute URL of the image
   * @return the retrieved image
   * @throws NullPointerException if url is null
   */
  Image getImage(URL url);

  /**
   * Returns the applet in the document for this object that has the
   * specified name.
   *
   * @param name the applet name
   * @return the requested applet, or <code>null</code> if not found
   */
  Applet getApplet(String name);

  /**
   * Returns a list of all the applets in the document for this object.
   *
   * @return a list of all the applets
   */
  Enumeration getApplets();

  /**
   * Displays the web page pointed to by the specified URL in the window
   * for this object.  This page replaces the document that is currently
   * there.
   *
   * @param url the URL of the web page to load; unspecified on an error
   */
  void showDocument(URL url);

  /**
   * Displays the web page pointed to be the sepcified URL in the window
   * with the specified name.  The standard names "_top", "_blank",
   * "_parent", and "_self" are allowed. An applet viewer may disregard
   * this request.
   *
   * @param url the URL of the web page to load
   * @param target the target window
   */
  void showDocument(URL url, String target);

  /**
   * Displays the specified message in the status window if that window
   * exists.
   *
   * @param message the status message, may be null
   */
  void showStatus(String message);

  /**
   * Associate a stream to a key for this applet context, possibly replacing
   * the old value. Stream associations are local to the applet context, for
   * security purposes.
   *
   * @param key the key to associate with
   * @param stream the stream value to tie to the key, or null to remove
   * @throws IOException if the stream is too large
   * @since 1.4
   */
  void setStream(String key, InputStream stream) throws IOException;

  /**
   * Return the stream associated with a given key in this applet context, or
   * null if nothing is associated. Stream associations are local to the
   * applet context, for security purposes.
   *
   * @param key the key to look up
   * @return the associated stream, or null
   * @since 1.4
   */
  InputStream getStream(String key);

  /**
   * Iterate over all keys that have associated streams. Stream associated
   * are local to the applet context, for security purposes.
   *
   * @return an iterator over the association keys
   * @since 1.4
   */
  Iterator getStreamKeys();
} // interface AppletContext
