/* AppletContext.java -- Access the applet's runtime environment.
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

import java.awt.Image;
import java.net.URL;
import java.util.Enumeration;

/**
  * This interface allows an applet access to the browser to retrieve
  * additional data files and display documents.  It also allows the
  * applet to find out other applets in the same document.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public interface AppletContext
{
  /**
    * Returns an audio clip from the specified URL.
    *
    * @param url The URL of the audio clip.
    *
    * @return The retrieved audio clip // FIXME: What happens on error?
    */
  AudioClip getAudioClip(URL url);

  /**
    * Returns an image from the specified URL.  Note that the image is not
    * actually retrieved until the applet attempts to display it, so this
    * method returns immediately.
    *
    * @param url The URL of the image.
    *
    * @return The retrieved image.  // FIXME: What happens on eror?
    */
  Image getImage(URL url);

  /**
    * Returns the applet in the document for this object that has the
    * specified name.
    *
    * @param name The applet name.
    *
    * @return The requested applet, or <code>null</code> if an applet with
    * the requested name cannot be found.
    */
  Applet getApplet(String name);

  /**
    * Returns a list of all the applets in the document for this object.
    *
    * @return A list of all the applets in the document for this object.
    */
  Enumeration getApplets();

  /**
    * Displays the web page pointed to by the specified URL in the window
    * for this object.  This page replaces the document that is currently
    * there.
    *
    * @param url The URL of the web page to load.
    */
  void showDocument(URL url);

  /**
    * Displays the web page pointed to be the sepcified URL in the window
    * with the specified name.  The standard names "_top", "_blank",
    * "_parent", and "_self" are allowed.
    *
    * @param url The URL of the web page to load.
    * @param target The target window.
    */
  void showDocument(URL url, String target);

  /**
    * Displays the specified message in the status window if that window
    * exists.
    *
    * @param message The status message.
    */
  void showStatus(String message);

} // interface AppletContext
