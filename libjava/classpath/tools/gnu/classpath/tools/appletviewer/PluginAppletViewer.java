/* PluginAppletViewer.java -- manages embeddable applet windows
   Copyright (C) 2003, 2006  Free Software Foundation, Inc.

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

package gnu.classpath.tools.appletviewer;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.net.MalformedURLException;
import java.nio.charset.Charset;
import java.util.HashMap;


/**
 * PluginAppletViewer communicates through pipes with a web browser
 * plugin.  A PluginAppletViewer manages applet windows that may be
 * embedded into web pages.
 */
class PluginAppletViewer
{
  // A mapping of instance IDs to PluginAppletWindows.
  static HashMap appletWindows = new HashMap ();

  private static BufferedReader pluginInputStream;
  private static BufferedWriter pluginOutputStream;

  static void start(InputStream inputStream, OutputStream outputStream)
    throws MalformedURLException, IOException
  {
    // Set up input and output pipes.  Use UTF-8 encoding.
    pluginInputStream =
      new BufferedReader(new InputStreamReader(inputStream,
                                               Charset.forName("UTF-8")));
    pluginOutputStream =
      new BufferedWriter(new OutputStreamWriter(outputStream,
                                                Charset.forName("UTF-8")));

    write("running");

    // Read first message.
    String message = read();

    PluginAppletWindow currentWindow = null;

    while (true)
      {
	if (message.startsWith("instance"))
	  {
	    // Read applet instance identifier.
	    String key = message.substring(9);

	    if (appletWindows.get(key) == null)
	      appletWindows.put(key, new PluginAppletWindow());

	    currentWindow = (PluginAppletWindow) appletWindows.get(key);
	  }
	else if (message.startsWith("tag"))
	  {
	    int pos = message.indexOf(' ', 4);
	    String documentbase = message.substring(4, pos);
        String tag = message.substring(pos + 1);
        currentWindow.setParser(tag, documentbase);
	  }
	else if (message.startsWith("handle"))
	  {
	    long handle = Long.parseLong(message.substring(7));

	    currentWindow.setHandle(handle);
	  }
	else if (message.startsWith("width"))
	  {
	    int width = Integer.parseInt(message.substring(6));

	    currentWindow.setSize(width, currentWindow.getHeight());
	  }
	else if (message.startsWith("height"))
	  {
	    int height = Integer.parseInt(message.substring(7));

	    currentWindow.setSize(currentWindow.getWidth(), height);
	  }
	else if (message.startsWith("destroy"))
	  {
	    appletWindows.remove(currentWindow);
	    currentWindow.dispose();
	  }

	// Read next message.
	message = read();
      }
  }

  /**
   * Write string to plugin.
   * 
   * @param message the message to write
   *
   * @exception IOException if an error occurs
   */
  static void write(String message) throws IOException
  {
    pluginOutputStream.write(message, 0, message.length());
    pluginOutputStream.newLine();
    pluginOutputStream.flush();

    System.err.println
      ("  " + Messages.getString("PluginAppletViewer.AppletViewerWrote")
       + message);
  }

  /**
   * Read string from plugin.
   *
   * @return the read string
   *
   * @exception IOException if an error occurs
   */
  static String read() throws IOException
  {
    String message = pluginInputStream.readLine();

    System.err.println
      ("  " + Messages.getString("PluginAppletViewer.AppletViewerRead")
       + message);

    if (message == null || message.equals("shutdown"))
      {
	// Close input/output channels to plugin.
	pluginInputStream.close();
	pluginOutputStream.close();

	System.err.println
          (Messages.getString("PluginAppletViewer.AppletViewerExiting"));

	System.exit(0);
      }

    return message;
  }
}
