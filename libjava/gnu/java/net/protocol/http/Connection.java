/* HttpURLConnection.java -- URLConnection class for HTTP protocol
   Copyright (C) 1998, 1999, 2000, 2002, 2003 Free Software Foundation, Inc.

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


package gnu.java.net.protocol.http;

import java.io.BufferedInputStream;
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.net.HttpURLConnection;
import java.net.ProtocolException;
import java.net.Socket;
import java.net.URL;
import java.net.URLConnection;
import java.util.Map;
import java.util.Vector;
import java.util.Hashtable;
import java.util.Enumeration;

/**
 * This subclass of java.net.URLConnection models a URLConnection via
 * the HTTP protocol.
 *
 * Status: Minimal subset of functionality.  Proxies only partially
 * handled; Redirects not yet handled.  FileNameMap handling needs to
 * be considered.  useCaches, ifModifiedSince, and
 * allowUserInteraction need consideration as well as doInput and
 * doOutput.
 * 
 * @author Aaron M. Renn <arenn@urbanophile.com>
 * @author Warren Levy <warrenl@cygnus.com>
 */
public final class Connection extends HttpURLConnection
{
  /**
   * The socket we are connected to
   */
  private Socket socket;
  private static int proxyPort = 80;
  private static boolean proxyInUse = false;
  private static String proxyHost = null;

  static 
  {
    // Recognize some networking properties listed at
    // http://java.sun.com/j2se/1.4/docs/guide/net/properties.html.
    String port = null;
    proxyHost = System.getProperty("http.proxyHost");
    if (proxyHost != null)
      {
	proxyInUse = true;
	if ((port = System.getProperty("http.proxyPort")) != null)
	  {
	    try
	      {
		proxyPort = Integer.parseInt(port);
	      }
	    catch (Throwable t)
	      {
		// Nothing.  
	      }
	  }
      }
  }

  /**
   * The InputStream for this connection.
   */
  private BufferedInputStream inputStream;

  /**
   * This is the object that holds the header field information
   */
  private Hashtable requestProperties = new Hashtable();
  private Hashtable hdrHash = new Hashtable();
  private Vector hdrVec = new Vector();

  /**
   * Calls superclass constructor to initialize
   */
  protected Connection(URL url)
  {
    super(url);
  }

  public void setRequestProperty(String key, String value)
  {
    if (connected)
      throw new IllegalAccessError("Connection already established.");

    requestProperties.put(key, value);
  }

  public String getRequestProperty(String key)
  {
    if (connected)
      throw new IllegalAccessError("Connection already established.");

    return (String) requestProperties.get(key);
  }

  /**
   * Connects to the remote host, sends the request, and parses the reply
   * code and header information returned
   */
  public void connect() throws IOException
  {
    // Call is ignored if already connected.
    if (connected)
      return;

    // Get address and port number.
    int port;
    if (proxyInUse)
      {
	port = proxyPort;
	socket = new Socket(proxyHost, port);
      }
    else
      {
	if ((port = url.getPort()) == -1)
	  port = 80;
	// Open socket and output stream.
	socket = new Socket(url.getHost(), port);
      }

    PrintWriter out = new PrintWriter(socket.getOutputStream());

    // Send request including any request properties that were set.
    out.print(getRequestMethod() + " " + url.getFile() + " HTTP/1.0\r\n");
    out.print("Host: " + url.getHost() + ":" + port + "\r\n");
    Enumeration reqKeys = requestProperties.keys();
    Enumeration reqVals = requestProperties.elements();
    while (reqKeys.hasMoreElements())
      out.print(reqKeys.nextElement() + ": " + reqVals.nextElement() + "\r\n");
    out.print("\r\n");
    out.flush();    
    getHttpHeaders();
    connected = true;
  }

  /**
   * Disconnects from the remote server.
   */
  public void disconnect()
  {
    if (socket != null)
      {
	try
	  {
	    socket.close();
	  }
	catch (IOException e)
	  {
	    // Ignore errors in closing socket.
	  }
	socket = null;
      }
  }

  /**
   * Return a boolean indicating whether or not this connection is
   * going through a proxy
   *
   * @return true if using a proxy, false otherwise
   */
  public boolean usingProxy()
  {
    return proxyInUse;
  }

  public InputStream getInputStream() throws IOException
  {
    if (!connected)
      connect();

    if (!doInput)
      throw new ProtocolException("Can't open InputStream if doInput is false");
    
    return inputStream;
  }

  public OutputStream getOutputStream() throws IOException
  {
    if (!connected)
      connect();

    if (! doOutput)
      throw new ProtocolException("Can't open OutputStream if doOutput is false");
    
    return socket.getOutputStream();
  }

  public String getHeaderField(String name)
  {
    if (!connected)
      try
        {
	  connect();
	}
      catch (IOException x)
        {
	  return null;
	}

    return (String) hdrHash.get(name.toLowerCase());
  }

  public Map getHeaderFields()
  {
    if (!connected)
      try
        {
	  connect();
	}
      catch (IOException x)
        {
	  return null;
	}

    return hdrHash;
  }

  public String getHeaderField(int n)
  {
    if (!connected)
      try
        {
	  connect();
	}
      catch (IOException x)
        {
	  return null;
	}

    if (n < hdrVec.size())
      return getField ((String) hdrVec.elementAt(n));
    return null;
  }

  public String getHeaderFieldKey(int n)
  {
    if (!connected)
      try
        {
	  connect();
	}
      catch (IOException x)
        {
	  return null;
	}

    if (n < hdrVec.size())
      return getKey ((String) hdrVec.elementAt(n));
    return null;
  }

  private String getKey(String str)
  {
    if (str == null)
      return null;
    int index = str.indexOf(':');
    if (index >= 0)
      return str.substring(0, index);
    else
      return null;
  }

  private String getField(String str)
  {
    if (str == null)
      return null;
    int index = str.indexOf(':');
    if (index >= 0)
      return str.substring(index + 1).trim();
    else
      return str;
  }

  /**
   * Read HTTP reply from inputStream.
   */
  private void getHttpHeaders() throws IOException
  {
    // Originally tried using a BufferedReader here to take advantage of
    // the readLine method and avoid the following, but the buffer read
    // past the end of the headers so the first part of the content was lost.
    // It is probably more robust than it needs to be, e.g. the byte[]
    // is unlikely to overflow and a '\r' should always be followed by a '\n',
    // but it is better to be safe just in case.
    inputStream = new BufferedInputStream(socket.getInputStream());

    int buflen = 100;
    byte[] buf = new byte[buflen];
    String line = "";
    boolean gotnl = false;
    byte[] ch = new byte[1];
    ch[0] = (byte) '\n';

    while (true)
      {
	// Check for leftover byte from non-'\n' after a '\r'.
	if (ch[0] != '\n')
	  line = line + '\r' + new String(ch, 0, 1);

	int i;
	// FIXME: This is rather inefficient.
	for (i = 0; i < buflen; i++)
	  {
	    buf[i] = (byte) inputStream.read();
	    if (buf[i] == -1)
	      throw new IOException("Malformed HTTP header");
	    if (buf[i] == '\r')
	      {
	        inputStream.read(ch, 0, 1);
		if (ch[0] == '\n')
		  gotnl = true;
		break;
	      }
	  }
	line = line + new String(buf, 0, i);

	// A '\r' '\n' combo indicates the end of the header entry.
	// If it wasn't found, cycle back through the loop and append
	// to 'line' until one is found.
	if (gotnl)
	  {
	    // A zero length entry signals the end of the headers.
	    if (line.length() == 0)
	      break;

	    // Store the header and reinitialize for next cycle.
	    hdrVec.addElement(line);
	    String key = getKey(line);
	    if (key != null)
	      hdrHash.put(key.toLowerCase(), getField(line));
	    line = "";
	    ch[0] = (byte) '\n';
	    gotnl = false;
	  }
      }
  }
}
