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
import java.io.BufferedOutputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.net.HttpURLConnection;
import java.net.ProtocolException;
import java.net.Socket;
import java.net.URL;
import java.net.URLConnection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import gnu.java.net.HeaderFieldHelper;

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
  private DataInputStream inputStream;

  /**
   * The OutputStream for this connection
   */
  private OutputStream outputStream;

  /**
   * bufferedOutputStream is a buffer to contain content of the HTTP request,
   * and will be written to outputStream all at once
   */
  private ByteArrayOutputStream bufferedOutputStream;

  /**
   * This object holds the request properties.
   */
  private HashMap requestProperties = new HashMap();

  /**
   * This is the object that holds the header field information
   */
  private HeaderFieldHelper headers = new HeaderFieldHelper();

  /**
   * Calls superclass constructor to initialize
   */
  protected Connection(URL url)
  {
    super(url);

    /* Set up some variables */
    doOutput = false;
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

    inputStream =
      new DataInputStream(new BufferedInputStream(socket.getInputStream()));
    outputStream = new BufferedOutputStream (socket.getOutputStream());

    sendRequest();
    receiveReply();

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
   * Write HTTP request header and content to outputWriter.
   */
  void sendRequest() throws IOException
  {
    // Create PrintWriter for easier sending of headers.
    PrintWriter outputWriter =
      new PrintWriter(new OutputStreamWriter(outputStream, "8859_1")); 
    
    // Send request including any request properties that were set.
    outputWriter.print (getRequestMethod() + " " + url.getFile()
                        + " HTTP/1.1\r\n");

    // Set additional HTTP headers.
    if (getRequestProperty ("Host") == null)
      setRequestProperty ("Host", url.getHost());
    
    if (getRequestProperty ("Connection") == null)
      setRequestProperty ("Connection", "Close");
    
    if (getRequestProperty ("user-agent") == null)
      setRequestProperty ("user-agent", "gnu-libgcj/"
                          + System.getProperty ("java.vm.version"));
    
    if (getRequestProperty ("accept") == null)
      setRequestProperty ("accept", "*/*");
    
    if (getRequestProperty ("Content-type") == null)
      setRequestProperty ("Content-type", "application/x-www-form-urlencoded");

    // Set correct content length.
    if (bufferedOutputStream != null)
      setRequestProperty ("Content-length", String.valueOf (bufferedOutputStream.size()));

    // Write all req_props name-value pairs to the output writer.
    Iterator itr = getRequestProperties().entrySet().iterator();

    while (itr.hasNext())
      {
        Map.Entry e = (Map.Entry) itr.next();
        outputWriter.print (e.getKey() + ": " + e.getValue() + "\r\n");
      }

    // One more CR-LF indicates end of header.
    outputWriter.print ("\r\n");
    outputWriter.flush();

    // Write content
    if (bufferedOutputStream != null)
      {
	bufferedOutputStream.writeTo (outputStream);
	outputStream.flush();
      }
  }

  /**
   * Read HTTP reply from inputStream.
   */
  private void receiveReply() throws IOException
  {
    // Parse the reply
    String line = inputStream.readLine();
    String saveline = line;
    int idx = line.indexOf (" ");

    if ((idx == -1)
        || (line.length() < (idx + 6)))
      throw new IOException ("Server reply was unparseable: " + saveline);

    headers.addHeaderField (null, line);

    line = line.substring (idx + 1);
    String code = line.substring (0, 3);
    
    try
      {
        responseCode = Integer.parseInt (code);
      }
    catch (NumberFormatException e)
      {
        throw new IOException ("Server reply was unparseable: " + saveline);
      }
    
    responseMessage = line.substring (4);

    // Now read the header lines
    String key = null, value = null;
    
    while (true)
      {
        line = inputStream.readLine();
        
        if (line.equals(""))
          break;

        // Check for folded lines
        if (line.startsWith (" ")
            || line.startsWith("\t"))
          {
            // Trim off leading space
            do
              {
                if (line.length() == 1)
                  throw new IOException("Server header lines were unparseable: "
                                        + line);

                line = line.substring (1);
              }
            while (line.startsWith(" ")
                   || line.startsWith("\t"));

            value = value + " " + line;
          }
        else 
          {
            if (key != null)
              {
                headers.addHeaderField (key.toLowerCase(), value);
                key = null;
                value = null;
              }

            // Parse out key and value
            idx = line.indexOf (":");
            if ((idx == -1)
                || (line.length() < (idx + 2)))
              throw new IOException ("Server header lines were unparseable: "
                                     + line);

            key = line.substring (0, idx);
            value = line.substring (idx + 1);

            // Trim off leading space
            while (value.startsWith (" ")
                   || value.startsWith ("\t"))
              {
                if (value.length() == 1)
                  throw new IOException ("Server header lines were unparseable: "
                                         + line);

                value = value.substring (1);
              }
          }
      }
    
    if (key != null)
      {
        headers.addHeaderField (key.toLowerCase(), value.toLowerCase());
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

  /**
   * Returns an InputStream for reading from this connection.  This stream
   * will be "queued up" for reading just the contents of the requested file.
   * Overrides URLConnection.getInputStream()
   *
   * @return An InputStream for this connection.
   *
   * @exception IOException If an error occurs
   */
  public InputStream getInputStream() throws IOException
  {
    if (!connected)
      connect();

    if (!doInput)
      throw new ProtocolException("Can't open InputStream if doInput is false");
    
    return inputStream;
  }

  /**
   * Returns on OutputStream for writing to this connection.
   *
   * @return An OutputStream for this connection.
   *
   * @exception IOException If an error occurs
   */
  public OutputStream getOutputStream() throws IOException
  {
    if (connected)
      throw new ProtocolException
	("You cannot get an output stream for an existing http connection");

    if (!doOutput)
      throw new ProtocolException
        ("Want output stream while haven't setDoOutput(true)");
    
    if (bufferedOutputStream == null)
      bufferedOutputStream = new ByteArrayOutputStream (256); //default is too small
    
    return bufferedOutputStream;
  }

  /**
   * Overrides java.net.HttpURLConnection.setRequestMethod() in order to
   * restrict the available methods to only those we support.
   *
   * @param method The RequestMethod to use
   *
   * @exception ProtocolException If the specified method is not valid
   */
  public void setRequestMethod (String method) throws ProtocolException
  {
    method = method.toUpperCase();
    
    if (method.equals("GET")
        || method.equals("HEAD")
        || method.equals("POST"))
      super.setRequestMethod (method);
    else
      throw new ProtocolException ("Unsupported or unknown request method " +
                                   method);
  }

  public void addRequestProperty(String key, String value)
  {
    if (connected)
      throw new IllegalStateException("Already connected");
    
    String old = (String) requestProperties.put(key, value);

    if (old != null)
      requestProperties.put(key, old + "," + value);
  }

  public String getRequestProperty(String key)
  {
    if (connected)
      throw new IllegalStateException("Already connected");
    
    return (String) requestProperties.get(key);
  }

  public void setRequestProperty(String key, String value)
  {
    if (connected)
      throw new IllegalStateException("Already connected");
    
    requestProperties.put(key, value);
  }

  public Map getRequestProperties()
  {
    if (connected)
      throw new IllegalStateException("Already connected");
    
    return requestProperties;
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

    return (String) headers.getHeaderFieldValueByKey(name.toLowerCase());
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

    return headers.getHeaderFields();
  }

  /**
   * This method returns the header field value at the specified numeric
   * index.
   *
   * @param n The index into the header field array
   *
   * @return The value of the specified header field, or <code>null</code>
   * if the specified index is not valid.
   */
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

    return headers.getHeaderFieldValueByIndex (n);
  }

  /**
   * This method returns the header field key at the specified numeric
   * index.
   *
   * @param n The index into the header field array
   *
   * @return The name of the header field key, or <code>null</code> if the
   * specified index is not valid.
   */
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

    return headers.getHeaderFieldKeyByIndex (n);
  }
}
