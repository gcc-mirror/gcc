// Connection.java - Implementation of HttpURLConnection for http protocol.

/* Copyright (C) 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.protocol.http;

import java.net.*;
import java.io.*;
import java.util.Vector;
import java.util.Hashtable;
import java.util.Enumeration;

/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date March 29, 1999.
 */

/**
 * Written using on-line Java Platform 1.2 API Specification, as well
 * as "The Java Class Libraries", 2nd edition (Addison-Wesley, 1998).
 * Status:  Minimal subset of functionality.  Proxies and Redirects
 *	not yet handled.  FileNameMap handling needs to be considered.
 *	useCaches, ifModifiedSince, and allowUserInteraction need
 *	consideration as well as doInput and doOutput.
 */

class Connection extends HttpURLConnection
{
  protected Socket sock = null;
  private static Hashtable defRequestProperties = new Hashtable();
  private Hashtable requestProperties;
  private Hashtable hdrHash = new Hashtable();
  private Vector hdrVec = new Vector();
  private boolean gotHeaders = false;
  private BufferedInputStream bufferedIn;

  public Connection(URL url)
  {
    super(url);
    requestProperties = (Hashtable) defRequestProperties.clone();
  }

  // Override method in URLConnection.
  public static void setDefaultRequestProperty(String key, String value)
  {
    defRequestProperties.put(key, value);
  }

  // Override method in URLConnection.
  public static String getDefaultRequestProperty(String key)
  {
    return (String) defRequestProperties.get(key);
  }

  // Override method in URLConnection.
  public void setRequestProperty(String key, String value)
  {
    if (connected)
      throw new IllegalAccessError("Connection already established.");

    requestProperties.put(key, value);
  }

  // Override method in URLConnection.
  public String getRequestProperty(String key)
  {
    if (connected)
      throw new IllegalAccessError("Connection already established.");

    return (String) requestProperties.get(key);
  }

  // Implementation of abstract method.
  public void connect() throws IOException
  {
    // Call is ignored if already connected.
    if (connected)
      return;

    // Get address and port number.
    int port;
    InetAddress destAddr = InetAddress.getByName(url.getHost());
    if ((port = url.getPort()) == -1)
      port = 80;

    // Open socket and output stream.
    sock = new Socket(destAddr, port);
    PrintWriter out = new PrintWriter(sock.getOutputStream());

    // Send request including any request properties that were set.
    out.print(getRequestMethod() + " " + url.getFile() + " HTTP/1.1\n");
    out.print("Host: " + url.getHost() + ":" + port + "\n");
    Enumeration reqKeys = requestProperties.keys();
    Enumeration reqVals = requestProperties.elements();
    while (reqKeys.hasMoreElements())
      out.print(reqKeys.nextElement() + ": " + reqVals.nextElement() + "\n");
    out.print("\n");
    out.flush();
    connected = true;
  }

  // Implementation of abstract method.
  public void disconnect()
  {
    if (sock != null)
      {
	try
	  {
	    sock.close();
	  }
	catch (IOException ex)
	  {
	    ; // Ignore errors in closing socket.
	  }
	sock = null;
      }
    connected = false;
  }

  // TODO: public boolean usingProxy()
  public boolean usingProxy()
  {
    throw new InternalError("HttpURLConnection.usingProxy not implemented");
  }

  // Override default method in URLConnection.
  public InputStream getInputStream() throws IOException
  {
    if (!connected)
      connect();

    if (! doInput)
      throw new ProtocolException("Can't open InputStream if doInput is false");
    if (bufferedIn == null)
      bufferedIn = new BufferedInputStream(sock.getInputStream());
    return bufferedIn;
  }

  // Override default method in URLConnection.
  public OutputStream getOutputStream() throws IOException
  {
    if (!connected)
      connect();

    if (! doOutput)
      throw new
	ProtocolException("Can't open OutputStream if doOutput is false");
    return sock.getOutputStream();
  }

  // Override default method in URLConnection.
  public String getHeaderField(String name)
  {
    try
      {
	getHttpHeaders();
      }
    catch (IOException x)
      {
	return null;
      }
    return (String) hdrHash.get(name.toLowerCase());
  }

  // Override default method in URLConnection.
  public String getHeaderField(int n)
  {
    try
      {
	getHttpHeaders();
      }
    catch (IOException x)
      {
	return null;
      }
    if (n < hdrVec.size())
      return getField((String) hdrVec.elementAt(n));

    return null;
  }

  // Override default method in URLConnection.
  public String getHeaderFieldKey(int n)
  {
    try
      {
	getHttpHeaders();
      }
    catch (IOException x)
      {
	return null;
      }
    if (n < hdrVec.size())
      return getKey((String) hdrVec.elementAt(n));

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

  private void getHttpHeaders() throws IOException
  {
    if (gotHeaders)
      return;
    gotHeaders = true;

    connect();

    // Originally tried using a BufferedReader here to take advantage of
    // the readLine method and avoid the following, but the buffer read
    // past the end of the headers so the first part of the content was lost.
    // It is probably more robust than it needs to be, e.g. the byte[]
    // is unlikely to overflow and a '\r' should always be followed by a '\n',
    // but it is better to be safe just in case.
    if (bufferedIn == null)
      bufferedIn = new BufferedInputStream(sock.getInputStream());

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
	for (i = 0; i < buflen; i++)
	  {
	    bufferedIn.read(buf, i, 1);
	    if (buf[i] == '\r')
	      {
	        bufferedIn.read(ch, 0, 1);
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
