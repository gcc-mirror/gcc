// Connection.java - Implementation of URLConnection for file protocol.

/* Copyright (C) 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.protocol.file;

import java.net.*;
import java.io.*;
import java.util.Vector;
import java.util.Hashtable;
import java.util.Enumeration;

/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date April 13, 1999.
 */

/**
 * Written using on-line Java Platform 1.2 API Specification, as well
 * as "The Java Class Libraries", 2nd edition (Addison-Wesley, 1998).
 * Status:  Minimal subset of functionality.
 */

class Connection extends URLConnection
{
  private Hashtable hdrHash = new Hashtable();
  private Vector hdrVec = new Vector();
  private boolean gotHeaders = false;
  private File fileIn;
  private InputStream inputStream;
  private OutputStream outputStream;

  public Connection(URL url)
  {
    super(url);
  }

  // Implementation of abstract method.
  public void connect() throws IOException
  {
    // Call is ignored if already connected.
    if (connected)
      return;

    // If not connected, then file needs to be openned.
    String fname = url.getFile();
    fileIn = new File(fname);
    if (doInput)
      inputStream = new BufferedInputStream(new FileInputStream(fileIn));
    if (doOutput)
      outputStream = new BufferedOutputStream(new FileOutputStream(fileIn));
    connected = true;
  }

  public InputStream getInputStream() throws IOException
  {
    if (! doInput)
      throw new ProtocolException("Can't open InputStream if doInput is false");
    if (!connected)
      connect();

    return inputStream;
  }

  // Override default method in URLConnection.
  public OutputStream getOutputStream() throws IOException
  {
    if (! doOutput)
      throw new
	ProtocolException("Can't open OutputStream if doOutput is false");

    if (!connected)
      connect();

    return outputStream;
  }

  // Override default method in URLConnection.
  public String getHeaderField(String name)
  {
    try
      {
	getHeaders();
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
	getHeaders();
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
	getHeaders();
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

  private void getHeaders() throws IOException
  {
    if (gotHeaders)
      return;
    gotHeaders = true;

    connect();

    // Yes, it is overkill to use the hash table and vector here since
    // we're only putting one header in the file, but in case we need
    // to add others later and for consistency, we'll implement it this way.

    // Add the only header we know about right now:  Content-length.
    long len = fileIn.length();
    String line = "Content-length: " + len;
    hdrVec.addElement(line);

    // The key will never be null in this scenario since we build up the
    // headers ourselves.  If we ever rely on getting a header from somewhere
    // else, then we may have to check if the result of getKey() is null.
    String key = getKey(line);
    hdrHash.put(key.toLowerCase(), Long.toString(len));
  }
}
