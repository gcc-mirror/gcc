/* Copyright (C) 1999, 2000, 2002  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.net;

import java.net.*;
import java.io.*;
import java.util.jar.*;
import java.util.zip.*;
import java.util.Map;
import java.util.Vector;
import java.util.Hashtable;
import java.security.cert.Certificate;

/**
 * @author Kresten Krab Thorup <krab@gnu.org>
 * @since 1.2
 * @date Aug 10, 1999.
 */


public abstract class JarURLConnection extends URLConnection
{
  // three different ways to say the same thing
  private final URL jarFileURL;

  /** The connection to the jar file itself. A JarURLConnection
   *  can represent an entry in a jar file or an entire jar file.  In
   *  either case this describes just the jar file itself. */
  protected URLConnection jarFileURLConnection;

  // If this is a connection to a jar file element this is set, otherwise null.
  private final String element;

  // Cached JarURLConnection's 
  static Hashtable conn_cache = new Hashtable();

  public URL getJarFileURL ()
  {
    return jarFileURL;
  }

  public String getEntryName ()
  {
    return element;
  }

  /**
   * Creates a new JarURLConnection
   *
   * @exception MalformedURLException If url is invalid
   *
   * @specnote This constructor is protected since JDK 1.4
   */
  protected JarURLConnection(URL url)
    throws MalformedURLException
  {
    super(url);

    String spec = url.getFile();
    int bang = spec.indexOf ("!/", 0);
    if (bang == -1)
      throw new MalformedURLException (url + ": No `!/' in spec.");

    // Extact the url for the jar itself.
    jarFileURL = new URL(spec.substring (0, bang));

    // Get the name of the element, if any.
    element = (bang+2==spec.length() ? null : spec.substring (bang+2));
  }

  public synchronized void connect() throws IOException
  {
    // Call is ignored if already connected.
    if (connected)
      return;

    if (getUseCaches())
      {
	jarFileURLConnection = (URLConnection) conn_cache.get (jarFileURL);

	if (jarFileURLConnection == null)
	  {
	    jarFileURLConnection = jarFileURL.openConnection ();
	    jarFileURLConnection.setUseCaches (true);
	    jarFileURLConnection.connect ();
	    conn_cache.put (jarFileURL, jarFileURLConnection);
	  }
      }
    else
      {
	jarFileURLConnection = jarFileURL.openConnection ();
	jarFileURLConnection.connect ();
      }

    connected = true;
  }

  public InputStream getInputStream() throws IOException
  {
    if (!connected)
      connect();

    if (! doInput)
      throw new ProtocolException("Can't open InputStream if doInput is false");

    if (element == null)
      {
	// This is a JarURLConnection for the entire jar file.  

	InputStream jar_is = new BufferedInputStream(
			jarFileURLConnection.getInputStream ());
	return new JarInputStream(jar_is);
      }

    // Reaching this point, we're looking for an element of a jar file.

    JarFile jarfile = null;

    try
      {
	jarfile = getJarFile ();
      }
    catch (java.io.IOException x)
      {
	/* ignore */
      }
    
    if (jarfile != null)
      {
	// this is the easy way...
	ZipEntry entry = jarfile.getEntry(element);
	if (entry != null)
	  return jarfile.getInputStream (entry);
	else
	  return null;
      }
    else
      {
	// If the jar file is not local, ...
	JarInputStream zis = new JarInputStream(
			jarFileURLConnection.getInputStream ());

	// This is hideous, we're doing a linear search...
	for (ZipEntry ent = zis.getNextEntry (); 
	     ent != null; 
	     ent = zis.getNextEntry ())
	  {
	    if (element.equals (ent.getName ()))
	      {
		int size = (int)ent.getSize();
		byte[] data = new byte[size];
		zis.read (data, 0, size);
		return new ByteArrayInputStream (data);
	      }
	  }
      }

    return null;
  }

  /**
   * Return the JAR entry object for this connection, if any
   *
   * @exception IOException If an error occurs
   */
  public JarEntry getJarEntry () throws IOException
  {
    JarFile jarfile = null;

    if (element == null)
      return null;

    if (! doInput)
      throw new ProtocolException("Can't open JarEntry if doInput is false");

    try
      {
	jarfile = getJarFile ();
      }
    catch (IOException x)
      {
	/* ignore */
      }
    
    if (jarfile == null)
      {
	JarInputStream zis = new JarInputStream(
			jarFileURLConnection.getInputStream ());

	// This is hideous, we're doing a linear search for the thing...
	for (ZipEntry ent = zis.getNextEntry (); 
	     ent != null; 
	     ent = zis.getNextEntry ())
	  {
	    if (element.equals (ent.getName ()))
	      {
		return new JarEntry (ent);
	      }
	  }
      }

    else
      {
	return jarfile.getJarEntry (element);
      }

    return null;
  }

  /**
   * Return the JAR file for this connection
   *
   * @exception IOException If an error occurs
   */
  public abstract JarFile getJarFile() throws IOException;


  // Steal and borrow from protocol/file/Connection.java

  private Hashtable hdrHash = new Hashtable();
  private Vector hdrVec = new Vector();
  private boolean gotHeaders = false;

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
  public Map getHeaderFields()
  {
    try
      {
        getHeaders();
      }
    catch (IOException x)
      {
        return null;
      }
    return hdrHash;
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
    long len = -1;

    if (element == null)
      if (jarFileURLConnection != null)
	len = jarFileURLConnection.getContentLength ();
    else
      {
	JarEntry entry = getJarEntry();
	if (entry != null)
	  len = entry.getSize ();
      }

    String line = "Content-length: " + len;
    hdrVec.addElement(line);

    // The key will never be null in this scenario since we build up the
    // headers ourselves.  If we ever rely on getting a header from somewhere
    // else, then we may have to check if the result of getKey() is null.
    String key = getKey(line);
    hdrHash.put(key.toLowerCase(), Long.toString(len));
  }

  /**
   * Returns an array of Certificate objects for the jar file entry specified
   * by this URL or null if there are none
   *
   * @return A Certificate array
   *
   * @exception IOException If an error occurs
   */
  public Certificate[] getCertificates() throws IOException
  {
    return getJarEntry().getCertificates();
  }

  /**
   * Returns the main Attributes for the JAR file for this connection
   *
   * @exception IOException If an error occurs
   */
  public Attributes getMainAttributes () throws IOException
  {
    return getManifest ().getMainAttributes ();
  }

  /**
   * Return the Attributes object for this connection if the URL for it points
   * to a JAR file entry, null otherwise
   *
   * @exception IOException If an error occurs
   */
  public Attributes getAttributes () throws IOException
  {
    // FIXME: implement this
    return null;
  }

  /**
   * Returns the Manifest for this connection, or null if none
   *
   * @exception IOException If an error occurs
   */
  public Manifest getManifest () throws IOException
  {
    JarFile file = getJarFile ();

    return (file != null) ? file.getManifest() : null;
  }
}
