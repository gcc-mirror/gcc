/* JarURLConnection.java -- Class for manipulating remote jar files
   Copyright (C) 1998, 2002, 2003 Free Software Foundation, Inc.

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


package java.net;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.IOException;
import java.util.jar.Attributes;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.jar.JarInputStream;
import java.util.jar.Manifest;
import java.util.zip.ZipEntry;
import java.util.Map;
import java.util.Vector;
import java.util.HashMap;
import java.util.Hashtable;
import java.security.cert.Certificate;

/**
 * This abstract class represents a common superclass for implementations
 * of jar URL's.  A jar URL is a special type of URL that allows JAR
 * files on remote systems to be accessed.  It has the form:
 * <p>
 * jar:<standard URL pointing to jar file>!/file/within/jarfile
 * <p> for example:
 * <p>
 * jar:http://www.urbanophile.com/java/foo.jar!/com/urbanophile/bar.class
 * <p>
 * That example URL points to the file /com/urbanophile/bar.class in the
 * remote JAR file http://www.urbanophile.com/java/foo.jar.  The HTTP
 * protocol is used only as an example.  Any supported remote protocol
 * can be used.
 * <p>
 * This class currently works by retrieving the entire jar file into a
 * local cache file, then performing standard jar operations on it.
 * (At least this is true for the default protocol implementation).
 *
 * @author Aaron M. Renn <arenn@urbanophile.com>
 * @author Kresten Krab Thorup <krab@gnu.org>
 * @date Aug 10, 1999.
 *
 * @since 1.2
 */
public abstract class JarURLConnection extends URLConnection
{
  /**
   * This is the actual URL that points the remote jar file.  This is parsed
   * out of the jar URL by the constructor.
   */
  private final URL jarFileURL;

  /**
   * The connection to the jar file itself. A JarURLConnection
   * can represent an entry in a jar file or an entire jar file.  In
   * either case this describes just the jar file itself.
   */
  protected URLConnection jarFileURLConnection;

  /**
   * This is the jar file "entry name" or portion after the "!/" in the
   * URL which represents the pathname inside the actual jar file.
   */
  private final String entryName;

  /**
   * Cached JarURLConnection objects .
   */
  static HashMap connectionCache = new HashMap();

  /**
   * Creates a JarURLConnection from an URL object
   *
   * @param URL url The URL object for this connection.
   *
   * @exception MalformedURLException If url is invalid
   *
   * @specnote This constructor is protected since JDK 1.4
   */
  protected JarURLConnection (URL url)
    throws MalformedURLException
  {
    super (url);

    if (!url.getProtocol().equals ("jar"))
      throw new MalformedURLException (url + ": Not jar protocol.");

    String spec = url.getFile();
    int bang = spec.indexOf ("!/");
    if (bang == -1)
      throw new MalformedURLException (url + ": No `!/' in spec.");

    // Extract the url for the jar itself.
    jarFileURL = new URL (spec.substring (0, bang));

    // Get the name of the entry, if any.
    entryName = spec.length() == (bang + 2) ? null : spec.substring (bang + 2);
  }

  /**
   * This method returns the "real" URL where the JarFile is located.
   * //****Is this right?*****
   *
   * @return The remote URL
   */
  public URL getJarFileURL ()
  {
    return jarFileURL;
  }

  /**
   * Returns the "entry name" portion of the jar URL.  This is the portion
   * after the "!/" in the jar URL that represents the pathname inside the
   * actual jar file.
   *
   * @return The entry name.
   */
  public String getEntryName ()
  {
    return entryName;
  }

  public synchronized void connect() throws IOException
  {
    // Call is ignored if already connected.
    if (connected)
      return;

    if (getUseCaches())
      {
	jarFileURLConnection = (URLConnection) connectionCache.get (jarFileURL);

	if (jarFileURLConnection == null)
	  {
	    jarFileURLConnection = jarFileURL.openConnection ();
	    jarFileURLConnection.setUseCaches (true);
	    jarFileURLConnection.connect ();
	    connectionCache.put (jarFileURL, jarFileURLConnection);
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

    if (entryName == null)
      {
	// This is a JarURLConnection for the entire jar file.  

	InputStream jar_is = new BufferedInputStream(
			jarFileURLConnection.getInputStream ());
	return new JarInputStream(jar_is);
      }

    // Reaching this point, we're looking for an entry of a jar file.

    JarFile jarfile = null;

    try
      {
	jarfile = getJarFile ();
      }
    catch (IOException x)
      {
	/* ignore */
      }
    
    if (jarfile != null)
      {
	// this is the easy way...
	ZipEntry entry = jarfile.getEntry (entryName);
        
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
	    if (entryName.equals (ent.getName()))
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
   * Returns the entry in this jar file specified by the URL.  
   * 
   * @return The jar entry
   *
   * @exception IOException If an error occurs
   */
  public JarEntry getJarEntry () throws IOException
  {
    JarFile jarfile = null;

    if (entryName == null)
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
	    if (entryName.equals (ent.getName()))
	      {
		return new JarEntry (ent);
	      }
	  }
      }

    else
      {
	return jarfile.getJarEntry (entryName);
      }

    return null;
  }

  /**
   * Returns a read-only JarFile object for the remote jar file
   *
   * @return The JarFile object
   *
   * @exception IOException If an error occurs
   */
  public abstract JarFile getJarFile () throws IOException;

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

    if (entryName == null)
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
  public Certificate[] getCertificates () throws IOException
  {
    JarEntry entry = getJarEntry();
    
    return entry != null ? entry.getCertificates() : null;
  }

  /**
   * Returns the main Attributes for the jar file specified in the URL or
   * null if there are none
   *
   * @return The main Attributes for the JAR file for this connection
   *
   * @exception IOException If an error occurs
   */
  public Attributes getMainAttributes () throws IOException
  {
    Manifest manifest = getManifest();
    
    return manifest != null ? manifest.getMainAttributes() : null;
  }

  /**
   * Returns the Attributes for the Jar entry specified by the URL or null
   * if none
   *
   * @return The Attributes object for this connection if the URL for it points
   * to a JAR file entry, null otherwise
   *
   * @exception IOException If an error occurs
   */
  public Attributes getAttributes () throws IOException
  {
    JarEntry entry = getJarEntry();

    return entry != null ? entry.getAttributes() : null;
  }

  /**
   * Returns a Manifest object for this jar file, or null if there is no
   * manifest.
   *
   * @return The Manifest for this connection, or null if none
   *
   * @exception IOException If an error occurs
   */
  public Manifest getManifest () throws IOException
  {
    JarFile file = getJarFile();

    return file != null ? file.getManifest() : null;
  }
}
