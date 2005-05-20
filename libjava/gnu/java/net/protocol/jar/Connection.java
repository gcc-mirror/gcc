/* Connection - jar url connection for java.net
   Copyright (C) 1999, 2002, 2003, 2005 Free Software Foundation, Inc.

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


package gnu.java.net.protocol.jar;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.IOException;
import java.net.JarURLConnection;
import java.net.MalformedURLException;
import java.net.ProtocolException;
import java.net.URL;
import java.net.URLConnection;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Locale;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.jar.JarInputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

/**
 * This subclass of java.net.JarURLConnection models a URLConnection via
 * the "jar" protocol.
 *
 * @author Kresten Krab Thorup <krab@gnu.org>
 */
public final class Connection extends JarURLConnection
{
  private static Hashtable file_cache = new Hashtable();

  /**
   * HTTP-style DateFormat, used to format the last-modified header.
   */
  private static SimpleDateFormat dateFormat
    = new SimpleDateFormat("EEE, dd MMM yyyy hh:mm:ss 'GMT'",
                           new Locale ("En", "Us", "Unix"));

  private JarFile jar_file;

  /**
   * Cached JarURLConnection objects.
   */
  static HashMap connectionCache = new HashMap();

  protected Connection(URL url)
    throws MalformedURLException
  {
    super(url);
  }

  public synchronized void connect() throws IOException
  {
    // Call is ignored if already connected.
    if (connected)
      return;

    if (getUseCaches())
      {
	jarFileURLConnection =
          (URLConnection) connectionCache.get(getJarFileURL());

	if (jarFileURLConnection == null)
	  {
	    jarFileURLConnection = getJarFileURL().openConnection();
	    jarFileURLConnection.setUseCaches(true);
	    jarFileURLConnection.connect();
	    connectionCache.put(getJarFileURL(), jarFileURLConnection);
	  }
      }
    else
      {
	jarFileURLConnection = getJarFileURL().openConnection();
	jarFileURLConnection.connect();
      }

    connected = true;
  }

  public InputStream getInputStream() throws IOException
  {
    if (!connected)
      connect();

    if (! doInput)
      throw new ProtocolException("Can't open InputStream if doInput is false");

    if (getEntryName() == null)
      {
	// This is a JarURLConnection for the entire jar file.  

	InputStream in = new BufferedInputStream
	  (jarFileURLConnection.getInputStream());
	return new JarInputStream(in);
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
	ZipEntry entry = jarfile.getEntry(getEntryName());
        
	if (entry != null)
	  return jarfile.getInputStream (entry);
      }
    else
      {
	// If the jar file is not local, ...
	JarInputStream zis = new JarInputStream(
			jarFileURLConnection.getInputStream ());

	// This is hideous, we're doing a linear search...
	for (ZipEntry entry = zis.getNextEntry(); 
	     entry != null; 
	     entry = zis.getNextEntry())
	  {
	    if (getEntryName().equals(entry.getName()))
	      {
		int size = (int) entry.getSize();
		byte[] data = new byte[size];
		zis.read (data, 0, size);
		return new ByteArrayInputStream (data);
	      }
	  }
      }

    throw new FileNotFoundException("No entry for \"" + getEntryName()
				    + "\" in \""
				    + getJarFileURL()
				    + "\"");
  }

  public synchronized JarFile getJarFile() throws IOException
  {
    if (!connected)
      connect();

    if (! doInput)
      throw new ProtocolException("Can't open JarFile if doInput is false");

    if (jar_file != null)
      return jar_file;

    URL jarFileURL = getJarFileURL();

    if (jarFileURL.getProtocol().equals ("file")
	&& jarFileURL.getHost().equals (""))
      {
	if (getUseCaches())
	  {
	    jar_file = (JarFile) file_cache.get (jarFileURL);
	    if (jar_file == null)
	      {
		jar_file = new JarFile (jarFileURL.getFile());
		file_cache.put (jarFileURL, jar_file);
	      }
	  }
	else
	  jar_file = new JarFile (jarFileURL.getFile());
      }
    else
      {
	URLConnection urlconn = jarFileURL.openConnection();
	InputStream is = urlconn.getInputStream();
	byte[] buf = new byte[4*1024];
	File f = File.createTempFile("cache", "jar");
	FileOutputStream fos = new FileOutputStream(f);
	int len = 0;
	while ((len = is.read(buf)) != -1)
	  fos.write(buf, 0, len);
        fos.close();
	// Always verify the Manifest, open read only and delete when done.
	jar_file = new JarFile (f, true,
				ZipFile.OPEN_READ | ZipFile.OPEN_DELETE);
      }

    return jar_file;
  }

  public String getHeaderField(String field)
  {
    try
      {
	if (!connected)
	  connect();

	if (field.equals("content-type"))
          return guessContentTypeFromName(getJarEntry().getName());
	else if (field.equals("content-length"))
          return Long.toString(getJarEntry().getSize());
	else if (field.equals("last-modified"))
	  {
	    synchronized (dateFormat)
	      {
        	return dateFormat.format(new Date(getJarEntry().getTime()));
	      }
	  }
      }
    catch (IOException e)
      {
        // Fall through.
      }
    return null;
  }

  public int getContentLength()
  {
    if (!connected)
      return -1;

    try
      {
        return (int) getJarEntry().getSize();
      }
    catch (IOException e)
      {
	return -1;
      }
  }

  public long getLastModified()
  {
    if (!connected)
      return -1;

    try
      {
	return getJarEntry().getTime();
      }
    catch (IOException e)
      {
	return -1;
      }
  }
}
