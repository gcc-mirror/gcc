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


package gnu.java.net.protocol.jar;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.JarURLConnection;
import java.net.MalformedURLException;
import java.net.ProtocolException;
import java.net.URL;
import java.net.URLConnection;
import java.util.Hashtable;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.zip.ZipFile;

/**
 * This subclass of java.net.JarURLConnection models a URLConnection via
 * the "jar" protocol.
 *
 * @author Kresten Krab Thorup (krab@gnu.org)
 */
public final class Connection extends JarURLConnection
{
  private JarFile jar_file;
  private JarEntry jar_entry;
  private URL jar_url;
  
  public static class JarFileCache
  {
    private static Hashtable cache = new Hashtable();
    private static final int READBUFSIZE = 4*1024;
    
    public static synchronized JarFile get (URL url, boolean useCaches)
       throws IOException
    {
      JarFile jf;
      if (useCaches)
        {
          jf = (JarFile) cache.get (url);
          if (jf != null)
            return jf;
        }

      if ("file".equals (url.getProtocol()))
	{
	  File f = new File (url.getFile());
	  jf = new JarFile (f, true, ZipFile.OPEN_READ);
	}
      else
	{
	  URLConnection urlconn = url.openConnection();
	  InputStream is = urlconn.getInputStream();
	  byte[] buf = new byte [READBUFSIZE];
	  File f = File.createTempFile ("cache", "jar");
	  FileOutputStream fos = new FileOutputStream (f); 
	  int len = 0;
	  
	  while ((len = is.read (buf)) != -1)
	    {
	      fos.write (buf, 0, len);
	    }
	  
	  fos.close();
	  // Always verify the Manifest, open read only and delete when done.
	  jf = new JarFile (f, true,
			    ZipFile.OPEN_READ | ZipFile.OPEN_DELETE);
	}

      if (useCaches)
        cache.put (url, jf);

      return jf;
    }
  }

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

    jar_url = getJarFileURL();
    jar_file = JarFileCache.get (jar_url, useCaches);
    String entry_name = getEntryName();
    
    if (entry_name != null
        && !entry_name.equals (""))
      {
        jar_entry = (JarEntry) jar_file.getEntry (entry_name);

        if(jar_entry == null)
          throw new IOException ("No entry for " + entry_name + " exists.");
      }

    connected = true;
  }

  public InputStream getInputStream() throws IOException
  {
    if (!connected)
      connect();

    if (! doInput)
      throw new ProtocolException("Can't open InputStream if doInput is false");
    
    if (jar_entry == null)
      throw new IOException (jar_url + " couldn't be found.");
    
    return jar_file.getInputStream (jar_entry);
  }

  public synchronized JarFile getJarFile() throws IOException
  {
    if (!connected)
      connect();

    if (! doInput)
      throw new ProtocolException("Can't open JarFile if doInput is false");

    return jar_file;
  }

  public int getContentLength()
  {
    if (!connected)
      return -1;

    return (int) jar_entry.getSize();
  }
}
