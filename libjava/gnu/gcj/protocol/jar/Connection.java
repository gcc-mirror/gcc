/* Copyright (C) 1999, 2002  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.protocol.jar;

import java.net.URL;
import java.net.URLConnection;
import java.net.JarURLConnection;
import java.net.URLStreamHandler;
import java.net.MalformedURLException;
import java.net.ProtocolException;
import java.io.IOException;
import java.io.InputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.util.jar.JarFile;
import java.util.zip.ZipFile;
import java.util.Hashtable;

/**
 * Written using on-line Java Platform 1.2 API Specification.
 *
 * @author Kresten Krab Thorup <krab@gnu.org>
 * @date Aug 10, 1999.
 */



public class Connection extends JarURLConnection 
{
  static Hashtable file_cache = new Hashtable();
  private JarFile jarfile;

  public Connection(URL url)
    throws MalformedURLException
  {
    super(url);
  }

  public synchronized JarFile getJarFile() throws java.io.IOException
  {
    if (!connected)
      connect();

    if (! doInput)
      throw new ProtocolException("Can't open JarFile if doInput is false");

    if (jarfile != null)
      return jarfile;

    URL jarFileURL = getJarFileURL ();

    if (jarFileURL.getProtocol ().equals ("file")
	&& jarFileURL.getHost ().equals (""))
      {
	if (getUseCaches())
	  {
	    jarfile = (JarFile) file_cache.get(jarFileURL);
	    if (jarfile == null)
	      {
		jarfile = new JarFile (jarFileURL.getFile ());
		file_cache.put (jarFileURL, jarfile);
	      }
	  }
	else
	  jarfile = new JarFile (jarFileURL.getFile ());
      }
    else
      {
	URLConnection urlconn = jarFileURL.openConnection();
	InputStream is = urlconn.getInputStream();
	byte[] buf = new byte[4*1024];
	File f = File.createTempFile("cache", "jar");
	FileOutputStream fos = new FileOutputStream(f);
	int len = 0;
	while((len = is.read(buf)) != -1)
	  fos.write(buf, 0, len);
        fos.close();
	// Always verify the Manifest, open read only and delete when done.
	// XXX ZipFile.OPEN_DELETE not yet implemented.
	// jf = new JarFile(f, true, ZipFile.OPEN_READ | ZipFile.OPEN_DELETE);
	jarfile = new JarFile(f, true, ZipFile.OPEN_READ);
      }

    return jarfile;
  }

}
