/* Connection - jar url connection for java.net
   Copyright (C) 1999, 2002, 2003 Free Software Foundation, Inc.

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

import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.IOException;
import java.net.JarURLConnection;
import java.net.MalformedURLException;
import java.net.ProtocolException;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLStreamHandler;
import java.util.Hashtable;
import java.util.jar.JarFile;
import java.util.zip.ZipFile;

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

    URL jarFileURL = getJarFileURL();

    if (jarFileURL.getProtocol().equals ("file")
	&& jarFileURL.getHost().equals (""))
      {
	if (getUseCaches())
	  {
	    jarfile = (JarFile) file_cache.get(jarFileURL);
	    if (jarfile == null)
	      {
		jarfile = new JarFile (jarFileURL.getFile());
		file_cache.put (jarFileURL, jarfile);
	      }
	  }
	else
	  jarfile = new JarFile (jarFileURL.getFile());
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
	// XXX ZipFile.OPEN_DELETE not yet implemented.
	// jf = new JarFile(f, true, ZipFile.OPEN_READ | ZipFile.OPEN_DELETE);
	jarfile = new JarFile(f, true, ZipFile.OPEN_READ);
      }

    return jarfile;
  }

}
