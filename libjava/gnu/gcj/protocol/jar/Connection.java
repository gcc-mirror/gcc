/* Copyright (C) 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.protocol.jar;

import java.net.URL;
import java.net.JarURLConnection;
import java.net.URLStreamHandler;
import java.net.MalformedURLException;
import java.net.ProtocolException;
import java.io.IOException;
import java.util.jar.JarFile;
import java.util.Hashtable;

/**
 * Written using on-line Java Platform 1.2 API Specification.
 * Status: Needs a way to download jar files and store them in the local file
 * system.  I don't know how to do that in a portable way.  For now, it can only handle 
 * connections to a jar:file: url's.
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
	/*
	  FIXME: Here we need to download and cache the jar
	  file in the local file system!  Stupid design.  Why
	  can't we just create a JarFile from a bag of bytes?
	*/

	throw new java.io.IOException("cannot create jar file from " +
				      jarFileURL);
      }

    return jarfile;
  }

}
