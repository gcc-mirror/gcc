// Connection.java - Implementation of URLConnection for gcjlib
// protocol.

/* Copyright (C) 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.java.net.protocol.gcjlib;

import java.io.InputStream;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import gnu.gcj.Core;
import gnu.gcj.runtime.SharedLibHelper;
import gnu.java.net.protocol.core.CoreInputStream;

/**
 * @author Tom Tromey <tromey@redhat.com>
 * @date January 10, 2003
 */
class Connection extends URLConnection
{
  String solib;
  String name;
  Core core;

  public Connection (URL url) throws MalformedURLException
  {
    super (url);
    int index = url.getFile().indexOf ("!/");
    
    if (index == -1)
      throw new MalformedURLException ("couldn't find !/ in gcjlib URL");

    name = url.getFile().substring (index + 2);
    solib = url.getFile().substring (0, index);
  }

  public void connect() throws IOException
  {
    if (core != null)
      return;
    // We can't create a new SharedLibHelper here, since we don't know
    // what parent class loader to use.
    SharedLibHelper helper = SharedLibHelper.findHelper(solib);
    if (helper == null)
      throw new IOException("library not loaded: " + solib);
    core = helper.findCore(name);
    if (core == null)
      throw new IOException("couldn't find core object: " + name);
  }

  public InputStream getInputStream() throws IOException
  {
    connect();
    return new CoreInputStream(core);
  }
  
  public String getHeaderField(String field)
  {
    try
      {
	if (!connected)
	  connect();

	if (field.equals("content-type"))
          return guessContentTypeFromName(name);
	else if (field.equals("content-length"))
          return Long.toString(core.length);
      }
    catch (IOException e)
      {
        // Fall through.
      }
    return null;
  }  
}
