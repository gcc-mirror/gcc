// URLStreamHandler.java - Superclass of all stream protocol handlers.

/* Copyright (C) 1999  Cygnus Solutions

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.net;

/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date March 4, 1999.
 */

/**
 * Written using on-line Java Platform 1.2 API Specification, as well
 * as "The Java Class Libraries", 2nd edition (Addison-Wesley, 1998).
 * Status:  Believed complete and correct.
 */

public abstract class URLStreamHandler
{
  protected abstract URLConnection openConnection(URL u)
    throws java.io.IOException;

protected void parseURL(URL u, String spec, int start, int limit)
  {
    String tmpStr;
    String host = "";	// Initialize to null string.
    String file;
    int port = -1;
    int colon;

    /* TBD: The JDK 1.2 doc specifically says that limit is the position
     * to stop parsing at and that it will be either the end of the string
     * or the position of '#'; thus the doc infers that this method does
     * not set the ref.
     */
    tmpStr = spec.substring(start, limit);
    int hostEnd = 0;
    if (tmpStr.startsWith("//"))
      {
	int slash = tmpStr.indexOf('/', 2);
	hostEnd = tmpStr.length();
	if (slash >= 0)
	  hostEnd = slash;

	host = tmpStr.substring(2, hostEnd);

	// Look for optional port number.
	if ((colon = host.indexOf(':')) >= 0)
	  {
	    try
	      {
		port = Integer.parseInt(host.substring(colon + 1));
	      }
	    catch (NumberFormatException e)
	      {
		; // Ignore invalid port values; port is already set to -1.
	      }
	    host = host.substring(0, colon);
	  }
      }

    if (hostEnd < tmpStr.length())
      file = ((tmpStr.startsWith("/")) ? "" : "/") + tmpStr.substring(hostEnd);
    else
      file = "/";

    u.set(u.getProtocol(), host, port, file, u.getRef());
  }
  
  protected void setURL(URL u, String protocol, String host, int port,
			String file, String ref)
  {
    u.set(protocol, host, port, file, ref);
  }

  protected String toExternalForm(URL u)
  {
    String resStr, host, file, ref;
    int port;

    resStr = u.getProtocol() + ":";
    host = u.getHost();
    port = u.getPort();
    file = u.getFile();
    ref = u.getRef();

    if (! host.equals(""))
      {
        resStr = resStr + "//" + host;
	if (port >= 0)
	  resStr = resStr + ":" + port;
      }

    resStr = resStr + file;

    if (ref != null)
      resStr = resStr + "#" + ref;

    return resStr;
  }
}
