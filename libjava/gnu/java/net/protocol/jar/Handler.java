/* gnu.java.net.protocol.jar.Handler - jar protocol handler for java.net
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

import gnu.java.net.URLParseError;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLStreamHandler;

/**
 * @author Kresten Krab Thorup (krab@gnu.org)
 */
public class Handler extends URLStreamHandler
{
  /**
   * A do nothing constructor
   */
  public Handler()
  {
  }

  /**
   * This method returs a new JarURLConnection for the specified URL
   *
   * @param url The URL to return a connection for
   *
   * @return The URLConnection
   *
   * @exception IOException If an error occurs
   */
  protected URLConnection openConnection(URL url) throws IOException
  {
    return new Connection(url);
  }

  /**
   * This method overrides URLStreamHandler's for parsing url of protocol "jar"
   *
   * @param url The URL object in which to store the results
   * @param url_string The String-ized URL to parse
   * @param start The position in the string to start scanning from
   * @param end The position in the string to stop scanning
   */
  protected void parseURL (URL url, String url_string, int start, int end)
  {
    // This method does not throw an exception or return a value.  Thus our
    // strategy when we encounter an error in parsing is to return without
    // doing anything.
    String file = url.getFile();
    
    if (!file.equals(""))
      { //has context url
	url_string = url_string.substring (start, end);
        if (url_string.startsWith("/"))
          { //url string is an absolute path
            int idx = file.lastIndexOf ("!/");
	    
	    if (idx < 0)
	      throw new URLParseError("no !/ in spec");
	    
	    file = file.substring (0, idx + 1) + url_string;
          }
        else
          {
            int idx = file.lastIndexOf ("/");
            if (idx == -1) //context path is weird
              file = "/" + url_string; 
            else if (idx == (file.length() - 1))
              //just concatenate two parts
              file = file + url_string;
            else
              // according to Java API Documentation, here is a little different 
              // with URLStreamHandler.parseURL
              // but JDK seems doesn't handle it well
              file = file.substring(0, idx + 1) + url_string;
          }
        
        setURL (url, "jar", url.getHost(), url.getPort(), file, null);
        return;
      }

    // Bunches of things should be true.  Make sure.
    if (end < start)
      return;
    if (end - start < 2)
      return;
    if (start > url_string.length())
      return;
    
    // Skip remains of protocol
    url_string = url_string.substring (start, end);

    int jar_stop;
    if ((jar_stop = url_string.indexOf("!/")) < 0)
      throw new URLParseError("no !/ in spec");

    try
      {
	new URL(url_string.substring (0, jar_stop));
      }
    catch (MalformedURLException e)
      {
	throw new URLParseError("invalid inner URL: " + e.getMessage());
      }
    
    if (!url.getProtocol().equals ("jar") )
      throw new URLParseError("unexpected protocol " + url.getProtocol());
        
    setURL (url, "jar", url.getHost(), url.getPort(), url_string, null);
  }

  /**
   * This method converts a Jar URL object into a String.
   *
   * @param url The URL object to convert
   */
  protected String toExternalForm (URL url)
  {
    String file = url.getFile();

    // return "jar:" + file;
    // Performance!!: 
    //  Do the concatenation manually to avoid resize StringBuffer's 
    //  internal buffer.
    StringBuffer sb = new StringBuffer (file.length() + 5);
    sb.append ("jar:");
    sb.append (file);
    return sb.toString();
  }
}
