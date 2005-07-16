/* XMLJ.java - 
   Copyright (C) 2004 Free Software Foundation, Inc.

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

package gnu.xml.libxmlj.util;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.Reader;
import java.io.Writer;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;

import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.sax.SAXSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import org.xml.sax.InputSource;

import gnu.xml.libxmlj.transform.GnomeTransformerFactory;

import gnu.xml.dom.ls.ReaderInputStream;
import gnu.xml.dom.ls.WriterOutputStream;

/**
 * Utility functions for libxmlj.
 */
public final class XMLJ
{

  static class XMLJShutdownHook
    implements Runnable
  {
    
    public void run ()
    {
      // Make sure finalizers are run
      System.gc ();
      Runtime.getRuntime ().runFinalization ();
      
      // Perform global cleanup on the native level
      GnomeTransformerFactory.freeLibxsltGlobal ();
    }
  
  }

  private static boolean initialised = false;
  
  public static void init ()
  {
    if (!initialised)
      {
        System.loadLibrary ("xmlj");
        
        XMLJShutdownHook hook = new XMLJShutdownHook ();  
        Runtime.getRuntime ().addShutdownHook (new Thread (hook));
      }
    initialised = true;
  }

  private static final int LOOKAHEAD = 50;
  
  /**
   * Returns an input stream for the specified input source.
   * This returns a pushback stream that libxmlj can use to detect the
   * character encoding of the stream.
   */
  public static NamedInputStream getInputStream (InputSource input)
    throws IOException
  {
    InputStream in = input.getByteStream ();
    String systemId = input.getSystemId ();
    if (in == null)
      {
       Reader r = input.getCharacterStream();
       if (r != null)
         in = new ReaderInputStream(r);
      }
    if (in == null)
      {
        in = getInputStream(systemId);
      }
    return new NamedInputStream (systemId, in, LOOKAHEAD);
  }

  /**
   * Returns an input stream for the specified transformer source.
   * This returns a pushback stream that libxmlj can use to detect the
   * character encoding of the stream.
   */
  public static NamedInputStream getInputStream (Source source)
    throws IOException
  {
    if (source instanceof SAXSource)
      {
        return getInputStream (((SAXSource) source).getInputSource ());
      }
    InputStream in = null;
    String systemId = source.getSystemId ();
    if (source instanceof StreamSource)
      {
        in = ((StreamSource) source).getInputStream ();
      }
    if (in == null)
      {
        in = getInputStream(systemId);
      }
    return new NamedInputStream (systemId, in, LOOKAHEAD);
  }

  private static InputStream getInputStream(String systemId)
    throws IOException
  {
    if (systemId == null)
      {
        throw new IOException("no system ID");
      }
    try
      {
        return new URL(systemId).openStream();
      }
    catch (MalformedURLException e)
      {
        return new FileInputStream(systemId);
      }
  }

  /**
   * Returns an input stream for the specified URL.
   * This returns a pushback stream that libxmlj can use to detect the
   * character encoding of the stream.
   */
  public static NamedInputStream getInputStream (URL url)
    throws IOException
  {
    return new NamedInputStream (url.toString (), url.openStream(),
                                 LOOKAHEAD);
  }

  /**
   * Convenience method for xmljDocLoader
   */
  static NamedInputStream xmljGetInputStream(String base, String url)
    throws IOException
  {
    try
      {
        if (base != null)
          {
            url = new URL(new URL(base), url).toString();
          }
      }
    catch (MalformedURLException e)
      {
      }
    InputStream in = getInputStream(url);
    return new NamedInputStream(url, in, LOOKAHEAD);
  }

  /**
   * Returns an output stream for the specified transformer result.
   */
  public static OutputStream getOutputStream (Result result)
    throws IOException
  {
    OutputStream out = null;
    if (result instanceof StreamResult)
      {
        out = ((StreamResult) result).getOutputStream ();
      }
    if (out == null)
      {
       Writer w = ((StreamResult) result).getWriter ();
       if (w != null)
         out = new WriterOutputStream (w);
      }
    if (out == null)
      {
        String systemId = result.getSystemId ();
        if (systemId == null)
          {
            throw new IOException ("no system ID");
          }
        try
          {
            URL url = new URL (systemId);
            URLConnection connection = url.openConnection ();
            connection.setDoOutput (true);
            out = connection.getOutputStream ();
          }
        catch (MalformedURLException e)
          {
            out = new FileOutputStream (systemId);
          }
      }

    return out;
  }

  /**
   * Returns the absolute form of the specified URI.
   * If the URI is already absolute, returns it as-is.
   * Otherwise returns a new URI relative to the given base URI.
   */
  public static String getAbsoluteURI (String base, String uri)
  {
    if (uri != null &&
        base != null &&
        (uri.length() > 0) &&
        (uri.indexOf(':') == -1) &&
        (uri.charAt(0) != '/'))
      {
        // URI is relative
        if (base.charAt(base.length() - 1) != '/')
          {
            int i = base.lastIndexOf('/');
            base = base.substring(0, i + 1);
          }
        return base + uri;
      }
    else
      {
        // URI is absolute or no base specified
        return uri;
      }
  }

  public static String getBaseURI(String uri)
  {
    if (uri != null)
      {
        int si = uri.lastIndexOf('/');
        if (si != -1)
          {
            uri = uri.substring(0, si + 1);
          }
      }
    return uri;
  }
  
}
