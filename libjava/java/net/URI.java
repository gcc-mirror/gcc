/* URI.java - An URI class
   Copyright (C) 2002 Free Software Foundation, Inc.

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

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;

/**
 * @author Michael Koch <konqueror@gmx.de>
 * @since 1.4
 */
public final class URI
  implements Comparable, Serializable
{
  static final long serialVersionUID = -6052424284110960213L;

  String string;
  private String scheme;
  private String schemeSpecificPart;
  private String authority;
  private String userInfo;
  private String host;
  private int port;
  private String path;
  private String query;
  private String fragment;

  private void readObject (ObjectInputStream is)
    throws ClassNotFoundException, IOException
  {
  }

  private void writeObject (ObjectOutputStream is)
    throws IOException
  {
  }

  private void parseURI (String str)
    throws URISyntaxException
  {
  }
  
  /**
   * Creates an URI from the given string
   *
   * @param str The string to create the URI from
   *
   * @exception URISyntaxException If the given string violates RFC 2396
   * @exception NullPointerException If str is null
   */
  public URI (String str)
    throws URISyntaxException
  {
  }
 
  /**
   * Create an URI from the given components
   *
   * @param scheme The scheme name
   * @param userInfo The username and authorization info
   * @param host The hostname
   * @param port The port number
   * @param path The path
   * @param query The query
   * @param fragment The fragment
   *
   * @exception URISyntaxException If the given string violates RFC 2396
   */
  public URI (String scheme, String userInfo, String host, int port,
	     String path, String query, String fragment)
    throws URISyntaxException
  {
  }

  /**
   * Create an URI from the given components
   *
   * @param scheme The scheme name
   * @param authority The authority
   * @param path The apth
   * @param query The query
   * @param fragment The fragmen
   *
   * @exception URISyntaxException If the given string violates RFC 2396
   */
  public URI (String scheme, String authority, String path, String query,
	     String fragment)
    throws URISyntaxException
  {
  }

  /**
   * Create an URI from the given components
   *
   * @param scheme The scheme name
   * @param host The hostname
   * @param path The path
   * @param fragment The fragment
   *
   * @exception URISyntaxException If the given string violates RFC 2396
   */
  public URI (String scheme, String host, String path, String fragment)
    throws URISyntaxException
  {
  }

  /**
   * Create an URI from the given components
   *
   * @param scheme The scheme name
   * @param ssp The scheme specific part
   * @param fragment The fragment
   *
   * @exception URISyntaxException If the given string violates RFC 2396
   */
  public URI (String scheme, String ssp, String fragment)
    throws URISyntaxException
  {
  }

  /**
   * Create an URI from the given string
   *
   * @param str The string to create the URI from
   *
   * @exception IllegalArgumentException If the given string violates RFC 2396
   * @exception NullPointerException If str is null
   */
  public static URI create (String str)
  {
    return null;
  }

  /**
   * Attempts to parse this URI's authority component, if defined,
   * into user-information, host, and port components
   *
   * @exception URISyntaxException If the given string violates RFC 2396
   */
  public URI parseServerAuthority ()
     throws URISyntaxException
  {
    return null;
  }

  /**
   * Returns a normalizes versions of the URI
   */
  public URI normalize ()
  {
    return null;
  }

  /**
   * Resolves the given URI against this URI
   *
   * @param uri The URI to resolve against this URI
   *
   * @return The resulting URI
   *
   * @exception NullPointerException If uri is null
   */
  public URI resolve (URI uri)
  { 
    return null;
  }

  /**
   * Resolves the given URI string against this URI
   *
   * @param str The URI as string to resolve against this URI
   *
   * @return The resulting URI
   *
   * @exception IllegalArgumentException If the given URI string
   * violates RFC 2396
   * @exception NullPointerException If uri is null
   */
  public URI resolve (String str)
    throws IllegalArgumentException
  {
    return null;
  }

  /**
   * Relativizes the given URI against this URI
   *
   * @param uri The URI to relativize this URI
   *
   * @return The resulting URI
   *
   * @exception NullPointerException If uri is null
   */
  public URI relativize (URI uri)
  {
    return null;
  }

  /**
   * Creates an URL from an URI
   *
   * @exception MalformedURLException If a protocol handler for the URL could
   * not be found, or if some other error occurred while constructing the URL
   * @exception IllegalArgumentException If the URI is not absolute
   */
  public URL toURL ()
    throws IllegalArgumentException, MalformedURLException
  {
    return null;
  }

  /**
   * Returns the scheme of the URI
   */
  public String getScheme ()
  {
    return scheme;
  }

  /**
   * Tells whether this URI is absolute or not
   */
  public boolean isAbsolute ()
  {
    return false;
  }

  /**
   * Tell whether this URI is opaque or not
   */
  public boolean isOpaque ()
  {
    return false;
  }

  /**
   * Returns the raw scheme specific part of this URI.
   * The scheme-specific part is never undefined, though it may be empty
   */
  public String getRawSchemeSpecificPart ()
  {
    return null;
  }

  /**
   * Returns the decoded scheme specific part of this URI.
   */
  public String getSchemeSpecificPart ()
  {
    return null;
  }

  /**
   * Returns the rae authority part of this URI
   */
  public String getRawAuthority ()
  {
    return authority;
  }

  /**
   * Returns the decoded authority part of this URI
   */
  public String getAuthority ()
  {
    return null;
  }

  /**
   * Returns the raw user info part of this URI
   */
  public String getRawUserInfo ()
  {
    return userInfo;
  }

  /**
   * Returns the decoded user info part of this URI
   */
  public String getUserInfo ()
  {
    return null;
  }

  /**
   * Returns the hostname of the URI
   */
  public String getHost ()
  {
    return host;
  }

  /**
   * Returns the port number of the URI
   */
  public int getPort ()
  {
    return port;
  }

  /**
   * Returns the raw path part of this URI
   */
  public String getRawPath ()
  {
    return path;
  }

  /**
   * Returns the path of the URI
   */
  public String getPath ()
  {
    return null;
  }

  /**
   * Returns the raw query part of this URI
   */
  public String getRawQuery ()
  {
    return query;
  }

  /**
   * Returns the query of the URI
   */
  public String getQuery ()
  {
    return null;
  }

  /**
   * Return the raw fragment part of this URI
   */
  public String getRawFragment ()
  {
    return fragment;
  }

  /**
   * Returns the fragment of the URI
   */
  public String getFragment ()
  {
    return null;
  }

  /**
   * Compares the URI with a given object
   *
   * @param obj The obj to compare the URI with
   */
  public boolean equals(Object obj)
  {
    return false;
  }

  /**
   * Computes the hascode of the URI
   */
  public int hashCode ()
  {
    return 0;
  }

  /**
   * Compare the URI with another object that must be an URI too
   *
   * @param obj This object to compare this URI with
   *
   * @exception ClassCastException If given object ist not an URI
   */
  public int compareTo (Object obj)
    throws ClassCastException
  {
    return 0;
  }

  /**
   * Returns the URI as string
   */
  public String toString ()
  {
    return "";
  }

  /**
   * Returns the URI as US-ASCII string
   */
  public String toASCIIString ()
  {
    return "";
  }
}
