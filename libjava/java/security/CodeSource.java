/* CodeSource.java -- Code location and certifcates
   Copyright (C) 1998 Free Software Foundation, Inc.

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

package java.security;

import java.io.Serializable;
import java.net.URL;
import java.net.SocketPermission;

/**
 * This class represents a location from which code is loaded (as
 * represented by a URL) and the list of certificates that are used to
 * check the signatures of signed code loaded from this source.
 *
 * @version 0.0
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 */
public class CodeSource implements Serializable
{
  private static final String linesep = System.getProperty("line.separator");

  /**
   * This is the URL that represents the code base from which code will
   * be loaded.
   */
  private URL location;

  /**
   * This is the list of certificates for this code base
   */
  // What is the serialized form of this?
  private java.security.cert.Certificate[] certs;

  /**
   * This method initializes a new instance of <code>CodeSource</code> that
   * loads code from the specified URL location and which uses the 
   * specified certificates for verifying signatures.
   *
   * @param location The location from which code will be loaded
   * @param certs The list of certificates used for verifying signatures on code from this source
   */
  public CodeSource(URL location, java.security.cert.Certificate[] certs)
  {
    this.location = location;
    this.certs = certs;
  }

  /**
   * This method returns the URL specifying the location from which code
   * will be loaded under this <code>CodeSource</code>.
   *
   * @return The code location for this <code>CodeSource</code>.
   */
  public final URL getLocation()
  {
    return location;
  }

  /**
   * This method returns the list of digital certificates that can be used
   * to verify the signatures of code loaded under this <code>CodeSource</code>.
   *
   * @return The certifcate list for this <code>CodeSource</code>.
   */
  public final java.security.cert.Certificate[] getCertificates()
  {
    return certs;
  }

  /**
   * This method tests to see if a specified <code>CodeSource</code> is 
   * implied by this object.  Effectively, to meet this test, the specified
   * object must have all the certifcates this object has (but may have 
   * more) and must have a location that is a subset of this object's.  In order
   * for this object to imply the specified object, the following must be
   * true:
   * <p>
   * <ol>
   * <li>The specified <code>CodeSource</code> must not be <code>null</code>.
   * <li>If the specified <code>CodeSource</code> has a certificate list, 
   * all of that object's certificates must be present in the certificate
   * list of this object.
   * <li>If this object does not have a <code>null</code> location, then
   * the following addtional tests must be passed.
   * <ol>
   * <li>The specified <code>CodeSource</code> must not have a <code>null</code> location.
   * <li>The specified <code>CodeSource</code>'s location must be equal to
   * this object's location, or<br>
   * <ul>
   * <li>The specifiec <code>CodeSource</code>'s location protocol, port, 
   * and ref (aka, anchor) must equal this objects, and
   * <li>The specified <code>CodeSource</code>'s location host must imply this
   * object's location host, as determined by contructing 
   * <code>SocketPermission</code> objects from each with no action list and
   * using that classes's <code>implies</code> method. And,
   * <li>If this object's location file ends with a '/', then the specified
   * object's location file must start with this object's location file.
   * Otherwise, the specified object's location file must start with this
   * object's location file with the '/' character appended to it.
   * </ul>
   * </ol>
   * </ol>
   *
   * @param cs The <code>CodeSource</code> to test against this object
   *
   * @return <code>true</code> if this specified <code>CodeSource</code> is specified by this object, <code>false</code> otherwise.
   */
  public boolean implies(CodeSource cs)
  {
    if (cs == null)
      return false;

    // First check the certificate list
    java.security.cert.Certificate[] their_certs = cs.getCertificates();
    java.security.cert.Certificate[] our_certs = getCertificates();

    if (our_certs != null)
      {
	if (their_certs == null)
	  return false;

	for (int i = 0; i < our_certs.length; i++)
	  {
	    int j;
	    for (j = 0; j < their_certs.length; j++)
	      if (our_certs[i].equals(their_certs[j]))
		break;

	    if (j == their_certs.length)
	      return false;
	  }
      }

    // Next check the location
    URL their_loc = getLocation();
    URL our_loc = getLocation();

    if (our_loc == null)
      return true;
    else if (their_loc == null)
      return false;

    if (!our_loc.getProtocol().equals(their_loc.getProtocol()))
      return false;

    if (our_loc.getPort() != -1)
      if (our_loc.getPort() != their_loc.getPort())
	return false;

    if (our_loc.getRef() != null)
      if (!our_loc.getRef().equals(their_loc.getRef()))
	return false;

    // See javadoc comments for what we are doing here.
    if (our_loc.getHost() != null)
      {
	String their_host = their_loc.getHost();
	if (their_host == null)
	  return false;

	SocketPermission our_sockperm =
	  new SocketPermission(our_loc.getHost(), "accept");
	SocketPermission their_sockperm =
	  new SocketPermission(their_host, "accept");

	if (!our_sockperm.implies(their_sockperm))
	  return false;
      }

    String our_file = our_loc.getFile();
    if (our_file != null)
      {
	if (!our_file.endsWith("/"))
	  our_file = our_file + "/";

	String their_file = their_loc.getFile();
	if (their_file == null)
	  return false;

	if (!their_file.startsWith(our_file))
	  return false;
      }

    return true;
  }

  /**
   * This method tests the specified <code>Object</code> for equality with
   * this object.  This will be true if and only if:
   * <p>
   * <ul>
   * <li>The specified object is not <code>null</code>.
   * <li>The specified object is an instance of <code>CodeSource</code>.
   * <li>The specified object's location is the same as this object's.
   * <li>The specified object's certificate list contains the exact same
   * entries as the object's.  Note that the order of the certificate lists
   * is not significant.
   * </ul>
   *
   * @param obj The <code>Object</code> to test against.
   *
   * @return <code>true</code> if the specified object is equal to this one, <code>false</code> otherwise.
   */
  public boolean equals(Object obj)
  {
    if (obj == null)
      return false;

    if (!(obj instanceof CodeSource))
      return false;

    CodeSource cs = (CodeSource) obj;

    // First check the certificate list
    java.security.cert.Certificate[] their_certs = cs.getCertificates();
    java.security.cert.Certificate[] our_certs = getCertificates();

    if ((our_certs == null) && (their_certs != null))
      return false;
    else if ((our_certs != null) && (their_certs == null))
      return false;

    if (our_certs != null)
      {
	if (our_certs.length != their_certs.length)
	  return false;

	for (int i = 0; i < our_certs.length; i++)
	  {
	    int j;
	    for (j = 0; j < their_certs.length; j++)
	      if (our_certs[i].equals(their_certs[j]))
		break;

	    if (j == their_certs.length)
	      return false;
	  }
      }

    // Now the location
    URL their_loc = cs.getLocation();
    URL our_loc = getLocation();

    if ((our_loc == null) && (their_loc != null))
      return false;

    if (!our_loc.equals(their_loc))
      return false;

    return true;
  }

  /**
   * This method returns a hash value for this object.
   *
   * @return A hash value for this object.
   */
  public int hashCode()
  {
    URL location = getLocation();
    if (location == null)
      return System.identityHashCode(this);

    return location.hashCode();
  }

  /**
   * This method returns a <code>String</code> that represents this object.
   * This <code>String</code> will contain the object's hash code, location,
   * and certificate list.
   *
   * @return A <code>String</code> for this object
   */
  public String toString()
  {
    StringBuffer sb = new StringBuffer("");

    sb.append(super.toString() + " (" + linesep);
    sb.append("Location: " + getLocation() + linesep);

    java.security.cert.Certificate[] certs = getCertificates();
    if (certs == null)
      sb.append("<none>" + linesep);
    else
      for (int i = 0; i < certs.length; i++)
	sb.append(certs[i] + linesep);

    sb.append(")" + linesep);

    return sb.toString();
  }
}
