/* SocketPermission.java -- Class modeling permissions for socket operations
   Copyright (C) 1998, 2000, 2001, 2002, 2004  Free Software Foundation, Inc.

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

import java.io.Serializable;
import java.security.Permission;
import java.security.PermissionCollection;


/**
 * This class models a specific set of permssions for connecting to a
 * host.  There are two elements to this, the host/port combination and
 * the permission list.
 * <p>
 * The host/port combination is specified as followed
 * <p>
 * <pre>
 * hostname[:[-]port[-[port]]]
 * </pre>
 * <p>
 * The hostname portion can be either a hostname or IP address.  If it is
 * a hostname, a wildcard is allowed in hostnames.  This wildcard is a "*"
 * and matches one or more characters.  Only one "*" may appear in the
 * host and it must be the leftmost character.  For example,
 * "*.urbanophile.com" matches all hosts in the "urbanophile.com" domain.
 * <p>
 * The port portion can be either a single value, or a range of values
 * treated as inclusive.  The first or the last port value in the range
 * can be omitted in which case either the minimum or maximum legal
 * value for a port (respectively) is used by default.  Here are some
 * examples:
 * <p><ul>
 * <li>8080 - Represents port 8080 only</li>
 * <li>2000-3000 - Represents ports 2000 through 3000 inclusive</li>
 * <li>-4000 - Represents ports 0 through 4000 inclusive</li>
 * <li>1024- - Represents ports 1024 through 65535 inclusive</li>
 * </ul><p>
 * The permission list is a comma separated list of individual permissions.
 * These individual permissions are:
 * <p>
 * <pre>
 * accept
 * connect
 * listen
 * resolve
 * </pre>
 * <p>
 * The "listen" permission is only relevant if the host is localhost.  If
 * any permission at all is specified, then resolve permission is implied to
 * exist.
 * <p>
 * Here are a variety of examples of how to create SocketPermission's
 * <p><pre>
 * SocketPermission("www.urbanophile.com", "connect");
 *   Can connect to any port on www.urbanophile.com
 * SocketPermission("www.urbanophile.com:80", "connect,accept");
 *   Can connect to or accept connections from www.urbanophile.com on port 80
 * SocketPermission("localhost:1024-", "listen,accept,connect");
 *   Can connect to, accept from, an listen on any local port number 1024
 *   and up.
 * SocketPermission("*.edu", "connect");
 *   Can connect to any host in the edu domain
 * SocketPermission("197.197.20.1", "accept");
 *   Can accept connections from 197.197.20.1
 * </pre><p>
 *
 * This class also supports IPv6 addresses.  These should be specified
 * in either RFC 2732 format or in full uncompressed form.
 *
 * @since 1.2
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 */
public final class SocketPermission extends Permission implements Serializable
{
  static final long serialVersionUID = -7204263841984476862L;

// FIXME: Needs serialization work, including readObject/writeObject methods.

  /**
   * A hostname/port combination as described above
   */
  private transient String hostport;

  /**
   * A comma separated list of actions for which we have permission
   */
  private String actions;

  /**
   * Initializes a new instance of <code>SocketPermission</code> with the
   * specified host/port combination and actions string.
   *
   * @param hostport The hostname/port number combination
   * @param actions The actions string
   */
  public SocketPermission(String hostport, String actions)
  {
    super(hostport);

    this.hostport = hostport;
    this.actions = actions;
  }

  /**
   * Tests this object for equality against another.  This will be true if
   * and only if the passed object is an instance of
   * <code>SocketPermission</code> and both its hostname/port combination
   * and permissions string are identical.
   *
   * @param obj The object to test against for equality
   *
   * @return <code>true</code> if object is equal to this object,
   *         <code>false</code> otherwise.
   */
  public boolean equals(Object obj)
  {
    if (! (obj instanceof SocketPermission))
      return false;

    if (((SocketPermission) obj).hostport.equals(hostport))
      if (((SocketPermission) obj).actions.equals(actions))
	return true;

    return false;
  }

  /**
   * Returns a hash code value for this object.  Overrides the
   * <code>Permission.hashCode()</code>.
   *
   * @return A hash code
   */
  public int hashCode()
  {
    int hash = 100;
    if (hostport != null)
      hash += hostport.hashCode();
    if (actions != null)
      hash += actions.hashCode();
    return hash;
  }

  /**
   * Returns the list of permission actions in this object in canonical
   * order.  The canonical order is "connect,listen,accept,resolve"
   *
   * @return The permitted action string.
   */
  public String getActions()
  {
    boolean found = false;
    StringBuffer sb = new StringBuffer("");

    if (actions.indexOf("connect") != -1)
      {
	sb.append("connect");
	found = true;
      }

    if (actions.indexOf("listen") != -1)
      if (found)
	sb.append(",listen");
      else
        {
	  sb.append("listen");
	  found = true;
        }

    if (actions.indexOf("accept") != -1)
      if (found)
	sb.append(",accept");
      else
        {
	  sb.append("accept");
	  found = true;
        }

    if (found)
      sb.append(",resolve");
    else if (actions.indexOf("resolve") != -1)
      sb.append("resolve");

    return sb.toString();
  }

  /**
   * Returns a new <code>PermissionCollection</code> object that can hold
   * <code>SocketPermission</code>'s.
   *
   * @return A new <code>PermissionCollection</code>.
   */
  public PermissionCollection newPermissionCollection()
  {
    // FIXME: Implement

    return null;
  }

  /**
   * Returns true if the permission object passed it is implied by the
   * this permission.  This will be true if:
   * 
   * <ul>
   * <li>The argument is of type <code>SocketPermission</code></li>
   * <li>The actions list of the argument are in this object's actions</li>
   * <li>The port range of the argument is within this objects port range</li>
   * <li>The hostname is equal to or a subset of this objects hostname</li>
   * </ul>
   *
   * <p>The argument's hostname will be a subset of this object's hostname if:</p>
   * 
   * <ul>
   * <li>The argument's hostname or IP address is equal to this object's.</li>
   * <li>The argument's canonical hostname is equal to this object's.</li>
   * <li>The argument's canonical name matches this domains hostname with
   * wildcards</li>
   * </ul>
   *
   * @param perm The <code>Permission</code> to check against
   *
   * @return <code>true</code> if the <code>Permission</code> is implied by
   * this object, <code>false</code> otherwise.
   */
  public boolean implies(Permission perm)
  {
    SocketPermission p;

    // First make sure we are the right object type
    if (perm instanceof SocketPermission)
      p = (SocketPermission) perm;
    else
      return false;

    // Next check the actions
    String ourlist = getActions();
    String theirlist = p.getActions();

    if (! ourlist.startsWith(theirlist))
      return false;

    // Now check ports
    int ourfirstport = 0;

    // Now check ports
    int ourlastport = 0;

    // Now check ports
    int theirfirstport = 0;

    // Now check ports
    int theirlastport = 0;

    // Get ours
    if (hostport.indexOf(":") == -1)
      {
	ourfirstport = 0;
	ourlastport = 65535;
      }
    else
      {
	// FIXME:  Needs bulletproofing.
	// This will dump if hostport if all sorts of bad data was passed to
	// the constructor
	String range = hostport.substring(hostport.indexOf(":") + 1);
	if (range.startsWith("-"))
	  ourfirstport = 0;
	else if (range.indexOf("-") == -1)
	  ourfirstport = Integer.parseInt(range);
	else
	  ourfirstport =
	    Integer.parseInt(range.substring(0, range.indexOf("-")));

	if (range.endsWith("-"))
	  ourlastport = 65535;
	else if (range.indexOf("-") == -1)
	  ourlastport = Integer.parseInt(range);
	else
	  ourlastport =
	    Integer.parseInt(range.substring(range.indexOf("-") + 1,
	                                     range.length()));
      }

    // Get theirs
    if (p.hostport.indexOf(":") == -1)
      {
	theirfirstport = 0;
	ourlastport = 65535;
      }
    else
      {
	// This will dump if hostport if all sorts of bad data was passed to
	// the constructor
	String range = p.hostport.substring(hostport.indexOf(":") + 1);
	if (range.startsWith("-"))
	  theirfirstport = 0;
	else if (range.indexOf("-") == -1)
	  theirfirstport = Integer.parseInt(range);
	else
	  theirfirstport =
	    Integer.parseInt(range.substring(0, range.indexOf("-")));

	if (range.endsWith("-"))
	  theirlastport = 65535;
	else if (range.indexOf("-") == -1)
	  theirlastport = Integer.parseInt(range);
	else
	  theirlastport =
	    Integer.parseInt(range.substring(range.indexOf("-") + 1,
	                                     range.length()));
      }

    // Now check them
    if ((theirfirstport < ourfirstport) || (theirlastport > ourlastport))
      return false;

    // Finally we can check the hosts
    String ourhost;

    // Finally we can check the hosts
    String theirhost;

    // Get ours
    if (hostport.indexOf(":") == -1)
      ourhost = hostport;
    else
      ourhost = hostport.substring(0, hostport.indexOf(":"));

    // Get theirs
    if (p.hostport.indexOf(":") == -1)
      theirhost = p.hostport;
    else
      theirhost = p.hostport.substring(0, p.hostport.indexOf(":"));

    // Are they equal?
    if (ourhost.equals(theirhost))
      return true;

    // Try the canonical names
    String ourcanonical = null;

    // Try the canonical names
    String theircanonical = null;
    try
      {
	ourcanonical = InetAddress.getByName(ourhost).getHostName();
	theircanonical = InetAddress.getByName(theirhost).getHostName();
      }
    catch (UnknownHostException e)
      {
	// Who didn't resolve?  Just assume current address is canonical enough
	// Is this ok to do?
	if (ourcanonical == null)
	  ourcanonical = ourhost;
	if (theircanonical == null)
	  theircanonical = theirhost;
      }

    if (ourcanonical.equals(theircanonical))
      return true;

    // Well, last chance.  Try for a wildcard
    if (ourhost.indexOf("*.") != -1)
      {
	String wild_domain = ourhost.substring(ourhost.indexOf("*" + 1));
	if (theircanonical.endsWith(wild_domain))
	  return true;
      }

    // Didn't make it
    return false;
  }
}
