/* SocketPermission.java -- Class modeling permissions for socket operations
   Copyright (C) 1998, 2000, 2001, 2002, 2004, 2006 Free Software
   Foundation, Inc.

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

package java.net;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.security.Permission;
import java.security.PermissionCollection;
import java.util.StringTokenizer;


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
 * @author Written by Aaron M. Renn (arenn@urbanophile.com)
 * @author Extensively modified by Gary Benson (gbenson@redhat.com)
 */
public final class SocketPermission extends Permission implements Serializable
{
  static final long serialVersionUID = -7204263841984476862L;

  /**
   * A hostname (possibly wildcarded).  Will be set if and only if
   * this object was initialized with a hostname.
   */
  private transient String hostname = null;

  /**
   * An IP address (IPv4 or IPv6).  Will be set if and only if this
   * object was initialized with a single literal IP address.
   */  
  private transient InetAddress address = null;
  
  /**
   * A range of ports.
   */
  private transient int minport;
  private transient int maxport;

  /**
   * Values used for minimum and maximum ports when one or both bounds
   * are omitted.  This class is essentially independent of the
   * networking code it describes, so we do not limit ports to the
   * usual network limits of 1 and 65535.
   */
  private static final int MIN_PORT = 0;
  private static final int MAX_PORT = Integer.MAX_VALUE;

  /**
   * The actions for which we have permission.  This field is present
   * to make the serialized form correct and should not be used by
   * anything other than writeObject: everything else should use
   * actionmask.
   */
  private String actions;

  /**
   * A bitmask representing the actions for which we have permission.
   */
  private transient int actionmask;

  /**
   * The available actions, in the canonical order required for getActions().
   */
  private static final String[] ACTIONS = new String[] {
    "connect", "listen", "accept", "resolve"};

  /**
   * Initializes a new instance of <code>SocketPermission</code> with the
   * specified host/port combination and actions string.
   *
   * @param hostport The hostname/port number combination
   * @param actions The actions string
   */
  public SocketPermission(String hostport, String actions)
  {
    super(processHostport(hostport));

    setHostPort(getName());
    setActions(actions);
  }

  /**
   * There are two cases in which hostport needs rewriting before
   * being passed to the superclass constructor.  If hostport is an
   * empty string then it is substituted with "localhost".  And if
   * the host part of hostport is a literal IPv6 address in the full
   * uncompressed form not enclosed with "[" and "]" then we enclose
   * it with them.
   */
  private static String processHostport(String hostport)
  {
    if (hostport.length() == 0)
      return "localhost";

    if (hostport.charAt(0) == '[')
      return hostport;

    int colons = 0;
    boolean colon_allowed = true;
    for (int i = 0; i < hostport.length(); i++)
      {
	if (hostport.charAt(i) == ':')
	  {
	    if (!colon_allowed)
	      throw new IllegalArgumentException("Ambiguous hostport part");
	    colons++;
	    colon_allowed = false;
	  }
	else
	  colon_allowed = true;
      }

    switch (colons)
      {
      case 0:
      case 1:
	// a hostname or IPv4 address
	return hostport;
	
      case 7:
	// an IPv6 address with no ports
	return "[" + hostport + "]";

      case 8:
	// an IPv6 address with ports
	int last_colon = hostport.lastIndexOf(':');
	return "[" + hostport.substring(0, last_colon) + "]"
	  + hostport.substring(last_colon);

      default:
	throw new IllegalArgumentException("Ambiguous hostport part");
      }
  }
  
  /**
   * Parse the hostport argument to the constructor.
   */
  private void setHostPort(String hostport)
  {
    // Split into host and ports
    String host, ports;
    if (hostport.charAt(0) == '[')
      {
	// host is a bracketed IPv6 address
	int end = hostport.indexOf("]");
	if (end == -1)
	  throw new IllegalArgumentException("Unmatched '['");
	host = hostport.substring(1, end);

	address = InetAddress.getByLiteral(host);
	if (address == null)
	  throw new IllegalArgumentException("Bad IPv6 address");

	if (end == hostport.length() - 1)
	  ports = "";
	else if (hostport.charAt(end + 1) == ':')
	  ports = hostport.substring(end + 2);
	else
	  throw new IllegalArgumentException("Bad character after ']'");
      }
    else
      {
	// host is a hostname or IPv4 address
	int sep = hostport.indexOf(":");
	if (sep == -1)
	  {
	    host = hostport;
	    ports = "";
	  }
	else
	  {
	    host = hostport.substring(0, sep);
	    ports = hostport.substring(sep + 1);
	  }

	address = InetAddress.getByLiteral(host);
	if (address == null)
	  {
	    if (host.lastIndexOf('*') > 0)
	      throw new IllegalArgumentException("Bad hostname");

	    hostname = host;
	  }
      }

    // Parse and validate the ports
    if (ports.length() == 0)
      {
	minport = MIN_PORT;
	maxport = MAX_PORT;
      }
    else
      {
	int sep = ports.indexOf("-");
	if (sep == -1)
	  {
	    // a single port
	    minport = maxport = Integer.parseInt(ports);
	  }
	else
	  {
	    if (ports.indexOf("-", sep + 1) != -1)
	      throw new IllegalArgumentException("Unexpected '-'");

	    if (sep == 0)
	      {
		// an upper bound
		minport = MIN_PORT;
		maxport = Integer.parseInt(ports.substring(1));
	      }
	    else if (sep == ports.length() - 1)
	      {
		// a lower bound
		minport =
		  Integer.parseInt(ports.substring(0, ports.length() - 1));
		maxport = MAX_PORT;
	      }
	    else
	      {
		// a range with two bounds
		minport = Integer.parseInt(ports.substring(0, sep));
		maxport = Integer.parseInt(ports.substring(sep + 1));
	      }
	  }
      }
  }
  
  /**
   * Parse the actions argument to the constructor.
   */
  private void setActions(String actionstring)
  {
    actionmask = 0;

    boolean resolve_needed = false;
    boolean resolve_present = false;
    
    StringTokenizer t = new StringTokenizer(actionstring, ",");
    while (t.hasMoreTokens())
      {
	String action = t.nextToken();
	action = action.trim().toLowerCase();
	setAction(action);

	if (action.equals("resolve"))
	  resolve_present = true;
	else
	  resolve_needed = true;
      }

    if (resolve_needed && !resolve_present)
      setAction("resolve");
  }

  /**
   * Parse one element of the actions argument to the constructor.
   */
  private void setAction(String action)
  {
    for (int i = 0; i < ACTIONS.length; i++)
      {
	if (action.equals(ACTIONS[i]))
	  {
	    actionmask |= 1 << i;
	    return;
	  }
      }
    throw new IllegalArgumentException("Unknown action " + action);
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
    SocketPermission p;

    if (obj instanceof SocketPermission)
      p = (SocketPermission) obj;
    else
      return false;

    if (p.actionmask != actionmask ||
	p.minport != minport ||
	p.maxport != maxport)
      return false;

    if (address != null)
      {
	if (p.address == null)
	  return false;
	else
	  return p.address.equals(address);
      }
    else
      {
	if (p.hostname == null)
	  return false;
	else
	  return p.hostname.equals(hostname);
      }
  }

  /**
   * Returns a hash code value for this object.  Overrides the
   * <code>Permission.hashCode()</code>.
   *
   * @return A hash code
   */
  public int hashCode()
  {
    int code = actionmask + minport + maxport;
    if (address != null)
      code += address.hashCode();
    else
      code += hostname.hashCode();
    return code;
  }

  /**
   * Returns the list of permission actions in this object in canonical
   * order.  The canonical order is "connect,listen,accept,resolve"
   *
   * @return The permitted action string.
   */
  public String getActions()
  {
    StringBuffer sb = new StringBuffer("");

    for (int i = 0; i < ACTIONS.length; i++)
      {
	if ((actionmask & (1 << i)) != 0)
	  {
	    if (sb.length() != 0)
	      sb.append(",");
	    sb.append(ACTIONS[i]);
	  }
      }

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
   * Returns an array of all IP addresses represented by this object.
   */
  private InetAddress[] getAddresses()
  {
    if (address != null)
      return new InetAddress[] {address};

    try
      {
	return InetAddress.getAllByName(hostname);
      }
    catch (UnknownHostException e)
      {
	return new InetAddress[0];
      }
  }

  /**
   * Returns the canonical hostname represented by this object,
   * or null if this object represents a wildcarded domain.
   */
  private String getCanonicalHostName()
  {
    if (address != null)
      return address.internalGetCanonicalHostName();
    if (hostname.charAt(0) == '*')
      return null;
    try
      {
	return InetAddress.getByName(hostname).internalGetCanonicalHostName();
      }
    catch (UnknownHostException e)
      {
	return null;
      }
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

    // If p was initialised with an empty hostname then we do not
    // imply it. This is not part of the spec, but it seems necessary.
    if (p.hostname != null && p.hostname.length() == 0)
      return false;
    
    // Next check the actions
    if ((p.actionmask & actionmask) != p.actionmask)
	return false;

    // Then check the ports
    if ((p.minport < minport) || (p.maxport > maxport))
      return false;

    // Finally check the hosts
    String p_canon = null;

    // Return true if this object was initialized with a single
    // IP address which one of p's IP addresses is equal to.
    if (address != null)
      {
	InetAddress[] addrs = p.getAddresses();
	for (int i = 0; i < addrs.length; i++)
	  {
	    if (address.equals(addrs[i]))
	      return true;
	  }
      }

    // Return true if this object is a wildcarded domain that
    // p's canonical name matches.
    if (hostname != null && hostname.charAt(0) == '*')
      {
	p_canon = p.getCanonicalHostName();
	if (p_canon != null && p_canon.endsWith(hostname.substring(1)))
	  return true;
	
      }

    // Return true if this one of this object's IP addresses
    // is equal to one of p's.
    if (address == null)
      {
	InetAddress[] addrs = p.getAddresses();
	InetAddress[] p_addrs = p.getAddresses();

	for (int i = 0; i < addrs.length; i++)
	  {
	    for (int j = 0; j < p_addrs.length; j++)
	      {
		if (addrs[i].equals(p_addrs[j]))
		  return true;
	      }
	  }
      }

    // Return true if this object's canonical name equals p's.
    String canon = getCanonicalHostName();
    if (canon != null)
      {
	if (p_canon == null)
	  p_canon = p.getCanonicalHostName();
	if (p_canon != null && canon.equals(p_canon))
	  return true;
      }

    // Didn't make it
    return false;
  }

  /**
   * Deserializes a <code>SocketPermission</code> object from
   * an input stream.
   *
   * @param input the input stream.
   * @throws IOException if an I/O error occurs in the stream.
   * @throws ClassNotFoundException if the class of the
   *         serialized object could not be found.
   */
  private void readObject(ObjectInputStream input)
    throws IOException, ClassNotFoundException
  {
    input.defaultReadObject();
    setHostPort(getName());
    setActions(actions);
  }

  /**
   * Serializes a <code>SocketPermission</code> object to an
   * output stream.
   *
   * @param output the output stream.
   * @throws IOException if an I/O error occurs in the stream.
   */
  private void writeObject(ObjectOutputStream output)
    throws IOException
  {
    actions = getActions();
    output.defaultWriteObject();
  }
}
