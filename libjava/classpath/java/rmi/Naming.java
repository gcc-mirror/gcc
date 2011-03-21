/* Naming.java --
   Copyright (c) 1996, 1997, 1998, 1999, 2004, 2006
   Free Software Foundation, Inc.

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


package java.rmi;

import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;

/**
 * <p>
 * The <code>Naming</code> class handles interactions with RMI registries.
 * Each method takes a URL in <code>String</code> form, which points to
 * the RMI registry.  The scheme of the URL is irrelevant.  The relevant
 * part is:
 * </p>
 * <p>
 * <code>//host:port/name</code>
 * </p>
 * <p>
 * which tells the method how to locate and access the registry.  The host
 * and port are both optional, and default to `localhost' and the standard
 * RMI registry port (1099) respectively.  The name is simply a string
 * used to refer to a particular service hosted by the registry.  The
 * registry does not attempt to interpret this further.
 * </p>
 * <p>
 * RMI services are registered using one of these names, and the same name
 * is later used by the client to lookup the service and access its methods.
 * Registries can be shared by multiple services, or a service can create
 * its own registry using <code>createRegistry()</code>.
 * </p>
 *
 * @author Original author unknown.
 * @author Ingo Proetel (proetel@aicas.com)
 * @author Guilhem Lavaux (guilhem@kaffe.org)
 * @author Jeroen Frijters (jeroen@frijters.net)
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.1
 */
public final class Naming
{
  /**
   * This class isn't intended to be instantiated.
   */
  private Naming()
  {
  }

  /**
   * Looks for the remote object that is associated with the named service.
   * Name and location is given in form of a URL without a scheme:
   *
   * <pre>
   * //host:port/service-name
   * </pre>
   *
   * The port is optional.
   *
   * @param name the service name and location
   * @return Remote-object that implements the named service
   * @throws NotBoundException if no object implements the service
   * @throws MalformedURLException
   * @throws RemoteException
   */
  public static Remote lookup(String name) throws NotBoundException,
    MalformedURLException, RemoteException
  {
    URL u = parseURL(name);
    String serviceName = getName(u);
    return (getRegistry(u).lookup(serviceName));
  }

  /**
   * Try to bind the given object to the given service name.
   *
   * @param name
   * @param obj
   * @throws AlreadyBoundException
   * @throws MalformedURLException
   * @throws RemoteException
   */
  public static void bind(String name, Remote obj)
    throws AlreadyBoundException, MalformedURLException, RemoteException
  {
    URL u = parseURL(name);
    String serviceName = getName(u);
    getRegistry(u).bind(serviceName, obj);
  }

  /**
   * Remove a binding for a given service name.
   *
   * @param name
   * @throws RemoteException
   * @throws NotBoundException
   * @throws MalformedURLException
   */
  public static void unbind(String name) throws RemoteException,
    NotBoundException, MalformedURLException
  {
    URL u = parseURL(name);
    String serviceName = getName(u);
    getRegistry(u).unbind(serviceName);
  }

  /**
   * Forces the binding between the given Remote-object and the given service
   * name, even if there was already an object bound to this name.
   *
   * @param name
   * @param obj
   * @throws RemoteException
   * @throws MalformedURLException
   */
  public static void rebind(String name, Remote obj) throws RemoteException,
    MalformedURLException
  {
    URL u = parseURL(name);
    String serviceName = getName(u);
    getRegistry(u).rebind(serviceName, obj);
  }

  /**
   * Lists all services at the named registry.
   *
   * @param name url that specifies the registry
   * @return list of services at the name registry
   * @throws RemoteException
   * @throws MalformedURLException
   */
  public static String[] list(String name) throws RemoteException,
    MalformedURLException
  {
    return (getRegistry(parseURL(name)).list());
  }

  private static Registry getRegistry(URL u) throws RemoteException
  {
    if (u.getPort() == - 1)
      {
        return (LocateRegistry.getRegistry(u.getHost()));
      }
    else
      {
        return (LocateRegistry.getRegistry(u.getHost(), u.getPort()));
      }
  }

  /**
   * Parses the supplied URL and converts it to use the HTTP protocol. From an
   * RMI perspective, the scheme is irrelevant and we want to be able to create
   * a URL for which a handler is available.
   *
   * @param name the URL in String form.
   * @throws MalformedURLException if the URL is invalid.
   */
  private static URL parseURL(String name) throws MalformedURLException
  {
    try
      {
        URI uri = new URI(name);
        String host = uri.getHost();
        int port = uri.getPort();
        String query = uri.getQuery();
        String path = uri.getPath();
        return new URL("http", (host == null ? "localhost" : host),
            (port == - 1 ? 1099 : port), uri.getPath()
                                         + (query == null ? "" : query));
      }
    catch (URISyntaxException e)
      {
        throw new MalformedURLException("The URL syntax was invalid: "
                                        + e.getMessage());
      }
  }

  /**
   * Checks that the URL contains a name, and removes any leading slashes.
   *
   * @param url the URL to check.
   * @throws MalformedURLException if no name is specified.
   */
  private static String getName(URL url) throws MalformedURLException
  {
    String filename = url.getFile();
    if (filename.length() == 0)
      throw new MalformedURLException("No path specified: " + url);
    // If the filename begins with a slash we must cut it for
    // name resolution.
    if (filename.charAt(0) == '/')
      return filename.substring(1);
    return filename;
  }
}
