/* Security.java --- Java base security class implmentation
   Copyright (C) 1999, 2001, 2002, 2003 Free Software Foundation, Inc.

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
import java.io.File;
import java.io.InputStream;
import java.io.IOException;
import java.io.FileNotFoundException;
import java.net.URL;
import java.security.Provider;
import java.util.Vector;
import java.util.Enumeration;
import java.util.Properties;

/**
   Security class that loads the Providers and provides an 
   interface to security properties.

   @author Mark Benvenuto <ivymccough@worldnet.att.net>
 */

public final class Security extends Object
{
  private static Vector providers = new Vector();
  private static Properties secprops = new Properties();

  static
  {
    String base = System.getProperty("gnu.classpath.home.url");
    String vendor = System.getProperty("gnu.classpath.vm.shortname");

    // Try VM specific security file
    boolean loaded = loadProviders(base, vendor);

    // Append classpath standard provider if possible
    if (!loadProviders(base, "classpath") && !loaded && providers.size() == 0)
      {
	// No providers found and both security files failed to load properly.
	System.err.println
		("WARNING: could not properly read security provider files:");
	System.err.println
		("         " + base + "/security/" + vendor + ".security");
	System.err.println
		("         " + base + "/security/" + "classpath" + ".security");
	System.err.println
		("         Falling back to standard GNU security provider");
	providers.addElement(new gnu.java.security.provider.Gnu());
      }
  }

  // This class can't be instantiated.
  private Security ()
  {
  }

  /**
   * Tries to load the vender specific security providers from the given
   * base URL. Returns true if the resource could be read and completely
   * parsed successfully, false otherwise.
   */
  private static boolean loadProviders(String baseUrl, String vendor)
  {
    if (baseUrl == null || vendor == null)
      return false;

    boolean result = true;
    String secfilestr = baseUrl + "/security/" + vendor + ".security";
    try
      {
	InputStream fin = new URL(secfilestr).openStream();
	secprops.load(fin);

	int i = 1;
	String name;

	while ((name = secprops.getProperty("security.provider." + i)) !=
	       null)
	  {
	    Exception exception = null;

	    try
	      {
		providers.addElement(Class.forName(name).newInstance());
	      }
	    catch (ClassNotFoundException x)
	      {
	        exception = x;
	      }
	    catch (InstantiationException x)
	      {
	        exception = x;
	      }
	    catch (IllegalAccessException x)
	      {
	        exception = x;
	      }
	    if (exception != null)
	      {
		System.err.println ("WARNING: Error loading security provider "
				    + name + ": " + exception);
		result = false;
	      }
	    i++;
	  }
      }
    catch (IOException ignored)
      {
	result = false;
      }

    return result;
  }

  /**
     Gets a specific property for an algorithm. This is used to produce
     specialized algorithm parsers.

     @deprecated it used to a return the value of a propietary property
     for the "SUN" Cryptographic Service Provider to obtain 
     algorithm-specific parameters. Used AlogorithmParameters and 
     KeyFactory instead.

     @param algName name of algorithm to get property of 
     @param propName name of property to check

     @return a string containing the value of the property
   */
  public static String getAlgorithmProperty(String algName, String propName)
  {
    /* TODO: Figure out what this actually does */
    return null;
  }

  /**
     Adds a new provider, at a specified position. The position is the
     preference order in which providers are searched for requested algorithms.
     Note that it is not guaranteed that this preference will be respected. The
     position is 1-based, that is, 1 is most preferred, followed by 2, and so
     on.
     <p>
     If the given provider is installed at the requested position, the
     provider that used to be at that position, and all providers with a
     position greater than position, are shifted up one position (towards the
     end of the list of installed providers).
     <p>
     A provider cannot be added if it is already installed.
     <p>
     <b>NOT IMPLEMENTED YET:</b>[
     First, if there is a security manager, its <code>checkSecurityAccess</code>
     method is called with the string
     <code>"insertProvider."+provider.getName()</code>
     to see if it's ok to add a new provider. If the default implementation of
     <code>checkSecurityAccess</code> is used (i.e., that method is not
     overriden), then this will result in a call to the security manager's
     <code>checkPermission</code> method with a <code>SecurityPermission(
     "insertProvider."+provider.getName())</code> permission.]

     @param provider the provider to be added.
     @param position the preference position that the caller would like for
     this provider.
     @return the actual preference position (1-based) in which the provider was
     added, or -1 if the provider was not added because it is already installed.
     @throws SecurityException if a security manager exists and its <code>
     SecurityManager.checkSecurityAccess(java.lang.String)</code> method denies
     access to add a new provider.
   */
  public static int insertProviderAt(Provider provider, int position)
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkSecurityAccess("insertProvider." + provider.getName());

    position--;
    int max = providers.size ();
    for (int i = 0; i < max; i++)
      {
	if (((Provider) providers.elementAt(i)).getName() ==
	    provider.getName())
	  return -1;
      }

    if (position < 0)
      position = 0;
    if (position > max)
      position = max;

    providers.insertElementAt(provider, position);

    return position + 1;
  }


  /**
     Adds a provider to the next position available.
     <p>
     <b>NOT IMPLEMENTED YET:</b> [
     First, if there is a security manager, its <code>checkSecurityAccess</code>
     method is called with the string
     <code>"insertProvider."+provider.getName()</code>
     to see if it's ok to add a new provider. If the default implementation of
     <code>checkSecurityAccess</code> is used (i.e., that method is not
     overriden), then this will result in a call to the security manager's
     <code>checkPermission</code> method with a <code>SecurityPermission(
     "insertProvider."+provider.getName())</code> permission.]

     @param provider the provider to be added.
     @return the preference position in which the provider was added, or <code>
     -1</code> if the provider was not added because it is already installed.
     @throws SecurityException if a security manager exists and its <code>
     SecurityManager.checkSecurityAccess(java.lang.String)</code> method denies
     access to add a new provider.
   */
  public static int addProvider(Provider provider)
  {
    return insertProviderAt (provider, providers.size () + 1);
  }

  /**
     Removes a provider. This allows dynamic unloading
     of providers. It will automatically shift up providers to a higher
     ranking. If the provider is not installed, it fails silently.

     This method checks the security manager with the call checkSecurityAccess
     with "removeProvider."+provider.getName() to see if the user can remove
     this provider.

     @param name name of the provider to add

     @throws SecurityException - if the security manager denies access to
     remove a new provider
   */
  public static void removeProvider(String name)
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkSecurityAccess("removeProvider." + name);

    Provider p = null;
    int max = providers.size ();
    for (int i = 0; i < max; i++)
      {
	if (((Provider) providers.elementAt(i)).getName() == name)
	  {
	    providers.remove(i);
	    break;
	  }
      }
  }

  /**
     Returns array containing all the providers. It is in the preference order 
     of the providers.

     @return an array of installed providers
   */
  public static Provider[] getProviders()
  {
    Provider array[] = new Provider[providers.size ()];
    providers.copyInto (array);
    return array;
  }

  /**
     Returns the provider with the specified name. It will return null 
     if the provider cannot be found. 

     @param name name of the requested provider

     @return requested provider
   */
  public static Provider getProvider(String name)
  {
    Provider p;
    int max = providers.size ();
    for (int i = 0; i < max; i++)
      {
	p = (Provider) providers.elementAt(i);
	if (p.getName() == name)
	  return p;
      }
    return null;
  }

  /**
     Gets the value of a security property.

     This method checks the security manager with the call checkSecurityAccess
     with "getProperty."+key to see if the user can get this property.

     @param key property to get

     @return value of the property      

     @throws SecurityException - if the security manager denies access to 
     getting a property
   */
  public static String getProperty(String key)
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkSecurityAccess("getProperty." + key);

    return secprops.getProperty(key);
  }


  /**
     Sets the value of a security property.

     This method checks the security manager with the call checkSecurityAccess
     with "setProperty."+key to see if the user can get this property.

     @param key property to set
     @param datnum new value of property

     @throws SecurityException - if the security manager denies access to 
     setting a property
   */
  public static void setProperty(String key, String datnum)
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkSecurityAccess("setProperty." + key);

    secprops.put(key, datnum);
  }
}
