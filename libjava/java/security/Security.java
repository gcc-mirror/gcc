/* Security.java --- Java base security class implmentation
   Copyright (C) 1999, 2001 Free Software Foundation, Inc.

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

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */

package java.security;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.FileNotFoundException;
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
  private static int providerCount = 0;
  private static Properties secprops;

  static
  {
    loadProviders();
  }

  private static void loadProviders()
  {
    String separator = System.getProperty("file.separator");
    String secfilestr = System.getProperty("java.home") +
      separator + "lib" + separator + "security" + separator +
      "classpath.security";

    providerCount = 0;
    try
      {
	File secFile = new File(secfilestr);
	FileInputStream fin = new FileInputStream(secFile);
	secprops = new Properties();
	secprops.load(fin);

	int i = 1;
	String name;
	StringBuffer pname = new StringBuffer("security.provider.");

	while ((name = secprops.getProperty(pname.append(i).toString())) !=
	       null)
	  {
	    Exception exception = null;
	    try
	      {
		providers.addElement(Class.forName(name).newInstance());
		providerCount++;
		i++;
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
	      System.err.println ("Error loading security provider " + name
	                          + ": " + exception);
	  }
      }
    catch (FileNotFoundException ignored)
      {
        // Actually we probibly shouldn't ignore these, once the security
	// properties file is actually installed somewhere.
      }
    catch (IOException ignored)
      {
      }
  }

  /**
     Gets a specific property for an algorithm. This is used to produce specialized
     algorithm parsers.

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
     Adds a new provider at the specified position. This allows dynamic loading
     of providers. It will check for duplication of providers.

     This class checks the security manager with the call checkSecurityAccess
     with "insertProvider."+provider.getName() to see if the user can add this
     provider.

     @param provider the provider to add
     @param position position to add the provider at

     @return the position the provider was added at, or -1 if a duplicate provider
     was found

     @throws SecurityException - if the security manager denies access to add a 
     new provider
   */
  public static int insertProviderAt(Provider provider, int position)
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkSecurityAccess("insertProvider." + provider.getName());

    for (int i = 0; i < providerCount; i++)
      {
	if (((Provider) providers.elementAt(i)).getName() ==
	    provider.getName())
	  return -1;
      }

    if (position < 0)
        position = 0;
    if (position > providerCount)
      position = providerCount;

    providers.insertElementAt(provider, position);
    providerCount++;

    return position;
  }


  /**
     Adds a new provider. This allows dynamic loading
     of providers. It will check for duplication of providers.

     This method checks the security manager with the call checkSecurityAccess
     with "insertProvider."+provider.getName() to see if the user can add this
     provider.

     @param provider the provider to add

     @return the position the provider was added at, or -1 if a duplicate provider
     was found

     @throws SecurityException - if the security manager denies access to add a 
     new provider
   */
  public static int addProvider(Provider provider)
  {
    SecurityManager sm = System.getSecurityManager();

    if (sm != null)
      sm.checkSecurityAccess("insertProvider." + provider.getName());

    for (int i = 0; i < providerCount; i++)
      {
	if (((Provider) providers.elementAt(i)).getName() ==
	    provider.getName())
	  return -1;
      }

    providers.addElement(provider);
    providerCount++;

    return providerCount - 1;
  }

  /**
     Removes a provider. This allows dynamic unloading
     of providers. It will automatically shift up providers to a higher
     ranking. If the provider is not installed, it fails silently.

     This method checks the security manager with the call checkSecurityAccess
     with "removeProvider."+provider.getName() to see if the user can remove this
     provider.

     @param name name of the provider to add

     @throws SecurityException - if the security manager denies access to remove a 
     new provider
   */
  public static void removeProvider(String name)
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkSecurityAccess("removeProvider." + name);

    Provider p = null;
    for (int i = 0; i < providerCount; i++)
      {
	if (((Provider) providers.elementAt(i)).getName() == name)
	  {
	    p = (Provider) providers.elementAt(i);
	    break;
	  }
      }

    if (p != null)
      if (providers.removeElement(p))
	  providerCount--;

  }

  /**
     Returns array containing all the providers. It is in the preference order 
     of the providers.

     @return an array of installed providers
   */
  public static Provider[] getProviders()
  {
    Provider array[] = new Provider[providerCount];
    for (int i = 0; i < providerCount; i++)
      array[i] = (Provider) providers.elementAt(i);
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
    Provider p = null;
    for (int i = 0; i < providerCount; i++)
      {
	p = (Provider) providers.elementAt(i);
	if (p.getName() == name)
	  break;
      }
    return p;
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
