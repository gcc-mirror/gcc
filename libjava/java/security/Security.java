/* Security.java --- Java base security class implementation
   Copyright (C) 1999, 2001, 2002, 2003, 2004  Free Software Foundation, Inc.

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

import gnu.java.security.action.GetPropertyAction;

import gnu.classpath.Configuration;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.Vector;

/**
 * This class centralizes all security properties and common security methods.
 * One of its primary uses is to manage providers.
 *
 * @author Mark Benvenuto (ivymccough@worldnet.att.net)
 */
public final class Security
{
  private static final String ALG_ALIAS = "Alg.Alias.";

  private static Vector providers = new Vector();
  private static Properties secprops = new Properties();
  
  static
    {
      GetPropertyAction getProp = new GetPropertyAction("gnu.classpath.home.url");
      String base = (String) AccessController.doPrivileged(getProp);
      getProp = new GetPropertyAction("gnu.classpath.vm.shortname");
      String vendor = (String) AccessController.doPrivileged(getProp);

      // Try VM specific security file
      boolean loaded = loadProviders (base, vendor);
    
      // Append classpath standard provider if possible
      if (!loadProviders (base, "classpath")
	  && !loaded
	  && providers.size() == 0)
	  {
	      if (Configuration.DEBUG)
		  {
		      /* No providers found and both security files failed to
		       * load properly. Give a warning in case of DEBUG is
		       * enabled. Could be done with java.util.logging later.
		       */
		      System.err.println
			  ("WARNING: could not properly read security provider files:");
		      System.err.println
			  ("         " + base + "/security/" + vendor
			   + ".security");
		      System.err.println
			  ("         " + base + "/security/" + "classpath"
			   + ".security");
		      System.err.println
			  ("         Falling back to standard GNU security provider");
		  }
	      providers.addElement (new gnu.java.security.provider.Gnu());
	  }
    }
  // This class can't be instantiated.
  private Security()
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
	while ((name = secprops.getProperty("security.provider." + i)) != null)
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
   * Gets a specified property for an algorithm. The algorithm name should be a
   * standard name. See Appendix A in the Java Cryptography Architecture API
   * Specification &amp; Reference for information about standard algorithm
   * names. One possible use is by specialized algorithm parsers, which may map
   * classes to algorithms which they understand (much like {@link Key} parsers
   * do).
   *
   * @param algName the algorithm name.
   * @param propName the name of the property to get.
   * @return the value of the specified property.
   * @deprecated This method used to return the value of a proprietary property
   * in the master file of the "SUN" Cryptographic Service Provider in order to
   * determine how to parse algorithm-specific parameters. Use the new
   * provider-based and algorithm-independent {@link AlgorithmParameters} and
   * {@link KeyFactory} engine classes (introduced in the Java 2 platform)
   * instead.
   */
  public static String getAlgorithmProperty(String algName, String propName)
  {
    if (algName == null || propName == null)
      return null;

    String property = String.valueOf(propName) + "." + String.valueOf(algName);
    Provider p;
    for (Iterator i = providers.iterator(); i.hasNext(); )
      {
        p = (Provider) i.next();
        for (Iterator j = p.keySet().iterator(); j.hasNext(); )
          {
            String key = (String) j.next();
            if (key.equalsIgnoreCase(property))
              return p.getProperty(key);
          }
      }
    return null;
  }

  /**
   * <p>Adds a new provider, at a specified position. The position is the
   * preference order in which providers are searched for requested algorithms.
   * Note that it is not guaranteed that this preference will be respected. The
   * position is 1-based, that is, <code>1</code> is most preferred, followed by
   * <code>2</code>, and so on.</p>
   *
   * <p>If the given provider is installed at the requested position, the
   * provider that used to be at that position, and all providers with a
   * position greater than position, are shifted up one position (towards the
   * end of the list of installed providers).</p>
   *
   * <p>A provider cannot be added if it is already installed.</p>
   *
   * <p>First, if there is a security manager, its <code>checkSecurityAccess()
   * </code> method is called with the string <code>"insertProvider."+provider.
   * getName()</code> to see if it's ok to add a new provider. If the default
   * implementation of <code>checkSecurityAccess()</code> is used (i.e., that
   * method is not overriden), then this will result in a call to the security
   * manager's <code>checkPermission()</code> method with a
   * <code>SecurityPermission("insertProvider."+provider.getName())</code>
   * permission.</p>
   *
   * @param provider the provider to be added.
   * @param position the preference position that the caller would like for
   * this provider.
   * @return the actual preference position in which the provider was added, or
   * <code>-1</code> if the provider was not added because it is already
   * installed.
   * @throws SecurityException if a security manager exists and its
   * {@link SecurityManager#checkSecurityAccess(String)} method denies access
   * to add a new provider.
   * @see #getProvider(String)
   * @see #removeProvider(String)
   * @see SecurityPermission
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
	if (((Provider) providers.elementAt(i)).getName().equals(provider.getName()))
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
   * <p>Adds a provider to the next position available.</p>
   *
   * <p>First, if there is a security manager, its <code>checkSecurityAccess()
   * </code> method is called with the string <code>"insertProvider."+provider.
   * getName()</code> to see if it's ok to add a new provider. If the default
   * implementation of <code>checkSecurityAccess()</code> is used (i.e., that
   * method is not overriden), then this will result in a call to the security
   * manager's <code>checkPermission()</code> method with a
   * <code>SecurityPermission("insertProvider."+provider.getName())</code>
   * permission.</p>
   *
   * @param provider the provider to be added.
   * @return the preference position in which the provider was added, or
   * <code>-1</code> if the provider was not added because it is already
   * installed.
   * @throws SecurityException if a security manager exists and its
   * {@link SecurityManager#checkSecurityAccess(String)} method denies access
   * to add a new provider.
   * @see #getProvider(String)
   * @see #removeProvider(String)
   * @see SecurityPermission
   */
  public static int addProvider(Provider provider)
  {
    return insertProviderAt (provider, providers.size () + 1);
  }

  /**
   * <p>Removes the provider with the specified name.</p>
   *
   * <p>When the specified provider is removed, all providers located at a
   * position greater than where the specified provider was are shifted down
   * one position (towards the head of the list of installed providers).</p>
   *
   * <p>This method returns silently if the provider is not installed.</p>
   *
   * <p>First, if there is a security manager, its <code>checkSecurityAccess()
   * </code> method is called with the string <code>"removeProvider."+name</code>
   * to see if it's ok to remove the provider. If the default implementation of
   * <code>checkSecurityAccess()</code> is used (i.e., that method is not
   * overriden), then this will result in a call to the security manager's
   * <code>checkPermission()</code> method with a <code>SecurityPermission(
   * "removeProvider."+name)</code> permission.</p>
   *
   * @param name the name of the provider to remove.
   * @throws SecurityException if a security manager exists and its
   * {@link SecurityManager#checkSecurityAccess(String)} method denies access
   * to remove the provider.
   * @see #getProvider(String)
   * @see #addProvider(Provider)
   */
  public static void removeProvider(String name)
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkSecurityAccess("removeProvider." + name);

    int max = providers.size ();
    for (int i = 0; i < max; i++)
      {
	if (((Provider) providers.elementAt(i)).getName().equals(name))
	  {
	    providers.remove(i);
	    break;
	  }
      }
  }

  /**
   * Returns an array containing all the installed providers. The order of the
   * providers in the array is their preference order.
   *
   * @return an array of all the installed providers.
   */
  public static Provider[] getProviders()
  {
    Provider[] array = new Provider[providers.size ()];
    providers.copyInto (array);
    return array;
  }

  /**
   * Returns the provider installed with the specified name, if any. Returns
   * <code>null</code> if no provider with the specified name is installed.
   *
   * @param name the name of the provider to get.
   * @return the provider of the specified name.
   * @see #removeProvider(String)
   * @see #addProvider(Provider)
   */
  public static Provider getProvider(String name)
  {
    Provider p;
    int max = providers.size ();
    for (int i = 0; i < max; i++)
      {
	p = (Provider) providers.elementAt(i);
	if (p.getName().equals(name))
	  return p;
      }
    return null;
  }

  /**
   * <p>Gets a security property value.</p>
   *
   * <p>First, if there is a security manager, its <code>checkPermission()</code>
   * method is called with a <code>SecurityPermission("getProperty."+key)</code>
   * permission to see if it's ok to retrieve the specified security property
   * value.</p>
   *
   * @param key the key of the property being retrieved.
   * @return the value of the security property corresponding to key.
   * @throws SecurityException if a security manager exists and its
   * {@link SecurityManager#checkPermission(Permission)} method denies access
   * to retrieve the specified security property value.
   * @see #setProperty(String, String)
   * @see SecurityPermission
   */
  public static String getProperty(String key)
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkSecurityAccess("getProperty." + key);

    return secprops.getProperty(key);
  }

  /**
   * <p>Sets a security property value.</p>
   *
   * <p>First, if there is a security manager, its <code>checkPermission()</code>
   * method is called with a <code>SecurityPermission("setProperty."+key)</code>
   * permission to see if it's ok to set the specified security property value.
   * </p>
   *
   * @param key the name of the property to be set.
   * @param datnum the value of the property to be set.
   * @throws SecurityException if a security manager exists and its
   * {@link SecurityManager#checkPermission(Permission)} method denies access
   * to set the specified security property value.
   * @see #getProperty(String)
   * @see SecurityPermission
   */
  public static void setProperty(String key, String datnum)
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkSecurityAccess("setProperty." + key);

    secprops.put(key, datnum);
  }

  /**
   * Returns a Set of Strings containing the names of all available algorithms
   * or types for the specified Java cryptographic service (e.g., Signature,
   * MessageDigest, Cipher, Mac, KeyStore). Returns an empty Set if there is no
   * provider that supports the specified service. For a complete list of Java
   * cryptographic services, please see the Java Cryptography Architecture API
   * Specification &amp; Reference. Note: the returned set is immutable.
   *
   * @param serviceName the name of the Java cryptographic service (e.g.,
   * Signature, MessageDigest, Cipher, Mac, KeyStore). Note: this parameter is
   * case-insensitive.
   * @return a Set of Strings containing the names of all available algorithms
   * or types for the specified Java cryptographic service or an empty set if
   * no provider supports the specified service.
   * @since 1.4
   */
  public static Set getAlgorithms(String serviceName)
  {
    HashSet result = new HashSet();
    if (serviceName == null || serviceName.length() == 0)
      return result;

    serviceName = serviceName.trim();
    if (serviceName.length() == 0)
      return result;

    serviceName = serviceName.toUpperCase()+".";
    Provider[] providers = getProviders();
    int ndx;
    for (int i = 0; i < providers.length; i++)
      for (Enumeration e = providers[i].propertyNames(); e.hasMoreElements(); )
        {
          String service = ((String) e.nextElement()).trim();
          if (service.toUpperCase().startsWith(serviceName))
            {
              service = service.substring(serviceName.length()).trim();
              ndx = service.indexOf(' '); // get rid of attributes
              if (ndx != -1)
                service = service.substring(0, ndx);
              result.add(service);
            }
        }
    return Collections.unmodifiableSet(result);
  }

  /**
   * <p>Returns an array containing all installed providers that satisfy the
   * specified selection criterion, or <code>null</code> if no such providers
   * have been installed. The returned providers are ordered according to their
   * preference order.</p>
   *
   * <p>A cryptographic service is always associated with a particular
   * algorithm or type. For example, a digital signature service is always
   * associated with a particular algorithm (e.g., <i>DSA</i>), and a
   * CertificateFactory service is always associated with a particular
   * certificate type (e.g., <i>X.509</i>).</p>
   *
   * <p>The selection criterion must be specified in one of the following two
   * formats:</p>
   *
   * <ul>
   *    <li><p>&lt;crypto_service&gt;.&lt;algorithm_or_type&gt;</p>
   *    <p>The cryptographic service name must not contain any dots.</p>
   *    <p>A provider satisfies the specified selection criterion iff the
   *    provider implements the specified algorithm or type for the specified
   *    cryptographic service.</p>
   *    <p>For example, "CertificateFactory.X.509" would be satisfied by any
   *    provider that supplied a CertificateFactory implementation for X.509
   *    certificates.</p></li>
   *
   *    <li><p>&lt;crypto_service&gt;.&lt;algorithm_or_type&gt; &lt;attribute_name&gt;:&lt;attribute_value&gt;</p>
   *    <p>The cryptographic service name must not contain any dots. There must
   *    be one or more space charaters between the the &lt;algorithm_or_type&gt;
   *    and the &lt;attribute_name&gt;.</p>
   *    <p>A provider satisfies this selection criterion iff the provider
   *    implements the specified algorithm or type for the specified
   *    cryptographic service and its implementation meets the constraint
   *    expressed by the specified attribute name/value pair.</p>
   *    <p>For example, "Signature.SHA1withDSA KeySize:1024" would be satisfied
   *    by any provider that implemented the SHA1withDSA signature algorithm
   *    with a keysize of 1024 (or larger).</p></li>
   * </ul>
   *
   * <p>See Appendix A in the Java Cryptogaphy Architecture API Specification
   * &amp; Reference for information about standard cryptographic service names,
   * standard algorithm names and standard attribute names.</p>
   *
   * @param filter the criterion for selecting providers. The filter is case-
   * insensitive.
   * @return all the installed providers that satisfy the selection criterion,
   * or null if no such providers have been installed.
   * @throws InvalidParameterException if the filter is not in the required
   * format.
   * @see #getProviders(Map)
   */
  public static Provider[] getProviders(String filter)
  {
    if (providers == null || providers.isEmpty())
      return null;

    if (filter == null || filter.length() == 0)
      return getProviders();

    HashMap map = new HashMap(1);
    int i = filter.indexOf(':');
    if (i == -1) // <service>.<algorithm>
      map.put(filter, "");
    else // <service>.<algorithm> <attribute>:<value>
      map.put(filter.substring(0, i), filter.substring(i+1));

    return getProviders(map);
  }

 /**
  * <p>Returns an array containing all installed providers that satisfy the
  * specified selection criteria, or <code>null</code> if no such providers
  * have been installed. The returned providers are ordered according to their
  * preference order.</p>
  *
  * <p>The selection criteria are represented by a map. Each map entry
  * represents a selection criterion. A provider is selected iff it satisfies
  * all selection criteria. The key for any entry in such a map must be in one
  * of the following two formats:</p>
  *
  * <ul>
  *    <li><p>&lt;crypto_service&gt;.&lt;algorithm_or_type&gt;</p>
  *    <p>The cryptographic service name must not contain any dots.</p>
  *    <p>The value associated with the key must be an empty string.</p>
  *    <p>A provider satisfies this selection criterion iff the provider
  *    implements the specified algorithm or type for the specified
  *    cryptographic service.</p></li>
  *
  *    <li><p>&lt;crypto_service&gt;.&lt;algorithm_or_type&gt; &lt;attribute_name&gt;</p>
  *    <p>The cryptographic service name must not contain any dots. There must
  *    be one or more space charaters between the &lt;algorithm_or_type&gt; and
  *    the &lt;attribute_name&gt;.</p>
  *    <p>The value associated with the key must be a non-empty string. A
  *    provider satisfies this selection criterion iff the provider implements
  *    the specified algorithm or type for the specified cryptographic service
  *    and its implementation meets the constraint expressed by the specified
  *    attribute name/value pair.</p></li>
  * </ul>
  *
  * <p>See Appendix A in the Java Cryptogaphy Architecture API Specification
  * &amp; Reference for information about standard cryptographic service names,
  * standard algorithm names and standard attribute names.</p>
  *
  * @param filter the criteria for selecting providers. The filter is case-
  * insensitive.
  * @return all the installed providers that satisfy the selection criteria,
  * or <code>null</code> if no such providers have been installed.
  * @throws InvalidParameterException if the filter is not in the required
  * format.
  * @see #getProviders(String)
  */
  public static Provider[] getProviders(Map filter)
  {
    if (providers == null || providers.isEmpty())
      return null;

    if (filter == null)
      return getProviders();

    Set querries = filter.keySet();
    if (querries == null || querries.isEmpty())
      return getProviders();

    LinkedHashSet result = new LinkedHashSet(providers); // assume all
    int dot, ws;
    String querry, service, algorithm, attribute, value;
    LinkedHashSet serviceProviders = new LinkedHashSet(); // preserve insertion order
    for (Iterator i = querries.iterator(); i.hasNext(); )
      {
        querry = (String) i.next();
        if (querry == null) // all providers
          continue;

        querry = querry.trim();
        if (querry.length() == 0) // all providers
          continue;

        dot = querry.indexOf('.');
        if (dot == -1) // syntax error
          throw new InvalidParameterException(
              "missing dot in '" + String.valueOf(querry)+"'");

        value = (String) filter.get(querry);
        // deconstruct querry into [service, algorithm, attribute]
        if (value == null || value.trim().length() == 0) // <service>.<algorithm>
          {
            value = null;
            attribute = null;
            service = querry.substring(0, dot).trim();
            algorithm = querry.substring(dot+1).trim();
          }
        else // <service>.<algorithm> <attribute>
          {
            ws = querry.indexOf(' ');
            if (ws == -1)
              throw new InvalidParameterException(
                  "value (" + String.valueOf(value) +
                  ") is not empty, but querry (" + String.valueOf(querry) +
                  ") is missing at least one space character");
            value = value.trim();
            attribute = querry.substring(ws+1).trim();
            // was the dot in the attribute?
            if (attribute.indexOf('.') != -1)
              throw new InvalidParameterException(
                  "attribute_name (" + String.valueOf(attribute) +
                  ") in querry (" + String.valueOf(querry) + ") contains a dot");

            querry = querry.substring(0, ws).trim();
            service = querry.substring(0, dot).trim();
            algorithm = querry.substring(dot+1).trim();
          }

        // service and algorithm must not be empty
        if (service.length() == 0)
          throw new InvalidParameterException(
              "<crypto_service> in querry (" + String.valueOf(querry) +
              ") is empty");

        if (algorithm.length() == 0)
          throw new InvalidParameterException(
              "<algorithm_or_type> in querry (" + String.valueOf(querry) +
              ") is empty");

        selectProviders(service, algorithm, attribute, value, result, serviceProviders);
        result.retainAll(serviceProviders); // eval next retaining found providers
        if (result.isEmpty()) // no point continuing
          break;
      }

    if (result.isEmpty())
      return null;

    return (Provider[]) result.toArray(new Provider[0]);
  }

  private static void selectProviders(String svc, String algo, String attr,
                                      String val, LinkedHashSet providerSet,
                                      LinkedHashSet result)
  {
    result.clear(); // ensure we start with an empty result set
    for (Iterator i = providerSet.iterator(); i.hasNext(); )
      {
        Provider p = (Provider) i.next();
        if (provides(p, svc, algo, attr, val))
          result.add(p);
      }
  }

  private static boolean provides(Provider p, String svc, String algo,
                                  String attr, String val)
  {
    Iterator it;
    String serviceDotAlgorithm = null;
    String key = null;
    String realVal;
    boolean found = false;
    // if <svc>.<algo> <attr> is in the set then so is <svc>.<algo>
    // but it may be stored under an alias <algo>. resolve
    outer: for (int r = 0; r < 3; r++) // guard against circularity
      {
        serviceDotAlgorithm = (svc+"."+String.valueOf(algo)).trim();
        for (it = p.keySet().iterator(); it.hasNext(); )
          {
            key = (String) it.next();
            if (key.equalsIgnoreCase(serviceDotAlgorithm)) // eureka
              {
                found = true;
                break outer;
              }
            // it may be there but as an alias
            if (key.equalsIgnoreCase(ALG_ALIAS + serviceDotAlgorithm))
              {
                algo = p.getProperty(key);
                continue outer;
              }
            // else continue inner
          }
      }

    if (!found)
      return false;

    // found a candidate for the querry.  do we have an attr to match?
    if (val == null) // <service>.<algorithm> querry
      return true;

    // <service>.<algorithm> <attribute>; find the key entry that match
    String realAttr;
    int limit = serviceDotAlgorithm.length() + 1;
    for (it = p.keySet().iterator(); it.hasNext(); )
      {
        key = (String) it.next();
        if (key.length() <= limit)
          continue;

        if (key.substring(0, limit).equalsIgnoreCase(serviceDotAlgorithm+" "))
          {
            realAttr = key.substring(limit).trim();
            if (! realAttr.equalsIgnoreCase(attr))
              continue;

            // eveything matches so far.  do the value
            realVal = p.getProperty(key);
            if (realVal == null)
              return false;

            realVal = realVal.trim();
            // is it a string value?
            if (val.equalsIgnoreCase(realVal))
              return true;

            // assume value is a number. cehck for greater-than-or-equal
            return (new Integer(val).intValue() >= new Integer(realVal).intValue());
          }
      }

    return false;
  }
}
