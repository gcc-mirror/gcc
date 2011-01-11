/* Security.java --- Java base security class implementation
   Copyright (C) 1999, 2001, 2002, 2003, 2004, 2005, 2006
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


package java.security;

import gnu.classpath.SystemProperties;

import gnu.classpath.Configuration;
import gnu.classpath.VMStackWalker;

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
 * One of its primary uses is to manage security providers.
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
      String base = SystemProperties.getProperty("gnu.classpath.home.url");
      String vendor = SystemProperties.getProperty("gnu.classpath.vm.shortname");

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
              // Note that this matches our classpath.security file.
              providers.addElement (new gnu.java.security.provider.Gnu());
              providers.addElement(new gnu.javax.crypto.jce.GnuCrypto());
              providers.addElement(new gnu.javax.crypto.jce.GnuSasl());
              providers.addElement(new gnu.javax.net.ssl.provider.Jessie());
              providers.addElement(new gnu.javax.security.auth.callback.GnuCallbacks());
          }
    }
  // This class can't be instantiated.
  private Security()
  {
  }

  /**
   * Tries to load the vender specific security providers from the given base
   * URL. Returns true if the resource could be read and completely parsed
   * successfully, false otherwise.
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
                ClassLoader sys = ClassLoader.getSystemClassLoader();
                providers.addElement(Class.forName(name, true, sys).newInstance());
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
   * Returns the value associated to a designated property name for a given
   * algorithm.
   *
   * @param algName
   *          the algorithm name.
   * @param propName
   *          the name of the property to return.
   * @return the value of the specified property or <code>null</code> if none
   *         found.
   * @deprecated Use the provider-based and algorithm-independent
   *             {@link AlgorithmParameters} and {@link KeyFactory} engine
   *             classes instead.
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
   * Inserts a new designated {@link Provider} at a designated (1-based)
   * position in the current list of installed {@link Provider}s,
   *
   * @param provider
   *          the new {@link Provider} to add.
   * @param position
   *          the position (starting from 1) of where to install
   *          <code>provider</code>.
   * @return the actual position, in the list of installed Providers. Returns
   *         <code>-1</code> if <code>provider</code> was laready in the
   *         list. The actual position may be different than the desired
   *         <code>position</code>.
   * @throws SecurityException
   *           if a {@link SecurityManager} is installed and it disallows this
   *           operation.
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
   * Appends the designated new {@link Provider} to the current list of
   * installed {@link Provider}s.
   *
   * @param provider
   *          the new {@link Provider} to append.
   * @return the position (starting from 1) of <code>provider</code> in the
   *         current list of {@link Provider}s, or <code>-1</code> if
   *         <code>provider</code> was already there.
   * @throws SecurityException
   *           if a {@link SecurityManager} is installed and it disallows this
   *           operation.
   * @see #getProvider(String)
   * @see #removeProvider(String)
   * @see SecurityPermission
   */
  public static int addProvider(Provider provider)
  {
    return insertProviderAt (provider, providers.size () + 1);
  }

  /**
   * Removes an already installed {@link Provider}, given its name, from the
   * current list of installed {@link Provider}s.
   *
   * @param name
   *          the name of an already installed {@link Provider} to remove.
   * @throws SecurityException
   *           if a {@link SecurityManager} is installed and it disallows this
   *           operation.
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
   * Returns the current list of installed {@link Provider}s as an array
   * ordered according to their installation preference order.
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
   * Returns an already installed {@link Provider} given its name.
   *
   * @param name
   *          the name of an already installed {@link Provider}.
   * @return the {@link Provider} known by <code>name</code>. Returns
   *         <code>null</code> if the current list of {@link Provider}s does
   *         not include one named <code>name</code>.
   * @see #removeProvider(String)
   * @see #addProvider(Provider)
   */
  public static Provider getProvider(String name)
  {
    if (name == null)
      return null;
    else
      {
        name = name.trim();
        if (name.length() == 0)
          return null;
      }
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
   * Returns the value associated with a Security propery.
   *
   * @param key
   *          the key of the property to fetch.
   * @return the value of the Security property associated with
   *         <code>key</code>. Returns <code>null</code> if no such property
   *         was found.
   * @throws SecurityException
   *           if a {@link SecurityManager} is installed and it disallows this
   *           operation.
   * @see #setProperty(String, String)
   * @see SecurityPermission
   */
  public static String getProperty(String key)
  {
    // XXX To prevent infinite recursion when the SecurityManager calls us,
    // don't do a security check if the caller is trusted (by virtue of having
    // been loaded by the bootstrap class loader).
    SecurityManager sm = System.getSecurityManager();
    if (sm != null && VMStackWalker.getCallingClassLoader() != null)
      sm.checkSecurityAccess("getProperty." + key);

    return secprops.getProperty(key);
  }

  /**
   * Sets or changes a designated Security property to a designated value.
   *
   * @param key
   *          the name of the property to set.
   * @param datum
   *          the new value of the property.
   * @throws SecurityException
   *           if a {@link SecurityManager} is installed and it disallows this
   *           operation.
   * @see #getProperty(String)
   * @see SecurityPermission
   */
  public static void setProperty(String key, String datum)
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkSecurityAccess("setProperty." + key);

    if (datum == null)
      secprops.remove(key);
    else
      secprops.put(key, datum);
  }

  /**
   * For a given <i>service</i> (e.g. Signature, MessageDigest, etc...) this
   * method returns the {@link Set} of all available algorithm names (instances
   * of {@link String}, from all currently installed {@link Provider}s.
   *
   * @param serviceName
   *          the case-insensitive name of a service (e.g. Signature,
   *          MessageDigest, etc).
   * @return a {@link Set} of {@link String}s containing the names of all
   *         algorithm names provided by all of the currently installed
   *         {@link Provider}s.
   * @since 1.4
   */
  public static Set<String> getAlgorithms(String serviceName)
  {
    HashSet<String> result = new HashSet<String>();
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
   * Returns an array of currently installed {@link Provider}s, ordered
   * according to their installation preference order, which satisfy a given
   * <i>selection</i> criterion.
   *
   * <p>This implementation recognizes a <i>selection</i> criterion written in
   * one of two following forms:</p>
   *
   * <ul>
   *   <li>&lt;crypto_service&gt;.&lt;algorithm_or_type&gt;: Where
   *   <i>crypto_service</i> is a case-insensitive string, similar to what has
   *   been described in the {@link #getAlgorithms(String)} method, and
   *   <i>algorithm_or_type</i> is a known case-insensitive name of an
   *   Algorithm, or one of its aliases.
   *
   *   <p>For example, "CertificateFactory.X.509" would return all the installed
   *   {@link Provider}s which provide a <i>CertificateFactory</i>
   *   implementation of <i>X.509</i>.</p></li>
   *
   *   <li>&lt;crypto_service&gt;.&lt;algorithm_or_type&gt; &lt;attribute_name&gt;:&lt;value&gt;:
   *   Where <i>crypto_service</i> is a case-insensitive string, similar to what
   *   has been described in the {@link #getAlgorithms(String)} method,
   *   <i>algorithm_or_type</i> is a case-insensitive known name of an Algorithm
   *   or one of its aliases, <i>attribute_name</i> is a case-insensitive
   *   property name with no whitespace characters, and no dots, in-between, and
   *   <i>value</i> is a {@link String} with no whitespace characters in-between.
   *
   *   <p>For example, "Signature.Sha1WithDSS KeySize:1024" would return all the
   *   installed {@link Provider}s which declared their ability to provide
   *   <i>Signature</i> services, using the <i>Sha1WithDSS</i> algorithm with
   *   key sizes of <i>1024</i>.</p></li>
   * </ul>
   *
   * @param filter
   *          the <i>selection</i> criterion for selecting among the installed
   *          {@link Provider}s.
   * @return all the installed {@link Provider}s which satisfy the <i>selection</i>
   *         criterion. Returns <code>null</code> if no installed
   *         {@link Provider}s were found which satisfy the <i>selection</i>
   *         criterion. Returns ALL installed {@link Provider}s if
   *         <code>filter</code> is <code>null</code> or is an empty string.
   * @throws InvalidParameterException
   *           if an exception occurs while parsing the <code>filter</code>.
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
   * Returns an array of currently installed {@link Provider}s which satisfy a
   * set of <i>selection</i> criteria.
   *
   * <p>The <i>selection</i> criteria are defined in a {@link Map} where each
   * element specifies a <i>selection</i> querry. The <i>Keys</i> in this
   * {@link Map} must be in one of the two following forms:</p>
   *
   * <ul>
   *   <li>&lt;crypto_service&gt;.&lt;algorithm_or_type&gt;: Where
   *   <i>crypto_service</i> is a case-insensitive string, similar to what has
   *   been described in the {@link #getAlgorithms(String)} method, and
   *   <i>algorithm_or_type</i> is a case-insensitive known name of an
   *   Algorithm, or one of its aliases. The <i>value</i> of the entry in the
   *   {@link Map} for such a <i>Key</i> MUST be the empty string.
   *   {@link Provider}s which provide an implementation for the designated
   *   <i>service algorithm</i> are included in the result.</li>
   *
   *   <li>&lt;crypto_service&gt;.&lt;algorithm_or_type&gt; &lt;attribute_name&gt;:
   *   Where <i>crypto_service</i> is a case-insensitive string, similar to what
   *   has been described in the {@link #getAlgorithms(String)} method,
   *   <i>algorithm_or_type</i> is a case-insensitive known name of an Algorithm
   *   or one of its aliases, and <i>attribute_name</i> is a case-insensitive
   *   property name with no whitespace characters, and no dots, in-between. The
   *   <i>value</i> of the entry in this {@link Map} for such a <i>Key</i> MUST
   *   NOT be <code>null</code> or an empty string. {@link Provider}s which
   *   declare the designated <i>attribute_name</i> and <i>value</i> for the
   *   designated <i>service algorithm</i> are included in the result.</li>
   * </ul>
   *
   * @param filter
   *          a {@link Map} of <i>selection querries</i>.
   * @return all currently installed {@link Provider}s which satisfy ALL the
   *         <i>selection</i> criteria defined in <code>filter</code>.
   *         Returns ALL installed {@link Provider}s if <code>filter</code>
   *         is <code>null</code> or empty.
   * @throws InvalidParameterException
   *           if an exception is encountered while parsing the syntax of the
   *           {@link Map}'s <i>keys</i>.
   * @see #getProviders(String)
   */
  public static Provider[] getProviders(Map<String,String> filter)
  {
    if (providers == null || providers.isEmpty())
      return null;

    if (filter == null)
      return getProviders();

    Set<String> querries = filter.keySet();
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

        value = filter.get(querry);
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

    return (Provider[]) result.toArray(new Provider[result.size()]);
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
            return (Integer.parseInt(val) >= Integer.parseInt(realVal));
          }
      }

    return false;
  }
}
