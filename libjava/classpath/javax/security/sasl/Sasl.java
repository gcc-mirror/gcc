/* Sasl.java --
   Copyright (C) 2003, 2004, 2005  Free Software Foundation, Inc.

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


package javax.security.sasl;

import java.security.Provider;
import java.security.Security;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Vector;

import javax.security.auth.callback.CallbackHandler;

/**
 * <p>A static class for creating SASL clients and servers.</p>
 *
 * <p>This class defines the policy of how to locate, load, and instantiate SASL
 * clients and servers.</p>
 *
 * <p>For example, an application or library gets a SASL client instance by
 * doing something like:</p>
 *
 * <pre>
 *SaslClient sc =
 *      Sasl.createSaslClient(mechanisms, authorizationID, protocol,
 *                            serverName, props, callbackHandler);
 * </pre>
 *
 * <p>It can then proceed to use the instance to create an authenticated
 * connection.</p>
 *
 * <p>Similarly, a server gets a SASL server instance by using code that looks
 * as follows:</p>
 *
 * <pre>
 *SaslServer ss =
 *      Sasl.createSaslServer(mechanism, protocol, serverName, props,
 *                            callbackHandler);
 * </pre>
 *
 * @since 1.5
 */
public class Sasl
{

  // Constants and variables
  // -------------------------------------------------------------------------

  /**
   * <p>The name of a property that specifies the quality-of-protection to use.
   * The property contains a comma-separated, ordered list of quality-of-
   * protection values that the client or server is willing to support. A qop
   * value is one of:</p>
   *
   * <ul>
   *    <li><code>"auth"</code> - authentication only,</li>
   *    <li><code>"auth-int"</code> - authentication plus integrity
   *    protection,</li>
   *    <li><code>"auth-conf"</code> - authentication plus integrity and
   *    confidentiality protection.</li>
   * </ul>
   *
   * <p>The order of the list specifies the preference order of the client or
   * server.</p>
   *
   * <p>If this property is absent, the default qop is <code>"auth"</code>.</p>
   *
   * <p>The value of this constant is <code>"javax.security.sasl.qop"</code>.</p>
   */
  public static final String QOP = "javax.security.sasl.qop";

  /**
   * <p>The name of a property that specifies the cipher strength to use. The
   * property contains a comma-separated, ordered list of cipher strength
   * values that the client or server is willing to support. A strength value
   * is one of:</p>
   *
   * <ul>
   *    <li><code>"low"</code>,</li>
   *    <li><code>"medium"</code>,</li>
   *    <li><code>"high"</code>.</li>
   * </ul>
   *
   * <p>The order of the list specifies the preference order of the client or
   * server. An implementation should allow configuration of the meaning of
   * these values. An application may use the Java Cryptography Extension (JCE)
   * with JCE-aware mechanisms to control the selection of cipher suites that
   * match the strength values.</p>
   *
   * <p>If this property is absent, the default strength is
   * <code>"high,medium,low"</code>.</p>
   *
   * <p>The value of this constant is <code>"javax.security.sasl.strength"</code>.
   * </p>
   */
  public static final String STRENGTH = "javax.security.sasl.strength";

  /**
   * <p>The name of a property that specifies whether the server must authenticate
   * to the client. The property contains <code>"true"</code> if the server
   * must authenticate the to client; <code>"false"</code> otherwise. The
   * default is <code>"false"</code>.</p>
   *
   * <p>The value of this constant is
   * <code>"javax.security.sasl.server.authentication"</code>.</p>
   */
  public static final String SERVER_AUTH = "javax.security.sasl.server.authentication";

  /**
   * <p>The name of a property that specifies the maximum size of the receive
   * buffer in bytes of {@link SaslClient}/{@link SaslServer}. The property
   * contains the string representation of an integer.</p>
   *
   * <p>If this property is absent, the default size is defined by the
   * mechanism.</p>
   *
   * <p>The value of this constant is <code>"javax.security.sasl.maxbuffer"</code>.
   * </p>
   */
  public static final String MAX_BUFFER = "javax.security.sasl.maxbuffer";

  /**
   * <p>The name of a property that specifies the maximum size of the raw send
   * buffer in bytes of {@link SaslClient}/{@link SaslServer}. The property
   * contains the string representation of an integer. The value of this
   * property is negotiated between the client and server during the
   * authentication exchange.</p>
   *
   * <p>The value of this constant is <code>"javax.security.sasl.rawsendsize"</code>.
   * </p>
   */
  public static final String RAW_SEND_SIZE = "javax.security.sasl.rawsendsize";

  /**
   * <p>The name of a property that specifies whether mechanisms susceptible
   * to simple plain passive attacks (e.g., "PLAIN") are not permitted. The
   * property contains <code>"true"</code> if such mechanisms are not
   * permitted; <code>"false"</code> if such mechanisms are permitted. The
   * default is <code>"false"</code>.</p>
   *
   * <p>The value of this constant is <code>"javax.security.sasl.policy.noplaintext"</code>.
   * </p>
   */
  public static final String POLICY_NOPLAINTEXT = "javax.security.sasl.policy.noplaintext";

  /**
   * <p>The name of a property that specifies whether mechanisms susceptible to
   * active (non-dictionary) attacks are not permitted. The property contains
   * <code>"true"</code> if mechanisms susceptible to active attacks are not
   * permitted; <code>"false"</code> if such mechanisms are permitted. The
   * default is <code>"false"</code>.</p>
   *
   * <p>The value of this constant is <code>"javax.security.sasl.policy.noactive"</code>.
   * </p>
   */
  public static final String POLICY_NOACTIVE = "javax.security.sasl.policy.noactive";

  /**
   * <p>The name of a property that specifies whether mechanisms susceptible to
   * passive dictionary attacks are not permitted. The property contains
   * <code>"true"</code> if mechanisms susceptible to dictionary attacks are
   * not permitted; <code>"false"</code> if such mechanisms are permitted. The
   * default is <code>"false"</code>.</p>
   *
   * <p>The value of this constant is <code>"javax.security.sasl.policy.nodictionary"</code>.
   * </p>
   */
  public static final String POLICY_NODICTIONARY = "javax.security.sasl.policy.nodictionary";

  /**
   * <p>The name of a property that specifies whether mechanisms that accept
   * anonymous login are not permitted. The property contains <code>"true"</code>
   * if mechanisms that accept anonymous login are not permitted; <code>"false"
   * </code> if such mechanisms are permitted. The default is <code>"false"</code>.
   * </p>
   *
   * <p>The value of this constant is <code>"javax.security.sasl.policy.noanonymous"</code>.
   * </p>
   */
  public static final String POLICY_NOANONYMOUS = "javax.security.sasl.policy.noanonymous";

  /**
   * The name of a property that specifies whether mechanisms that implement
   * forward secrecy between sessions are required. Forward secrecy means that
   * breaking into one session will not automatically provide information for
   * breaking into future sessions. The property contains <code>"true"</code>
   * if mechanisms that implement forward secrecy between sessions are
   * required; <code>"false"</code> if such mechanisms are not required. The
   * default is <code>"false"</code>.
   *
   * <p>The value of this constant is <code>"javax.security.sasl.policy.forward"</code>.
   * </p>
   */
  public static final String POLICY_FORWARD_SECRECY = "javax.security.sasl.policy.forward";

  /**
   * The name of a property that specifies whether mechanisms that pass client
   * credentials are required. The property contains <code>"true"</code> if
   * mechanisms that pass client credentials are required; <code>"false"</code>
   * if such mechanisms are not required. The default is <code>"false"</code>.
   *
   * <p>The value of this constant is <code>"javax.security.sasl.policy.credentials"</code>.
   * </p>
   */
  public static final String POLICY_PASS_CREDENTIALS = "javax.security.sasl.policy.credentials";

  /**
   * <p>The name of a property that specifies whether to reuse previously
   * authenticated session information. The property contains <code>"true"</code>
   * if the mechanism implementation may attempt to reuse previously
   * authenticated session information; it contains <code>"false"</code> if the
   * implementation must not reuse previously authenticated session information.
   * A setting of <code>"true"</code> serves only as a hint; it does not
   * necessarily entail actual reuse because reuse might not be possible due to
   * a number of reasons, including, but not limited to, lack of mechanism
   * support for reuse, expiration of reusable information, and the peer's
   * refusal to support reuse. The property's default value is <code>"false"</code>.
   * </p>
   *
   * <p>The value of this constant is <code>"javax.security.sasl.reuse"</code>.
   * Note that all other parameters and properties required to create a SASL
   * client/server instance must be provided regardless of whether this
   * property has been supplied. That is, you cannot supply any less
   * information in anticipation of reuse. Mechanism implementations that
   * support reuse might allow customization of its implementation for factors
   * such as cache size, timeouts, and criteria for reuseability. Such
   * customizations are implementation-dependent.</p>
   */
  public static final String REUSE = "javax.security.sasl.reuse";

  /**
   * <p>The name of a property which specifies the credentials to use.
   * The value of the property is a mechanism-specific object which can
   * be used to supply credentials to a mechanism which provides delegated
   * authentication.</p>
   *
   * <p>The value of this constant is <code>"javax.security.sasl.credentials"</code>.</p>
   */
  public static final String CREDENTIALS = "javax.security.sasl.credentials";

  private static final String CLIENT_FACTORY_SVC = "SaslClientFactory.";
  private static final String SERVER_FACTORY_SVC = "SaslServerFactory.";
  private static final String ALIAS = "Alg.Alias.";

  // Constructor(s)
  // -------------------------------------------------------------------------

  private Sasl()
  {
    super();
  }

  // Class methods
  // -------------------------------------------------------------------------

  /**
   * Creates a {@link SaslClient} for the specified mechanism.
   *
   * <p>This method uses the JCA Security Provider Framework, described in the
   * "Java Cryptography Architecture API Specification &amp; Reference", for
   * locating and selecting a {@link SaslClient} implementation.</p>
   *
   * <p>First, it obtains an ordered list of {@link SaslClientFactory}
   * instances from the registered security providers for the
   * <code>"SaslClientFactory"</code> service and the specified mechanism. It
   * then invokes <code>createSaslClient()</code> on each factory instance on
   * the list until one produces a non-null {@link SaslClient} instance. It
   * returns the non-null {@link SaslClient} instance, or <code>null</code> if
   * the search fails to produce a non-null {@link SaslClient} instance.</p>
   *
   * <p>A security provider for <code>SaslClientFactory</code> registers with
   * the JCA Security Provider Framework keys of the form:</p>
   *
   * <pre>
   *    SaslClientFactory.mechanism_name
   * </pre>
   *
   * <p>and values that are class names of implementations of {@link
   * SaslClientFactory}.</p>
   *
   * <p>For example, a provider that contains a factory class,
   * <code>com.wiz.sasl.digest.ClientFactory</code>, that supports the
   * <code>"DIGEST-MD5"</code> mechanism would register the following entry
   * with the JCA:</p>
   *
   * <pre>
   *    SaslClientFactory.DIGEST-MD5     com.wiz.sasl.digest.ClientFactory
   * </pre>
   *
   * <p>See the "Java Cryptography Architecture API Specification &amp;
   * Reference" for information about how to install and configure security
   * service providers.</p>
   *
   * @param mechanisms the non-null list of mechanism names to try. Each is the
   * IANA-registered name of a SASL mechanism. (e.g. "GSSAPI", "CRAM-MD5").
   * @param authorizationID the possibly <code>null</code> protocol-dependent
   * identification to be used for authorization. If <code>null</code> or
   * empty, the server derives an authorization ID from the client's
   * authentication credentials. When the SASL authentication completes
   * successfully, the specified entity is granted access.
   * @param protocol the non-null string name of the protocol for which the
   * authentication is being performed (e.g. "ldap").
   * @param serverName the non-null fully-qualified host name of the server to
   * authenticate to.
   * @param props the possibly null set of properties used to select the SASL
   * mechanism and to configure the authentication exchange of the selected
   * mechanism. For example, if props contains the {@link Sasl#POLICY_NOPLAINTEXT}
   * property with the value <code>"true"</code>, then the selected SASL
   * mechanism must not be susceptible to simple plain passive attacks. In
   * addition to the standard properties declared in this class, other,
   * possibly mechanism-specific, properties can be included. Properties not
   * relevant to the selected mechanism are ignored.
   * @param cbh the possibly <code>null</code> callback handler to used by the
   * SASL mechanisms to get further information from the application/library to
   * complete the authentication. For example, a SASL mechanism might require
   * the authentication ID, password and realm from the caller. The
   * authentication ID is requested by using a
   * {@link javax.security.auth.callback.NameCallback}. The password is
   * requested by using a {@link javax.security.auth.callback.PasswordCallback}.
   * The realm is requested by using a {@link RealmChoiceCallback} if there is
   * a list of realms to choose from, and by using a {@link RealmCallback} if
   * the realm must be entered.
   * @return a possibly <code>null</code> {@link SaslClient} created using the
   * parameters supplied. If <code>null</code>, the method could not find a
   * {@link SaslClientFactory} that will produce one.
   * @throws SaslException if a {@link SaslClient} cannot be created because
   * of an error.
   */
  public static SaslClient createSaslClient(String[] mechanisms,
                                            String authorizationID,
                                            String protocol,
                                            String serverName,
                                            Map<String, ?> props,
                                            CallbackHandler cbh)
    throws SaslException
  {
    if (mechanisms == null)
      {
        return null;
      }
    Provider[] providers = Security.getProviders();
    if (providers == null || providers.length == 0)
      {
        return null;
      }

    SaslClient result = null;
    SaslClientFactory factory = null;
    String m, clazz = null, upper, alias;
    int j;
    Provider p;
    for (int i = 0; i < mechanisms.length; i++)
      {
        m = mechanisms[i];
        if (m == null)
          continue;
        for (j = 0; j < providers.length; j++)
          {
            p = providers[j];
            if (p != null)
              {
                // try the name as is
                clazz = p.getProperty(CLIENT_FACTORY_SVC + m);
                if (clazz == null) // try all uppercase
                  {
                    upper = m.toUpperCase();
                    clazz = p.getProperty(CLIENT_FACTORY_SVC + upper);
                    if (clazz == null) // try if it's an alias
                      {
                        alias = p.getProperty(ALIAS + CLIENT_FACTORY_SVC + m);
                        if (alias == null) // try all-uppercase alias name
                          {
                            alias = p.getProperty(ALIAS + CLIENT_FACTORY_SVC + upper);
                            if (alias == null) // spit the dummy
                              continue;
                          }
                        clazz = p.getProperty(CLIENT_FACTORY_SVC + alias);
                      }
                  }
                if (clazz == null)
                  continue;
                else
                  clazz = clazz.trim();
              }

            try
              {
                result = null;
                factory = (SaslClientFactory) Class.forName(clazz).newInstance();
                result = factory.createSaslClient(mechanisms, authorizationID,
                                                  protocol, serverName, props, cbh);
              }
            catch (ClassCastException ignored) // ignore instantiation exceptions
              {
              }
            catch (ClassNotFoundException ignored)
              {
              }
            catch (InstantiationException ignored)
              {
              }
            catch (IllegalAccessException ignored)
              {
              }
            if (result != null)
              return result;
          }
      }
    return null;
  }

  /**
   * Gets an enumeration of known factories for producing a {@link SaslClient}
   * instance. This method uses the same sources for locating factories as
   * <code>createSaslClient()</code>.
   *
   * @return a non-null {@link Enumeration} of known factories for producing a
   * {@link SaslClient} instance.
   * @see #createSaslClient(String[],String,String,String,Map,CallbackHandler)
   */
  public static Enumeration<SaslClientFactory> getSaslClientFactories()
  {
    Vector result = new Vector();
    HashSet names = new HashSet();
    Provider[] providers = Security.getProviders();
    Iterator it;
    if (providers != null)
      {
        Provider p;
        String key;
        for (int i = 0; i < providers.length; i++)
          {
            p = providers[i];
            for (it = p.keySet().iterator(); it.hasNext(); )
              {
                key = (String) it.next();
                // add key's binding (a) it is a class of a client factory,
                // and (b) the key does not include blanks
                if (key.startsWith(CLIENT_FACTORY_SVC) && key.indexOf(" ") == -1)
                  {
                    names.add(p.getProperty(key));
                    break;
                  }
              }
          }
      }
    // we have the factory class names in names; instantiate and enumerate
    String c;
    for (it = names.iterator(); it.hasNext(); )
      {
        c = (String) it.next();
        try
          {
            SaslClientFactory f = (SaslClientFactory) Class.forName(c).newInstance();
            if (f != null)
              result.add(f);
          } catch (ClassCastException ignored) { // ignore instantiation exceptions
          } catch (ClassNotFoundException ignored) {
          } catch (InstantiationException ignored) {
          } catch (IllegalAccessException ignored) {
          }
      }

    return result.elements();
  }

  /**
   * Creates a {@link SaslServer} for the specified mechanism.
   *
   * <p>This method uses the JCA Security Provider Framework, described in the
   * "Java Cryptography Architecture API Specification &amp; Reference", for
   * locating and selecting a SaslServer implementation.</p>
   *
   * <p>First, it obtains an ordered list of {@link SaslServerFactory}
   * instances from the registered security providers for the
   * <code>"SaslServerFactory"</code> service and the specified mechanism. It
   * then invokes <code>createSaslServer()</code> on each factory instance on
   * the list until one produces a non-null {@link SaslServer} instance. It
   * returns the non-null {@link SaslServer} instance, or <code>null</code> if
   * the search fails to produce a non-null {@link SaslServer} instance.</p>
   *
   * <p>A security provider for {@link SaslServerFactory} registers with the
   * JCA Security Provider Framework keys of the form:</p>
   *
   * <pre>
   *    SaslServerFactory.mechanism_name
   * </pre>
   *
   * <p>and values that are class names of implementations of {@link
   * SaslServerFactory}.</p>
   *
   * <p>For example, a provider that contains a factory class,
   * <code>com.wiz.sasl.digest.ServerFactory</code>, that supports the
   * <code>"DIGEST-MD5"</code> mechanism would register the following entry
   * with the JCA:</p>
   *
   * <pre>
   *    SaslServerFactory.DIGEST-MD5     com.wiz.sasl.digest.ServerFactory
   * </pre>
   *
   * <p>See the "Java Cryptography Architecture API Specification &amp;
   * Reference" for information about how to install and configure security
   * service providers.</p>
   *
   * @param mechanism the non-null mechanism name. It must be an
   * IANA-registered name of a SASL mechanism. (e.g. "GSSAPI", "CRAM-MD5").
   * @param protocol the non-null string name of the protocol for which the
   * authentication is being performed (e.g. "ldap").
   * @param serverName the non-null fully qualified host name of the server.
   * @param props the possibly <code>null</code> set of properties used to
   * select the SASL mechanism and to configure the authentication exchange of
   * the selected mechanism. For example, if props contains the {@link
   * Sasl#POLICY_NOPLAINTEXT} property with the value <code>"true"</code>, then
   * the selected SASL mechanism must not be susceptible to simple plain
   * passive attacks. In addition to the standard properties declared in this
   * class, other, possibly mechanism-specific, properties can be included.
   * Properties not relevant to the selected mechanism are ignored.
   * @param cbh the possibly <code>null</code> callback handler to used by the
   * SASL mechanisms to get further information from the application/library to
   * complete the authentication. For example, a SASL mechanism might require
   * the authentication ID, password and realm from the caller. The
   * authentication ID is requested by using a
   * {@link javax.security.auth.callback.NameCallback}. The password is
   * requested by using a {@link javax.security.auth.callback.PasswordCallback}.
   * The realm is requested by using a {@link RealmChoiceCallback} if there is
   * a list of realms to choose from, and by using a {@link RealmCallback} if
   * the realm must be entered.
   * @return a possibly <code>null</code> {@link SaslServer} created using the
   * parameters supplied. If <code>null</code>, the method cannot find a
   * {@link SaslServerFactory} instance that will produce one.
   * @throws SaslException if a {@link SaslServer} instance cannot be created
   * because of an error.
   */
  public static SaslServer createSaslServer(String mechanism, String protocol,
                                            String serverName,
                                            Map<String, ?> props,
                                            CallbackHandler cbh)
    throws SaslException
  {
    if (mechanism == null)
      return null;
    Provider[] providers = Security.getProviders();
    if (providers == null || providers.length == 0)
      return null;

    SaslServer result = null;
    SaslServerFactory factory = null;
    String clazz = null, upper, alias = null;
    int j;
    Provider p;
    for (j = 0; j < providers.length; j++)
      {
        p = providers[j];
        if (p != null)
          {
            // try the name as is
            clazz = p.getProperty(SERVER_FACTORY_SVC + mechanism);
            if (clazz == null) // try all uppercase
              {
                upper = mechanism.toUpperCase();
                clazz = p.getProperty(SERVER_FACTORY_SVC + upper);
                if (clazz == null) // try if it's an alias
                  {
                    alias = p.getProperty(ALIAS + SERVER_FACTORY_SVC + mechanism);
                    if (alias == null) // try all-uppercase alias name
                      {
                        alias = p.getProperty(ALIAS + SERVER_FACTORY_SVC + upper);
                        if (alias == null) // spit the dummy
                          continue;
                      }
                  }
                clazz = p.getProperty(SERVER_FACTORY_SVC + alias);
              }
          }
        if (clazz == null)
          continue;
        else
          clazz = clazz.trim();

        try
          {
            result = null;
            factory = (SaslServerFactory) Class.forName(clazz).newInstance();
            result =
              factory.createSaslServer(mechanism, protocol, serverName, props, cbh);
          }
        catch (ClassCastException ignored) // ignore instantiation exceptions
          {
          }
        catch (ClassNotFoundException ignored)
          {
          }
        catch (InstantiationException ignored)
          {
          }
        catch (IllegalAccessException ignored)
          {
          }
        if (result != null)
          return result;
      }
    return null;
  }

  /**
   * Gets an enumeration of known factories for producing a {@link SaslServer}
   * instance. This method uses the same sources for locating factories as
   * <code>createSaslServer()</code>.
   *
   * @return a non-null {@link Enumeration} of known factories for producing a
   * {@link SaslServer} instance.
   * @see #createSaslServer(String,String,String,Map,CallbackHandler)
   */
  public static Enumeration<SaslServerFactory> getSaslServerFactories()
  {
    Vector result = new Vector();
    HashSet names = new HashSet();
    Provider[] providers = Security.getProviders();
    Iterator it;
    if (providers != null)
      {
        Provider p;
        String key;
        for (int i = 0; i < providers.length; i++)
          {
            p = providers[i];
            for (it = p.keySet().iterator(); it.hasNext(); )
              {
                key = (String) it.next();
                // add key's binding (a) it is a class of a server factory,
                // and (b) the key does not include blanks
                if (key.startsWith(SERVER_FACTORY_SVC) && key.indexOf(" ") == -1)
                  {
                    names.add(p.getProperty(key));
                    break;
                  }
              }
          }
      }
    // we have the factory class names in names; instantiate and enumerate
    String c;
    for (it = names.iterator(); it.hasNext(); )
      {
        c = (String) it.next();
        try
          {
            SaslServerFactory f = (SaslServerFactory) Class.forName(c).newInstance();
            if (f != null)
              result.add(f);
          }
        catch (ClassCastException ignored) // ignore instantiation exceptions
          {
          }
        catch (ClassNotFoundException ignored)
          {
          }
        catch (InstantiationException ignored)
          {
          }
        catch (IllegalAccessException ignored)
          {
          }
      }

    return result.elements();
  }
}
