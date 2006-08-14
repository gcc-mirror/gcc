/* ClientFactory.java -- 
   Copyright (C) 2003, 2006 Free Software Foundation, Inc.

This file is a part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
USA

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
exception statement from your version.  */


package gnu.javax.crypto.sasl;

import gnu.java.security.Registry;
import gnu.javax.crypto.sasl.anonymous.AnonymousClient;
import gnu.javax.crypto.sasl.crammd5.CramMD5Client;
import gnu.javax.crypto.sasl.plain.PlainClient;
import gnu.javax.crypto.sasl.srp.SRPClient;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.security.auth.callback.CallbackHandler;
import javax.security.sasl.Sasl;
import javax.security.sasl.SaslClient;
import javax.security.sasl.SaslClientFactory;
import javax.security.sasl.SaslException;

/**
 * The implementation of {@link SaslClientFactory}.
 */
public class ClientFactory
    implements SaslClientFactory
{
  // implicit 0-arguments constructor

  public static final Set getNames()
  {
    return Collections.unmodifiableSet(new HashSet(Arrays.asList(getNamesInternal(null))));
  }

  private static final String[] getNamesInternal(Map props)
  {
    String[] all = new String[] {
        Registry.SASL_SRP_MECHANISM,
        Registry.SASL_CRAM_MD5_MECHANISM,
        Registry.SASL_PLAIN_MECHANISM,
        Registry.SASL_ANONYMOUS_MECHANISM };
    if (props == null)
      return all;
    if (hasPolicy(Sasl.POLICY_PASS_CREDENTIALS, props))
      return new String[0];
    List result = new ArrayList(all.length);
    for (int i = 0; i < all.length;)
      result.add(all[i++]);
    if (hasPolicy(Sasl.POLICY_NOPLAINTEXT, props))
      result.remove(Registry.SASL_PLAIN_MECHANISM);
    if (hasPolicy(Sasl.POLICY_NOACTIVE, props))
      {
        result.remove(Registry.SASL_CRAM_MD5_MECHANISM);
        result.remove(Registry.SASL_PLAIN_MECHANISM);
      }
    if (hasPolicy(Sasl.POLICY_NODICTIONARY, props))
      {
        result.remove(Registry.SASL_CRAM_MD5_MECHANISM);
        result.remove(Registry.SASL_PLAIN_MECHANISM);
      }
    if (hasPolicy(Sasl.POLICY_NOANONYMOUS, props))
      {
        result.remove(Registry.SASL_ANONYMOUS_MECHANISM);
      }
    if (hasPolicy(Sasl.POLICY_FORWARD_SECRECY, props))
      {
        result.remove(Registry.SASL_CRAM_MD5_MECHANISM);
        result.remove(Registry.SASL_ANONYMOUS_MECHANISM);
        result.remove(Registry.SASL_PLAIN_MECHANISM);
      }
    return (String[]) result.toArray(new String[0]);
  }

  public static final ClientMechanism getInstance(String mechanism)
  {
    if (mechanism == null)
      return null;
    mechanism = mechanism.trim().toUpperCase();
    if (mechanism.equals(Registry.SASL_SRP_MECHANISM))
      return new SRPClient();
    if (mechanism.equals(Registry.SASL_CRAM_MD5_MECHANISM))
      return new CramMD5Client();
    if (mechanism.equals(Registry.SASL_PLAIN_MECHANISM))
      return new PlainClient();
    if (mechanism.equals(Registry.SASL_ANONYMOUS_MECHANISM))
      return new AnonymousClient();
    return null;
  }

  public SaslClient createSaslClient(String[] mechanisms,
                                     String authorisationID, String protocol,
                                     String serverName, Map props,
                                     CallbackHandler cbh) throws SaslException
  {
    ClientMechanism result = null;
    String mechanism;
    for (int i = 0; i < mechanisms.length; i++)
      {
        mechanism = mechanisms[i];
        result = getInstance(mechanism);
        if (result != null)
          break;
      }
    if (result != null)
      {
        HashMap attributes = new HashMap();
        if (props != null)
          attributes.putAll(props);
        attributes.put(Registry.SASL_AUTHORISATION_ID, authorisationID);
        attributes.put(Registry.SASL_PROTOCOL, protocol);
        attributes.put(Registry.SASL_SERVER_NAME, serverName);
        attributes.put(Registry.SASL_CALLBACK_HANDLER, cbh);
        result.init(attributes);
        return result;
      }
    throw new SaslException("No supported mechanism found in given mechanism list");
  }

  public String[] getMechanismNames(Map props)
  {
    return getNamesInternal(props);
  }

  private static boolean hasPolicy(String propertyName, Map props)
  {
    return "true".equalsIgnoreCase(String.valueOf(props.get(propertyName)));
  }
}
