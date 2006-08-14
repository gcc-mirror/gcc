/* PlainClient.java -- 
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


package gnu.javax.crypto.sasl.plain;

import gnu.java.security.Registry;
import gnu.javax.crypto.sasl.ClientMechanism;

import javax.security.sasl.SaslClient;
import javax.security.sasl.SaslException;
import javax.security.auth.callback.Callback;
import javax.security.auth.callback.NameCallback;
import javax.security.auth.callback.PasswordCallback;

/**
 * The PLAIN SASL client-side mechanism.
 */
public class PlainClient
    extends ClientMechanism
    implements SaslClient
{
  public PlainClient()
  {
    super(Registry.SASL_PLAIN_MECHANISM);
  }

  protected void initMechanism() throws SaslException
  {
  }

  protected void resetMechanism() throws SaslException
  {
  }

  public boolean hasInitialResponse()
  {
    return true;
  }

  public byte[] evaluateChallenge(final byte[] challenge) throws SaslException
  {
    try
      {
        final String username;
        final char[] password;
        Callback[] callbacks;
        if ((! properties.containsKey(Registry.SASL_USERNAME))
            && (! properties.containsKey(Registry.SASL_PASSWORD)))
          {
            callbacks = new Callback[2];
            final NameCallback nameCB;
            final String defaultName = System.getProperty("user.name");
            if (defaultName == null)
              nameCB = new NameCallback("username: ");
            else
              nameCB = new NameCallback("username: ", defaultName);
            final PasswordCallback pwdCB = new PasswordCallback("password: ",
                                                                false);
            callbacks[0] = nameCB;
            callbacks[1] = pwdCB;
            this.handler.handle(callbacks);
            username = nameCB.getName();
            password = pwdCB.getPassword();
          }
        else
          {
            if (properties.containsKey(Registry.SASL_USERNAME))
              username = (String) properties.get(Registry.SASL_USERNAME);
            else
              {
                callbacks = new Callback[1];
                final NameCallback nameCB;
                final String defaultName = System.getProperty("user.name");
                if (defaultName == null)
                  nameCB = new NameCallback("username: ");
                else
                  nameCB = new NameCallback("username: ", defaultName);
                callbacks[0] = nameCB;
                this.handler.handle(callbacks);
                username = nameCB.getName();
              }
            if (properties.containsKey(Registry.SASL_PASSWORD))
              password = ((String) properties.get(Registry.SASL_PASSWORD)).toCharArray();
            else
              {
                callbacks = new Callback[1];
                final PasswordCallback pwdCB = new PasswordCallback("password: ",
                                                                    false);
                callbacks[0] = pwdCB;
                this.handler.handle(callbacks);
                password = pwdCB.getPassword();
              }
          }
        if (password == null)
          throw new SaslException("null password supplied");
        final StringBuffer sb = new StringBuffer();
        if (authorizationID != null)
          sb.append(authorizationID);
        sb.append('\0');
        sb.append(username);
        sb.append('\0');
        sb.append(password);
        this.complete = true;
        final byte[] response = sb.toString().getBytes("UTF-8");
        return response;
      }
    catch (Exception x)
      {
        if (x instanceof SaslException)
          throw (SaslException) x;
        throw new SaslException("evaluateChallenge()", x);
      }
  }

  protected String getNegotiatedQOP()
  {
    return Registry.QOP_AUTH;
  }
}
