/* PlainServer.java --
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
import gnu.javax.crypto.sasl.NoSuchUserException;
import gnu.javax.crypto.sasl.ServerMechanism;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.StringTokenizer;

import javax.security.sasl.SaslException;
import javax.security.sasl.SaslServer;

/**
 * The PLAIN SASL server-side mechanism.
 */
public class PlainServer
    extends ServerMechanism
    implements SaslServer
{
  public PlainServer()
  {
    super(Registry.SASL_PLAIN_MECHANISM);
  }

  protected void initMechanism() throws SaslException
  {
  }

  protected void resetMechanism() throws SaslException
  {
  }

  public byte[] evaluateResponse(final byte[] response) throws SaslException
  {
    if (response == null)
      return null;
    try
      {
        final String nullStr = new String("\0");
        final StringTokenizer strtok = new StringTokenizer(new String(response),
                                                           nullStr, true);
        authorizationID = strtok.nextToken();
        if (! authorizationID.equals(nullStr))
          strtok.nextToken();
        else
          authorizationID = null;
        final String id = strtok.nextToken();
        if (id.equals(nullStr))
          throw new SaslException("No identity given");
        if (authorizationID == null)
          authorizationID = id;
        if ((! authorizationID.equals(nullStr)) && (! authorizationID.equals(id)))
          throw new SaslException("Delegation not supported");
        strtok.nextToken();
        final byte[] pwd;
        try
          {
            pwd = strtok.nextToken().getBytes("UTF-8");
          }
        catch (UnsupportedEncodingException x)
          {
            throw new SaslException("evaluateResponse()", x);
          }
        if (pwd == null)
          throw new SaslException("No password given");
        final byte[] password;
        try
          {
            password = new String(lookupPassword(id)).getBytes("UTF-8");
          }
        catch (UnsupportedEncodingException x)
          {
            throw new SaslException("evaluateResponse()", x);
          }
        if (! Arrays.equals(pwd, password))
          throw new SaslException("Password incorrect");
        this.complete = true;
        return null;
      }
    catch (NoSuchElementException x)
      {
        throw new SaslException("evaluateResponse()", x);
      }
  }

  protected String getNegotiatedQOP()
  {
    return Registry.QOP_AUTH;
  }

  private char[] lookupPassword(final String userName) throws SaslException
  {
    try
      {
        if (! authenticator.contains(userName))
          throw new NoSuchUserException(userName);
        final Map userID = new HashMap();
        userID.put(Registry.SASL_USERNAME, userName);
        final Map credentials = authenticator.lookup(userID);
        final String password = (String) credentials.get(Registry.SASL_PASSWORD);
        if (password == null)
          throw new SaslException("lookupPassword()", new InternalError());
        return password.toCharArray();
      }
    catch (IOException x)
      {
        if (x instanceof SaslException)
          throw (SaslException) x;
        throw new SaslException("lookupPassword()", x);
      }
  }
}
