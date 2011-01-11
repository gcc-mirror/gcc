/* SRPAuthInfoProvider.java --
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


package gnu.javax.crypto.sasl.srp;

import gnu.java.security.Registry;
import gnu.java.security.util.Util;
import gnu.javax.crypto.sasl.IAuthInfoProvider;
import gnu.javax.crypto.sasl.NoSuchUserException;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import javax.security.sasl.AuthenticationException;

/**
 * The SRP mechanism authentication information provider implementation.
 */
public class SRPAuthInfoProvider
    implements IAuthInfoProvider
{
  private PasswordFile passwordFile = null;

  // implicit 0-args constrcutor

  public void activate(Map context) throws AuthenticationException
  {
    try
      {
        if (context == null)
          passwordFile = new PasswordFile();
        else
          {
            passwordFile = (PasswordFile) context.get(SRPRegistry.PASSWORD_DB);
            if (passwordFile == null)
              {
                String pfn = (String) context.get(SRPRegistry.PASSWORD_FILE);
                if (pfn == null)
                  passwordFile = new PasswordFile();
                else
                  passwordFile = new PasswordFile(pfn);
              }
          }
      }
    catch (IOException x)
      {
        throw new AuthenticationException("activate()", x);
      }
  }

  public void passivate() throws AuthenticationException
  {
    passwordFile = null;
  }

  public boolean contains(String userName) throws AuthenticationException
  {
    if (passwordFile == null)
      throw new AuthenticationException("contains()",
                                        new IllegalStateException());
    boolean result = false;
    try
      {
        result = passwordFile.contains(userName);
      }
    catch (IOException x)
      {
        throw new AuthenticationException("contains()", x);
      }
    return result;
  }

  public Map lookup(Map userID) throws AuthenticationException
  {
    if (passwordFile == null)
      throw new AuthenticationException("lookup()", new IllegalStateException());
    Map result = new HashMap();
    try
      {
        String userName = (String) userID.get(Registry.SASL_USERNAME);
        if (userName == null)
          throw new NoSuchUserException("");
        String mdName = (String) userID.get(SRPRegistry.MD_NAME_FIELD);
        String[] data = passwordFile.lookup(userName, mdName);
        result.put(SRPRegistry.USER_VERIFIER_FIELD, data[0]);
        result.put(SRPRegistry.SALT_FIELD, data[1]);
        result.put(SRPRegistry.CONFIG_NDX_FIELD, data[2]);
      }
    catch (Exception x)
      {
        if (x instanceof AuthenticationException)
          throw (AuthenticationException) x;
        throw new AuthenticationException("lookup()", x);
      }
    return result;
  }

  public void update(Map userCredentials) throws AuthenticationException
  {
    if (passwordFile == null)
      throw new AuthenticationException("update()", new IllegalStateException());
    try
      {
        String userName = (String) userCredentials.get(Registry.SASL_USERNAME);
        String password = (String) userCredentials.get(Registry.SASL_PASSWORD);
        String salt = (String) userCredentials.get(SRPRegistry.SALT_FIELD);
        String config = (String) userCredentials.get(SRPRegistry.CONFIG_NDX_FIELD);
        if (salt == null || config == null)
          passwordFile.changePasswd(userName, password);
        else
          passwordFile.add(userName, password, Util.fromBase64(salt), config);
      }
    catch (Exception x)
      {
        if (x instanceof AuthenticationException)
          throw (AuthenticationException) x;
        throw new AuthenticationException("update()", x);
      }
  }

  public Map getConfiguration(String mode) throws AuthenticationException
  {
    if (passwordFile == null)
      throw new AuthenticationException("getConfiguration()",
                                        new IllegalStateException());
    Map result = new HashMap();
    try
      {
        String[] data = passwordFile.lookupConfig(mode);
        result.put(SRPRegistry.SHARED_MODULUS, data[0]);
        result.put(SRPRegistry.FIELD_GENERATOR, data[1]);
      }
    catch (Exception x)
      {
        if (x instanceof AuthenticationException)
          throw (AuthenticationException) x;
        throw new AuthenticationException("getConfiguration()", x);
      }
    return result;
  }
}
