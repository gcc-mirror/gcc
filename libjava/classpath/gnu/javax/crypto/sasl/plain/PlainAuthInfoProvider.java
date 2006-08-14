/* PlainAuthInfoProvider.java -- 
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
import gnu.javax.crypto.sasl.IAuthInfoProvider;
import gnu.javax.crypto.sasl.NoSuchUserException;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import javax.security.sasl.AuthenticationException;

/**
 * The PLAIN mechanism authentication information provider implementation.
 */
public class PlainAuthInfoProvider
    implements IAuthInfoProvider, PlainRegistry
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
            String pfn = (String) context.get(PASSWORD_FILE);
            if (pfn == null)
              passwordFile = new PasswordFile();
            else
              passwordFile = new PasswordFile(pfn);
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
        String[] data = passwordFile.lookup(userName);
        result.put(Registry.SASL_USERNAME, data[0]);
        result.put(Registry.SASL_PASSWORD, data[1]);
        result.put(UID_FIELD, data[2]);
        result.put(GID_FIELD, data[3]);
        result.put(GECOS_FIELD, data[4]);
        result.put(DIR_FIELD, data[5]);
        result.put(SHELL_FIELD, data[6]);
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
        String uid = (String) userCredentials.get(UID_FIELD);
        String gid = (String) userCredentials.get(GID_FIELD);
        String gecos = (String) userCredentials.get(GECOS_FIELD);
        String dir = (String) userCredentials.get(DIR_FIELD);
        String shell = (String) userCredentials.get(SHELL_FIELD);
        if (uid == null || gid == null || gecos == null || dir == null
            || shell == null)
          passwordFile.changePasswd(userName, password);
        else
          {
            String[] attributes = new String[] { uid, gid, gecos, dir, shell };
            passwordFile.add(userName, password, attributes);
          }
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
    throw new AuthenticationException("", new UnsupportedOperationException());
  }
}
