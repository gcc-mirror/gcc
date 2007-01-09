/* StorePasswdCmd.java -- The storepasswd command handler of the keytool
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package gnu.classpath.tools.keytool;

import gnu.classpath.Configuration;
import gnu.classpath.SystemProperties;
import gnu.classpath.tools.common.ClasspathToolParser;
import gnu.classpath.tools.getopt.Option;
import gnu.classpath.tools.getopt.OptionException;
import gnu.classpath.tools.getopt.OptionGroup;
import gnu.classpath.tools.getopt.Parser;

import java.io.IOException;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateException;
import java.util.Arrays;
import java.util.logging.Logger;

import javax.security.auth.callback.Callback;
import javax.security.auth.callback.CallbackHandler;
import javax.security.auth.callback.PasswordCallback;
import javax.security.auth.callback.TextOutputCallback;
import javax.security.auth.callback.UnsupportedCallbackException;

/**
 * The <b>-storepasswd</b> keytool command handler is used to change the
 * password which protects the integrity of the key store.
 * <p>
 * Possible options for this command are:
 * <p>
 * <dl>
 *      <dt>-new PASSWORD</dt>
 *      <dd>The new, and different, password which will be used to protect the
 *      designated key store.
 *      <p></dd>
 *      
 *      <dt>-storetype STORE_TYPE</dt>
 *      <dd>Use this option to specify the type of the key store to use. The
 *      default value, if this option is omitted, is that of the property
 *      <code>keystore.type</code> in the security properties file, which is
 *      obtained by invoking the {@link java.security.KeyStore#getDefaultType()}
 *      static method.
 *      <p></dd>
 *      
 *      <dt>-keystore URL</dt>
 *      <dd>Use this option to specify the location of the key store to use.
 *      The default value is a file {@link java.net.URL} referencing the file
 *      named <code>.keystore</code> located in the path returned by the call to
 *      {@link java.lang.System#getProperty(String)} using <code>user.home</code>
 *      as argument.
 *      <p>
 *      If a URL was specified, but was found to be malformed --e.g. missing
 *      protocol element-- the tool will attempt to use the URL value as a file-
 *      name (with absolute or relative path-name) of a key store --as if the
 *      protocol was <code>file:</code>.
 *      <p></dd>
 *      
 *      <dt>-storepass PASSWORD</dt>
 *      <dd>Use this option to specify the password protecting the key store. If
 *      this option is omitted from the command line, you will be prompted to
 *      provide a password.
 *      <p></dd>
 *      
 *      <dt>-provider PROVIDER_CLASS_NAME</dt>
 *      <dd>A fully qualified class name of a Security Provider to add to the
 *      current list of Security Providers already installed in the JVM in-use.
 *      If a provider class is specified with this option, and was successfully
 *      added to the runtime --i.e. it was not already installed-- then the tool
 *      will attempt to removed this Security Provider before exiting.
 *      <p></dd>
 *      
 *      <dt>-v</dt>
 *      <dd>Use this option to enable more verbose output.</dd>
 * </dl>
 */
class StorePasswdCmd extends Command
{
  private static final Logger log = Logger.getLogger(StorePasswdCmd.class.getName());
  protected String _newPassword;
  protected String _ksType;
  protected String _ksURL;
  protected String _ksPassword;
  protected String _providerClassName;
  private char[] newStorePasswordChars;

  // default 0-arguments constructor

  // public setters -----------------------------------------------------------

  /** @param password the new key-store password to use. */
  public void setNew(String password)
  {
    this._newPassword = password;
  }

  /** @param type the key-store type to use. */
  public void setStoretype(String type)
  {
    this._ksType = type;
  }

  /** @param url the key-store URL to use. */
  public void setKeystore(String url)
  {
    this._ksURL = url;
  }

  /** @param password the key-store password to use. */
  public void setStorepass(String password)
  {
    this._ksPassword = password;
  }

  /** @param className a security provider fully qualified class name to use. */
  public void setProvider(String className)
  {
    this._providerClassName = className;
  }

  // life-cycle methods -------------------------------------------------------

  void setup() throws Exception
  {
    setKeyStoreParams(_providerClassName, _ksType, _ksPassword, _ksURL);
    setNewKeystorePassword(_newPassword);
    if (Configuration.DEBUG)
      {
        log.fine("-storepasswd handler will use the following options:"); //$NON-NLS-1$
        log.fine("  -storetype=" + storeType); //$NON-NLS-1$
        log.fine("  -keystore=" + storeURL); //$NON-NLS-1$
        log.fine("  -provider=" + provider); //$NON-NLS-1$
        log.fine("  -v=" + verbose); //$NON-NLS-1$
      }
  }

  void start() throws KeyStoreException, NoSuchAlgorithmException,
      CertificateException, IOException
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "start"); //$NON-NLS-1$
    saveKeyStore(newStorePasswordChars);
    if (Configuration.DEBUG)
      log.exiting(getClass().getName(), "start"); //$NON-NLS-1$
  }

  // own methods --------------------------------------------------------------

  Parser getParser()
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "getParser"); //$NON-NLS-1$
    Parser result = new ClasspathToolParser(Main.STOREPASSWD_CMD, true);
    result.setHeader(Messages.getString("StorePasswdCmd.18")); //$NON-NLS-1$
    result.setFooter(Messages.getString("StorePasswdCmd.17")); //$NON-NLS-1$
    OptionGroup options = new OptionGroup(Messages.getString("StorePasswdCmd.16")); //$NON-NLS-1$
    options.add(new Option(Main.NEW_OPT,
                           Messages.getString("StorePasswdCmd.15"), //$NON-NLS-1$
                           Messages.getString("StorePasswdCmd.8")) //$NON-NLS-1$
    {
      public void parsed(String argument) throws OptionException
      {
        _newPassword = argument;
      }
    });
    options.add(new Option(Main.STORETYPE_OPT,
                           Messages.getString("StorePasswdCmd.13"), //$NON-NLS-1$
                           Messages.getString("StorePasswdCmd.12")) //$NON-NLS-1$
    {
      public void parsed(String argument) throws OptionException
      {
        _ksType = argument;
      }
    });
    options.add(new Option(Main.KEYSTORE_OPT,
                           Messages.getString("StorePasswdCmd.11"), //$NON-NLS-1$
                           Messages.getString("StorePasswdCmd.10")) //$NON-NLS-1$
    {
      public void parsed(String argument) throws OptionException
      {
        _ksURL = argument;
      }
    });
    options.add(new Option(Main.STOREPASS_OPT,
                           Messages.getString("StorePasswdCmd.9"), //$NON-NLS-1$
                           Messages.getString("StorePasswdCmd.8")) //$NON-NLS-1$
    {
      public void parsed(String argument) throws OptionException
      {
        _ksPassword = argument;
      }
    });
    options.add(new Option(Main.PROVIDER_OPT,
                           Messages.getString("StorePasswdCmd.7"), //$NON-NLS-1$
                           Messages.getString("StorePasswdCmd.6")) //$NON-NLS-1$
    {
      public void parsed(String argument) throws OptionException
      {
        _providerClassName = argument;
      }
    });
    options.add(new Option(Main.VERBOSE_OPT,
                           Messages.getString("StorePasswdCmd.5")) //$NON-NLS-1$
    {
      public void parsed(String argument) throws OptionException
      {
        verbose = true;
      }
    });
    result.add(options);
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "getParser", result); //$NON-NLS-1$
    return result;
  }

  protected void setNewKeystorePassword(String password) throws IOException,
      UnsupportedCallbackException
  {
    if (password != null)
      newStorePasswordChars = password.toCharArray();
    else
      {
        boolean ok = false;
        Callback[] prompts = new Callback[1];
        Callback[] errors = new Callback[1];
        for (int i = 0; i < 3; i++)
          if (prompt4NewPassword(getCallbackHandler(), prompts, errors))
            {
              ok = true;
              break;
            }
        if (! ok)
          throw new SecurityException(Messages.getString("StorePasswdCmd.19")); //$NON-NLS-1$
      }
  }

  private boolean prompt4NewPassword(CallbackHandler handler,
                                     Callback[] prompts, Callback[] errors)
      throws IOException, UnsupportedCallbackException
  {
    // prompt user (1st time) to provide one
    String p = Messages.getString("StorePasswdCmd.20"); //$NON-NLS-1$
    PasswordCallback pcb = new PasswordCallback(p, false);
    prompts[0] = pcb;
    handler.handle(prompts);
    char[] pwd1 = pcb.getPassword();
    pcb.clearPassword();
    String ls = SystemProperties.getProperty("line.separator"); //$NON-NLS-1$
    if (pwd1 == null || pwd1.length < 6)
      {
        String m = Messages.getString("StorePasswdCmd.21") + ls; //$NON-NLS-1$
        errors[0] = new TextOutputCallback(TextOutputCallback.ERROR, m);
        handler.handle(errors);
        return false;
      }

    if (Arrays.equals(storePasswordChars, pwd1))
      {
        String m = Messages.getString("StorePasswdCmd.22") + ls; //$NON-NLS-1$
        errors[0] = new TextOutputCallback(TextOutputCallback.ERROR, m);
        handler.handle(errors);
        return false;
      }

    // prompt user (2nd time) for confirmation
    pcb = new PasswordCallback(Messages.getString("StorePasswdCmd.23"), false); //$NON-NLS-1$
    prompts[0] = pcb;
    handler.handle(prompts);
    char[] pwd2 = pcb.getPassword();
    pcb.clearPassword();
    if (! Arrays.equals(pwd1, pwd2))
      {
        String m = Messages.getString("StorePasswdCmd.24") + ls; //$NON-NLS-1$
        errors[0] = new TextOutputCallback(TextOutputCallback.ERROR, m);
        handler.handle(errors);
        return false;
      }

    newStorePasswordChars = pwd2;
    return true;
  }
}
