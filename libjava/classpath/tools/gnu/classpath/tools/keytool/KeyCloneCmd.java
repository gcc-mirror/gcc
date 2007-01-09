/* KeyCloneCmd.java -- The keyclone command handler of the keytool
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
import gnu.classpath.tools.common.ClasspathToolParser;
import gnu.classpath.tools.getopt.Option;
import gnu.classpath.tools.getopt.OptionException;
import gnu.classpath.tools.getopt.OptionGroup;
import gnu.classpath.tools.getopt.Parser;

import java.io.IOException;
import java.security.Key;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.UnrecoverableKeyException;
import java.security.cert.Certificate;
import java.security.cert.CertificateException;
import java.util.logging.Logger;

import javax.security.auth.callback.Callback;
import javax.security.auth.callback.CallbackHandler;
import javax.security.auth.callback.NameCallback;
import javax.security.auth.callback.PasswordCallback;
import javax.security.auth.callback.TextOutputCallback;
import javax.security.auth.callback.UnsupportedCallbackException;

/**
 * The <b>-keyclone</b> keytool command handler is used to clone an existing
 * key store entry associated with a designated alias, with its private key and
 * chain of certificates.
 * <p>
 * Possible options for this command are:
 * <p>
 * <dl>
 *      <dt>-alias ALIAS</dt>
 *      <dd>Every entry, be it a <i>Key Entry</i> or a <i>Trusted
 *      Certificate</i>, in a key store is uniquely identified by a user-defined
 *      <i>Alias</i> string. Use this option to specify the <i>Alias</i> to use
 *      when referring to an entry in the key store. Unless specified otherwise,
 *      a default value of <code>mykey</code> shall be used when this option is
 *      omitted from the command line.
 *      <p></dd>
 *      
 *      <dt>-dest ALIAS</dt>
 *      <dd>Use this option to specify the new <i>Alias</i> which will be used
 *      to identify the cloned copy of the <i>Key Entry</i>.
 *      <p></dd>
 *      
 *      <dt>-keypass PASSWORD</dt>
 *      <dd>Use this option to specify the password which the tool will use to
 *      unlock the <i>Key Entry</i> associated with the designated <i>Alias</i>.
 *      <p>
 *      If this option is omitted, the tool will first attempt to unlock the
 *      <i>Key Entry</i> using the same password protecting the key store. If
 *      this fails, you will then be prompted to provide a password.
 *      <p></dd>
 *      
 *      <dt>-new PASSWORD</dt>
 *      <dd>Use this option to specify the password protecting the private key
 *      material of the newly cloned copy of the <i>Key Entry</i>.
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
class KeyCloneCmd extends Command
{
  private static final Logger log = Logger.getLogger(KeyCloneCmd.class.getName());
  protected String _alias;
  protected String _destAlias;
  protected String _password;
  protected String _newPassword;
  protected String _ksType;
  protected String _ksURL;
  protected String _ksPassword;
  protected String _providerClassName;
  private String destinationAlias;
  private char[] newKeyPasswordChars;

  // default 0-arguments constructor

  // public setters -----------------------------------------------------------

  /** @param alias the existing alias to use. */
  public void setAlias(String alias)
  {
    this._alias = alias;
  }

  /** @param alias the new alias to use. */
  public void setDest(String alias)
  {
    this._destAlias = alias;
  }

  /** @param password the existing (private) key password to use. */
  public void setKeypass(String password)
  {
    this._password = password;
  }

  /** @param password the new (private) key password to use. */
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
    setAliasParam(_alias);
    setKeyPasswordNoPrompt(_password);
    setDestinationAlias(_destAlias);
    if (Configuration.DEBUG)
      {
        log.fine("-keyclone handler will use the following options:"); //$NON-NLS-1$
        log.fine("  -alias=" + alias); //$NON-NLS-1$
        log.fine("  -dest=" + destinationAlias); //$NON-NLS-1$
        log.fine("  -storetype=" + storeType); //$NON-NLS-1$
        log.fine("  -keystore=" + storeURL); //$NON-NLS-1$
        log.fine("  -provider=" + provider); //$NON-NLS-1$
        log.fine("  -v=" + verbose); //$NON-NLS-1$
      }
  }

  void start() throws KeyStoreException, NoSuchAlgorithmException, IOException,
      UnsupportedCallbackException, UnrecoverableKeyException,
      CertificateException
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "start"); //$NON-NLS-1$
    if (store.containsAlias(destinationAlias))
      throw new SecurityException(Messages.getString("KeyCloneCmd.23")); //$NON-NLS-1$

    Key privateKey = getAliasPrivateKey();

    setNewKeyPassword(_newPassword);
    Certificate[] chain = store.getCertificateChain(alias);

    store.setKeyEntry(destinationAlias, privateKey, newKeyPasswordChars, chain);

    saveKeyStore();
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "start"); //$NON-NLS-1$
  }

  // own methods --------------------------------------------------------------

  Parser getParser()
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "getParser"); //$NON-NLS-1$
    Parser result = new ClasspathToolParser(Main.KEYCLONE_CMD, true);
    result.setHeader(Messages.getString("KeyCloneCmd.22")); //$NON-NLS-1$
    result.setFooter(Messages.getString("KeyCloneCmd.21")); //$NON-NLS-1$
    OptionGroup options = new OptionGroup(Messages.getString("KeyCloneCmd.20")); //$NON-NLS-1$
    options.add(new Option(Main.ALIAS_OPT,
                           Messages.getString("KeyCloneCmd.19"), //$NON-NLS-1$
                           Messages.getString("KeyCloneCmd.16")) //$NON-NLS-1$
    {
      public void parsed(String argument) throws OptionException
      {
        _alias = argument;
      }
    });
    options.add(new Option(Main.DEST_OPT,
                           Messages.getString("KeyCloneCmd.17"), //$NON-NLS-1$
                           Messages.getString("KeyCloneCmd.16")) //$NON-NLS-1$
    {
      public void parsed(String argument) throws OptionException
      {
        _destAlias = argument;
      }
    });
    options.add(new Option(Main.KEYPASS_OPT,
                           Messages.getString("KeyCloneCmd.15"), //$NON-NLS-1$
                           Messages.getString("KeyCloneCmd.6")) //$NON-NLS-1$
    {
      public void parsed(String argument) throws OptionException
      {
        _password = argument;
      }
    });
    options.add(new Option(Main.NEW_OPT,
                           Messages.getString("KeyCloneCmd.13"), //$NON-NLS-1$
                           Messages.getString("KeyCloneCmd.6")) //$NON-NLS-1$
    {
      public void parsed(String argument) throws OptionException
      {
        _newPassword = argument;
      }
    });
    options.add(new Option(Main.STORETYPE_OPT,
                           Messages.getString("KeyCloneCmd.11"), //$NON-NLS-1$
                           Messages.getString("KeyCloneCmd.10")) //$NON-NLS-1$
    {
      public void parsed(String argument) throws OptionException
      {
        _ksType = argument;
      }
    });
    options.add(new Option(Main.KEYSTORE_OPT,
                           Messages.getString("KeyCloneCmd.9"), //$NON-NLS-1$
                           Messages.getString("KeyCloneCmd.8")) //$NON-NLS-1$
    {
      public void parsed(String argument) throws OptionException
      {
        _ksURL = argument;
      }
    });
    options.add(new Option(Main.STOREPASS_OPT,
                           Messages.getString("KeyCloneCmd.7"), //$NON-NLS-1$
                           Messages.getString("KeyCloneCmd.6")) //$NON-NLS-1$
    {
      public void parsed(String argument) throws OptionException
      {
        _ksPassword = argument;
      }
    });
    options.add(new Option(Main.PROVIDER_OPT,
                           Messages.getString("KeyCloneCmd.5"), //$NON-NLS-1$
                           Messages.getString("KeyCloneCmd.4")) //$NON-NLS-1$
    {
      public void parsed(String argument) throws OptionException
      {
        _providerClassName = argument;
      }
    });
    options.add(new Option(Main.VERBOSE_OPT,
                           Messages.getString("KeyCloneCmd.3")) //$NON-NLS-1$
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

  private void setDestinationAlias(String name) throws IOException,
      UnsupportedCallbackException
  {
    if (name == null || name.trim().length() == 0) // ask user to provide one
      {
        NameCallback ncb = new NameCallback(Messages.getString("KeyCloneCmd.26")); //$NON-NLS-1$
        getCallbackHandler().handle(new Callback[] { ncb });
        name = ncb.getName();
        if (name == null || name.trim().length() == 0)
          throw new IllegalArgumentException(Messages.getString("KeyCloneCmd.27")); //$NON-NLS-1$
      }

    destinationAlias = name.trim();
  }

  private void setNewKeyPassword(String password) throws IOException,
      UnsupportedCallbackException
  {
    if (password != null)
      newKeyPasswordChars = password.toCharArray();
    else // ask user to provide one
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
    String p = Messages.getFormattedString("KeyCloneCmd.28", //$NON-NLS-1$
                                           new String[] { destinationAlias,
                                                          String.valueOf(keyPasswordChars) });
    PasswordCallback pcb = new PasswordCallback(p, false);
    prompts[0] = pcb;
    handler.handle(prompts);
    char[] pwd1 = pcb.getPassword();
    pcb.clearPassword();
    if (pwd1 == null || pwd1.length == 0)
      {
        newKeyPasswordChars = (char[]) keyPasswordChars.clone();
        return true;
      }

    if (pwd1.length < 6)
      {
        errors[0] = new TextOutputCallback(TextOutputCallback.ERROR,
                                           Messages.getString("StorePasswdCmd.21")); //$NON-NLS-1$
        handler.handle(errors);
        return false;
      }

    newKeyPasswordChars = pwd1;
    return true;
  }
}
