/* IdentityDBCmd.java -- The identitydb command handler of the keytool
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

import java.util.logging.Logger;

/**
 * <b>NOT IMPLEMENTED YET</b>
 * <p>
 * The <b>-identitydb</b> keytool command handler is used to read the JDK 1.1.x-
 * style identity database and add its entries to the key store. If a key store
 * does not exist, it is created.
 * <p>
 * Possible options for this command are:
 * <p>
 * <dl>
 *      <dt>-file FILE_NAME</dt>
 *      <dd>The fully qualified path of the identity file to import. If this
 *      option is omitted, the tool will process STDIN.
 *      <p></dd>
 *      
 *      <dt>-storetype STORE_TYP}</dt>
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
class IdentityDBCmd extends Command
{
  private static final Logger log = Logger.getLogger(IdentityDBCmd.class.getName());
  private String _idbFileName;
  private String _ksType;
  private String _ksURL;
  private String _ksPassword;
  private String _providerClassName;

  // default 0-arguments constructor

  // public setters -----------------------------------------------------------

  /** @param pathName the fully qualified path name of the file to process. */
  public void setFile(String pathName)
  {
    this._idbFileName = pathName;
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

  int processArgs(String[] args, int i)
  {
    int limit = args.length;
    String opt;
    while (++i < limit)
      {
        opt = args[i];
        log.finest("args[" + i + "]=" + opt);
        if (opt == null || opt.length() == 0)
          continue;

        if ("-file".equals(opt)) // -file FILE_NAME
          _idbFileName = args[++i];
        else if ("-storetype".equals(opt)) // -storetype STORE_TYPE
          _ksType = args[++i];
        else if ("-keystore".equals(opt)) // -keystore URL
          _ksURL = args[++i];
        else if ("-storepass".equals(opt)) // -storepass PASSWORD
          _ksPassword = args[++i];
        else if ("-provider".equals(opt)) // -provider PROVIDER_CLASS_NAME
          _providerClassName = args[++i];
        else if ("-v".equals(opt))
          verbose = true;
        else
          break;
      }

    return i;
  }

  void setup() throws Exception
  {
    setInputStreamParam(_idbFileName);
    setKeyStoreParams(_providerClassName, _ksType, _ksPassword, _ksURL);

    log.finer("-identitydb handler will use the following options:");
    log.finer("  -file=" + _idbFileName);
    log.finer("  -storetype=" + storeType);
    log.finer("  -keystore=" + storeURL);
    log.finer("  -storepass=" + new String(storePasswordChars));
    log.finer("  -provider=" + provider);
    log.finer("  -v=" + verbose);
  }
}
