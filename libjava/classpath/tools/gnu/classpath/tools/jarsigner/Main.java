/* Main.java -- JAR signing and verification tool not unlike jarsigner
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


package gnu.classpath.tools.jarsigner;

import gnu.classpath.SystemProperties;
import gnu.classpath.tools.HelpPrinter;
import gnu.classpath.tools.common.CallbackUtil;
import gnu.classpath.tools.common.ProviderUtil;
import gnu.java.security.OID;
import gnu.java.security.Registry;
import gnu.javax.security.auth.callback.ConsoleCallbackHandler;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.security.Key;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.Provider;
import java.security.Security;
import java.security.UnrecoverableKeyException;
import java.security.cert.Certificate;
import java.security.cert.CertificateException;
import java.util.Locale;
import java.util.jar.Attributes.Name;
import java.util.logging.Logger;

import javax.security.auth.callback.Callback;
import javax.security.auth.callback.CallbackHandler;
import javax.security.auth.callback.PasswordCallback;
import javax.security.auth.callback.UnsupportedCallbackException;

/**
 * The GNU Classpath implementation of the <i>jarsigner</i> tool.
 * <p>
 * The <i>jarsigner</i> tool is used to sign and verify JAR (Java ARchive)
 * files.
 * <p>
 * This implementation is intended to be compatible with the behaviour
 * described in the public documentation of the same tool included in JDK 1.4.
 */
public class Main
{
  private static final Logger log = Logger.getLogger(Main.class.getName());
  private static final String HELP_PATH = "jarsigner/jarsigner.txt"; //$NON-NLS-1$
  private static final Locale EN_US_LOCALE = new Locale("en", "US"); //$NON-NLS-1$ //$NON-NLS-2$
  static final String DIGEST = "SHA1-Digest"; //$NON-NLS-1$
  static final String DIGEST_MANIFEST = "SHA1-Digest-Manifest"; //$NON-NLS-1$
  static final Name DIGEST_ATTR = new Name(DIGEST);
  static final Name DIGEST_MANIFEST_ATTR = new Name(DIGEST_MANIFEST);
  static final OID DSA_SIGNATURE_OID = new OID(Registry.DSA_OID_STRING);
  static final OID RSA_SIGNATURE_OID = new OID(Registry.RSA_OID_STRING);

  private boolean verify;
  private String ksURL;
  private String ksType;
  private String password;
  private String ksPassword;
  private String sigFileName;
  private String signedJarFileName;
  private boolean verbose;
  private boolean certs;
  private boolean internalSF;
  private boolean sectionsOnly;
  private String providerClassName;
  private String jarFileName;
  private String alias;

  protected Provider provider;
  private boolean providerInstalled;
  private char[] ksPasswordChars;
  private KeyStore store;
  private char[] passwordChars;
  private PrivateKey signerPrivateKey;
  private Certificate[] signerCertificateChain;
  /** The callback handler to use when needing to interact with user. */
  private CallbackHandler handler;

  private Main()
  {
    super();
  }

  public static final void main(String[] args)
  {
    log.entering(Main.class.getName(), "main", args); //$NON-NLS-1$

    Main tool = new Main();
    try
      {
        tool.processArgs(args);
        tool.start();
      }
    catch (SecurityException x)
      {
        log.throwing(Main.class.getName(), "main", x); //$NON-NLS-1$
        System.err.println(Messages.getString("Main.7") + x.getMessage()); //$NON-NLS-1$
      }
    catch (Exception x)
      {
        log.throwing(Main.class.getName(), "main", x); //$NON-NLS-1$
        System.err.println(Messages.getString("Main.9") + x); //$NON-NLS-1$
      }

    tool.teardown();

    log.exiting(Main.class.getName(), "main"); //$NON-NLS-1$
    // System.exit(0);
  }

  // helper methods -----------------------------------------------------------

  /**
   * Read the command line arguments setting the tool's parameters in
   * preparation for the user desired action.
   * 
   * @param args an array of options (strings).
   * @throws Exception if an exceptio occurs during the process.
   */
  private void processArgs(String[] args) throws Exception
  {
    log.entering(this.getClass().getName(), "processArgs", args); //$NON-NLS-1$

    HelpPrinter.checkHelpKey(args, HELP_PATH);
    if (args == null || args.length == 0)
      HelpPrinter.printHelpAndExit(HELP_PATH);

    int limit = args.length;
    log.finest("args.length=" + limit); //$NON-NLS-1$
    int i = 0;
    String opt;
    while (i < limit)
      {
        opt = args[i++];
        log.finest("args[" + (i - 1) + "]=" + opt); //$NON-NLS-1$ //$NON-NLS-2$
        if (opt == null || opt.length() == 0)
          continue;

        if ("-verify".equals(opt)) // -verify //$NON-NLS-1$
          verify = true;
        else if ("-keystore".equals(opt)) // -keystore URL //$NON-NLS-1$
          ksURL = args[i++];
        else if ("-storetype".equals(opt)) // -storetype STORE_TYPE //$NON-NLS-1$
          ksType = args[i++];
        else if ("-storepass".equals(opt)) // -storepass PASSWORD //$NON-NLS-1$
          ksPassword = args[i++];
        else if ("-keypass".equals(opt)) // -keypass PASSWORD //$NON-NLS-1$
          password = args[i++];
        else if ("-sigfile".equals(opt)) // -sigfile NAME //$NON-NLS-1$
          sigFileName = args[i++];
        else if ("-signedjar".equals(opt)) // -signedjar FILE_NAME //$NON-NLS-1$
          signedJarFileName = args[i++];
        else if ("-verbose".equals(opt)) // -verbose //$NON-NLS-1$
          verbose = true;
        else if ("-certs".equals(opt)) // -certs //$NON-NLS-1$
          certs = true;
        else if ("-internalsf".equals(opt)) // -internalsf //$NON-NLS-1$
          internalSF = true;
        else if ("-sectionsonly".equals(opt)) // -sectionsonly //$NON-NLS-1$
          sectionsOnly = true;
        else if ("-provider".equals(opt)) // -provider PROVIDER_CLASS_NAME //$NON-NLS-1$
          providerClassName = args[i++];
        else
          {
            jarFileName = opt;
            if (! verify)
              alias = args[i++];

            break;
          }
      }

    if (i < limit) // more options than needed
      log.fine("Last argument is assumed at index #" + (i - 1) //$NON-NLS-1$
               + ". Remaining arguments (" + args[i] //$NON-NLS-1$
               + "...) will be ignored"); //$NON-NLS-1$

    setupCommonParams();
    if (verify)
      {
        log.finer("Will verify with the following parameters:"); //$NON-NLS-1$
        log.finer("     jar-file = '" + jarFileName + "'"); //$NON-NLS-1$ //$NON-NLS-2$
        log.finer("Options:"); //$NON-NLS-1$
        log.finer("     provider = '" + providerClassName + "'"); //$NON-NLS-1$ //$NON-NLS-2$
        log.finer("      verbose ? " + verbose); //$NON-NLS-1$
        log.finer("        certs ? " + certs); //$NON-NLS-1$
        log.finer("   internalsf ? " + internalSF); //$NON-NLS-1$
        log.finer(" sectionsonly ? " + sectionsOnly); //$NON-NLS-1$
      }
    else // sign
      {
        setupSigningParams();

        log.finer("Will sign with the following parameters:"); //$NON-NLS-1$
        log.finer("     jar-file = '" + jarFileName + "'"); //$NON-NLS-1$ //$NON-NLS-2$
        log.finer("        alias = '" + alias + "'"); //$NON-NLS-1$ //$NON-NLS-2$
        log.finer("Options:"); //$NON-NLS-1$
        log.finer("     keystore = '" + ksURL + "'"); //$NON-NLS-1$ //$NON-NLS-2$
        log.finer("    storetype = '" + ksType + "'"); //$NON-NLS-1$ //$NON-NLS-2$
        log.finer("    storepass = '" + ksPassword + "'"); //$NON-NLS-1$ //$NON-NLS-2$
        log.finer("      keypass = '" + password + "'"); //$NON-NLS-1$ //$NON-NLS-2$
        log.finer("      sigfile = '" + sigFileName + "'"); //$NON-NLS-1$ //$NON-NLS-2$
        log.finer("    signedjar = '" + signedJarFileName + "'"); //$NON-NLS-1$ //$NON-NLS-2$
        log.finer("     provider = '" + providerClassName + "'"); //$NON-NLS-1$ //$NON-NLS-2$
        log.finer("      verbose ? " + verbose); //$NON-NLS-1$
        log.finer("   internalsf ? " + internalSF); //$NON-NLS-1$
        log.finer(" sectionsonly ? " + sectionsOnly); //$NON-NLS-1$
      }

    log.exiting(this.getClass().getName(), "processArgs"); //$NON-NLS-1$
  }

  /**
   * Invokes the <code>start()</code> method of the concrete handler.
   * <p>
   * Depending on the result of processing the command line arguments, this
   * handler may be one for signing the jar, or verifying it.
   * 
   * @throws Exception if an exception occurs during the process.
   */
  private void start() throws Exception
  {
    log.entering(this.getClass().getName(), "start"); //$NON-NLS-1$

    if (verify)
      {
        JarVerifier jv = new JarVerifier(this);
        jv.start();
      }
    else
      {
        JarSigner js = new JarSigner(this);
        js.start();
      }

    log.exiting(this.getClass().getName(), "start"); //$NON-NLS-1$
  }

  /**
   * Ensures that the underlying JVM is left in the same state as we found it
   * when we first launched the tool. Specifically, if we have installed a new
   * security provider then now is the time to remove it.
   * <p>
   * Note (rsn): this may not be necessary if we terminate the JVM; i.e. call
   * {@link System#exit(int)} at the end of the tool's invocation. Nevertheless
   * it's good practive to return the JVM to its initial state.
   */
  private void teardown()
  {
    log.entering(this.getClass().getName(), "teardown"); //$NON-NLS-1$

    if (providerInstalled)
      ProviderUtil.removeProvider(provider.getName());

    log.exiting(this.getClass().getName(), "teardown"); //$NON-NLS-1$
  }

  /**
   * After processing the command line arguments, this method is invoked to
   * process the common parameters which may have been encountered among the
   * actual arguments.
   * <p>
   * Common parameters are those which are allowed in both signing and
   * verification modes.
   * 
   * @throws InstantiationException if a security provider class name is
   *           specified but that class name is that of either an interface or
   *           an abstract class.
   * @throws IllegalAccessException if a security provider class name is
   *           specified but no 0-arguments constructor is defined for that
   *           class.
   * @throws ClassNotFoundException if a security provider class name is
   *           specified but no such class was found in the classpath.
   * @throws IOException if the JAR file name for signing, or verifying, does
   *           not exist, exists but denotes a directory, or is not readable.
   */
  private void setupCommonParams() throws InstantiationException,
      IllegalAccessException, ClassNotFoundException, IOException
  {
    log.entering(this.getClass().getName(), "setupCommonParams"); //$NON-NLS-1$

    if (jarFileName == null)
      HelpPrinter.printHelpAndExit(HELP_PATH);

    File jar = new File(jarFileName);
    if (! jar.exists())
      throw new FileNotFoundException(jarFileName);

    if (jar.isDirectory())
      throw new IOException(Messages.getFormattedString("Main.70", jarFileName)); //$NON-NLS-1$

    if (! jar.canRead())
      throw new IOException(Messages.getFormattedString("Main.72", jarFileName)); //$NON-NLS-1$ //$NON-NLS-2$

    if (providerClassName != null && providerClassName.length() > 0)
      {
        provider = (Provider) Class.forName(providerClassName).newInstance();
        // is it already installed?
        String providerName = provider.getName();
        Provider installedProvider = Security.getProvider(providerName);
        if (installedProvider != null)
          log.finer("Provider " + providerName + " is already installed"); //$NON-NLS-1$ //$NON-NLS-2$
        else // install it
          installNewProvider();
      }

    if (! verbose && certs)
      {
        log.fine("Option <certs> is set but <verbose> is not. Ignored"); //$NON-NLS-1$
        certs = false;
      }

    log.exiting(this.getClass().getName(), "setupCommonParams"); //$NON-NLS-1$
  }

  /**
   * Install the user defined security provider in the underlying JVM.
   * <p>
   * Also record this fact so we can remove it when we exit the tool.
   */
  private void installNewProvider()
  {
    log.entering(this.getClass().getName(), "installNewProvider"); //$NON-NLS-1$

    providerInstalled = ProviderUtil.addProvider(provider) != -1;

    log.exiting(this.getClass().getName(), "installNewProvider"); //$NON-NLS-1$
  }

  /**
   * After processing the command line arguments, this method is invoked to
   * process the parameters which may have been encountered among the actual
   * arguments, and which are specific to the signing action of the tool.
   * 
   * @throws KeyStoreException if no implementation of the designated (or
   *           default type) of a key store is availabe.
   * @throws IOException if an I/O related exception occurs during the process.
   * @throws NoSuchAlgorithmException if an implementation of an algorithm used
   *           by the key store is not available.
   * @throws CertificateException if an exception occurs while reading a
   *           certificate from the key store.
   * @throws UnsupportedCallbackException if no implementation of a password
   *           callback is available.
   * @throws UnrecoverableKeyException if the wrong password was used to unlock
   *           the key store.
   * @throws SecurityException if the designated alias is not known to the key
   *           store or is not an Alias of a Key Entry.
   */
  private void setupSigningParams() throws KeyStoreException, IOException,
      NoSuchAlgorithmException, CertificateException,
      UnsupportedCallbackException, UnrecoverableKeyException
  {
    log.entering(this.getClass().getName(), "setupSigningParams"); //$NON-NLS-1$

    if (ksURL == null || ksURL.trim().length() == 0)
      {
        String userHome = SystemProperties.getProperty("user.home"); //$NON-NLS-1$
        if (userHome == null || userHome.trim().length() == 0)
          throw new SecurityException(Messages.getString("Main.85")); //$NON-NLS-1$

        ksURL = "file:" + userHome.trim() + "/.keystore"; //$NON-NLS-1$ //$NON-NLS-2$
      }
    else
      {
        ksURL = ksURL.trim();
        if (ksURL.indexOf(":") == -1) //$NON-NLS-1$
          ksURL = "file:" + ksURL; //$NON-NLS-1$
      }

    if (ksType == null || ksType.trim().length() == 0)
      ksType = KeyStore.getDefaultType();
    else
      ksType = ksType.trim();

    store = KeyStore.getInstance(ksType);

    if (ksPassword == null)
      {
        // ask the user to provide one
        PasswordCallback pcb = new PasswordCallback(Messages.getString("Main.92"), //$NON-NLS-1$
                                                    false);
        getCallbackHandler().handle(new Callback[] { pcb });
        ksPasswordChars = pcb.getPassword();
      }
    else
      ksPasswordChars = ksPassword.toCharArray();

    URL url = new URL(ksURL);
    InputStream stream = url.openStream();
    store.load(stream, ksPasswordChars);

    if (alias == null)
      HelpPrinter.printHelpAndExit(HELP_PATH);

    if (! store.containsAlias(alias))
      throw new SecurityException(Messages.getFormattedString("Main.6", alias)); //$NON-NLS-1$

    if (! store.isKeyEntry(alias))
      throw new SecurityException(Messages.getFormattedString("Main.95", alias)); //$NON-NLS-1$

    Key key;
    if (password == null)
      {
        passwordChars = ksPasswordChars;
        try
          {
            key = store.getKey(alias, passwordChars);
          }
        catch (UnrecoverableKeyException x)
          {
            // ask the user to provide one
            String prompt = Messages.getFormattedString("Main.97", alias); //$NON-NLS-1$
            PasswordCallback pcb = new PasswordCallback(prompt, false);
            getCallbackHandler().handle(new Callback[] { pcb });
            passwordChars = pcb.getPassword();
            // take 2
            key = store.getKey(alias, passwordChars);
          }
      }
    else
      {
        passwordChars = password.toCharArray();
        key = store.getKey(alias, passwordChars);
      }

    if (! (key instanceof PrivateKey))
      throw new SecurityException(Messages.getFormattedString("Main.99", alias)); //$NON-NLS-1$

    signerPrivateKey = (PrivateKey) key;
    signerCertificateChain = store.getCertificateChain(alias);
    log.finest(String.valueOf(signerCertificateChain));

    if (sigFileName == null)
      sigFileName = alias;

    sigFileName = sigFileName.toUpperCase(EN_US_LOCALE);
    if (sigFileName.length() > 8)
      sigFileName = sigFileName.substring(0, 8);

    char[] chars = sigFileName.toCharArray();
    for (int i = 0; i < chars.length; i++)
      {
        char c = chars[i];
        if (! (Character.isLetter(c)
            || Character.isDigit(c)
            || c == '_'
            || c == '-'))
          chars[i] = '_';
      }

    sigFileName = new String(chars);

    if (signedJarFileName == null)
      signedJarFileName = jarFileName;

    log.exiting(this.getClass().getName(), "setupSigningParams"); //$NON-NLS-1$
  }

  boolean isVerbose()
  {
    return verbose;
  }

  boolean isCerts()
  {
    return certs;
  }

  String getSigFileName()
  {
    return this.sigFileName;
  }

  String getJarFileName()
  {
    return this.jarFileName;
  }

  boolean isSectionsOnly()
  {
    return this.sectionsOnly;
  }

  boolean isInternalSF()
  {
    return this.internalSF;
  }

  PrivateKey getSignerPrivateKey()
  {
    return this.signerPrivateKey;
  }

  Certificate[] getSignerCertificateChain()
  {
    return signerCertificateChain;
  }

  String getSignedJarFileName()
  {
    return this.signedJarFileName;
  }

  /**
   * Return a CallbackHandler which uses the Console (System.in and System.out)
   * for interacting with the user.
   * <p>
   * This method first finds all currently installed security providers capable
   * of providing such service and then in turn attempts to instantiate the
   * handler from those providers. As soon as one provider returns a non-null
   * instance of the callback handler, the search stops and that instance is
   * set to be used from now on.
   * <p>
   * If no installed providers were found, this method falls back on the GNU
   * provider, by-passing the Security search mechanism. The default console
   * callback handler implementation is {@link ConsoleCallbackHandler}.
   * 
   * @return a console-based {@link CallbackHandler}.
   */
  protected CallbackHandler getCallbackHandler()
  {
    if (handler == null)
      handler = CallbackUtil.getConsoleHandler();

    return handler;
  }
}
