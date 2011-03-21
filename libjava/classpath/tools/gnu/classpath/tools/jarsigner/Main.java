/* Main.java -- JAR signing and verification tool not unlike jarsigner
   Copyright (C) 2006, 2007 Free Software Foundation, Inc.

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

import gnu.classpath.Configuration;
import gnu.classpath.SystemProperties;
import gnu.classpath.tools.common.CallbackUtil;
import gnu.classpath.tools.common.ClasspathToolParser;
import gnu.classpath.tools.common.ProviderUtil;
import gnu.classpath.tools.getopt.FileArgumentCallback;
import gnu.classpath.tools.getopt.Option;
import gnu.classpath.tools.getopt.OptionException;
import gnu.classpath.tools.getopt.OptionGroup;
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
import java.util.ArrayList;
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
  protected static final Logger log = Logger.getLogger(Main.class.getName());
  static final String KEYTOOL_TOOL = "jarsigner"; //$NON-NLS-1$
  private static final Locale EN_US_LOCALE = new Locale("en", "US"); //$NON-NLS-1$ //$NON-NLS-2$
  static final String DIGEST = "SHA1-Digest"; //$NON-NLS-1$
  static final String DIGEST_MANIFEST = "SHA1-Digest-Manifest"; //$NON-NLS-1$
  static final Name DIGEST_ATTR = new Name(DIGEST);
  static final Name DIGEST_MANIFEST_ATTR = new Name(DIGEST_MANIFEST);
  static final OID DSA_SIGNATURE_OID = new OID(Registry.DSA_OID_STRING);
  static final OID RSA_SIGNATURE_OID = new OID(Registry.RSA_OID_STRING);

  protected boolean verify;
  protected String ksURL;
  protected String ksType;
  protected String password;
  protected String ksPassword;
  protected String sigFileName;
  protected String signedJarFileName;
  protected boolean verbose;
  protected boolean certs;
  protected boolean internalSF;
  protected boolean sectionsOnly;
  protected String providerClassName;
  protected String jarFileName;
  protected String alias;

  protected Provider provider;
  private boolean providerInstalled;
  private char[] ksPasswordChars;
  private KeyStore store;
  private char[] passwordChars;
  private PrivateKey signerPrivateKey;
  private Certificate[] signerCertificateChain;
  /** The callback handler to use when needing to interact with user. */
  private CallbackHandler handler;
  /** The command line parser. */
  private ToolParser cmdLineParser;
  protected ArrayList<String> fileAndAlias = new ArrayList<String>();

  private Main()
  {
    super();
  }

  public static final void main(String[] args)
  {
    if (Configuration.DEBUG)
      log.entering(Main.class.getName(), "main", args); //$NON-NLS-1$
    Main tool = new Main();
    int result = 1;
    try
      {
        tool.processArgs(args);
        tool.start();
        result = 0;
      }
    catch (SecurityException x)
      {
        if (Configuration.DEBUG)
          log.throwing(Main.class.getName(), "main", x); //$NON-NLS-1$
        System.err.println(Messages.getString("Main.7") + x.getMessage()); //$NON-NLS-1$
      }
    catch (Exception x)
      {
        if (Configuration.DEBUG)
          log.throwing(Main.class.getName(), "main", x); //$NON-NLS-1$
        System.err.println(Messages.getString("Main.9") + x); //$NON-NLS-1$
      }
    finally
      {
        tool.teardown();
      }
    if (Configuration.DEBUG)
      log.exiting(Main.class.getName(), "main", Integer.valueOf(result)); //$NON-NLS-1$
    System.exit(result);
  }

  /**
   * Read the command line arguments setting the tool's parameters in
   * preparation for the user desired action.
   *
   * @param args an array of options (strings).
   * @throws Exception if an exception occurs during the process.
   */
  private void processArgs(String[] args) throws Exception
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "processArgs", args); //$NON-NLS-1$
    cmdLineParser = new ToolParser();
    cmdLineParser.initializeParser();
    cmdLineParser.parse(args, new ToolParserCallback());

    setupCommonParams();
    if (verify)
      {
        if (Configuration.DEBUG)
          {
            log.fine("Will verify with the following parameters:"); //$NON-NLS-1$
            log.fine("     jar-file = '" + jarFileName + "'"); //$NON-NLS-1$ //$NON-NLS-2$
            log.fine("Options:"); //$NON-NLS-1$
            log.fine("     provider = '" + providerClassName + "'"); //$NON-NLS-1$ //$NON-NLS-2$
            log.fine("      verbose ? " + verbose); //$NON-NLS-1$
            log.fine("        certs ? " + certs); //$NON-NLS-1$
            log.fine("   internalsf ? " + internalSF); //$NON-NLS-1$
            log.fine(" sectionsonly ? " + sectionsOnly); //$NON-NLS-1$
          }
      }
    else // sign
      {
        setupSigningParams();
        if (Configuration.DEBUG)
          {
            log.fine("Will sign with the following parameters:"); //$NON-NLS-1$
            log.fine("     jar-file = '" + jarFileName + "'"); //$NON-NLS-1$ //$NON-NLS-2$
            log.fine("        alias = '" + alias + "'"); //$NON-NLS-1$ //$NON-NLS-2$
            log.fine("Options:"); //$NON-NLS-1$
            log.fine("     keystore = '" + ksURL + "'"); //$NON-NLS-1$ //$NON-NLS-2$
            log.fine("    storetype = '" + ksType + "'"); //$NON-NLS-1$ //$NON-NLS-2$
            log.fine("    storepass = '" + ksPassword + "'"); //$NON-NLS-1$ //$NON-NLS-2$
            log.fine("      keypass = '" + password + "'"); //$NON-NLS-1$ //$NON-NLS-2$
            log.fine("      sigfile = '" + sigFileName + "'"); //$NON-NLS-1$ //$NON-NLS-2$
            log.fine("    signedjar = '" + signedJarFileName + "'"); //$NON-NLS-1$ //$NON-NLS-2$
            log.fine("     provider = '" + providerClassName + "'"); //$NON-NLS-1$ //$NON-NLS-2$
            log.fine("      verbose ? " + verbose); //$NON-NLS-1$
            log.fine("   internalsf ? " + internalSF); //$NON-NLS-1$
            log.fine(" sectionsonly ? " + sectionsOnly); //$NON-NLS-1$
          }
      }
    if (Configuration.DEBUG)
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
    if (Configuration.DEBUG)
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
    if (Configuration.DEBUG)
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
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "teardown"); //$NON-NLS-1$
    if (providerInstalled)
      ProviderUtil.removeProvider(provider.getName());

    if (Configuration.DEBUG)
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
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "setupCommonParams"); //$NON-NLS-1$
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
          {
            if (Configuration.DEBUG)
              log.finer("Provider " + providerName + " is already installed"); //$NON-NLS-1$ //$NON-NLS-2$
          }
        else // install it
          installNewProvider();
      }

    if (! verbose && certs)
      {
        if (Configuration.DEBUG)
          log.fine("Option <certs> is set but <verbose> is not. Ignored"); //$NON-NLS-1$
        certs = false;
      }

    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "setupCommonParams"); //$NON-NLS-1$
  }

  /**
   * Install the user defined security provider in the underlying JVM.
   * <p>
   * Also record this fact so we can remove it when we exit the tool.
   */
  private void installNewProvider()
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "installNewProvider"); //$NON-NLS-1$
    providerInstalled = ProviderUtil.addProvider(provider) != -1;
    if (Configuration.DEBUG)
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
    if (Configuration.DEBUG)
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
    if (Configuration.DEBUG)
      log.fine(String.valueOf(signerCertificateChain));

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

    if (Configuration.DEBUG)
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

  private class ToolParserCallback
      extends FileArgumentCallback
  {
    public void notifyFile(String fileArgument)
    {
      fileAndAlias.add(fileArgument);
    }
  }

  private class ToolParser
      extends ClasspathToolParser
  {
    public ToolParser()
    {
      super(KEYTOOL_TOOL, true);
    }

    protected void validate() throws OptionException
    {
      if (fileAndAlias.size() < 1)
        throw new OptionException(Messages.getString("Main.133")); //$NON-NLS-1$

      jarFileName = (String) fileAndAlias.get(0);
      if (! verify) // must have an ALIAS. use "mykey" if undefined
        if (fileAndAlias.size() < 2)
          {
            if (Configuration.DEBUG)
              log.fine("Missing ALIAS argument. Will use [mykey] instead"); //$NON-NLS-1$
            alias = "mykey"; //$NON-NLS-1$
          }
        else
          alias = fileAndAlias.get(1);
    }

    public void initializeParser()
    {
      setHeader(Messages.getString("Main.2")); //$NON-NLS-1$
      setFooter(Messages.getString("Main.1")); //$NON-NLS-1$
      OptionGroup signGroup = new OptionGroup(Messages.getString("Main.0")); //$NON-NLS-1$
      signGroup.add(new Option("keystore", //$NON-NLS-1$
                               Messages.getString("Main.101"), //$NON-NLS-1$
                               Messages.getString("Main.102")) //$NON-NLS-1$
      {
        public void parsed(String argument) throws OptionException
        {
          ksURL = argument;
        }
      });
      signGroup.add(new Option("storetype", //$NON-NLS-1$
                               Messages.getString("Main.104"), //$NON-NLS-1$
                               Messages.getString("Main.105")) //$NON-NLS-1$
      {
        public void parsed(String argument) throws OptionException
        {
          ksType = argument;
        }
      });
      signGroup.add(new Option("storepass", //$NON-NLS-1$
                               Messages.getString("Main.107"), //$NON-NLS-1$
                               Messages.getString("Main.108")) //$NON-NLS-1$
      {
        public void parsed(String argument) throws OptionException
        {
          ksPassword = argument;
        }
      });
      signGroup.add(new Option("keypass", //$NON-NLS-1$
                               Messages.getString("Main.110"), //$NON-NLS-1$
                               Messages.getString("Main.111")) //$NON-NLS-1$
      {
        public void parsed(String argument) throws OptionException
        {
          password = argument;
        }
      });
      signGroup.add(new Option("sigfile", //$NON-NLS-1$
                               Messages.getString("Main.113"), //$NON-NLS-1$
                               Messages.getString("Main.114")) //$NON-NLS-1$
      {
        public void parsed(String argument) throws OptionException
        {
          sigFileName = argument;
        }
      });
      signGroup.add(new Option("signedjar", //$NON-NLS-1$
                               Messages.getString("Main.116"), //$NON-NLS-1$
                               Messages.getString("Main.117")) //$NON-NLS-1$
      {
        public void parsed(String argument) throws OptionException
        {
          signedJarFileName = argument;
        }
      });
      add(signGroup);

      OptionGroup verifyGroup = new OptionGroup(Messages.getString("Main.118")); //$NON-NLS-1$
      verifyGroup.add(new Option("verify", //$NON-NLS-1$
                                 Messages.getString("Main.120")) //$NON-NLS-1$
      {
        public void parsed(String argument) throws OptionException
        {
          verify = true;
        }
      });
      verifyGroup.add(new Option("certs", //$NON-NLS-1$
                                 Messages.getString("Main.122")) //$NON-NLS-1$
      {
        public void parsed(String argument) throws OptionException
        {
          certs = true;
        }
      });
      add(verifyGroup);

      OptionGroup commonGroup = new OptionGroup(Messages.getString("Main.123")); //$NON-NLS-1$
      commonGroup.add(new Option("verbose", //$NON-NLS-1$
                                 Messages.getString("Main.125")) //$NON-NLS-1$
      {
        public void parsed(String argument) throws OptionException
        {
          verbose = true;
        }
      });
      commonGroup.add(new Option("internalsf", //$NON-NLS-1$
                                 Messages.getString("Main.127")) //$NON-NLS-1$
      {
        public void parsed(String argument) throws OptionException
        {
          internalSF = true;
        }
      });
      commonGroup.add(new Option("sectionsonly", //$NON-NLS-1$
                                 Messages.getString("Main.129")) //$NON-NLS-1$
      {
        public void parsed(String argument) throws OptionException
        {
          sectionsOnly = true;
        }
      });
      commonGroup.add(new Option("provider", //$NON-NLS-1$
                                 Messages.getString("Main.131"), //$NON-NLS-1$
                                 Messages.getString("Main.132")) //$NON-NLS-1$
      {
        public void parsed(String argument) throws OptionException
        {
          providerClassName = argument;
        }
      });
      add(commonGroup);
    }
  }
}
