/* Command.java -- Abstract implementation of a keytool command handler
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
import gnu.classpath.tools.common.CallbackUtil;
import gnu.classpath.tools.common.ProviderUtil;
import gnu.classpath.tools.common.SecurityProviderInfo;
import gnu.classpath.tools.getopt.Parser;
import gnu.java.security.OID;
import gnu.java.security.Registry;
import gnu.java.security.der.BitString;
import gnu.java.security.der.DER;
import gnu.java.security.der.DERReader;
import gnu.java.security.der.DERValue;
import gnu.java.security.der.DERWriter;
import gnu.java.security.hash.IMessageDigest;
import gnu.java.security.hash.MD5;
import gnu.java.security.hash.Sha160;
import gnu.java.security.util.Util;
import gnu.java.security.x509.X500DistinguishedName;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.math.BigInteger;
import java.net.URL;
import java.net.URLConnection;
import java.security.InvalidKeyException;
import java.security.InvalidParameterException;
import java.security.Key;
import java.security.KeyPairGenerator;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.Provider;
import java.security.PublicKey;
import java.security.Signature;
import java.security.SignatureException;
import java.security.UnrecoverableKeyException;
import java.security.cert.Certificate;
import java.security.cert.CertificateEncodingException;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.security.interfaces.DSAKey;
import java.security.interfaces.RSAKey;
import java.util.ArrayList;
import java.util.Date;
import java.util.logging.Logger;
import java.util.prefs.Preferences;

import javax.security.auth.callback.Callback;
import javax.security.auth.callback.CallbackHandler;
import javax.security.auth.callback.NameCallback;
import javax.security.auth.callback.PasswordCallback;
import javax.security.auth.callback.UnsupportedCallbackException;

/**
 * A base class of the keytool command to facilitate implementation of concrete
 * keytool Handlers.
 */
abstract class Command
{
  // Fields and constants -----------------------------------------------------

  private static final Logger log = Logger.getLogger(Command.class.getName());
  /** Default value for the ALIAS argument. */
  private static final String DEFAULT_ALIAS = "mykey"; //$NON-NLS-1$
  /** Default algorithm for key-pair generation. */
  private static final String DEFAULT_KEY_ALGORITHM = "DSA"; //$NON-NLS-1$
  /** Default DSA digital signature algorithm to use with DSA keys. */
  private static final String DSA_SIGNATURE_ALGORITHM = "SHA1withDSA"; //$NON-NLS-1$
  /** Default RSA digital signature algorithm to use with RSA keys. */
  private static final String RSA_SIGNATURE_ALGORITHM = "MD5withRSA"; //$NON-NLS-1$
  /** Default validity (in days) of newly generated certificates. */
  private static final int DEFAULT_VALIDITY = 90;
  /** OID of SHA1withDSA signature algorithm as stated in RFC-2459. */
  protected static final OID SHA1_WITH_DSA = new OID("1.2.840.10040.4.3"); //$NON-NLS-1$
  /** OID of MD2withRSA signature algorithm as stated in RFC-2459. */
  private static final OID MD2_WITH_RSA = new OID("1.2.840.113549.1.1.2"); //$NON-NLS-1$
  /** OID of MD5withRSA signature algorithm as stated in RFC-2459. */
  private static final OID MD5_WITH_RSA = new OID("1.2.840.113549.1.1.4"); //$NON-NLS-1$
  /** OID of SHA1withRSA signature algorithm as stated in RFC-2459. */
  private static final OID SHA1_WITH_RSA = new OID("1.2.840.113549.1.1.5"); //$NON-NLS-1$
  /** Number of milliseconds in one day. */
  private static final long MILLIS_IN_A_DAY = 24 * 60 * 60 * 1000L;

  /** The Alias to use. */
  protected String alias;
  /** The password characters protecting a Key Entry. */
  protected char[] keyPasswordChars;
  /** A security provider to add. */
  protected Provider provider;
  /** The key store type. */
  protected String storeType;
  /** The password characters protecting the key store. */
  protected char[] storePasswordChars;
  /** The key store URL. */
  protected URL storeURL;
  /** The input stream from the key store URL. */
  protected InputStream storeStream;
  /** The key store instance to use. */
  protected KeyStore store;
  /** The output stream the concrete handler will use. */
  protected OutputStream outStream;
  /** Whether we are printing to System.out. */
  protected boolean systemOut;
  /** The key-pair generation algorithm instance to use. */
  protected KeyPairGenerator keyPairGenerator;
  /** The digital signature algorithm instance to use. */
  protected Signature signatureAlgorithm;
  /** Validity period, in number of days, to use when generating certificates. */
  protected int validityInDays;
  /** The input stream the concrete handler will use. */
  protected InputStream inStream;
  /** Whether verbose output is required or not. */
  protected boolean verbose;

  /** MD5 hash to use when generating certificate fingerprints. */
  private IMessageDigest md5 = new MD5();
  /** SHA1 hash to use when generating certificate fingerprints. */
  private IMessageDigest sha = new Sha160();
  /** The new position of a user-defined provider if it is not already installed. */
  private int providerNdx = -2;
  /** The callback handler to use when needing to interact with user. */
  private CallbackHandler handler;
  /** The shutdown hook. */
  private ShutdownHook shutdownThread;

  // Constructor(s) -----------------------------------------------------------

  protected Command()
  {
    super();
    shutdownThread = new ShutdownHook();
    Runtime.getRuntime().addShutdownHook(shutdownThread);
  }

  // Methods ------------------------------------------------------------------

  /**
   * A public method to allow using any keytool command handler programmatically
   * by using a JavaBeans style of parameter(s) initialization. The user is
   * assumed to have set individually the required options through their
   * respective setters before invoking this method.
   * <p>
   * If an exception is encountered during the processing of the command, this
   * implementation attempts to release any resources that may have been
   * allocated at the time the exception occurs, before re-throwing that
   * exception.
   * 
   * @throws Exception if an exception occurs during the processing of this
   *           command. For a more comprehensive list of exceptions that may
   *           occur, see the documentation of the {@link #setup()} and
   *           {@link #start()} methods.
   */
  public void doCommand() throws Exception
  {
    try
      {
        setup();
        start();
      }
    finally
      {
        teardown();
        if (shutdownThread != null)
          Runtime.getRuntime().removeShutdownHook(shutdownThread);
      }
  }

  /**
   * @param flag whether to use, or not, more verbose output while processing
   *          the command.
   */
  public void setVerbose(String flag)
  {
    this.verbose = Boolean.valueOf(flag).booleanValue();
  }

  // life-cycle methods -------------------------------------------------------

  /**
   * Given a potential sub-array of options for this concrete handler, starting
   * at position <code>startIndex + 1</code>, potentially followed by other
   * commands and their options, this method sets up this concrete command
   * handler with its own options and returns the index of the first unprocessed
   * argument in the array.
   * <p>
   * The general contract of this method is that it is invoked with the
   * <code>startIndex</code> argument pointing to the keyword argument that
   * uniquelly identifies the command itself; e.g. <code>-genkey</code> or
   * <code>-list</code>, etc...
   * 
   * @param args an array of options for this handler and possibly other
   *          commands and their options.
   * @return the remaining un-processed <code>args</code>.
   */
  String[] processArgs(String[] args)
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "processArgs", args); //$NON-NLS-1$
    Parser cmdOptionsParser = getParser();
    String[] result = cmdOptionsParser.parse(args);
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "processArgs", result); //$NON-NLS-1$
    return result;
  }

  /**
   * Initialize this concrete command handler for later invocation of the
   * {@link #start()} or {@link #doCommand()} methods.
   * <p>
   * Handlers usually initialize their local variables and resources within the
   * scope of this call.
   * 
   * @throws IOException if an I/O related exception, such as opening an input
   *           stream, occurs during the execution of this method.
   * @throws UnsupportedCallbackException if a requested callback handler
   *           implementation was not found, or was found but encountered an
   *           exception during its processing.
   * @throws ClassNotFoundException if a designated security provider class was
   *           not found.
   * @throws IllegalAccessException no 0-arguments constructor for the
   *           designated security provider class was found.
   * @throws InstantiationException the designated security provider class is
   *           not instantiable.
   * @throws KeyStoreException if an exception occurs during the instantiation
   *           of the KeyStore.
   * @throws CertificateException if a certificate related exception, such as
   *           expiry, occurs during the loading of the KeyStore.
   * @throws NoSuchAlgorithmException if no current security provider can
   *           provide a needed algorithm referenced by the KeyStore or one of
   *           its Key Entries or Certificates.
   */
  abstract void setup() throws Exception;

  /**
   * Do the real work this handler is supposed to do.
   * <p>
   * The code in this (abstract) class throws a <i>Not implemented yet</i>
   * runtime exception. Concrete implementations MUST override this method.
   * 
   * @throws CertificateException If no concrete implementation was found for a
   *           certificate Factory of a designated type. In this tool, the type
   *           is usually X.509 v1.
   * @throws KeyStoreException if a keys-store related exception occurs; e.g.
   *           the key store has not been initialized.
   * @throws IOException if an I/O related exception occurs during the process.
   * @throws SignatureException if a digital signature related exception occurs.
   * @throws InvalidKeyException if the genereated keys are invalid.
   * @throws UnrecoverableKeyException if the password used to unlock a key in
   *           the key store was invalid.
   * @throws NoSuchAlgorithmException if a concrete implementation of an
   *           algorithm used to store a Key Entry was not found at runtime.
   * @throws UnsupportedCallbackException if a requested callback handler
   *           implementation was not found, or was found but encountered an
   *           exception during its processing.
   */
  void start() throws Exception
  {
    throw new RuntimeException("Not implemented yet"); //$NON-NLS-1$
  }

  /**
   * Tear down the handler, releasing any resources which may have been
   * allocated at setup time.
   */
  void teardown()
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "teardown"); //$NON-NLS-1$
    if (storeStream != null)
      try
        {
          storeStream.close();
        }
      catch (IOException ignored)
        {
          if (Configuration.DEBUG)
            log.fine("Exception while closing key store URL stream. Ignored: " //$NON-NLS-1$
                     + ignored);
        }

    if (outStream != null)
      {
        try
          {
            outStream.flush();
          }
        catch (IOException ignored)
          {
          }

        if (! systemOut)
          try
            {
              outStream.close();
            }
          catch (IOException ignored)
            {
            }
      }

    if (inStream != null)
      try
        {
          inStream.close();
        }
      catch (IOException ignored)
        {
        }

    if (providerNdx > 0)
      ProviderUtil.removeProvider(provider.getName());

    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "teardown"); //$NON-NLS-1$
  }

  // parameter setup and validation methods -----------------------------------

  /**
   * @return a {@link Parser} that knows how to parse the concrete command's
   *         options.
   */
  abstract Parser getParser();

  /**
   * Convenience method to setup the key store given its type, its password, its
   * location and portentially a specialized security provider.
   * <p>
   * Calls the method with the same name and 5 arguments passing
   * <code>false</code> to the first argument implying that no attempt to
   * create the keystore will be made if one was not found at the designated
   * location.
   * 
   * @param className the potentially null fully qualified class name of a
   *          security provider to add at runtime, if no installed provider is
   *          able to provide a key store implementation of the desired type.
   * @param type the potentially null type of the key store to request from the
   *          key store factory.
   * @param password the potentially null password protecting the key store.
   * @param url the URL of the key store.
   */
  protected void setKeyStoreParams(String className, String type,
                                   String password, String url)
      throws IOException, UnsupportedCallbackException, KeyStoreException,
      NoSuchAlgorithmException, CertificateException
  {
    setKeyStoreParams(false, className, type, password, url);
  }

  /**
   * Convenience method to setup the key store given its type, its password, its
   * location and portentially a specialized security provider.
   * 
   * @param createIfNotFound if <code>true</code> then create the keystore if
   *          it was not found; otherwise do not.
   * @param className the potentially null fully qualified class name of a
   *          security provider to add at runtime, if no installed provider is
   *          able to provide a key store implementation of the desired type.
   * @param type the potentially null type of the key store to request from the
   *          key store factory.
   * @param password the potentially null password protecting the key store.
   * @param url the URL of the key store.
   */
  protected void setKeyStoreParams(boolean createIfNotFound, String className,
                                   String type, String password, String url)
      throws IOException, UnsupportedCallbackException, KeyStoreException,
      NoSuchAlgorithmException, CertificateException
  {
    setProviderClassNameParam(className);
    setKeystoreTypeParam(type);
    setKeystoreURLParam(createIfNotFound, url, password);
  }

  /**
   * Set a security provider class name to (install and) use for key store
   * related operations.
   * 
   * @param className the possibly null, fully qualified class name of a
   *          security provider to add, if it is not already installed, to the
   *          set of available providers.
   */
  private void setProviderClassNameParam(String className)
  {
    if (Configuration.DEBUG)
      log.fine("setProviderClassNameParam(" + className + ")"); //$NON-NLS-1$ //$NON-NLS-2$
    if (className != null && className.trim().length() > 0)
      {
        className = className.trim();
        SecurityProviderInfo spi = ProviderUtil.addProvider(className);
        provider = spi.getProvider();
        if (provider == null)
          {
            if (Configuration.DEBUG)
              log.fine("Was unable to add provider from class " + className);
          }
        providerNdx = spi.getPosition();
      }
  }

  /**
   * Set the type of key store to initialize, load and use.
   * 
   * @param type the possibly null type of the key store. if this argument is
   *          <code>null</code>, or is an empty string, then this method sets
   *          the type of the key store to be the default value returned from
   *          the invocation of the {@link KeyStore#getDefaultType()} method.
   *          For GNU Classpath this is <i>gkr</i> which stands for the "Gnu
   *          KeyRing" specifications.
   */
  private void setKeystoreTypeParam(String type)
  {
    if (Configuration.DEBUG)
      log.fine("setKeystoreTypeParam(" + type + ")"); //$NON-NLS-1$ //$NON-NLS-2$
    if (type == null || type.trim().length() == 0)
      storeType = KeyStore.getDefaultType();
    else
      storeType = type.trim();
  }

  /**
   * Set the key password given a command line option argument. If no value was
   * present on the command line then prompt the user to provide one.
   * 
   * @param password a possibly null key password gleaned from the command line.
   * @throws IOException if an I/O related exception occurs.
   * @throws UnsupportedCallbackException if no concrete implementation of a
   *         password callback was found at runtime.
   */
  protected void setKeyPasswordParam(String password) throws IOException,
      UnsupportedCallbackException
  {
    setKeyPasswordNoPrompt(password);
    if (keyPasswordChars == null)
      setKeyPasswordParam();
  }

  /**
   * Set the Alias to use when associating Key Entries and Trusted Certificates
   * in the current key store.
   * 
   * @param name the possibly null alias to use. If this arfument is
   *          <code>null</code>, then a default value of <code>mykey</code>
   *          will be used instead.
   */
  protected void setAliasParam(String name)
  {
    alias = name == null ? DEFAULT_ALIAS : name.trim();
  }

  /**
   * Set the key password given a command line option argument.
   * 
   * @param password a possibly null key password gleaned from the command line.
   */
  protected void setKeyPasswordNoPrompt(String password)
  {
    if (password != null)
      keyPasswordChars = password.toCharArray();
  }

  /**
   * Prompt the user to provide a password to protect a Key Entry in the key
   * store.
   * 
   * @throws IOException if an I/O related exception occurs.
   * @throws UnsupportedCallbackException if no concrete implementation of a
   *           password callback was found at runtime.
   * @throws SecurityException if no password is available, even after prompting
   *           the user.
   */
  private void setKeyPasswordParam() throws IOException,
      UnsupportedCallbackException
  {
    String prompt = Messages.getFormattedString("Command.21", alias); //$NON-NLS-1$
    PasswordCallback pcb = new PasswordCallback(prompt, false);
    getCallbackHandler().handle(new Callback[] { pcb });
    keyPasswordChars = pcb.getPassword();
    pcb.clearPassword();
    if (keyPasswordChars == null)
      throw new SecurityException(Messages.getString("Command.23")); //$NON-NLS-1$
  }

  private void setKeystorePasswordParam(String password) throws IOException,
      UnsupportedCallbackException
  {
    if (password != null)
      storePasswordChars = password.toCharArray();
    else // ask the user to provide one
      {
        String prompt = Messages.getString("Command.24"); //$NON-NLS-1$
        PasswordCallback pcb = new PasswordCallback(prompt, false);
        getCallbackHandler().handle(new Callback[] { pcb });
        storePasswordChars = pcb.getPassword();
        pcb.clearPassword();
      }
  }

  /**
   * Set the key store URL to use.
   * 
   * @param createIfNotFound when <code>true</code> an attempt to create a
   *          keystore at the designated location will be made. If
   *          <code>false</code> then no file creation is carried out, which
   *          may cause an exception to be thrown later.
   * @param url the full, or partial, URL to the keystore location.
   * @param password an eventually null string to use when loading the keystore.
   * @throws IOException
   * @throws KeyStoreException
   * @throws UnsupportedCallbackException
   * @throws NoSuchAlgorithmException
   * @throws CertificateException
   */
  private void setKeystoreURLParam(boolean createIfNotFound, String url,
                                     String password) throws IOException,
      KeyStoreException, UnsupportedCallbackException, NoSuchAlgorithmException,
      CertificateException
  {
    if (Configuration.DEBUG)
      log.fine("setKeystoreURLParam(" + url + ")"); //$NON-NLS-1$ //$NON-NLS-2$
    if (url == null || url.trim().length() == 0)
      {
        String userHome = SystemProperties.getProperty("user.home"); //$NON-NLS-1$
        if (userHome == null || userHome.trim().length() == 0)
          throw new InvalidParameterException(Messages.getString("Command.36")); //$NON-NLS-1$

        url = userHome.trim() + "/.keystore"; //$NON-NLS-1$
        // if it does not exist create it if required
        if (createIfNotFound)
          new File(url).createNewFile();
        url = "file:" + url; //$NON-NLS-1$
      }
    else
      {
        url = url.trim();
        if (url.indexOf(":") == -1) // if it does not exist create it //$NON-NLS-1$
          {
            if (createIfNotFound)
              new File(url).createNewFile();
          }
        url = "file:" + url; //$NON-NLS-1$
      }

    boolean newKeyStore = false;
    storeURL = new URL(url);
    storeStream = storeURL.openStream();
    if (storeStream.available() == 0)
      {
        if (Configuration.DEBUG)
          log.fine("Store is empty. Will use <null> when loading, to create it"); //$NON-NLS-1$
        newKeyStore = true;
      }

    try
      {
        store = KeyStore.getInstance(storeType);
      }
    catch (KeyStoreException x)
      {
        if (provider != null)
          throw x;

        if (Configuration.DEBUG)
          log.fine("Exception while getting key store with default provider(s)." //$NON-NLS-1$
                   + " Will prompt user for another provider and continue"); //$NON-NLS-1$
        String prompt = Messages.getString("Command.40"); //$NON-NLS-1$
        NameCallback ncb = new NameCallback(prompt);
        getCallbackHandler().handle(new Callback[] { ncb });
        String className = ncb.getName();
        setProviderClassNameParam(className); // we may have a Provider
        if (provider == null)
          {
            x.fillInStackTrace();
            throw x;
          }
        // try again
        store = KeyStore.getInstance(storeType, provider);
      }

    setKeystorePasswordParam(password);

    // now we have a KeyStore instance. load it
    // KeyStore public API claims: "...In order to create an empty keystore,
    // you pass null as the InputStream argument to the load method.
    if (newKeyStore)
      store.load(null, storePasswordChars);
    else
      store.load(storeStream, storePasswordChars);

    // close the stream
    try
    {
      storeStream.close();
      storeStream = null;
    }
    catch (IOException x)
    {
      if (Configuration.DEBUG)
        log.fine("Exception while closing the key store input stream: " + x //$NON-NLS-1$
                 + ". Ignore"); //$NON-NLS-1$
    }
  }

  protected void setOutputStreamParam(String fileName) throws SecurityException,
      IOException
  {
    if (fileName == null || fileName.trim().length() == 0)
      {
        outStream = System.out;
        systemOut = true;
      }
    else
      {
        fileName = fileName.trim();
        File outFile = new File(fileName);
        if (! outFile.exists())
          {
            boolean ok = outFile.createNewFile();
            if (!ok)
              throw new InvalidParameterException(Messages.getFormattedString("Command.19", //$NON-NLS-1$
                                                                              fileName));
          }
        else
          {
            if (! outFile.isFile())
              throw new InvalidParameterException(Messages.getFormattedString("Command.42", //$NON-NLS-1$
                                                                              fileName));
            if (! outFile.canWrite())
              throw new InvalidParameterException(Messages.getFormattedString("Command.44", //$NON-NLS-1$
                                                                              fileName));
          }
        outStream = new FileOutputStream(outFile);
      }
  }

  protected void setInputStreamParam(String fileName)
      throws FileNotFoundException
  {
    if (fileName == null || fileName.trim().length() == 0)
      inStream = System.in;
    else
      {
        fileName = fileName.trim();
        File inFile = new File(fileName);
        if (! (inFile.exists() && inFile.isFile() && inFile.canRead()))
          throw new InvalidParameterException(Messages.getFormattedString("Command.46", //$NON-NLS-1$
                                                                          fileName));
        inStream = new FileInputStream(inFile);
      }
  }

  /**
   * Set both the key-pair generation algorithm, and the digital signature
   * algorithm instances to use when generating new entries.
   * 
   * @param kpAlg the possibly null name of a key-pair generator algorithm.
   *          if this argument is <code>null</code> or is an empty string, the
   *          "DSS" algorithm will be used.
   * @param sigAlg the possibly null name of a digital signature algorithm.
   *          If this argument is <code>null</code> or is an empty string, this
   *          method uses the "SHA1withDSA" (Digital Signature Standard, a.k.a.
   *          DSA, with the Secure Hash Algorithm function) as the default
   *          algorithm if, and only if, the key-pair generation algorithm ends
   *          up being "DSS"; otherwise, if the key-pair generation algorithm
   *          was "RSA", then the "MD5withRSA" signature algorithm will be used.
   *          If the key-pair generation algorithm is neither "DSS" (or its
   *          alias "DSA"), nor is it "RSA", then an exception is thrown.
   * @throws NoSuchAlgorithmException if no concrete implementation of the
   *           designated algorithm is available.
   */
  protected void setAlgorithmParams(String kpAlg, String sigAlg)
      throws NoSuchAlgorithmException
  {
    if (kpAlg == null || kpAlg.trim().length() == 0)
      kpAlg = DEFAULT_KEY_ALGORITHM;
    else
      kpAlg = kpAlg.trim().toLowerCase();

    keyPairGenerator = KeyPairGenerator.getInstance(kpAlg);

    if (sigAlg == null || sigAlg.trim().length() == 0)
      if (kpAlg.equalsIgnoreCase(Registry.DSS_KPG)
          || kpAlg.equalsIgnoreCase(Registry.DSA_KPG))
        sigAlg = DSA_SIGNATURE_ALGORITHM;
      else if (kpAlg.equalsIgnoreCase(Registry.RSA_KPG))
        sigAlg = RSA_SIGNATURE_ALGORITHM;
      else
        throw new IllegalArgumentException(
            Messages.getFormattedString("Command.20", //$NON-NLS-1$
                                        new String[] { sigAlg, kpAlg }));
    else
      sigAlg = sigAlg.trim().toLowerCase();

    signatureAlgorithm = Signature.getInstance(sigAlg);
  }

  /**
   * Set the signature algorithm to use when digitally signing private keys,
   * certificates, etc...
   * <p>
   * If the designated algorithm name is <code>null</code> or is an empty
   * string, this method checks the private key (the second argument) and based
   * on its type decides which algorithm to use. The keytool public
   * specification states that if the private key is a DSA key, then the
   * signature algorithm will be <code>SHA1withDSA</code>, otherwise if it is
   * an RSA private key, then the signature algorithm will be
   * <code>MD5withRSA</code>. If the private key is neither a private DSA nor
   * a private RSA key, then this method throws an
   * {@link IllegalArgumentException}.
   * 
   * @param algorithm the possibly null name of a digital signature algorithm.
   * @param privateKey an instance of a private key to use as a fal-back option
   *          when <code>algorithm</code> is invalid.
   * @throws NoSuchAlgorithmException if no concrete implementation of the
   *           designated, or default, signature algorithm is available.
   */
  protected void setSignatureAlgorithmParam(String algorithm, Key privateKey)
      throws NoSuchAlgorithmException
  {
    if (algorithm == null || algorithm.trim().length() == 0)
      if (privateKey instanceof DSAKey)
        algorithm = DSA_SIGNATURE_ALGORITHM;
      else if (privateKey instanceof RSAKey)
        algorithm = RSA_SIGNATURE_ALGORITHM;
      else
        throw new InvalidParameterException(Messages.getString("Command.48")); //$NON-NLS-1$
    else
      algorithm = algorithm.trim();

    signatureAlgorithm = Signature.getInstance(algorithm);
  }

  /**
   * Set the validity period, in number of days, to use when issuing new
   * certificates.
   * 
   * @param days the number of days, as a string, the generated certificate will
   *          be valid for, starting from today's date. if this argument is
   *          <code>null</code>, a default value of <code>90</code> days
   *          will be used.
   * @throws NumberFormatException if the designated string is not a decimal
   *           integer.
   * @throws InvalidParameterException if the integer value of the non-null
   *           string is not greater than zero.
   */
  protected void setValidityParam(String days)
  {
    if (days == null || days.trim().length() == 0)
      validityInDays = DEFAULT_VALIDITY;
    else
      {
        days = days.trim();
        validityInDays = Integer.parseInt(days);
        if (validityInDays < 1)
          throw new InvalidParameterException(Messages.getString("Command.51")); //$NON-NLS-1$
      }
  }

  /**
   * RFC-2459 (http://rfc.net/rfc2459.html) fully describes the structure and
   * semantics of X.509 certificates. The ASN.1 structures below are gleaned
   * from that reference.
   * 
   * <pre>
   *  Certificate ::= SEQUENCE {
   *    tbsCertificate      TBSCertificate,
   *    signatureAlgorithm  AlgorithmIdentifier,
   *    signatureValue      BIT STRING
   *  }
   *  
   *  TBSCertificate ::= SEQUENCE {
   *    version           [0] EXPLICIT Version DEFAULT v1,
   *    serialNumber          CertificateSerialNumber,
   *    signature             AlgorithmIdentifier,
   *    issuer                Name,
   *    validity              Validity,
   *    subject               Name,
   *    subjectPublicKeyInfo  SubjectPublicKeyInfo
   *  }
   *  
   *  Version ::= INTEGER { v1(0), v2(1), v3(2) }
   *  
   *  CertificateSerialNumber ::= INTEGER
   *  
   *  Validity ::= SEQUENCE {
   *    notBefore  Time,
   *    notAfter   Time
   *  }
   *  
   *  Time ::= CHOICE {
   *    utcTime      UTCTime,
   *    generalTime  GeneralizedTime
   *  }
   *  
   *  UniqueIdentifier ::= BIT STRING
   *  
   *  SubjectPublicKeyInfo ::= SEQUENCE {
   *    algorithm         AlgorithmIdentifier,
   *    subjectPublicKey  BIT STRING
   *  }
   * </pre>
   * 
   * @param distinguishedName the X.500 Distinguished Name to use as both the
   *          Issuer and Subject of the self-signed certificate to generate.
   * @param publicKey the public key of the issuer/subject.
   * @param privateKey the private key of the issuer/signer.
   * @return the DER encoded form of a self-signed X.509 v1 certificate.
   * @throws IOException If an I/O related exception occurs during the process.
   * @throws SignatureException If a digital signature related exception occurs.
   * @throws InvalidKeyException if the designated private key is invalid.
   * @throws InvalidParameterException if the concrete signature algorithm does
   *           not know its name, no OID is known/supported for that name, or we
   *           were unable to match the name to a known string for which we can
   *           use a standard OID.
   */
  protected byte[] getSelfSignedCertificate(X500DistinguishedName distinguishedName,
                                            PublicKey publicKey,
                                            PrivateKey privateKey)
      throws IOException, SignatureException, InvalidKeyException
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "getSelfSignedCertificate", //$NON-NLS-1$
                   new Object[] { distinguishedName, publicKey, privateKey });
    byte[] versionBytes = new DERValue(DER.INTEGER, BigInteger.ZERO).getEncoded();
    DERValue derVersion = new DERValue(DER.CONSTRUCTED | DER.CONTEXT | 0,
                                       versionBytes.length, versionBytes, null);

    // NOTE (rsn): the next 3 lines should be atomic but they're not.
    Preferences prefs = Preferences.systemNodeForPackage(this.getClass());
    int lastSerialNumber = prefs.getInt(Main.LAST_SERIAL_NUMBER, 0) + 1;
    prefs.putInt(Main.LAST_SERIAL_NUMBER, lastSerialNumber);
    DERValue derSerialNumber = new DERValue(DER.INTEGER,
                                            BigInteger.valueOf(lastSerialNumber));

    OID signatureID = getSignatureAlgorithmOID();
    DERValue derSignatureID = new DERValue(DER.OBJECT_IDENTIFIER, signatureID);
    ArrayList signature = new ArrayList(1);
    signature.add(derSignatureID);
    // rfc-2459 states the following:
    //
    // for the DSA signature:
    // ...Where the id-dsa-with-sha1 algorithm identifier appears as the
    // algorithm field in an AlgorithmIdentifier, the encoding shall omit
    // the parameters field.  That is, the AlgorithmIdentifier shall be a
    // SEQUENCE of one component - the OBJECT IDENTIFIER id-dsa-with-sha1.
    // 
    // for RSA signatures:
    // ...When any of these three OIDs (i.e. xxxWithRSAEncryption) appears
    // within the ASN.1 type AlgorithmIdentifier, the parameters component of
    // that type shall be the ASN.1 type NULL.
    if (! signatureID.equals(SHA1_WITH_DSA))
      signature.add(new DERValue(DER.NULL, null));

    DERValue derSignature = new DERValue(DER.CONSTRUCTED | DER.SEQUENCE,
                                         signature);

    DERValue derIssuer = new DERReader(distinguishedName.getDer()).read();

    long notBefore = System.currentTimeMillis();
    long notAfter = notBefore + validityInDays * MILLIS_IN_A_DAY;
    
    ArrayList validity = new ArrayList(2);
    validity.add(new DERValue(DER.UTC_TIME, new Date(notBefore)));
    validity.add(new DERValue(DER.UTC_TIME, new Date(notAfter)));
    DERValue derValidity = new DERValue(DER.CONSTRUCTED | DER.SEQUENCE,
                                        validity);

    // for a self-signed certificate subject and issuer are identical
    DERValue derSubject = derIssuer;

    DERValue derSubjectPublicKeyInfo = new DERReader(publicKey.getEncoded()).read();

    ArrayList tbsCertificate = new ArrayList(7);
    tbsCertificate.add(derVersion);
    tbsCertificate.add(derSerialNumber);
    tbsCertificate.add(derSignature);
    tbsCertificate.add(derIssuer);
    tbsCertificate.add(derValidity);
    tbsCertificate.add(derSubject);
    tbsCertificate.add(derSubjectPublicKeyInfo);
    DERValue derTBSCertificate = new DERValue(DER.CONSTRUCTED | DER.SEQUENCE,
                                              tbsCertificate);

    // The 'signature' field MUST contain the same algorithm identifier as the
    // 'signatureAlgorithm' field in the sequence Certificate.
    DERValue derSignatureAlgorithm = derSignature;

    signatureAlgorithm.initSign(privateKey);
    signatureAlgorithm.update(derTBSCertificate.getEncoded());
    byte[] sigBytes = signatureAlgorithm.sign();
    DERValue derSignatureValue = new DERValue(DER.BIT_STRING,
                                              new BitString(sigBytes));

    ArrayList certificate = new ArrayList(3);
    certificate.add(derTBSCertificate);
    certificate.add(derSignatureAlgorithm);
    certificate.add(derSignatureValue);
    DERValue derCertificate = new DERValue(DER.CONSTRUCTED | DER.SEQUENCE,
                                           certificate);

    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    DERWriter.write(baos, derCertificate);
    byte[] result = baos.toByteArray();
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "getSelfSignedCertificate"); //$NON-NLS-1$
    return result;
  }

  /**
   * This method attempts to find, and return, an OID representing the digital
   * signature algorithm used to sign the certificate. The OIDs returned are
   * those described in RFC-2459. They are listed here for the sake of
   * completness.
   * 
   * <pre>
   *  id-dsa-with-sha1 OBJECT IDENTIFIER ::= {
   *    iso(1) member-body(2) us(840) x9-57 (10040) x9cm(4) 3
   *  }
   *  
   *  md2WithRSAEncryption OBJECT IDENTIFIER ::= {
   *    iso(1) member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs-1(1) 2
   *  }
   *  
   *  md5WithRSAEncryption OBJECT IDENTIFIER ::= {
   *    iso(1) member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs-1(1) 4
   *  }
   *  
   *  sha-1WithRSAEncryption OBJECT IDENTIFIER ::= {
   *    iso(1) member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs-1(1) 5
   *  }
   * </pre>
   * 
   * <b>IMPORTANT</b>: This method checks the signature algorithm name against
   * (a) The GNU algorithm implementation's name, and (b) publicly referenced
   * names of the same algorithm. In other words this search is not
   * comprehensive and may fail for uncommon names of the same algorithms.
   * 
   * @return the OID of the signature algorithm in use.
   * @throws InvalidParameterException if the concrete signature algorithm does
   *           not know its name, no OID is known/supported for that name, or we
   *           were unable to match the name to a known string for which we can
   *           return an OID.
   */
  protected OID getSignatureAlgorithmOID()
  {
    String algorithm = signatureAlgorithm.getAlgorithm();
    // if we already have a non-null signature then the name was valid.  the
    // only case where algorithm is invalid would be if the implementation is
    // flawed.  check anyway
    if (algorithm == null || algorithm.trim().length() == 0)
      throw new InvalidParameterException(Messages.getString("Command.52")); //$NON-NLS-1$

    algorithm = algorithm.trim();
    if (algorithm.equalsIgnoreCase(Registry.DSS_SIG)
        || algorithm.equalsIgnoreCase("SHA1withDSA")) //$NON-NLS-1$
      return SHA1_WITH_DSA;
    
    if (algorithm.equalsIgnoreCase(Registry.RSA_PKCS1_V1_5_SIG + "-" //$NON-NLS-1$
                                   + Registry.MD2_HASH)
        || algorithm.equalsIgnoreCase("MD2withRSA")) //$NON-NLS-1$
      return MD2_WITH_RSA;

    if (algorithm.equalsIgnoreCase(Registry.RSA_PKCS1_V1_5_SIG + "-" //$NON-NLS-1$
                                   + Registry.MD5_HASH)
        || algorithm.equalsIgnoreCase("MD5withRSA") //$NON-NLS-1$
        || algorithm.equalsIgnoreCase("rsa")) //$NON-NLS-1$
      return MD5_WITH_RSA;

    if (algorithm.equalsIgnoreCase(Registry.RSA_PKCS1_V1_5_SIG + "-" //$NON-NLS-1$
                                   + Registry.SHA160_HASH)
        || algorithm.equalsIgnoreCase("SHA1withRSA")) //$NON-NLS-1$
      return SHA1_WITH_RSA;

    throw new InvalidParameterException(Messages.getFormattedString("Command.60", //$NON-NLS-1$
                                                                    algorithm));
  }

  /**
   * Saves the key store using the designated password. This operation is called
   * by handlers if/when the key store password has changed, or amendements have
   * been made to the contents of the store; e.g. addition of a new Key Entry or
   * a Trusted Certificate.
   * 
   * @param password the password protecting the key store.
   * @throws IOException if an I/O related exception occurs during the process.
   * @throws CertificateException if any of the certificates in the current key
   *           store could not be persisted.
   * @throws NoSuchAlgorithmException if a required data integrity algorithm
   *           implementation was not found.
   * @throws KeyStoreException if the key store has not been loaded previously.
   */
  protected void saveKeyStore(char[] password) throws IOException,
      KeyStoreException, NoSuchAlgorithmException, CertificateException
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "saveKeyStore"); //$NON-NLS-1$
    URLConnection con = storeURL.openConnection();
    con.setDoOutput(true);
    con.setUseCaches(false);
    OutputStream out = con.getOutputStream();
    if (verbose)
      System.out.println(Messages.getFormattedString("Command.63", storeURL.getPath())); //$NON-NLS-1$

    store.store(out, password);
    out.flush();
    out.close();
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "saveKeyStore"); //$NON-NLS-1$
  }

  /**
   * Convenience method. Calls the method with the same name passing it the
   * same password characters used to initially load the key-store. 
   * 
   * @throws IOException if an I/O related exception occurs during the process.
   * @throws KeyStoreException if the key store has not been loaded previously.
   * @throws NoSuchAlgorithmException if a required data integrity algorithm
   *           implementation was not found.
   * @throws CertificateException if any of the certificates in the current key
   *           store could not be persisted.
   */
  protected void saveKeyStore() throws IOException, KeyStoreException,
      NoSuchAlgorithmException, CertificateException
  {
    saveKeyStore(storePasswordChars);
  }

  /**
   * Prints a human-readable form of the designated certificate to a designated
   * {@link PrintWriter}.
   * 
   * @param certificate the certificate to process.
   * @param writer where to print it.
   * @throws CertificateEncodingException if an exception occurs while obtaining
   *           the DER encoded form <code>certificate</code>.
   */
  protected void printVerbose(Certificate certificate, PrintWriter writer)
      throws CertificateEncodingException
  {
    X509Certificate x509 = (X509Certificate) certificate;
    writer.println(Messages.getFormattedString("Command.66", x509.getSubjectDN())); //$NON-NLS-1$
    writer.println(Messages.getFormattedString("Command.67", x509.getIssuerDN())); //$NON-NLS-1$
    writer.println(Messages.getFormattedString("Command.68", x509.getSerialNumber())); //$NON-NLS-1$
    writer.println(Messages.getFormattedString("Command.69", x509.getNotBefore())); //$NON-NLS-1$
    writer.println(Messages.getFormattedString("Command.70", x509.getNotAfter())); //$NON-NLS-1$
    writer.println(Messages.getString("Command.71")); //$NON-NLS-1$
    byte[] derBytes = certificate.getEncoded();
    writer.println(Messages.getFormattedString("Command.72", digest(md5, derBytes))); //$NON-NLS-1$
    writer.println(Messages.getFormattedString("Command.73", digest(sha, derBytes))); //$NON-NLS-1$
  }

  /**
   * Convenience method. Prints a human-readable form of the designated
   * certificate to <code>System.out</code>.
   * 
   * @param certificate the certificate to process.
   * @throws CertificateEncodingException if an exception occurs while obtaining
   *           the DER encoded form <code>certificate</code>.
   */
  protected void printVerbose(Certificate certificate)
      throws CertificateEncodingException
  {
    printVerbose(certificate, new PrintWriter(System.out, true));
  }

  /**
   * Digest the designated contents with MD5 and return a string representation
   * suitable for use as a fingerprint; i.e. sequence of hexadecimal pairs of
   * characters separated by a colon.
   * 
   * @param contents the non-null contents to digest.
   * @return a sequence of hexadecimal pairs of characters separated by colons.
   */
  protected String digestWithMD5(byte[] contents)
  {
    return digest(md5, contents);
  }

  private String digest(IMessageDigest hash, byte[] encoded)
  {
    hash.update(encoded);
    byte[] b = hash.digest();
    StringBuilder sb = new StringBuilder().append(Util.toString(b, 0, 1));
    for (int i = 1; i < b.length; i++)
      sb.append(":").append(Util.toString(b, i, 1)); //$NON-NLS-1$

    String result = sb.toString();
    return result;
  }

  /**
   * Ensure that the currently set Alias is contained in the currently set key
   * store; otherwise throw an exception.
   * 
   * @throws KeyStoreException if the keystore has not been loaded.
   * @throws IllegalArgumentException if the currently set alias is not known to
   *           the currently set key store.
   */
  protected void ensureStoreContainsAlias() throws KeyStoreException
  {
    if (! store.containsAlias(alias))
      throw new IllegalArgumentException(Messages.getFormattedString("Command.75", //$NON-NLS-1$
                                                                     alias));
  }

  /**
   * Ensure that the currently set Alias is associated with a Key Entry in the
   * currently set key store; otherwise throw an exception.
   * 
   * @throws KeyStoreException if the keystore has not been loaded.
   * @throws SecurityException if the currently set alias is not a Key Entry in
   *           the currently set key store.
   */
  protected void ensureAliasIsKeyEntry() throws KeyStoreException
  {
    if (! store.isKeyEntry(alias))
      throw new SecurityException(Messages.getFormattedString("Command.77", //$NON-NLS-1$
                                                              alias));
  }

  protected Key getAliasPrivateKey() throws KeyStoreException,
      NoSuchAlgorithmException, IOException, UnsupportedCallbackException,
      UnrecoverableKeyException
  {
    ensureAliasIsKeyEntry();
    Key result;
    if (keyPasswordChars == null)
      try
        {
          result = store.getKey(alias, storePasswordChars);
          // it worked. assign to keyPasswordChars for later use
          keyPasswordChars = storePasswordChars;
        }
      catch (UnrecoverableKeyException x)
        {
          // prompt the user to provide one
          setKeyPasswordParam();
          result = store.getKey(alias, keyPasswordChars);
        }
    else
      result = store.getKey(alias, keyPasswordChars);

    return result;
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
   * callback handler implementation is
   * {@link gnu.javax.security.auth.callback.ConsoleCallbackHandler}.
   * 
   * @return a console-based {@link CallbackHandler}.
   */
  protected CallbackHandler getCallbackHandler()
  {
    if (handler == null)
      handler = CallbackUtil.getConsoleHandler();

    return handler;
  }

  // Inner class(es) ==========================================================

  private class ShutdownHook
      extends Thread
  {
    public void run()
    {
      teardown();
    }
  }
}
