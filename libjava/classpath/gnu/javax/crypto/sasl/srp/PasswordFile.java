/* PasswordFile.java -- 
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

import gnu.java.lang.CPStringBuilder;

import gnu.java.security.Registry;
import gnu.java.security.util.Util;
import gnu.javax.crypto.key.srp6.SRPAlgorithm;
import gnu.javax.crypto.sasl.NoSuchUserException;
import gnu.javax.crypto.sasl.UserAlreadyExistsException;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.math.BigInteger;
import java.util.HashMap;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.StringTokenizer;

/**
 * The implementation of SRP password files.
 * <p>
 * For SRP, there are three (3) files:
 * <ol>
 * <li>The password configuration file: tpasswd.conf. It contains the pairs
 * &lt;N,g> indexed by a number for each pair used for a user. By default, this
 * file's pathname is constructed from the base password file pathname by
 * prepending it with the ".conf" suffix.</li>
 * <li>The base password file: tpasswd. It contains the related password
 * entries for all the users with values computed using SRP's default message
 * digest algorithm: SHA-1 (with 160-bit output block size).</li>
 * <li>The extended password file: tpasswd2. Its name, by default, is
 * constructed by adding the suffix "2" to the fully qualified pathname of the
 * base password file. It contains, in addition to the same fields as the base
 * password file, albeit with a different verifier value, an extra field
 * identifying the message digest algorithm used to compute this (verifier)
 * value.</li>
 * </ol>
 * <p>
 * This implementation assumes the following message digest algorithm codes:
 * <ul>
 * <li>0: the default hash algorithm, which is SHA-1 (or its alias SHA-160).</li>
 * <li>1: MD5.</li>
 * <li>2: RIPEMD-128.</li>
 * <li>3: RIPEMD-160.</li>
 * <li>4: SHA-256.</li>
 * <li>5: SHA-384.</li>
 * <li>6: SHA-512.</li>
 * </ul>
 * <p>
 * <b>IMPORTANT:</b> This method computes the verifiers as described in
 * RFC-2945, which differs from the description given on the web page for SRP-6.
 * <p>
 * Reference:
 * <ol>
 * <li><a href="http://srp.stanford.edu/design.html">SRP Protocol Design</a><br>
 * Thomas J. Wu.</li>
 * </ol>
 */
public class PasswordFile
{
  // names of property keys used in this class
  private static final String USER_FIELD = "user";
  private static final String VERIFIERS_FIELD = "verifier";
  private static final String SALT_FIELD = "salt";
  private static final String CONFIG_FIELD = "config";
  private static String DEFAULT_FILE;
  static
    {
      DEFAULT_FILE = System.getProperty(SRPRegistry.PASSWORD_FILE,
                                        SRPRegistry.DEFAULT_PASSWORD_FILE);
    }
  /** The SRP algorithm instances used by this object. */
  private static final HashMap srps;
  static
    {
      final HashMap map = new HashMap(SRPRegistry.SRP_ALGORITHMS.length);
      // The first entry MUST exist. The others are optional.
      map.put("0", SRP.instance(SRPRegistry.SRP_ALGORITHMS[0]));
      for (int i = 1; i < SRPRegistry.SRP_ALGORITHMS.length; i++)
        {
          try
            {
              map.put(String.valueOf(i),
                      SRP.instance(SRPRegistry.SRP_ALGORITHMS[i]));
            }
          catch (Exception x)
            {
              System.err.println("Ignored: " + x);
              x.printStackTrace(System.err);
            }
        }
      srps = map;
    }

  private String confName, pwName, pw2Name;
  private File configFile, passwdFile, passwd2File;
  private long lastmodPasswdFile, lastmodPasswd2File;
  private HashMap entries = new HashMap();
  private HashMap configurations = new HashMap();
  // default N values to use when creating a new password.conf file
  private static final BigInteger[] Nsrp = new BigInteger[] {
      SRPAlgorithm.N_2048,
      SRPAlgorithm.N_1536,
      SRPAlgorithm.N_1280,
      SRPAlgorithm.N_1024,
      SRPAlgorithm.N_768,
      SRPAlgorithm.N_640,
      SRPAlgorithm.N_512 };

  public PasswordFile() throws IOException
  {
    this(DEFAULT_FILE);
  }

  public PasswordFile(final File pwFile) throws IOException
  {
    this(pwFile.getAbsolutePath());
  }

  public PasswordFile(final String pwName) throws IOException
  {
    this(pwName, pwName + "2", pwName + ".conf");
  }

  public PasswordFile(final String pwName, final String confName)
      throws IOException
  {
    this(pwName, pwName + "2", confName);
  }

  public PasswordFile(final String pwName, final String pw2Name,
                      final String confName) throws IOException
  {
    super();

    this.pwName = pwName;
    this.pw2Name = pw2Name;
    this.confName = confName;

    readOrCreateConf();
    update();
  }

  /**
   * Returns a string representing the decimal value of an integer identifying
   * the message digest algorithm to use for the SRP computations.
   * 
   * @param mdName the canonical name of a message digest algorithm.
   * @return a string representing the decimal value of an ID for that
   *         algorithm.
   */
  private static final String nameToID(final String mdName)
  {
    if (Registry.SHA_HASH.equalsIgnoreCase(mdName)
        || Registry.SHA1_HASH.equalsIgnoreCase(mdName)
        || Registry.SHA160_HASH.equalsIgnoreCase(mdName))
      return "0";
    else if (Registry.MD5_HASH.equalsIgnoreCase(mdName))
      return "1";
    else if (Registry.RIPEMD128_HASH.equalsIgnoreCase(mdName))
      return "2";
    else if (Registry.RIPEMD160_HASH.equalsIgnoreCase(mdName))
      return "3";
    else if (Registry.SHA256_HASH.equalsIgnoreCase(mdName))
      return "4";
    else if (Registry.SHA384_HASH.equalsIgnoreCase(mdName))
      return "5";
    else if (Registry.SHA512_HASH.equalsIgnoreCase(mdName))
      return "6";
    return "0";
  }

  /**
   * Checks if the current configuration file contains the &lt;N, g> pair for
   * the designated <code>index</code>.
   * 
   * @param index a string representing 1-digit identification of an &lt;N, g>
   *          pair used.
   * @return <code>true</code> if the designated <code>index</code> is that
   *         of a known &lt;N, g> pair, and <code>false</code> otherwise.
   * @throws IOException if an exception occurs during the process.
   * @see SRPRegistry#N_2048_BITS
   * @see SRPRegistry#N_1536_BITS
   * @see SRPRegistry#N_1280_BITS
   * @see SRPRegistry#N_1024_BITS
   * @see SRPRegistry#N_768_BITS
   * @see SRPRegistry#N_640_BITS
   * @see SRPRegistry#N_512_BITS
   */
  public synchronized boolean containsConfig(final String index)
      throws IOException
  {
    checkCurrent();
    return configurations.containsKey(index);
  }

  /**
   * Returns a pair of strings representing the pair of <code>N</code> and
   * <code>g</code> MPIs for the designated <code>index</code>.
   * 
   * @param index a string representing 1-digit identification of an &lt;N, g>
   *          pair to look up.
   * @return a pair of strings, arranged in an array, where the first (at index
   *         position #0) is the repesentation of the MPI <code>N</code>, and
   *         the second (at index position #1) is the representation of the MPI
   *         <code>g</code>. If the <code>index</code> refers to an unknown
   *         pair, then an empty string array is returned.
   * @throws IOException if an exception occurs during the process.
   */
  public synchronized String[] lookupConfig(final String index)
      throws IOException
  {
    checkCurrent();
    String[] result = null;
    if (configurations.containsKey(index))
      result = (String[]) configurations.get(index);
    return result;
  }

  public synchronized boolean contains(final String user) throws IOException
  {
    checkCurrent();
    return entries.containsKey(user);
  }

  public synchronized void add(final String user, final String passwd,
                               final byte[] salt, final String index)
      throws IOException
  {
    checkCurrent();
    if (entries.containsKey(user))
      throw new UserAlreadyExistsException(user);
    final HashMap fields = new HashMap(4);
    fields.put(USER_FIELD, user); // 0
    fields.put(VERIFIERS_FIELD, newVerifiers(user, salt, passwd, index)); // 1
    fields.put(SALT_FIELD, Util.toBase64(salt)); // 2
    fields.put(CONFIG_FIELD, index); // 3
    entries.put(user, fields);
    savePasswd();
  }

  public synchronized void changePasswd(final String user, final String passwd)
      throws IOException
  {
    checkCurrent();
    if (! entries.containsKey(user))
      throw new NoSuchUserException(user);
    final HashMap fields = (HashMap) entries.get(user);
    final byte[] salt;
    try
      {
        salt = Util.fromBase64((String) fields.get(SALT_FIELD));
      }
    catch (NumberFormatException x)
      {
        throw new IOException("Password file corrupt");
      }
    final String index = (String) fields.get(CONFIG_FIELD);
    fields.put(VERIFIERS_FIELD, newVerifiers(user, salt, passwd, index));
    entries.put(user, fields);
    savePasswd();
  }

  public synchronized void savePasswd() throws IOException
  {
    final FileOutputStream f1 = new FileOutputStream(passwdFile);
    final FileOutputStream f2 = new FileOutputStream(passwd2File);
    PrintWriter pw1 = null;
    PrintWriter pw2 = null;
    try
      {
        pw1 = new PrintWriter(f1, true);
        pw2 = new PrintWriter(f2, true);
        this.writePasswd(pw1, pw2);
      }
    finally
      {
        if (pw1 != null)
          try
            {
              pw1.flush();
            }
          finally
            {
              pw1.close();
            }
        if (pw2 != null)
          try
            {
              pw2.flush();
            }
          finally
            {
              pw2.close();
            }
        try
          {
            f1.close();
          }
        catch (IOException ignored)
          {
          }
        try
          {
            f2.close();
          }
        catch (IOException ignored)
          {
          }
      }
    lastmodPasswdFile = passwdFile.lastModified();
    lastmodPasswd2File = passwd2File.lastModified();
  }

  /**
   * Returns the triplet: verifier, salt and configuration file index, of a
   * designated user, and a designated message digest algorithm name, as an
   * array of strings.
   * 
   * @param user the username.
   * @param mdName the canonical name of the SRP's message digest algorithm.
   * @return a string array containing, in this order, the BASE-64 encodings of
   *         the verifier, the salt and the index in the password configuration
   *         file of the MPIs N and g of the designated user.
   */
  public synchronized String[] lookup(final String user, final String mdName)
      throws IOException
  {
    checkCurrent();
    if (! entries.containsKey(user))
      throw new NoSuchUserException(user);
    final HashMap fields = (HashMap) entries.get(user);
    final HashMap verifiers = (HashMap) fields.get(VERIFIERS_FIELD);
    final String salt = (String) fields.get(SALT_FIELD);
    final String index = (String) fields.get(CONFIG_FIELD);
    final String verifier = (String) verifiers.get(nameToID(mdName));
    return new String[] { verifier, salt, index };
  }

  private synchronized void readOrCreateConf() throws IOException
  {
    configurations.clear();
    final FileInputStream fis;
    configFile = new File(confName);
    try
      {
        fis = new FileInputStream(configFile);
        readConf(fis);
      }
    catch (FileNotFoundException x)
      { // create a default one
        final String g = Util.toBase64(Util.trim(new BigInteger("2")));
        String index, N;
        for (int i = 0; i < Nsrp.length; i++)
          {
            index = String.valueOf(i + 1);
            N = Util.toBase64(Util.trim(Nsrp[i]));
            configurations.put(index, new String[] { N, g });
          }
        FileOutputStream f0 = null;
        PrintWriter pw0 = null;
        try
          {
            f0 = new FileOutputStream(configFile);
            pw0 = new PrintWriter(f0, true);
            this.writeConf(pw0);
          }
        finally
          {
            if (pw0 != null)
              pw0.close();
            else if (f0 != null)
              f0.close();
          }
      }
  }

  private void readConf(final InputStream in) throws IOException
  {
    final BufferedReader din = new BufferedReader(new InputStreamReader(in));
    String line, index, N, g;
    StringTokenizer st;
    while ((line = din.readLine()) != null)
      {
        st = new StringTokenizer(line, ":");
        try
          {
            index = st.nextToken();
            N = st.nextToken();
            g = st.nextToken();
          }
        catch (NoSuchElementException x)
          {
            throw new IOException("SRP password configuration file corrupt");
          }
        configurations.put(index, new String[] { N, g });
      }
  }

  private void writeConf(final PrintWriter pw)
  {
    String ndx;
    String[] mpi;
    CPStringBuilder sb;
    for (Iterator it = configurations.keySet().iterator(); it.hasNext();)
      {
        ndx = (String) it.next();
        mpi = (String[]) configurations.get(ndx);
        sb = new CPStringBuilder(ndx)
            .append(":").append(mpi[0])
            .append(":").append(mpi[1]);
        pw.println(sb.toString());
      }
  }

  /**
   * Compute the new verifiers for the designated username and password.
   * <p>
   * <b>IMPORTANT:</b> This method computes the verifiers as described in
   * RFC-2945, which differs from the description given on the web page for
   * SRP-6.
   * 
   * @param user the user's name.
   * @param s the user's salt.
   * @param password the user's password
   * @param index the index of the &lt;N, g> pair to use for this user.
   * @return a {@link java.util.Map} of user verifiers.
   * @throws UnsupportedEncodingException if the US-ASCII decoder is not
   *           available on this platform.
   */
  private HashMap newVerifiers(final String user, final byte[] s,
                               final String password, final String index)
      throws UnsupportedEncodingException
  {
    // to ensure inter-operability with non-java tools
    final String[] mpi = (String[]) configurations.get(index);
    final BigInteger N = new BigInteger(1, Util.fromBase64(mpi[0]));
    final BigInteger g = new BigInteger(1, Util.fromBase64(mpi[1]));
    final HashMap result = new HashMap(srps.size());
    BigInteger x, v;
    SRP srp;
    for (int i = 0; i < srps.size(); i++)
      {
        final String digestID = String.valueOf(i);
        srp = (SRP) srps.get(digestID);
        x = new BigInteger(1, srp.computeX(s, user, password));
        v = g.modPow(x, N);
        final String verifier = Util.toBase64(v.toByteArray());
        result.put(digestID, verifier);
      }
    return result;
  }

  private synchronized void update() throws IOException
  {
    entries.clear();
    FileInputStream fis;
    passwdFile = new File(pwName);
    lastmodPasswdFile = passwdFile.lastModified();
    try
      {
        fis = new FileInputStream(passwdFile);
        readPasswd(fis);
      }
    catch (FileNotFoundException ignored)
      {
      }
    passwd2File = new File(pw2Name);
    lastmodPasswd2File = passwd2File.lastModified();
    try
      {
        fis = new FileInputStream(passwd2File);
        readPasswd2(fis);
      }
    catch (FileNotFoundException ignored)
      {
      }
  }

  private void checkCurrent() throws IOException
  {
    if (passwdFile.lastModified() > lastmodPasswdFile
        || passwd2File.lastModified() > lastmodPasswd2File)
      update();
  }

  private void readPasswd(final InputStream in) throws IOException
  {
    final BufferedReader din = new BufferedReader(new InputStreamReader(in));
    String line, user, verifier, salt, index;
    StringTokenizer st;
    while ((line = din.readLine()) != null)
      {
        st = new StringTokenizer(line, ":");
        try
          {
            user = st.nextToken();
            verifier = st.nextToken();
            salt = st.nextToken();
            index = st.nextToken();
          }
        catch (NoSuchElementException x)
          {
            throw new IOException("SRP base password file corrupt");
          }
        final HashMap verifiers = new HashMap(6);
        verifiers.put("0", verifier);
        final HashMap fields = new HashMap(4);
        fields.put(USER_FIELD, user);
        fields.put(VERIFIERS_FIELD, verifiers);
        fields.put(SALT_FIELD, salt);
        fields.put(CONFIG_FIELD, index);
        entries.put(user, fields);
      }
  }

  private void readPasswd2(final InputStream in) throws IOException
  {
    final BufferedReader din = new BufferedReader(new InputStreamReader(in));
    String line, digestID, user, verifier;
    StringTokenizer st;
    HashMap fields, verifiers;
    while ((line = din.readLine()) != null)
      {
        st = new StringTokenizer(line, ":");
        try
          {
            digestID = st.nextToken();
            user = st.nextToken();
            verifier = st.nextToken();
          }
        catch (NoSuchElementException x)
          {
            throw new IOException("SRP extended password file corrupt");
          }
        fields = (HashMap) entries.get(user);
        if (fields != null)
          {
            verifiers = (HashMap) fields.get(VERIFIERS_FIELD);
            verifiers.put(digestID, verifier);
          }
      }
  }

  private void writePasswd(final PrintWriter pw1, final PrintWriter pw2)
      throws IOException
  {
    String user, digestID;
    HashMap fields, verifiers;
    CPStringBuilder sb1, sb2;
    Iterator j;
    final Iterator i = entries.keySet().iterator();
    while (i.hasNext())
      {
        user = (String) i.next();
        fields = (HashMap) entries.get(user);
        if (! user.equals(fields.get(USER_FIELD)))
          throw new IOException("Inconsistent SRP password data");
        verifiers = (HashMap) fields.get(VERIFIERS_FIELD);
        sb1 = new CPStringBuilder(user)
            .append(":").append((String) verifiers.get("0"))
            .append(":").append((String) fields.get(SALT_FIELD))
            .append(":").append((String) fields.get(CONFIG_FIELD));
        pw1.println(sb1.toString());
        // write extended information
        j = verifiers.keySet().iterator();
        while (j.hasNext())
          {
            digestID = (String) j.next();
            if (! "0".equals(digestID))
              {
                // #0 is the default digest, already present in tpasswd!
                sb2 = new CPStringBuilder(digestID)
                    .append(":").append(user)
                    .append(":").append((String) verifiers.get(digestID));
                pw2.println(sb2.toString());
              }
          }
      }
  }
}
