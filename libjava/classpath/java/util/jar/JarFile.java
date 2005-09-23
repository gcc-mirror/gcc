/* JarFile.java - Representation of a jar file
   Copyright (C) 2000, 2003, 2004, 2005 Free Software Foundation, Inc.

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


package java.util.jar;

import gnu.java.io.Base64InputStream;
import gnu.java.security.OID;
import gnu.java.security.pkcs.PKCS7SignedData;
import gnu.java.security.pkcs.SignerInfo;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.InvalidKeyException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.Signature;
import java.security.SignatureException;
import java.security.cert.CRLException;
import java.security.cert.Certificate;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.zip.ZipEntry;
import java.util.zip.ZipException;
import java.util.zip.ZipFile;

/**
 * Representation of a jar file.
 * <p>
 * Note that this class is not a subclass of java.io.File but a subclass of
 * java.util.zip.ZipFile and you can only read JarFiles with it (although
 * there are constructors that take a File object).
 *
 * @since 1.2
 * @author Mark Wielaard (mark@klomp.org)
 * @author Casey Marshall (csm@gnu.org) wrote the certificate and entry
 *  verification code.
 */
public class JarFile extends ZipFile
{
  // Fields

  /** The name of the manifest entry: META-INF/MANIFEST.MF */
  public static final String MANIFEST_NAME = "META-INF/MANIFEST.MF";

  /** The META-INF directory entry. */
  private static final String META_INF = "META-INF/";

  /** The suffix for PKCS7 DSA signature entries. */
  private static final String PKCS7_DSA_SUFFIX = ".DSA";

  /** The suffix for PKCS7 RSA signature entries. */
  private static final String PKCS7_RSA_SUFFIX = ".RSA";

  /** The suffix for digest attributes. */
  private static final String DIGEST_KEY_SUFFIX = "-Digest";

  /** The suffix for signature files. */
  private static final String SF_SUFFIX = ".SF";

  // Signature OIDs.
  private static final OID MD2_OID = new OID("1.2.840.113549.2.2");
  private static final OID MD4_OID = new OID("1.2.840.113549.2.4");
  private static final OID MD5_OID = new OID("1.2.840.113549.2.5");
  private static final OID SHA1_OID = new OID("1.3.14.3.2.26");
  private static final OID DSA_ENCRYPTION_OID = new OID("1.2.840.10040.4.1");
  private static final OID RSA_ENCRYPTION_OID = new OID("1.2.840.113549.1.1.1");

  /**
   * The manifest of this file, if any, otherwise null.
   * Read when first needed.
   */
  private Manifest manifest;

  /** Whether to verify the manifest and all entries. */
  boolean verify;

  /** Whether the has already been loaded. */
  private boolean manifestRead = false;

  /** Whether the signature files have been loaded. */
  boolean signaturesRead = false;

  /**
   * A map between entry names and booleans, signaling whether or
   * not that entry has been verified.
   * Only be accessed with lock on this JarFile*/
  HashMap verified = new HashMap();

  /**
   * A mapping from entry name to certificates, if any.
   * Only accessed with lock on this JarFile.
   */
  HashMap entryCerts;

  static boolean DEBUG = false;
  static void debug(Object msg)
  {
    System.err.print(JarFile.class.getName());
    System.err.print(" >>> ");
    System.err.println(msg);
  }

  // Constructors

  /**
   * Creates a new JarFile. All jar entries are verified (when a Manifest file
   * for this JarFile exists). You need to actually open and read the complete
   * jar entry (with <code>getInputStream()</code>) to check its signature.
   *
   * @param fileName the name of the file to open
   * @exception FileNotFoundException if the fileName cannot be found
   * @exception IOException if another IO exception occurs while reading
   */
  public JarFile(String fileName) throws FileNotFoundException, IOException
  {
    this(fileName, true);
  }

  /**
   * Creates a new JarFile. If verify is true then all jar entries are
   * verified (when a Manifest file for this JarFile exists). You need to
   * actually open and read the complete jar entry
   * (with <code>getInputStream()</code>) to check its signature.
   *
   * @param fileName the name of the file to open
   * @param verify checks manifest and entries when true and a manifest
   * exists, when false no checks are made
   * @exception FileNotFoundException if the fileName cannot be found
   * @exception IOException if another IO exception occurs while reading
   */
  public JarFile(String fileName, boolean verify) throws
    FileNotFoundException, IOException
  {
    super(fileName);
    if (verify)
      {
	manifest = readManifest();
	verify();
      }
  }

  /**
   * Creates a new JarFile. All jar entries are verified (when a Manifest file
   * for this JarFile exists). You need to actually open and read the complete
   * jar entry (with <code>getInputStream()</code>) to check its signature.
   *
   * @param file the file to open as a jar file
   * @exception FileNotFoundException if the file does not exits
   * @exception IOException if another IO exception occurs while reading
   */
  public JarFile(File file) throws FileNotFoundException, IOException
  {
    this(file, true);
  }

  /**
   * Creates a new JarFile. If verify is true then all jar entries are
   * verified (when a Manifest file for this JarFile exists). You need to
   * actually open and read the complete jar entry
   * (with <code>getInputStream()</code>) to check its signature.
   *
   * @param file the file to open to open as a jar file
   * @param verify checks manifest and entries when true and a manifest
   * exists, when false no checks are made
   * @exception FileNotFoundException if file does not exist
   * @exception IOException if another IO exception occurs while reading
   */
  public JarFile(File file, boolean verify) throws FileNotFoundException,
    IOException
  {
    super(file);
    if (verify)
      {
	manifest = readManifest();
	verify();
      }
  }

  /**
   * Creates a new JarFile with the indicated mode. If verify is true then
   * all jar entries are verified (when a Manifest file for this JarFile
   * exists). You need to actually open and read the complete jar entry
   * (with <code>getInputStream()</code>) to check its signature.
   * manifest and if the manifest exists and verify is true verfies it.
   *
   * @param file the file to open to open as a jar file
   * @param verify checks manifest and entries when true and a manifest
   * exists, when false no checks are made
   * @param mode either ZipFile.OPEN_READ or
   *             (ZipFile.OPEN_READ | ZipFile.OPEN_DELETE)
   * @exception FileNotFoundException if the file does not exist
   * @exception IOException if another IO exception occurs while reading
   * @exception IllegalArgumentException when given an illegal mode
   * 
   * @since 1.3
   */
  public JarFile(File file, boolean verify, int mode) throws
    FileNotFoundException, IOException, IllegalArgumentException
  {
    super(file, mode);
    if (verify)
      {
	manifest = readManifest();
	verify();
      }
  }

  // Methods

  /**
   * XXX - should verify the manifest file
   */
  private void verify()
  {
    // only check if manifest is not null
    if (manifest == null)
      {
	verify = false;
	return;
      }

    verify = true;
    // XXX - verify manifest
  }

  /**
   * Parses and returns the manifest if it exists, otherwise returns null.
   */
  private Manifest readManifest()
  {
    try
      {
	ZipEntry manEntry = super.getEntry(MANIFEST_NAME);
	if (manEntry != null)
	  {
	    InputStream in = super.getInputStream(manEntry);
	    manifestRead = true;
	    return new Manifest(in);
	  }
	else
	  {
	    manifestRead = true;
	    return null;
	  }
      }
    catch (IOException ioe)
      {
	manifestRead = true;
	return null;
      }
  }

  /**
   * Returns a enumeration of all the entries in the JarFile.
   * Note that also the Jar META-INF entries are returned.
   *
   * @exception IllegalStateException when the JarFile is already closed
   */
  public Enumeration entries() throws IllegalStateException
  {
    return new JarEnumeration(super.entries(), this);
  }

  /**
   * Wraps a given Zip Entries Enumeration. For every zip entry a
   * JarEntry is created and the corresponding Attributes are looked up.
   */
  private static class JarEnumeration implements Enumeration
  {

    private final Enumeration entries;
    private final JarFile jarfile;

    JarEnumeration(Enumeration e, JarFile f)
    {
      entries = e;
      jarfile = f;
    }

    public boolean hasMoreElements()
    {
      return entries.hasMoreElements();
    }

    public Object nextElement()
    {
      ZipEntry zip = (ZipEntry) entries.nextElement();
      JarEntry jar = new JarEntry(zip);
      Manifest manifest;
      try
	{
	  manifest = jarfile.getManifest();
	}
      catch (IOException ioe)
	{
	  manifest = null;
	}

      if (manifest != null)
	{
	  jar.attr = manifest.getAttributes(jar.getName());
	}

      synchronized(jarfile)
	{
	  if (jarfile.verify && !jarfile.signaturesRead)
	    try
	      {
		jarfile.readSignatures();
	      }
	    catch (IOException ioe)
	      {
		if (JarFile.DEBUG)
		  {
		    JarFile.debug(ioe);
		    ioe.printStackTrace();
		  }
		jarfile.signaturesRead = true; // fudge it.
	      }

	  // Include the certificates only if we have asserted that the
	  // signatures are valid. This means the certificates will not be
	  // available if the entry hasn't been read yet.
	  if (jarfile.entryCerts != null
	      && jarfile.verified.get(zip.getName()) == Boolean.TRUE)
	    {
	      Set certs = (Set) jarfile.entryCerts.get(jar.getName());
	      if (certs != null)
		jar.certs = (Certificate[])
		  certs.toArray(new Certificate[certs.size()]);
	    }
	}
      return jar;
    }
  }

  /**
   * XXX
   * It actually returns a JarEntry not a zipEntry
   * @param name XXX
   */
  public synchronized ZipEntry getEntry(String name)
  {
    ZipEntry entry = super.getEntry(name);
    if (entry != null)
      {
	JarEntry jarEntry = new JarEntry(entry);
	Manifest manifest;
	try
	  {
	    manifest = getManifest();
	  }
	catch (IOException ioe)
	  {
	    manifest = null;
	  }

	if (manifest != null)
	  {
	    jarEntry.attr = manifest.getAttributes(name);
          }

	if (verify && !signaturesRead)
	  try
	    {
	      readSignatures();
	    }
	  catch (IOException ioe)
	    {
	      if (DEBUG)
		{
		  debug(ioe);
		  ioe.printStackTrace();
		}
	      signaturesRead = true;
	    }
	// See the comments in the JarEnumeration for why we do this
	// check.
	if (DEBUG)
	  debug("entryCerts=" + entryCerts + " verified " + name
		+ " ? " + verified.get(name));
	if (entryCerts != null && verified.get(name) == Boolean.TRUE)
	  {
	    Set certs = (Set) entryCerts.get(name);
	    if (certs != null)
	      jarEntry.certs = (Certificate[])
		certs.toArray(new Certificate[certs.size()]);
	  }
	return jarEntry;
      }
    return null;
  }

  /**
   * Returns an input stream for the given entry. If configured to
   * verify entries, the input stream returned will verify them while
   * the stream is read, but only on the first time.
   *
   * @param entry The entry to get the input stream for.
   * @exception ZipException XXX
   * @exception IOException XXX
   */
  public synchronized InputStream getInputStream(ZipEntry entry) throws
    ZipException, IOException
  {
    // If we haven't verified the hash, do it now.
    if (!verified.containsKey(entry.getName()) && verify)
      {
        if (DEBUG)
          debug("reading and verifying " + entry);
        return new EntryInputStream(entry, super.getInputStream(entry), this);
      }
    else
      {
        if (DEBUG)
          debug("reading already verified entry " + entry);
        if (verify && verified.get(entry.getName()) == Boolean.FALSE)
          throw new ZipException("digest for " + entry + " is invalid");
        return super.getInputStream(entry);
      }
  }

  /**
   * Returns the JarEntry that belongs to the name if such an entry
   * exists in the JarFile. Returns null otherwise
   * Convenience method that just casts the result from <code>getEntry</code>
   * to a JarEntry.
   *
   * @param name the jar entry name to look up
   * @return the JarEntry if it exists, null otherwise
   */
  public JarEntry getJarEntry(String name)
  {
    return (JarEntry) getEntry(name);
  }

  /**
   * Returns the manifest for this JarFile or null when the JarFile does not
   * contain a manifest file.
   */
  public synchronized Manifest getManifest() throws IOException
  {
    if (!manifestRead)
      manifest = readManifest();

    return manifest;
  }

  // Only called with lock on this JarFile.
  // Package private for use in inner classes.
  void readSignatures() throws IOException
  {
    Map pkcs7Dsa = new HashMap();
    Map pkcs7Rsa = new HashMap();
    Map sigFiles = new HashMap();

    // Phase 1: Read all signature files. These contain the user
    // certificates as well as the signatures themselves.
    for (Enumeration e = super.entries(); e.hasMoreElements(); )
      {
        ZipEntry ze = (ZipEntry) e.nextElement();
        String name = ze.getName();
        if (name.startsWith(META_INF))
          {
            String alias = name.substring(META_INF.length());
            if (alias.lastIndexOf('.') >= 0)
              alias = alias.substring(0, alias.lastIndexOf('.'));

            if (name.endsWith(PKCS7_DSA_SUFFIX) || name.endsWith(PKCS7_RSA_SUFFIX))
              {
                if (DEBUG)
                  debug("reading PKCS7 info from " + name + ", alias=" + alias);
                PKCS7SignedData sig = null;
                try
                  {
                    sig = new PKCS7SignedData(super.getInputStream(ze));
                  }
                catch (CertificateException ce)
                  {
                    IOException ioe = new IOException("certificate parsing error");
                    ioe.initCause(ce);
                    throw ioe;
                  }
                catch (CRLException crle)
                  {
                    IOException ioe = new IOException("CRL parsing error");
                    ioe.initCause(crle);
                    throw ioe;
                  }
                if (name.endsWith(PKCS7_DSA_SUFFIX))
                  pkcs7Dsa.put(alias, sig);
                else if (name.endsWith(PKCS7_RSA_SUFFIX))
                  pkcs7Rsa.put(alias, sig);
              }
            else if (name.endsWith(SF_SUFFIX))
              {
                if (DEBUG)
                  debug("reading signature file for " + alias + ": " + name);
                Manifest sf = new Manifest(super.getInputStream(ze));
                sigFiles.put(alias, sf);
                if (DEBUG)
                  debug("result: " + sf);
              }
          }
      }

    // Phase 2: verify the signatures on any signature files.
    Set validCerts = new HashSet();
    Map entryCerts = new HashMap();
    for (Iterator it = sigFiles.entrySet().iterator(); it.hasNext(); )
      {
        int valid = 0;
        Map.Entry e = (Map.Entry) it.next();
        String alias = (String) e.getKey();

        PKCS7SignedData sig = (PKCS7SignedData) pkcs7Dsa.get(alias);
        if (sig != null)
          {
            Certificate[] certs = sig.getCertificates();
            Set signerInfos = sig.getSignerInfos();
            for (Iterator it2 = signerInfos.iterator(); it2.hasNext(); )
              verify(certs, (SignerInfo) it2.next(), alias, validCerts);
          }

        sig = (PKCS7SignedData) pkcs7Rsa.get(alias);
        if (sig != null)
          {
            Certificate[] certs = sig.getCertificates();
            Set signerInfos = sig.getSignerInfos();
            for (Iterator it2 = signerInfos.iterator(); it2.hasNext(); )
              verify(certs, (SignerInfo) it2.next(), alias, validCerts);
          }

        // It isn't a signature for anything. Punt it.
        if (validCerts.isEmpty())
          {
            it.remove();
            continue;
          }

        entryCerts.put(e.getValue(), new HashSet(validCerts));
        validCerts.clear();
      }

    // Phase 3: verify the signature file signatures against the manifest,
    // mapping the entry name to the target certificates.
    this.entryCerts = new HashMap();
    for (Iterator it = entryCerts.entrySet().iterator(); it.hasNext(); )
      {
        Map.Entry e = (Map.Entry) it.next();
        Manifest sigfile = (Manifest) e.getKey();
        Map entries = sigfile.getEntries();
        Set certificates = (Set) e.getValue();

        for (Iterator it2 = entries.entrySet().iterator(); it2.hasNext(); )
          {
            Map.Entry e2 = (Map.Entry) it2.next();
            String entryname = String.valueOf(e2.getKey());
            Attributes attr = (Attributes) e2.getValue();
            if (verifyHashes(entryname, attr))
              {
                if (DEBUG)
                  debug("entry " + entryname + " has certificates " + certificates);
                Set s = (Set) this.entryCerts.get(entryname);
                if (s != null)
                  s.addAll(certificates);
                else
                  this.entryCerts.put(entryname, new HashSet(certificates));
              }
          }
      }

    signaturesRead = true;
  }

  /**
   * Tell if the given signer info is over the given alias's signature file,
   * given one of the certificates specified.
   */
  private void verify(Certificate[] certs, SignerInfo signerInfo,
                      String alias, Set validCerts)
  {
    Signature sig = null;
    try
      {
        OID alg = signerInfo.getDigestEncryptionAlgorithmId();
        if (alg.equals(DSA_ENCRYPTION_OID))
          {
            if (!signerInfo.getDigestAlgorithmId().equals(SHA1_OID))
              return;
            sig = Signature.getInstance("SHA1withDSA");
          }
        else if (alg.equals(RSA_ENCRYPTION_OID))
          {
            OID hash = signerInfo.getDigestAlgorithmId();
            if (hash.equals(MD2_OID))
              sig = Signature.getInstance("md2WithRsaEncryption");
            else if (hash.equals(MD4_OID))
              sig = Signature.getInstance("md4WithRsaEncryption");
            else if (hash.equals(MD5_OID))
              sig = Signature.getInstance("md5WithRsaEncryption");
            else if (hash.equals(SHA1_OID))
              sig = Signature.getInstance("sha1WithRsaEncryption");
            else
              return;
          }
        else
          {
            if (DEBUG)
              debug("unsupported signature algorithm: " + alg);
            return;
          }
      }
    catch (NoSuchAlgorithmException nsae)
      {
        if (DEBUG)
          {
            debug(nsae);
            nsae.printStackTrace();
          }
        return;
      }
    ZipEntry sigFileEntry = super.getEntry(META_INF + alias + SF_SUFFIX);
    if (sigFileEntry == null)
      return;
    for (int i = 0; i < certs.length; i++)
      {
        if (!(certs[i] instanceof X509Certificate))
          continue;
        X509Certificate cert = (X509Certificate) certs[i];
        if (!cert.getIssuerX500Principal().equals(signerInfo.getIssuer()) ||
            !cert.getSerialNumber().equals(signerInfo.getSerialNumber()))
          continue;
        try
          {
            sig.initVerify(cert.getPublicKey());
            InputStream in = super.getInputStream(sigFileEntry);
            if (in == null)
              continue;
            byte[] buf = new byte[1024];
            int len = 0;
            while ((len = in.read(buf)) != -1)
              sig.update(buf, 0, len);
            if (sig.verify(signerInfo.getEncryptedDigest()))
              {
                if (DEBUG)
                  debug("signature for " + cert.getSubjectDN() + " is good");
                validCerts.add(cert);
              }
          }
        catch (IOException ioe)
          {
            continue;
          }
        catch (InvalidKeyException ike)
          {
            continue;
          }
        catch (SignatureException se)
          {
            continue;
          }
      }
  }

  /**
   * Verifies that the digest(s) in a signature file were, in fact, made
   * over the manifest entry for ENTRY.
   *
   * @param entry The entry name.
   * @param attr The attributes from the signature file to verify.
   */
  private boolean verifyHashes(String entry, Attributes attr)
  {
    int verified = 0;

    // The bytes for ENTRY's manifest entry, which are signed in the
    // signature file.
    byte[] entryBytes = null;
    try
      {
	ZipEntry e = super.getEntry(entry);
	if (e == null)
	  {
	    if (DEBUG)
	      debug("verifyHashes: no entry '" + entry + "'");
	    return false;
	  }
        entryBytes = readManifestEntry(e);
      }
    catch (IOException ioe)
      {
        if (DEBUG)
          {
            debug(ioe);
            ioe.printStackTrace();
          }
        return false;
      }

    for (Iterator it = attr.entrySet().iterator(); it.hasNext(); )
      {
        Map.Entry e = (Map.Entry) it.next();
        String key = String.valueOf(e.getKey());
        if (!key.endsWith(DIGEST_KEY_SUFFIX))
          continue;
        String alg = key.substring(0, key.length() - DIGEST_KEY_SUFFIX.length());
        try
          {
            byte[] hash = Base64InputStream.decode((String) e.getValue());
            MessageDigest md = MessageDigest.getInstance(alg);
            md.update(entryBytes);
            byte[] hash2 = md.digest();
            if (DEBUG)
              debug("verifying SF entry " + entry + " alg: " + md.getAlgorithm()
                    + " expect=" + new java.math.BigInteger(hash).toString(16)
                    + " comp=" + new java.math.BigInteger(hash2).toString(16));
            if (!Arrays.equals(hash, hash2))
              return false;
            verified++;
          }
        catch (IOException ioe)
          {
            if (DEBUG)
              {
                debug(ioe);
                ioe.printStackTrace();
              }
            return false;
          }
        catch (NoSuchAlgorithmException nsae)
          {
            if (DEBUG)
              {
                debug(nsae);
                nsae.printStackTrace();
              }
            return false;
          }
      }

    // We have to find at least one valid digest.
    return verified > 0;
  }

  /**
   * Read the raw bytes that comprise a manifest entry. We can't use the
   * Manifest object itself, because that loses information (such as line
   * endings, and order of entries).
   */
  private byte[] readManifestEntry(ZipEntry entry) throws IOException
  {
    InputStream in = super.getInputStream(super.getEntry(MANIFEST_NAME));
    ByteArrayOutputStream out = new ByteArrayOutputStream();
    byte[] target = ("Name: " + entry.getName()).getBytes();
    int t = 0, c, prev = -1, state = 0, l = -1;

    while ((c = in.read()) != -1)
      {
//         if (DEBUG)
//           debug("read "
//                 + (c == '\n' ? "\\n" : (c == '\r' ? "\\r" : String.valueOf((char) c)))
//                 + " state=" + state + " prev="
//                 + (prev == '\n' ? "\\n" : (prev == '\r' ? "\\r" : String.valueOf((char) prev)))
//                 + " t=" + t + (t < target.length ? (" target[t]=" + (char) target[t]) : "")
//                 + " l=" + l);
        switch (state)
          {

          // Step 1: read until we find the "target" bytes: the start
          // of the entry we need to read.
          case 0:
            if (((byte) c) != target[t])
              t = 0;
            else
              {
                t++;
                if (t == target.length)
                  {
                    out.write(target);
                    state = 1;
                  }
              }
            break;

          // Step 2: assert that there is a newline character after
          // the "target" bytes.
          case 1:
            if (c != '\n' && c != '\r')
              {
                out.reset();
                t = 0;
                state = 0;
              }
            else
              {
                out.write(c);
                state = 2;
              }
            break;

          // Step 3: read this whole entry, until we reach an empty
          // line.
          case 2:
            if (c == '\n')
              {
                out.write(c);
                // NL always terminates a line.
                if (l == 0 || (l == 1 && prev == '\r'))
                  return out.toByteArray();
                l = 0;
              }
            else
              {
                // Here we see a blank line terminated by a CR,
                // followed by the next entry. Technically, `c' should
                // always be 'N' at this point.
                if (l == 1 && prev == '\r')
                  return out.toByteArray();
                out.write(c);
                l++;
              }
            prev = c;
            break;

          default:
            throw new RuntimeException("this statement should be unreachable");
          }
      }

    // The last entry, with a single CR terminating the line.
    if (state == 2 && prev == '\r' && l == 0)
      return out.toByteArray();

    // We should not reach this point, we didn't find the entry (or, possibly,
    // it is the last entry and is malformed).
    throw new IOException("could not find " + entry + " in manifest");
  }

  /**
   * A utility class that verifies jar entries as they are read.
   */
  private static class EntryInputStream extends FilterInputStream
  {
    private final JarFile jarfile;
    private final long length;
    private long pos;
    private final ZipEntry entry;
    private final byte[][] hashes;
    private final MessageDigest[] md;
    private boolean checked;

    EntryInputStream(final ZipEntry entry,
		     final InputStream in,
		     final JarFile jar)
      throws IOException
    {
      super(in);
      this.entry = entry;
      this.jarfile = jar;

      length = entry.getSize();
      pos = 0;
      checked = false;

      Attributes attr;
      Manifest manifest = jarfile.getManifest();
      if (manifest != null)
	attr = manifest.getAttributes(entry.getName());
      else
	attr = null;
      if (DEBUG)
        debug("verifying entry " + entry + " attr=" + attr);
      if (attr == null)
        {
          hashes = new byte[0][];
          md = new MessageDigest[0];
        }
      else
        {
          List hashes = new LinkedList();
          List md = new LinkedList();
          for (Iterator it = attr.entrySet().iterator(); it.hasNext(); )
            {
              Map.Entry e = (Map.Entry) it.next();
              String key = String.valueOf(e.getKey());
              if (key == null)
                continue;
              if (!key.endsWith(DIGEST_KEY_SUFFIX))
                continue;
              hashes.add(Base64InputStream.decode((String) e.getValue()));
              try
                {
                  md.add(MessageDigest.getInstance
                         (key.substring(0, key.length() - DIGEST_KEY_SUFFIX.length())));
                }
              catch (NoSuchAlgorithmException nsae)
                {
                  IOException ioe = new IOException("no such message digest: " + key);
                  ioe.initCause(nsae);
                  throw ioe;
                }
            }
          if (DEBUG)
            debug("digests=" + md);
          this.hashes = (byte[][]) hashes.toArray(new byte[hashes.size()][]);
          this.md = (MessageDigest[]) md.toArray(new MessageDigest[md.size()]);
        }
    }

    public boolean markSupported()
    {
      return false;
    }

    public void mark(int readLimit)
    {
    }

    public void reset()
    {
    }

    public int read() throws IOException
    {
      int b = super.read();
      if (b == -1)
        {
          eof();
          return -1;
        }
      for (int i = 0; i < md.length; i++)
        md[i].update((byte) b);
      pos++;
      if (length > 0 && pos >= length)
        eof();
      return b;
    }

    public int read(byte[] buf, int off, int len) throws IOException
    {
      int count = super.read(buf, off, (int) Math.min(len, (length != 0
                                                            ? length - pos
                                                            : Integer.MAX_VALUE)));
      if (count == -1 || (length > 0 && pos >= length))
        {
          eof();
          return -1;
        }
      for (int i = 0; i < md.length; i++)
        md[i].update(buf, off, count);
      pos += count;
      if (length != 0 && pos >= length)
        eof();
      return count;
    }

    public int read(byte[] buf) throws IOException
    {
      return read(buf, 0, buf.length);
    }

    public long skip(long bytes) throws IOException
    {
      byte[] b = new byte[1024];
      long amount = 0;
      while (amount < bytes)
        {
          int l = read(b, 0, (int) Math.min(b.length, bytes - amount));
          if (l == -1)
            break;
          amount += l;
        }
      return amount;
    }

    private void eof() throws IOException
    {
      if (checked)
        return;
      checked = true;
      for (int i = 0; i < md.length; i++)
        {
          byte[] hash = md[i].digest();
          if (DEBUG)
            debug("verifying " + md[i].getAlgorithm() + " expect="
                  + new java.math.BigInteger(hashes[i]).toString(16)
                  + " comp=" + new java.math.BigInteger(hash).toString(16));
          if (!Arrays.equals(hash, hashes[i]))
            {
	      synchronized(jarfile)
		{
		  if (DEBUG)
		    debug(entry + " could NOT be verified");
		  jarfile.verified.put(entry.getName(), Boolean.FALSE);
		}
	      return;
	      // XXX ??? what do we do here?
	      // throw new ZipException("message digest mismatch");
            }
        }

      synchronized(jarfile)
	{
	  if (DEBUG)
	    debug(entry + " has been VERIFIED");
	  jarfile.verified.put(entry.getName(), Boolean.TRUE);
	}
    }
  }
}
