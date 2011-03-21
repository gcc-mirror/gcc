/* JarUtils.java -- Utility methods for reading/writing Manifest[-like] files
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


package gnu.java.util.jar;

import gnu.classpath.SystemProperties;

import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.util.Iterator;
import java.util.Map;
import java.util.jar.Attributes;
import java.util.jar.JarException;
import java.util.jar.Attributes.Name;
import java.util.logging.Logger;

/**
 * Utility methods for reading and writing JAR <i>Manifest</i> and
 * <i>Manifest-like</i> files.
 * <p>
 * JAR-related files that resemble <i>Manifest</i> files are Signature files
 * (with an <code>.SF</code> extension) found in signed JARs.
 */
public abstract class JarUtils
{
  // We used to log here, but this causes problems during bootstrap,
  // and it didn't seem worthwhile to preserve this.  Still, this
  // might be useful for debugging.
  // private static final Logger log = Logger.getLogger(JarUtils.class.getName());
  public static final String META_INF = "META-INF/";
  public static final String DSA_SUFFIX = ".DSA";
  public static final String SF_SUFFIX = ".SF";
  public static final String NAME = "Name";

  /**
   * The original string representation of the manifest version attribute name.
   */
  public static final String MANIFEST_VERSION = "Manifest-Version";

  /**
   * The original string representation of the signature version attribute
   * name.
   */
  public static final String SIGNATURE_VERSION = "Signature-Version";

  /** Platform-independent line-ending. */
  public static final byte[] CRLF = new byte[] { 0x0D, 0x0A };
  private static final String DEFAULT_MF_VERSION = "1.0";
  private static final String DEFAULT_SF_VERSION = "1.0";
  private static final Name CREATED_BY = new Name("Created-By");
  private static final String CREATOR = SystemProperties.getProperty("java.version")
                                        + " ("
                                        + SystemProperties.getProperty("java.vendor")
                                        + ")";

  // default 0-arguments constructor

  // Methods for reading Manifest files from InputStream ----------------------

  public static void
  readMFManifest(Attributes attr, Map entries, InputStream in)
      throws IOException
  {
    BufferedReader br = new BufferedReader(new InputStreamReader(in, "UTF-8"));
    readMainSection(attr, br);
    readIndividualSections(entries, br);
  }

  public static void
  readSFManifest(Attributes attr, Map entries, InputStream in)
      throws IOException
  {
    BufferedReader br = new BufferedReader(new InputStreamReader(in, "UTF-8"));
    String version_header = Name.SIGNATURE_VERSION.toString();
    try
      {
        String version = expectHeader(version_header, br);
        attr.putValue(SIGNATURE_VERSION, version);
        // This may cause problems during VM bootstrap.
        // if (! DEFAULT_SF_VERSION.equals(version))
        //  log.warning("Unexpected version number: " + version
        //              + ". Continue (but may fail later)");
      }
    catch (IOException ioe)
      {
        throw new JarException("Signature file MUST start with a "
                               + version_header + ": " + ioe.getMessage());
      }
    read_attributes(attr, br);

    // read individual sections
    String s = br.readLine();
    while (s != null && s.length() > 0)
      {
        Attributes eAttr = readSectionName(s, br, entries);
        read_attributes(eAttr, br);
        s = br.readLine();
      }
  }

  private static void readMainSection(Attributes attr, BufferedReader br)
      throws IOException
  {
    // According to the spec we should actually call read_version_info() here.
    read_attributes(attr, br);
    // Explicitly set Manifest-Version attribute if not set in Main
    // attributes of Manifest.
    // XXX (rsn): why 0.0 and not 1.0?
    if (attr.getValue(Name.MANIFEST_VERSION) == null)
      attr.putValue(MANIFEST_VERSION, "0.0");
  }

  private static void readIndividualSections(Map entries, BufferedReader br)
      throws IOException
  {
    String s = br.readLine();
    while (s != null && (! s.equals("")))
      {
        Attributes attr = readSectionName(s, br, entries);
        read_attributes(attr, br);
        s = br.readLine();
      }
  }

  /**
   * Pedantic method that requires the next attribute in the Manifest to be the
   * "Manifest-Version". This follows the Manifest spec closely but reject some
   * jar Manifest files out in the wild.
   */
  private static void readVersionInfo(Attributes attr, BufferedReader br)
      throws IOException
  {
    String version_header = Name.MANIFEST_VERSION.toString();
    try
      {
        String value = expectHeader(version_header, br);
        attr.putValue(MANIFEST_VERSION, value);
      }
    catch (IOException ioe)
      {
        throw new JarException("Manifest should start with a " + version_header
                               + ": " + ioe.getMessage());
      }
  }

  private static String expectHeader(String header, BufferedReader br)
      throws IOException
  {
    String s = br.readLine();
    if (s == null)
      throw new JarException("unexpected end of file");

    return expectHeader(header, br, s);
  }

  private static void read_attributes(Attributes attr, BufferedReader br)
      throws IOException
  {
    String s = br.readLine();
    while (s != null && (! s.equals("")))
      {
        readAttribute(attr, s, br);
        s = br.readLine();
      }
  }

  private static void
  readAttribute(Attributes attr, String s, BufferedReader br) throws IOException
  {
    try
      {
        int colon = s.indexOf(": ");
        String name = s.substring(0, colon);
        String value_start = s.substring(colon + 2);
        String value = readHeaderValue(value_start, br);
        attr.putValue(name, value);
      }
    catch (IndexOutOfBoundsException iobe)
      {
        throw new JarException("Manifest contains a bad header: " + s);
      }
  }

  private static String readHeaderValue(String s, BufferedReader br)
      throws IOException
  {
    boolean try_next = true;
    while (try_next)
      {
        // Lets see if there is something on the next line
        br.mark(1);
        if (br.read() == ' ')
          s += br.readLine();
        else
          {
            br.reset();
            try_next = false;
          }
      }
    return s;
  }

  private static Attributes
  readSectionName(String s, BufferedReader br, Map entries) throws JarException
  {
    try
      {
        String name = expectHeader(NAME, br, s);
        Attributes attr = new Attributes();
        entries.put(name, attr);
        return attr;
      }
    catch (IOException ioe)
      {
        throw new JarException("Section should start with a Name header: "
                               + ioe.getMessage());
      }
  }

  private static String expectHeader(String header, BufferedReader br, String s)
      throws IOException
  {
    try
      {
        String name = s.substring(0, header.length() + 1);
        if (name.equalsIgnoreCase(header + ":"))
          {
            String value_start = s.substring(header.length() + 2);
            return readHeaderValue(value_start, br);
          }
      }
    catch (IndexOutOfBoundsException ignored)
      {
      }
    // If we arrive here, something went wrong
    throw new JarException("unexpected '" + s + "'");
  }

  // Methods for writing Manifest files to an OutputStream --------------------

  public static void
  writeMFManifest(Attributes attr, Map entries, OutputStream stream)
      throws IOException
  {
    BufferedOutputStream out = stream instanceof BufferedOutputStream
                               ? (BufferedOutputStream) stream
                               : new BufferedOutputStream(stream, 4096);
    writeVersionInfo(attr, out);
    Iterator i;
    Map.Entry e;
    for (i = attr.entrySet().iterator(); i.hasNext();)
      {
        e = (Map.Entry) i.next();
        // Don't print the manifest version again
        if (! Name.MANIFEST_VERSION.equals(e.getKey()))
          writeAttributeEntry(e, out);
      }
    out.write(CRLF);

    Iterator j;
    for (i = entries.entrySet().iterator(); i.hasNext();)
      {
        e = (Map.Entry) i.next();
        writeHeader(NAME, e.getKey().toString(), out);
        Attributes eAttr = (Attributes) e.getValue();
        for (j = eAttr.entrySet().iterator(); j.hasNext();)
          {
            Map.Entry e2 = (Map.Entry) j.next();
            writeAttributeEntry(e2, out);
          }
        out.write(CRLF);
      }

    out.flush();
  }

  public static void
  writeSFManifest(Attributes attr, Map entries, OutputStream stream)
      throws IOException
  {
    BufferedOutputStream out = stream instanceof BufferedOutputStream
                               ? (BufferedOutputStream) stream
                               : new BufferedOutputStream(stream, 4096);
    writeHeader(Name.SIGNATURE_VERSION.toString(), DEFAULT_SF_VERSION, out);
    writeHeader(CREATED_BY.toString(), CREATOR, out);
    Iterator i;
    Map.Entry e;
    for (i = attr.entrySet().iterator(); i.hasNext();)
      {
        e = (Map.Entry) i.next();
        Name name = (Name) e.getKey();
        if (Name.SIGNATURE_VERSION.equals(name) || CREATED_BY.equals(name))
          continue;

        writeHeader(name.toString(), (String) e.getValue(), out);
      }
    out.write(CRLF);

    Iterator j;
    for (i = entries.entrySet().iterator(); i.hasNext();)
      {
        e = (Map.Entry) i.next();
        writeHeader(NAME, e.getKey().toString(), out);
        Attributes eAttr = (Attributes) e.getValue();
        for (j = eAttr.entrySet().iterator(); j.hasNext();)
          {
            Map.Entry e2 = (Map.Entry) j.next();
            writeHeader(e2.getKey().toString(), (String) e2.getValue(), out);
          }
        out.write(CRLF);
      }

    out.flush();
  }

  private static void writeVersionInfo(Attributes attr, OutputStream out)
      throws IOException
  {
    // First check if there is already a version attribute set
    String version = attr.getValue(Name.MANIFEST_VERSION);
    if (version == null)
      version = DEFAULT_MF_VERSION;

    writeHeader(Name.MANIFEST_VERSION.toString(), version, out);
  }

  private static void writeAttributeEntry(Map.Entry entry, OutputStream out)
      throws IOException
  {
    String name = entry.getKey().toString();
    String value = entry.getValue().toString();
    if (name.equalsIgnoreCase(NAME))
      throw new JarException("Attributes cannot be called 'Name'");

    if (name.startsWith("From"))
      throw new JarException("Header cannot start with the four letters 'From'"
                             + name);

    writeHeader(name, value, out);
  }

  /**
   * The basic method for writing <code>Mainfest</code> attributes. This
   * implementation respects the rule stated in the Jar Specification concerning
   * the maximum allowed line length; i.e.
   *
   * <pre>
   * No line may be longer than 72 bytes (not characters), in its UTF8-encoded
   * form. If a value would make the initial line longer than this, it should
   * be continued on extra lines (each starting with a single SPACE).
   * </pre>
   *
   * and
   *
   * <pre>
   * Because header names cannot be continued, the maximum length of a header
   * name is 70 bytes (there must be a colon and a SPACE after the name).
   * </pre>
   *
   * @param name the name of the attribute.
   * @param value the value of the attribute.
   * @param out the output stream to write the attribute's name/value pair to.
   * @throws IOException if an I/O related exception occurs during the process.
   */
  private static void writeHeader(String name, String value, OutputStream out)
      throws IOException
  {
    String target = name + ": ";
    byte[] b = target.getBytes("UTF-8");
    if (b.length > 72)
      throw new IOException("Attribute's name already longer than 70 bytes");

    if (b.length == 72)
      {
        out.write(b);
        out.write(CRLF);
        target = " " + value;
      }
    else
      target = target + value;

    int n;
    while (true)
      {
        b = target.getBytes("UTF-8");
        if (b.length < 73)
          {
            out.write(b);
            break;
          }

        // find an appropriate character position to break on
        n = 72;
        while (true)
          {
            b = target.substring(0, n).getBytes("UTF-8");
            if (b.length < 73)
              break;

            n--;
            if (n < 1)
              throw new IOException("Header is unbreakable and longer than 72 bytes");
          }

        out.write(b);
        out.write(CRLF);
        target = " " + target.substring(n);
      }

    out.write(CRLF);
  }
}
