/* Attributes.java -- Reads, writes and manipulaties jar manifest files
   Copyright (C) 2000 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.Writer;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

/**
 * Reads, writes and manipulaties jar manifest files.
 * XXX
 * 
 * @since 1.2
 * @author Mark Wielaard (mark@klomp.org)
 */
public class Manifest implements Cloneable
{
  // Fields

  /** The main attributes of the manifest (jar file). */
  private final Attributes mainAttr;

  /** A map of atrributes for all entries described in this Manifest. */
  private final Map entries;

  // Constructors

  /**
   * Creates a new empty Manifest.
   */
  public Manifest()
  {
    mainAttr = new Attributes();
    entries = new Hashtable();
  }

  /**
   * Creates a Manifest from the supplied input stream.
   *
   * @see read(Inputstream)
   * @see write(OutputStream)
   *
   * @param InputStream the input stream to read the manifest from
   * @exception IOException when an i/o exception occurs or the input stream
   * does not describe a valid manifest
   */
  public Manifest(InputStream in) throws IOException
  {
    this();
    read(in);
  }

  /**
   * Creates a Manifest from another Manifest.
   * Makes a deep copy of the main attributes, but a shallow copy of
   * the other entries. This means that you can freely add, change or remove
   * the main attributes or the entries of the new manifest without effecting
   * the original manifest, but adding, changing or removing attributes from
   * a particular entry also changes the attributes of that entry in the
   * original manifest.
   *
   * @see clone()
   * @param man the Manifest to copy from
   */
  public Manifest(Manifest man)
  {
    mainAttr = new Attributes(man.getMainAttributes());
    entries = new Hashtable(man.getEntries());
  }

  // Methods

  /**
   * Gets the main attributes of this Manifest.
   */
  public Attributes getMainAttributes()
  {
    return mainAttr;
  }

  /**
   * Gets a map of entry Strings to Attributes for all the entries described
   * in this manifest. Adding, changing or removing from this entries map
   * changes the entries of this manifest.
   */
  public Map getEntries()
  {
    return entries;
  }

  /**
   * Returns the Attributes associated with the Entry.
   * <p>
   * Implemented as:
   * <code>return (Attributes)getEntries().get(entryName)</code>
   *
   * @param entryName the name of the entry to look up
   * @return the attributes associated with the entry or null when none
   */
  public Attributes getAttributes(String entryName)
  {
    return (Attributes) getEntries().get(entryName);
  }

  /**
   * Clears the main attributes and removes all the entries from the
   * manifest.
   */
  public void clear()
  {
    mainAttr.clear();
    entries.clear();
  }

  /**
   * XXX
   */
  public void read(InputStream in) throws IOException
  {
    BufferedReader br =
      new BufferedReader(new InputStreamReader(in, "8859_1"));
    read_main_section(getMainAttributes(), br);
    read_individual_sections(getEntries(), br);
  }

  // Private Static methods for reading the Manifest file from BufferedReader

  private static void read_main_section(Attributes attr,
					BufferedReader br) throws IOException
  {
    // According to the spec we should actually call read_version_info() here.
    read_attributes(attr, br);
    // Explicitly set Manifest-Version attribute if not set in Main
    // attributes of Manifest.
    if (attr.getValue(Attributes.Name.MANIFEST_VERSION) == null)
	    attr.putValue(Attributes.Name.MANIFEST_VERSION, "0.0");
  }

  /**
   * Pedantic method that requires the next attribute in the Manifest to be
   * the "Manifest-Version". This follows the Manifest spec closely but
   * reject some jar Manifest files out in the wild.
   */
  private static void read_version_info(Attributes attr,
					BufferedReader br) throws IOException
  {
    String version_header = Attributes.Name.MANIFEST_VERSION.toString();
    try
      {
	String value = expect_header(version_header, br);
	attr.putValue(Attributes.Name.MANIFEST_VERSION, value);
      }
    catch (IOException ioe)
      {
	throw new JarException("Manifest should start with a " +
			       version_header + ": " + ioe.getMessage());
      }
  }

  private static String expect_header(String header, BufferedReader br)
    throws IOException
  {
    String s = br.readLine();
    if (s == null)
      {
	throw new JarException("unexpected end of file");
      }
    return expect_header(header, br, s);
  }

  private static String expect_header(String header, BufferedReader br,
				      String s) throws IOException
  {
    try
      {
	String name = s.substring(0, header.length() + 1);
	if (name.equalsIgnoreCase(header + ":"))
	  {
	    String value_start = s.substring(header.length() + 2);
	    return read_header_value(value_start, br);
	  }
      }
    catch (IndexOutOfBoundsException iobe)
      {
      }
    // If we arrive here, something went wrong
    throw new JarException("unexpected '" + s + "'");
  }

  private static String read_header_value(String s, BufferedReader br)
    throws IOException
  {
    boolean try_next = true;
    while (try_next)
      {
	// Lets see if there is something on the next line
	br.mark(1);
	if (br.read() == ' ')
	  {
	    s += br.readLine();
	  }
	else
	  {
	    br.reset();
	    try_next = false;
	  }
      }
    return s;
  }

  private static void read_attributes(Attributes attr,
				      BufferedReader br) throws IOException
  {
    String s = br.readLine();
    while (s != null && (!s.equals("")))
      {
	read_attribute(attr, s, br);
	s = br.readLine();
      }
  }

  private static void read_attribute(Attributes attr, String s,
				     BufferedReader br) throws IOException
  {
    try
      {
	int colon = s.indexOf(": ");
	String name = s.substring(0, colon);
	String value_start = s.substring(colon + 2);
	String value = read_header_value(value_start, br);
	attr.putValue(name, value);
      }
    catch (IndexOutOfBoundsException iobe)
      {
	throw new JarException("Manifest contains a bad header: " + s);
      }
  }

  private static void read_individual_sections(Map entries,
					       BufferedReader br) throws
    IOException
  {
    String s = br.readLine();
    while (s != null && (!s.equals("")))
      {
	Attributes attr = read_section_name(s, br, entries);
	read_attributes(attr, br);
	s = br.readLine();
      }
  }

  private static Attributes read_section_name(String s, BufferedReader br,
					      Map entries) throws JarException
  {
    try
      {
	String name = expect_header("Name", br, s);
	Attributes attr = new Attributes();
	entries.put(name, attr);
	return attr;
      }
    catch (IOException ioe)
      {
	throw new JarException
	  ("Section should start with a Name header: " + ioe.getMessage());
      }
  }

  /**
   * XXX
   */
  public void write(OutputStream out) throws IOException
  {
    PrintWriter pw =
      new PrintWriter(new
		      BufferedWriter(new OutputStreamWriter(out, "8859_1")));
    write_main_section(getMainAttributes(), pw);
    pw.println();
    write_individual_sections(getEntries(), pw);
    if (pw.checkError())
      {
	throw new JarException("Error while writing manifest");
      }
  }

  // Private Static functions for writing the Manifest file to a PrintWriter

  private static void write_main_section(Attributes attr,
					 PrintWriter pw) throws JarException
  {
    write_version_info(attr, pw);
    write_main_attributes(attr, pw);
  }

  private static void write_version_info(Attributes attr, PrintWriter pw)
  {
    // First check if there is already a version attribute set
    String version = attr.getValue(Attributes.Name.MANIFEST_VERSION);
    if (version == null)
      {
	version = "1.0";
      }
    write_header(Attributes.Name.MANIFEST_VERSION.toString(), version, pw);
  }

  private static void write_header(String name, String value, PrintWriter pw)
  {
    pw.print(name + ": ");

    int last = 68 - name.length();
    if (last > value.length())
      {
	pw.println(value);
      }
    else
      {
	pw.println(value.substring(0, last));
      }
    while (last < value.length())
      {
	pw.print(" ");
	int end = (last + 69);
	if (end > value.length())
	  {
	    pw.println(value.substring(last));
	  }
	else
	  {
	    pw.println(value.substring(last, end));
	  }
	last = end;
      }
  }

  private static void write_main_attributes(Attributes attr, PrintWriter pw) 
    throws JarException
  {
    Iterator it = attr.entrySet().iterator();
    while (it.hasNext())
      {
	Map.Entry entry = (Map.Entry) it.next();
	// Don't print the manifest version again
	if (!Attributes.Name.MANIFEST_VERSION.equals(entry.getKey()))
	  {
	    write_attribute_entry(entry, pw);
	  }
      }
  }

  private static void write_attribute_entry(Map.Entry entry, PrintWriter pw) 
    throws JarException
  {
    String name = entry.getKey().toString();
    String value = entry.getValue().toString();

    if (name.equalsIgnoreCase("Name"))
      {
	throw new JarException("Attributes cannot be called 'Name'");
      }
    if (name.startsWith("From"))
      {
	throw new
	  JarException("Header cannot start with the four letters 'From'" +
		       name);
      }
    write_header(name, value, pw);
  }

  private static void write_individual_sections(Map entries, PrintWriter pw)
    throws JarException
  {

    Iterator it = entries.entrySet().iterator();
    while (it.hasNext())
      {
	Map.Entry entry = (Map.Entry) it.next();
	write_header("Name", entry.getKey().toString(), pw);
	write_entry_attributes((Attributes) entry.getValue(), pw);
	pw.println();
      }
  }

  private static void write_entry_attributes(Attributes attr, PrintWriter pw) 
    throws JarException
  {
    Iterator it = attr.entrySet().iterator();
    while (it.hasNext())
      {
	Map.Entry entry = (Map.Entry) it.next();
	write_attribute_entry(entry, pw);
      }
  }

  /**
   * Makes a deep copy of the main attributes, but a shallow copy of
   * the other entries. This means that you can freely add, change or remove
   * the main attributes or the entries of the new manifest without effecting
   * the original manifest, but adding, changing or removing attributes from
   * a particular entry also changes the attributes of that entry in the
   * original manifest. Calls <CODE>new Manifest(this)</CODE>.
   */
  public Object clone()
  {
    return new Manifest(this);
  }

  /**
   * Checks if another object is equal to this Manifest object.
   * Another Object is equal to this Manifest object if it is an instance of
   * Manifest and the main attributes and the entries of the other manifest
   * are equal to this one.
   */
  public boolean equals(Object o)
  {
    return (o instanceof Manifest) &&
      (mainAttr.equals(((Manifest) o).mainAttr)) &&
      (entries.equals(((Manifest) o).entries));
  }

  /**
   * Calculates the hash code of the manifest. Implemented by a xor of the
   * hash code of the main attributes with the hash code of the entries map.
   */
  public int hashCode()
  {
    return mainAttr.hashCode() ^ entries.hashCode();
  }

}
