/* Properties.java -- a set of persistent properties
   Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005  Free Software Foundation, Inc.

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


package java.util;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintStream;
import java.io.PrintWriter;

/**
 * A set of persistent properties, which can be saved or loaded from a stream.
 * A property list may also contain defaults, searched if the main list
 * does not contain a property for a given key.
 *
 * An example of a properties file for the german language is given
 * here.  This extends the example given in ListResourceBundle.
 * Create a file MyResource_de.properties with the following contents
 * and put it in the CLASSPATH.  (The character
 * <code>\</code><code>u00e4</code> is the german umlaut)
 *
 * 
<pre>s1=3
s2=MeineDisk
s3=3. M\<code></code>u00e4rz 96
s4=Die Diskette ''{1}'' enth\<code></code>u00e4lt {0} in {2}.
s5=0
s6=keine Dateien
s7=1
s8=eine Datei
s9=2
s10={0,number} Dateien
s11=Das Formatieren schlug fehl mit folgender Exception: {0}
s12=FEHLER
s13=Ergebnis
s14=Dialog
s15=Auswahlkriterium
s16=1,3</pre>
 *
 * <p>Although this is a sub class of a hash table, you should never
 * insert anything other than strings to this property, or several
 * methods, that need string keys and values, will fail.  To ensure
 * this, you should use the <code>get/setProperty</code> method instead
 * of <code>get/put</code>.
 *
 * Properties are saved in ISO 8859-1 encoding, using Unicode escapes with
 * a single <code>u</code> for any character which cannot be represented.
 *
 * @author Jochen Hoenicke
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see PropertyResourceBundle
 * @status updated to 1.4
 */
public class Properties extends Hashtable
{
  // WARNING: Properties is a CORE class in the bootstrap cycle. See the
  // comments in vm/reference/java/lang/Runtime for implications of this fact.

  /**
   * The property list that contains default values for any keys not
   * in this property list.
   *
   * @serial the default properties
   */
  protected Properties defaults;

  /**
   * Compatible with JDK 1.0+.
   */
  private static final long serialVersionUID = 4112578634029874840L;

  /**
   * Creates a new empty property list with no default values.
   */
  public Properties()
  {
  }

  /**
   * Create a new empty property list with the specified default values.
   *
   * @param defaults a Properties object containing the default values
   */
  public Properties(Properties defaults)
  {
    this.defaults = defaults;
  }

  /**
   * Adds the given key/value pair to this properties.  This calls
   * the hashtable method put.
   *
   * @param key the key for this property
   * @param value the value for this property
   * @return The old value for the given key
   * @see #getProperty(String)
   * @since 1.2
   */
  public Object setProperty(String key, String value)
  {
    return put(key, value);
  }

  /**
   * Reads a property list from an input stream.  The stream should
   * have the following format: <br>
   *
   * An empty line or a line starting with <code>#</code> or
   * <code>!</code> is ignored.  An backslash (<code>\</code>) at the
   * end of the line makes the line continueing on the next line
   * (but make sure there is no whitespace after the backslash).
   * Otherwise, each line describes a key/value pair. <br>
   *
   * The chars up to the first whitespace, = or : are the key.  You
   * can include this caracters in the key, if you precede them with
   * a backslash (<code>\</code>). The key is followed by optional
   * whitespaces, optionally one <code>=</code> or <code>:</code>,
   * and optionally some more whitespaces.  The rest of the line is
   * the resource belonging to the key. <br>
   *
   * Escape sequences <code>\t, \n, \r, \\, \", \', \!, \#, \ </code>(a
   * space), and unicode characters with the
   * <code>\\u</code><em>xxxx</em> notation are detected, and
   * converted to the corresponding single character. <br>
   *
   * 
<pre># This is a comment
key     = value
k\:5      \ a string starting with space and ending with newline\n
# This is a multiline specification; note that the value contains
# no white space.
weekdays: Sunday,Monday,Tuesday,Wednesday,\\
          Thursday,Friday,Saturday
# The safest way to include a space at the end of a value:
label   = Name:\\u0020</pre>
   *
   * @param inStream the input stream
   * @throws IOException if an error occurred when reading the input
   * @throws NullPointerException if in is null
   */
  public void load(InputStream inStream) throws IOException
  {
    // The spec says that the file must be encoded using ISO-8859-1.
    BufferedReader reader =
      new BufferedReader(new InputStreamReader(inStream, "ISO-8859-1"));
    String line;

    while ((line = reader.readLine()) != null)
      {
        char c = 0;
        int pos = 0;
	// Leading whitespaces must be deleted first.
        while (pos < line.length()
               && Character.isWhitespace(c = line.charAt(pos)))
          pos++;

        // If empty line or begins with a comment character, skip this line.
        if ((line.length() - pos) == 0
	    || line.charAt(pos) == '#' || line.charAt(pos) == '!')
          continue;

        // The characters up to the next Whitespace, ':', or '='
        // describe the key.  But look for escape sequences.
        StringBuffer key = new StringBuffer();
        while (pos < line.length()
               && ! Character.isWhitespace(c = line.charAt(pos++))
               && c != '=' && c != ':')
          {
            if (c == '\\')
              {
                if (pos == line.length())
                  {
                    // The line continues on the next line.  If there
                    // is no next line, just treat it as a key with an
                    // empty value.
                    line = reader.readLine();
		    if (line == null)
		      line = "";
                    pos = 0;
                    while (pos < line.length()
                           && Character.isWhitespace(c = line.charAt(pos)))
                      pos++;
                  }
                else
                  {
                    c = line.charAt(pos++);
                    switch (c)
                      {
                      case 'n':
                        key.append('\n');
                        break;
                      case 't':
                        key.append('\t');
                        break;
                      case 'r':
                        key.append('\r');
                        break;
                      case 'u':
                        if (pos + 4 <= line.length())
                          {
                            char uni = (char) Integer.parseInt
                              (line.substring(pos, pos + 4), 16);
                            key.append(uni);
                            pos += 4;
                          }        // else throw exception?
                        break;
                      default:
                        key.append(c);
                        break;
                      }
                  }
              }
            else
              key.append(c);
          }

        boolean isDelim = (c == ':' || c == '=');
        while (pos < line.length()
               && Character.isWhitespace(c = line.charAt(pos)))
          pos++;

        if (! isDelim && (c == ':' || c == '='))
          {
            pos++;
            while (pos < line.length()
                   && Character.isWhitespace(c = line.charAt(pos)))
              pos++;
          }

        StringBuffer element = new StringBuffer(line.length() - pos);
        while (pos < line.length())
          {
            c = line.charAt(pos++);
            if (c == '\\')
              {
                if (pos == line.length())
                  {
                    // The line continues on the next line.
                    line = reader.readLine();

		    // We might have seen a backslash at the end of
		    // the file.  The JDK ignores the backslash in
		    // this case, so we follow for compatibility.
		    if (line == null)
		      break;

                    pos = 0;
                    while (pos < line.length()
                           && Character.isWhitespace(c = line.charAt(pos)))
                      pos++;
                    element.ensureCapacity(line.length() - pos +
                                           element.length());
                  }
                else
                  {
                    c = line.charAt(pos++);
                    switch (c)
                      {
                      case 'n':
                        element.append('\n');
                        break;
                      case 't':
                        element.append('\t');
                        break;
                      case 'r':
                        element.append('\r');
                        break;
                      case 'u':
                        if (pos + 4 <= line.length())
                          {
                            char uni = (char) Integer.parseInt
                              (line.substring(pos, pos + 4), 16);
                            element.append(uni);
                            pos += 4;
                          }        // else throw exception?
                        break;
                      default:
                        element.append(c);
                        break;
                      }
                  }
              }
            else
              element.append(c);
          }
        put(key.toString(), element.toString());
      }
  }

  /**
   * Calls <code>store(OutputStream out, String header)</code> and
   * ignores the IOException that may be thrown.
   *
   * @param out the stream to write to
   * @param header a description of the property list
   * @throws ClassCastException if this property contains any key or
   *         value that are not strings
   * @deprecated use {@link #store(OutputStream, String)} instead
   */
  public void save(OutputStream out, String header)
  {
    try
      {
        store(out, header);
      }
    catch (IOException ex)
      {
      }
  }

  /**
   * Writes the key/value pairs to the given output stream, in a format
   * suitable for <code>load</code>.<br>
   *
   * If header is not null, this method writes a comment containing
   * the header as first line to the stream.  The next line (or first
   * line if header is null) contains a comment with the current date.
   * Afterwards the key/value pairs are written to the stream in the
   * following format.<br>
   *
   * Each line has the form <code>key = value</code>.  Newlines,
   * Returns and tabs are written as <code>\n,\t,\r</code> resp.
   * The characters <code>\, !, #, =</code> and <code>:</code> are
   * preceeded by a backslash.  Spaces are preceded with a backslash,
   * if and only if they are at the beginning of the key.  Characters
   * that are not in the ascii range 33 to 127 are written in the
   * <code>\</code><code>u</code>xxxx Form.<br>
   *
   * Following the listing, the output stream is flushed but left open.
   *
   * @param out the output stream
   * @param header the header written in the first line, may be null
   * @throws ClassCastException if this property contains any key or
   *         value that isn't a string
   * @throws IOException if writing to the stream fails
   * @throws NullPointerException if out is null
   * @since 1.2
   */
  public void store(OutputStream out, String header) throws IOException
  {
    // The spec says that the file must be encoded using ISO-8859-1.
    PrintWriter writer
      = new PrintWriter(new OutputStreamWriter(out, "ISO-8859-1"));
    if (header != null)
      writer.println("#" + header);
    writer.println ("#" + Calendar.getInstance ().getTime ());
    
    Iterator iter = entrySet ().iterator ();
    int i = size ();
    StringBuffer s = new StringBuffer (); // Reuse the same buffer.
    while (--i >= 0)
      {
        Map.Entry entry = (Map.Entry) iter.next ();
        formatForOutput ((String) entry.getKey (), s, true);
        s.append ('=');
        formatForOutput ((String) entry.getValue (), s, false);
        writer.println (s);
      }

    writer.flush ();
  }

  /**
   * Gets the property with the specified key in this property list.
   * If the key is not found, the default property list is searched.
   * If the property is not found in the default, null is returned.
   *
   * @param key The key for this property
   * @return the value for the given key, or null if not found
   * @throws ClassCastException if this property contains any key or
   *         value that isn't a string
   * @see #defaults
   * @see #setProperty(String, String)
   * @see #getProperty(String, String)
   */
  public String getProperty(String key)
  {
    Properties prop = this;
    // Eliminate tail recursion.
    do
      {
        String value = (String) prop.get(key);
        if (value != null)
          return value;
        prop = prop.defaults;
      }
    while (prop != null);
    return null;
  }

  /**
   * Gets the property with the specified key in this property list.  If
   * the key is not found, the default property list is searched.  If the
   * property is not found in the default, the specified defaultValue is
   * returned.
   *
   * @param key The key for this property
   * @param defaultValue A default value
   * @return The value for the given key
   * @throws ClassCastException if this property contains any key or
   *         value that isn't a string
   * @see #defaults
   * @see #setProperty(String, String)
   */
  public String getProperty(String key, String defaultValue)
  {
    String prop = getProperty(key);
    if (prop == null)
      prop = defaultValue;
    return prop;
  }

  /**
   * Returns an enumeration of all keys in this property list, including
   * the keys in the default property list.
   *
   * @return an Enumeration of all defined keys
   */
  public Enumeration propertyNames()
  {
    // We make a new Set that holds all the keys, then return an enumeration
    // for that. This prevents modifications from ruining the enumeration,
    // as well as ignoring duplicates.
    Properties prop = this;
    Set s = new HashSet();
    // Eliminate tail recursion.
    do
      {
        s.addAll(prop.keySet());
        prop = prop.defaults;
      }
    while (prop != null);
    return Collections.enumeration(s);
  }

  /**
   * Prints the key/value pairs to the given print stream.  This is 
   * mainly useful for debugging purposes.
   *
   * @param out the print stream, where the key/value pairs are written to
   * @throws ClassCastException if this property contains a key or a
   *         value that isn't a string
   * @see #list(PrintWriter)
   */
  public void list(PrintStream out)
  {
    PrintWriter writer = new PrintWriter (out);
    list (writer);
  }

  /**
   * Prints the key/value pairs to the given print writer.  This is
   * mainly useful for debugging purposes.
   *
   * @param out the print writer where the key/value pairs are written to
   * @throws ClassCastException if this property contains a key or a
   *         value that isn't a string
   * @see #list(PrintStream)
   * @since 1.1
   */
  public void list(PrintWriter out)
  {
    out.println ("-- listing properties --");

    Iterator iter = entrySet ().iterator ();
    int i = size ();
    while (--i >= 0)
      {
        Map.Entry entry = (Map.Entry) iter.next ();
        out.print ((String) entry.getKey () + "=");

        // JDK 1.3/1.4 restrict the printed value, but not the key,
        // to 40 characters, including the truncating ellipsis.
        String s = (String ) entry.getValue ();
        if (s != null && s.length () > 40)
          out.println (s.substring (0, 37) + "...");
        else
          out.println (s);
      }
    out.flush ();
  }

  /**
   * Formats a key or value for output in a properties file.
   * See store for a description of the format.
   *
   * @param str the string to format
   * @param buffer the buffer to add it to
   * @param key true if all ' ' must be escaped for the key, false if only
   *        leading spaces must be escaped for the value
   * @see #store(OutputStream, String)
   */
  private void formatForOutput(String str, StringBuffer buffer, boolean key)
  {
    if (key)
      {
        buffer.setLength(0);
        buffer.ensureCapacity(str.length());
      }
    else
      buffer.ensureCapacity(buffer.length() + str.length());
    boolean head = true;
    int size = str.length();
    for (int i = 0; i < size; i++)
      {
        char c = str.charAt(i);
        switch (c)
          {
          case '\n':
            buffer.append("\\n");
            break;
          case '\r':
            buffer.append("\\r");
            break;
          case '\t':
            buffer.append("\\t");
            break;
          case ' ':
            buffer.append(head ? "\\ " : " ");
            break;
          case '\\':
          case '!':
          case '#':
          case '=':
          case ':':
            buffer.append('\\').append(c);
            break;
          default:
            if (c < ' ' || c > '~')
              {
                String hex = Integer.toHexString(c);
                buffer.append("\\u0000".substring(0, 6 - hex.length()));
                buffer.append(hex);
              }
            else
              buffer.append(c);
          }
        if (c != ' ')
          head = key;
      }
  }
} // class Properties
