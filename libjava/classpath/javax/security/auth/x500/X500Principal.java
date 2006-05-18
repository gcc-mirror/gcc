/* X500Principal.java -- X.500 principal.
   Copyright (C) 2003, 2004, 2005 Free Software Foundation, Inc.

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


package javax.security.auth.x500;

import gnu.java.security.OID;
import gnu.java.security.der.DER;
import gnu.java.security.der.DERReader;
import gnu.java.security.der.DERValue;

import java.io.ByteArrayInputStream;
import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;
import java.io.NotActiveException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Reader;
import java.io.Serializable;
import java.io.StringReader;

import java.security.Principal;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

public final class X500Principal implements Principal, Serializable
{
  private static final long serialVersionUID = -500463348111345721L;

  // Constants and fields.
  // ------------------------------------------------------------------------

  public static final String CANONICAL = "CANONICAL";
  public static final String RFC1779 = "RFC1779";
  public static final String RFC2253 = "RFC2253";

  private static final OID CN         = new OID("2.5.4.3");
  private static final OID C          = new OID("2.5.4.6");
  private static final OID L          = new OID("2.5.4.7");
  private static final OID ST         = new OID("2.5.4.8");
  private static final OID STREET     = new OID("2.5.4.9");
  private static final OID O          = new OID("2.5.4.10");
  private static final OID OU         = new OID("2.5.4.11");
  private static final OID DC         = new OID("0.9.2342.19200300.100.1.25");
  private static final OID UID        = new OID("0.9.2342.19200300.100.1.1");

  private transient List components;
  private transient Map currentRdn;
  private transient boolean fixed;
  private transient byte[] encoded;

  // Constructors.
  // ------------------------------------------------------------------------

  private X500Principal()
  {
    components = new LinkedList();
    currentRdn = new LinkedHashMap();
    components.add (currentRdn);
  }

  public X500Principal (String name)
  {
    this();
    if (name == null)
      throw new NullPointerException();
    try
      {
        parseString (name);
      }
    catch (IOException ioe)
      {
        IllegalArgumentException iae = new IllegalArgumentException("malformed name");
        iae.initCause (ioe);
        throw iae;
      }
  }

  public X500Principal (byte[] encoded)
  {
    this(new ByteArrayInputStream (encoded));
  }

  public X500Principal (InputStream encoded)
  {
    this();
    try
      {
        parseDer (encoded);
      }
    catch (IOException ioe)
      {
        throw new IllegalArgumentException (ioe.toString());
      }
  }

  // Instance methods.
  // ------------------------------------------------------------------------

  public int hashCode()
  {
    int result = size();
    for (int i = 0; i < size(); ++i)
      {
        Map m = (Map) components.get(i);
        for (Iterator it2 = m.entrySet().iterator(); it2.hasNext(); )
          {
            Map.Entry e = (Map.Entry) it2.next();
            // We don't bother looking at the value of the entry.
            result = result * 31 + ((OID) e.getKey()).hashCode();
          }
      }
    return result;
  }

  public boolean equals(Object o)
  {
    if (!(o instanceof X500Principal))
      return false;
    if (size() != ((X500Principal) o).size())
      return false;
    for (int i = 0; i < size(); i++)
      {
        Map m = (Map) components.get (i);
        for (Iterator it2 = m.entrySet().iterator(); it2.hasNext(); )
          {
            Map.Entry e = (Map.Entry) it2.next();
            OID oid = (OID) e.getKey();
            String v1 = (String) e.getValue();
            String v2 = ((X500Principal) o).getComponent (oid, i);
            if (v2 == null)
              return false;
            if (!compressWS (v1).equalsIgnoreCase (compressWS (v2)))
              return false;
          }
      }
    return true;
  }

  public byte[] getEncoded()
  {
    if (encoded == null)
      encodeDer();
    return (byte[]) encoded.clone();
  }

  public String getName()
  {
    return getName (RFC2253);
  }

  public String getName (final String format)
  {
    boolean rfc2253 = RFC2253.equalsIgnoreCase (format) ||
      CANONICAL.equalsIgnoreCase (format);
    boolean rfc1779 = RFC1779.equalsIgnoreCase (format);
    boolean canon   = CANONICAL.equalsIgnoreCase (format);
    if (! (rfc2253 || rfc1779 || canon))
      throw new IllegalArgumentException ("unsupported format " + format);
    StringBuffer str = new StringBuffer();
    for (Iterator it = components.iterator(); it.hasNext(); )
      {
        Map m = (Map) it.next();
        for (Iterator it2 = m.entrySet().iterator(); it2.hasNext(); )
          {
            Map.Entry entry = (Map.Entry) it2.next();
            OID oid = (OID) entry.getKey();
            String value = (String) entry.getValue();
            if (oid.equals (CN))
              str.append ("CN");
            else if (oid.equals (C))
              str.append ("C");
            else if (oid.equals (L))
              str.append ("L");
            else if (oid.equals (ST))
              str.append ("ST");
            else if (oid.equals (STREET))
              str.append ("STREET");
            else if (oid.equals (O))
              str.append ("O");
            else if (oid.equals (OU))
              str.append ("OU");
            else if (oid.equals (DC) && rfc2253)
              str.append ("DC");
            else if (oid.equals (UID) && rfc2253)
              str.append ("UID");
            else
              str.append (oid.toString());
            str.append('=');
            str.append(value);
            if (it2.hasNext())
              str.append('+');
          }
        if (it.hasNext())
          str.append(',');
      }
    if (canon)
      return str.toString().toUpperCase (Locale.US).toLowerCase (Locale.US);
    return str.toString();
  }

  public String toString()
  {
    return getName (RFC2253);
  }

  // Serialization methods.
  // ------------------------------------------------------------------------

  private void writeObject (ObjectOutputStream out) throws IOException
  {
    if (encoded != null)
      encodeDer();
    out.writeObject (encoded);
  }

  private void readObject (ObjectInputStream in)
    throws IOException, NotActiveException, ClassNotFoundException
  {
    byte[] buf = (byte[]) in.readObject();
    parseDer (new ByteArrayInputStream (buf));
  }

  // Own methods.
  // -------------------------------------------------------------------------

  private int size()
  {
    return components.size();
  }

  private String getComponent(OID oid, int rdn)
  {
    if (rdn >= size())
      return null;
    return (String) ((Map) components.get (rdn)).get (oid);
  }

  private void encodeDer()
  {
    ArrayList name = new ArrayList(components.size());
    for (Iterator it = components.iterator(); it.hasNext(); )
      {
        Map m = (Map) it.next();
        if (m.isEmpty())
          continue;
        Set rdn = new HashSet();
        for (Iterator it2 = m.entrySet().iterator(); it2.hasNext(); )
          {
            Map.Entry e = (Map.Entry) it2.next();
            ArrayList atav = new ArrayList(2);
            atav.add(new DERValue(DER.OBJECT_IDENTIFIER, e.getKey()));
            atav.add(new DERValue(DER.UTF8_STRING, e.getValue()));
            rdn.add(new DERValue(DER.SEQUENCE|DER.CONSTRUCTED, atav));
          }
        name.add(new DERValue(DER.SET|DER.CONSTRUCTED, rdn));
      }
    DERValue val = new DERValue(DER.SEQUENCE|DER.CONSTRUCTED, name);
    encoded = val.getEncoded();
  }

  private int sep;

  private void parseString(String str) throws IOException
  {
    Reader in = new StringReader(str);
    while (true)
      {
        String key = readAttributeType(in);
        if (key == null)
          break;
        String value = readAttributeValue(in);
        putComponent(key, value);
        if (sep == ',')
          newRelativeDistinguishedName();
        if (sep == -1)
          break;
      }
  }

  private String readAttributeType(Reader in) throws IOException
  {
    StringBuffer buf = new StringBuffer();
    int ch;
    while ((ch = in.read()) != '=')
      {
        if (ch == -1)
          {
            if (buf.length() > 0)
              throw new EOFException("partial name read: " + buf);
            return null;
          }
        if (ch > 127)
          throw new IOException("Invalid char: " + (char) ch);
        if (Character.isLetterOrDigit((char) ch) || ch == '-' || ch == '.')
          buf.append((char) ch);
        else
          throw new IOException("Invalid char: " + (char) ch);
      }
    return buf.toString();
  }

  private String readAttributeValue(Reader in) throws IOException
  {
    StringBuffer buf = new StringBuffer();
    int ch = in.read();
    if (ch == '#')
      {
        while (true)
          {
            ch = in.read();
            if (('a' <= ch && ch <= 'f') || ('A' <= ch && ch <= 'F')
                || Character.isDigit((char) ch))
              buf.append((char) ch);
            else if (ch == '+' || ch == ',')
              {
                sep = ch;
                String hex = buf.toString();
                return new String(toByteArray(hex));
              }
            else
              throw new IOException("illegal character: " + (char) ch);
          }
      }
    else if (ch == '"')
      {
        while (true)
          {
            ch = in.read();
            if (ch == '"')
              break;
            else if (ch == '\\')
              {
                ch = in.read();
                if (ch == -1)
                  throw new EOFException();
                if (('a' <= ch && ch <= 'f') || ('A' <= ch && ch <= 'F')
                    || Character.isDigit((char) ch))
                  {
                    int i = Character.digit((char) ch, 16) << 4;
                    ch = in.read();
                    if (!(('a' <= ch && ch <= 'f') || ('A' <= ch && ch <= 'F')
                          || Character.isDigit((char) ch)))
                      throw new IOException("illegal hex char");
                    i |= Character.digit((char) ch, 16);
                    buf.append((char) i);
                  }
                else
                  buf.append((char) ch);
              }
            else
              buf.append((char) ch);
          }
        sep = in.read();
        if (sep != '+' && sep != ',')
          throw new IOException("illegal character: " + (char) ch);
        return buf.toString();
      }
    else
      {
        while (true)
          {
            switch (ch)
              {
              case '+':
              case ',':
                sep = ch;
                return buf.toString();
              case '\\':
                ch = in.read();
                if (ch == -1)
                  throw new EOFException();
                if (('a' <= ch && ch <= 'f') || ('A' <= ch && ch <= 'F')
                    || Character.isDigit((char) ch))
                  {
                    int i = Character.digit((char) ch, 16) << 4;
                    ch = in.read();
                    if (!(('a' <= ch && ch <= 'f') || ('A' <= ch && ch <= 'F')
                          || Character.isDigit((char) ch)))
                      throw new IOException("illegal hex char");
                    i |= Character.digit((char) ch, 16);
                    buf.append((char) i);
                  }
                else
                  buf.append((char) ch);
                break;
              case '=':
              case '<':
              case '>':
              case '#':
              case ';':
                throw new IOException("illegal character: " + (char) ch);
              case -1:
                sep = -1;
                return buf.toString ();
              default:
                buf.append((char) ch);
              }
            ch = in.read ();
          }
      }
  }

  private void parseDer (InputStream encoded) throws IOException
  {
    DERReader der = new DERReader (encoded);
    DERValue name = der.read();
    if (!name.isConstructed())
      throw new IOException ("malformed Name");
    this.encoded = name.getEncoded();
    int len = 0;
    while (len < name.getLength())
      {
        DERValue rdn = der.read();
        if (!rdn.isConstructed())
          throw new IOException ("badly formed RDNSequence");
        int len2 = 0;
        while (len2 < rdn.getLength())
          {
            DERValue atav = der.read();
            if (!atav.isConstructed())
              throw new IOException ("badly formed AttributeTypeAndValue");
            DERValue val = der.read();
            if (val.getTag() != DER.OBJECT_IDENTIFIER)
              throw new IOException ("badly formed AttributeTypeAndValue");
            OID oid = (OID) val.getValue();
            val = der.read();
            if (!(val.getValue() instanceof String))
              throw new IOException ("badly formed AttributeTypeAndValue");
            String value = (String) val.getValue();
            putComponent(oid, value);
            len2 += atav.getEncodedLength();
          }
        len += rdn.getEncodedLength();
        if (len < name.getLength())
          newRelativeDistinguishedName();
      }
  }

  private void newRelativeDistinguishedName()
  {
    currentRdn = new LinkedHashMap();
    components.add(currentRdn);
  }

  private void putComponent(OID oid, String value)
  {
    currentRdn.put(oid, value);
  }

  private void putComponent(String name, String value)
  {
    name = name.trim().toLowerCase();
    if (name.equals("cn"))
      putComponent(CN, value);
    else if (name.equals("c"))
      putComponent(C, value);
    else if (name.equals("l"))
      putComponent(L, value);
    else if (name.equals("street"))
      putComponent(STREET, value);
    else if (name.equals("st"))
      putComponent(ST, value);
    else if (name.equals ("o"))
      putComponent (O, value);
    else if (name.equals ("ou"))
      putComponent (OU, value);
    else if (name.equals("dc"))
      putComponent(DC, value);
    else if (name.equals("uid"))
      putComponent(UID, value);
    else
      putComponent(new OID(name), value);
  }

  private static String compressWS(String str)
  {
    StringBuffer buf = new StringBuffer();
    char lastChar = 0;
    for (int i = 0; i < str.length(); i++)
      {
        char c = str.charAt(i);
        if (Character.isWhitespace(c))
          {
            if (!Character.isWhitespace(lastChar))
              buf.append(' ');
          }
        else
          buf.append(c);
        lastChar = c;
      }
    return buf.toString().trim();
  }

  private static byte[] toByteArray (String str)
  {
    int limit = str.length();
    byte[] result = new byte[((limit + 1) / 2)];
    int i = 0, j = 0;
    if ((limit % 2) == 1)
      {
        result[j++] = (byte) Character.digit (str.charAt(i++), 16);
      }
    while (i < limit)
      {
        result[j  ]  = (byte) (Character.digit (str.charAt(i++), 16) << 4);
        result[j++] |= (byte)  Character.digit (str.charAt(i++), 16);
      }
    return result;
  }
}
