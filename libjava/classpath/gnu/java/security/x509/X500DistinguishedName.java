/* X500DistinguishedName.java -- X.500 distinguished name.
   Copyright (C) 2004, 2006  Free Software Foundation, Inc.

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


package gnu.java.security.x509;

import gnu.java.lang.CPStringBuilder;

import gnu.java.security.OID;
import gnu.java.security.der.DER;
import gnu.java.security.der.DERReader;
import gnu.java.security.der.DERValue;

import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.io.StringReader;
import java.security.Principal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class X500DistinguishedName implements Principal
{
  // Constants and fields.
  // -------------------------------------------------------------------------

  public static final OID CN         = new OID("2.5.4.3");
  public static final OID C          = new OID("2.5.4.6");
  public static final OID L          = new OID("2.5.4.7");
  public static final OID ST         = new OID("2.5.4.8");
  public static final OID STREET     = new OID("2.5.4.9");
  public static final OID O          = new OID("2.5.4.10");
  public static final OID OU         = new OID("2.5.4.11");
  public static final OID T          = new OID("2.5.4.12");
  public static final OID DNQ        = new OID("2.5.4.46");
  public static final OID NAME       = new OID("2.5.4.41");
  public static final OID GIVENNAME  = new OID("2.5.4.42");
  public static final OID INITIALS   = new OID("2.5.4.43");
  public static final OID GENERATION = new OID("2.5.4.44");
  public static final OID EMAIL      = new OID("1.2.840.113549.1.9.1");
  public static final OID DC         = new OID("0.9.2342.19200300.100.1.25");
  public static final OID UID        = new OID("0.9.2342.19200300.100.1.1");

  private List components;
  private Map currentRdn;
  private boolean fixed;
  private String stringRep;
  private byte[] encoded;

  // Constructors.
  // -------------------------------------------------------------------------

  public X500DistinguishedName()
  {
    components = new LinkedList();
    currentRdn = new LinkedHashMap();
    components.add(currentRdn);
  }

  public X500DistinguishedName(String name)
  {
    this();
    try
      {
        parseString(name);
      }
    catch (IOException ioe)
      {
        throw new IllegalArgumentException(ioe.toString());
      }
  }

  public X500DistinguishedName(byte[] encoded) throws IOException
  {
    this();
    parseDer(new DERReader(encoded));
  }

  public X500DistinguishedName(InputStream encoded) throws IOException
  {
    this();
    parseDer(new DERReader(encoded));
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  public String getName()
  {
    return toString();
  }

  public void newRelativeDistinguishedName()
  {
    if (fixed || currentRdn.isEmpty()) return;
    currentRdn = new LinkedHashMap();
    components.add(currentRdn);
  }

  public int size()
  {
    return components.size();
  }

  public int countComponents()
  {
    int count = 0;
    for (Iterator it = components.iterator(); it.hasNext(); )
      {
        count += ((Map) it.next()).size();
      }
    return count;
  }

  public boolean containsComponent(OID oid, String value)
  {
    for (Iterator it = components.iterator(); it.hasNext(); )
      {
        Map rdn = (Map) it.next();
        String s = (String) rdn.get(oid);
        if (s == null)
          continue;
        if (compressWS(value).equalsIgnoreCase(compressWS(s)))
          return true;
      }
    return false;
  }

  public String getComponent(OID oid)
  {
    for (Iterator it = components.iterator(); it.hasNext(); )
      {
        Map rdn = (Map) it.next();
        if (rdn.containsKey(oid))
          return (String) rdn.get(oid);
      }
    return null;
  }

  public String getComponent(OID oid, int rdn)
  {
    if (rdn >= size())
      return null;
    return (String) ((Map) components.get(rdn)).get(oid);
  }

  public void putComponent(OID oid, String value)
  {
    currentRdn.put(oid, value);
  }

  public void putComponent(String name, String value)
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
    else if (name.equals("t"))
      putComponent(T, value);
    else if (name.equals("dnq"))
      putComponent(DNQ, value);
    else if (name.equals("name"))
      putComponent(NAME, value);
    else if (name.equals("givenname"))
      putComponent(GIVENNAME, value);
    else if (name.equals("initials"))
      putComponent(INITIALS, value);
    else if (name.equals("generation"))
      putComponent(GENERATION, value);
    else if (name.equals("email"))
      putComponent(EMAIL, value);
    else if (name.equals("dc"))
      putComponent(DC, value);
    else if (name.equals("uid"))
      putComponent(UID, value);
    else if (name.equals("o"))
      putComponent(O, value);
    else if (name.equals("ou"))
      putComponent(OU, value);
    else
      putComponent(new OID(name), value);
  }

  public void setUnmodifiable()
  {
    if (fixed) return;
    fixed = true;
    List newComps = new ArrayList(components.size());
    for (Iterator it = components.iterator(); it.hasNext(); )
      {
        Map rdn = (Map) it.next();
        rdn = Collections.unmodifiableMap(rdn);
        newComps.add(rdn);
      }
    components = Collections.unmodifiableList(newComps);
    currentRdn = Collections.EMPTY_MAP;
  }

  public int hashCode()
  {
    int sum = 0;
    for (Iterator it = components.iterator(); it.hasNext(); )
      {
        Map m = (Map) it.next();
        for (Iterator it2 = m.entrySet().iterator(); it2.hasNext(); )
          {
            Map.Entry e = (Map.Entry) it2.next();
            sum += e.getKey().hashCode();
            sum += e.getValue().hashCode();
          }
      }
    return sum;
  }

  public boolean equals(Object o)
  {
    if (!(o instanceof X500DistinguishedName))
      return false;
    if (size() != ((X500DistinguishedName) o).size())
      return false;
    for (int i = 0; i < size(); i++)
      {
        Map m = (Map) components.get(i);
        for (Iterator it2 = m.entrySet().iterator(); it2.hasNext(); )
          {
            Map.Entry e = (Map.Entry) it2.next();
            OID oid = (OID) e.getKey();
            String v1 = (String) e.getValue();
            String v2 = ((X500DistinguishedName) o).getComponent(oid, i);
            if (!compressWS(v1).equalsIgnoreCase(compressWS(v2)))
              return false;
          }
      }
    return true;
  }

  public String toString()
  {
    if (fixed && stringRep != null)
      return stringRep;
    CPStringBuilder str = new CPStringBuilder();
    for (Iterator it = components.iterator(); it.hasNext(); )
      {
        Map m = (Map) it.next();
        for (Iterator it2 = m.entrySet().iterator(); it2.hasNext(); )
          {
            Map.Entry entry = (Map.Entry) it2.next();
            OID oid = (OID) entry.getKey();
            String value = (String) entry.getValue();
            if (oid.equals(CN))
              str.append("CN");
            else if (oid.equals(C))
              str.append("C");
            else if (oid.equals(L))
              str.append("L");
            else if (oid.equals(ST))
              str.append("ST");
            else if (oid.equals(STREET))
              str.append("STREET");
            else if (oid.equals(O))
              str.append("O");
            else if (oid.equals(OU))
              str.append("OU");
            else if (oid.equals(T))
              str.append("T");
            else if (oid.equals(DNQ))
              str.append("DNQ");
            else if (oid.equals(NAME))
              str.append("NAME");
            else
              str.append(oid.toString());
            str.append('=');
            str.append(value);
            if (it2.hasNext())
              str.append("+");
          }
        if (it.hasNext())
          str.append(',');
      }
    return (stringRep = str.toString());
  }

  public byte[] getDer()
  {
    if (fixed && encoded != null)
      return (byte[]) encoded.clone();

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
    return (byte[]) (encoded = val.getEncoded()).clone();
  }

  // Own methods.
  // -------------------------------------------------------------------------

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
      }
    setUnmodifiable();
  }

  private String readAttributeType(Reader in) throws IOException
  {
    CPStringBuilder buf = new CPStringBuilder();
    int ch;
    while ((ch = in.read()) != '=')
      {
        if (ch == -1)
          {
            if (buf.length() > 0)
              throw new EOFException();
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
    CPStringBuilder buf = new CPStringBuilder();
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
                return new String(Util.toByteArray(hex));
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
        if (sep != '+' || sep != ',')
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
                throw new EOFException();
              default:
                buf.append((char) ch);
                ch = in.read();
                if (ch == -1)
                  return buf.toString();
              }
          }
      }
  }

  private void parseDer(DERReader der) throws IOException
  {
    DERValue name = der.read();
    if (!name.isConstructed())
      throw new IOException("malformed Name");
    encoded = name.getEncoded();
    int len = 0;
    while (len < name.getLength())
      {
        DERValue rdn = der.read();
        if (!rdn.isConstructed())
          throw new IOException("badly formed RDNSequence");
        int len2 = 0;
        while (len2 < rdn.getLength())
          {
            DERValue atav = der.read();
            if (!atav.isConstructed())
              throw new IOException("badly formed AttributeTypeAndValue");
            DERValue val = der.read();
            if (val.getTag() != DER.OBJECT_IDENTIFIER)
              throw new IOException("badly formed AttributeTypeAndValue");
            OID oid = (OID) val.getValue();
            val = der.read();
            if (!(val.getValue() instanceof String))
              throw new IOException("badly formed AttributeTypeAndValue");
            String value = (String) val.getValue();
            putComponent(oid, value);
            len2 += atav.getEncodedLength();
          }
        len += rdn.getEncodedLength();
        if (len < name.getLength())
          newRelativeDistinguishedName();
      }
    setUnmodifiable();
  }

  private static String compressWS(String str)
  {
    CPStringBuilder buf = new CPStringBuilder();
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
}
