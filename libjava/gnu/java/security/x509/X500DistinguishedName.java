/* X500DistinguishedName.java -- X.500 name.
   Copyright (C) 2003 Free Software Foundation, Inc.

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


package gnu.java.security.x509;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.IOException;
import java.io.StreamTokenizer;
import java.io.StringReader;

import java.security.Principal;

import java.util.HashSet;
import java.util.LinkedList;

import gnu.java.io.ASN1ParsingException;
import gnu.java.security.OID;
import gnu.java.security.der.DER;
import gnu.java.security.der.DERReader;
import gnu.java.security.der.DERValue;
import gnu.java.security.der.DERWriter;

/**
 * A X.500 distinguished name. Distinguished names are sequences of
 * ATTRIB=VALUE pairs, where ATTRIB is one of the following:
 *
 * <table cellpadding="0" cellspacing="0" border="0">
 * <tr>
 * <th bgcolor="#CCCCFF" align="left">Name</th>
 * <th bgcolor="#CCCCFF" align="left">X.500 AttributeType</th>
 * <th bgcolor="#CCCCFF" align="left">ObjectIdentifier</th>
 * </tr>
 * <tr>
 * <td align="left">CN</td>
 * <td align="left">commonName</td>
 * <td align="left">2.5.4.3</td>
 * </tr>
 * <tr>
 * <td align="left">C</td>
 * <td align="left">countryName</td>
 * <td align="left">2.5.4.6</td>
 * </tr>
 * <tr>
 * <td align="left">L</td>
 * <td align="left">localityName</td>
 * <td align="left">2.5.4.7</td>
 * </tr>
 * <tr>
 * <td align="left">ST</td>
 * <td align="left">stateOrProvinceName</td>
 * <td align="left">2.5.4.8</td>
 * </tr>
 * <tr>
 * <td align="left">STREET</td>
 * <td align="left">streetAddress</td>
 * <td align="left">2.5.4.9</td>
 * </tr>
 * <tr>
 * <td align="left">O</td>
 * <td align="left">organizationName</td>
 * <td align="left">2.5.4.10</td>
 * </tr>
 * <tr>
 * <td align="left">OU</td>
 * <td align="left">organizationUnitName</td>
 * <td align="left">2.5.4.11</td>
 * </tr>
 * <tr>
 * <td align="left">DC</td>
 * <td align="left">domainComponent</td>
 * <td align="left">0.9.2342.19200300.100.1.25</td>
 * </tr>
 * <tr>
 * <td align="left">UID</td>
 * <td align="left">userid</td>
 * <td align="left"0.9.2342.19200300.100.1.1></td>
 * </tr>
 * <tr>
 * <td align="left">DNQ or DNQUALIFIER(*)</td>
 * <td align="left">domainNameQualifier</td>
 * <td align="left">2.5.4.46</td>
 * </tr>
 * <tr>
 * <td align="left">SURNAME(*)</td>
 * <td align="left">name</td>
 * <td align="left">2.5.4.41</td>
 * </tr>
 * <tr>
 * <td align="left">GIVENNAME(*)</td>
 * <td align="left">givenName</td>
 * <td align="left">2.5.4.42</td>
 * </tr>
 * <tr>
 * <td align="left">INITIALS(*)</td>
 * <td align="left">initials</td>
 * <td align="left">2.5.4.43</td>
 * </tr>
 * <tr>
 * <td align="left">EMAILADDRESS(*)</td>
 * <td align="left">emailAddress</td>
 * <td align="left">2.5.4.44</td>
 * </tr>
 * </table>
 *
 * <p><i>(*) = attributes not specified in RFC1779 or RFC2253, but
 * recognized anyway.</i>
 *
 * <p>Distinguished names of this form are used in the lightweight
 * directory access protocol (LDAP) and in the issuer and subject fields
 * of X.509 certificates.
 *
 * @author Casey Marshall (rsdio@metastatic.org)
 * @see javax.security.auth.x500.X500Principal
 * @status DER decoding/encoding works, RFC1779 and RFC2253 need to be
 *         made more robust.
 */
public class X500DistinguishedName
{

  // Constants and fields.
  // ------------------------------------------------------------------------

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

  private String commonName;
  private String country;
  private String locality;
  private String orgUnit;
  private String organization;
  private String street;
  private String state;
  private String title;
  private String dnQualifier;
  private String surname;
  private String givenName;
  private String initials;
  private String generation;
  private String email;
  private String domainComponent;
  private String userid;

  private String nameRFC1779;
  private String nameRFC2253;
  private String nameCanonical;

  private transient byte[] encoded;

  // Constructors.
  // ------------------------------------------------------------------------

  /**
   * Create a new X500DistinguishedName from the RFC1779 or RFC2253
   * encoded form.
   *
   * @param name The encoded name.
   * @throws IllegalArgumentException If the name cannot be parsed.
   */
  public X500DistinguishedName(String name)
  {
    if (name == null)
      throw new NullPointerException();
    try
      {
        parseDN(name, true);
      }
    catch (Exception e)
      {
        parseDN(name, false);
      }
  }

  /**
   * Create a new X500DistinguishedName from the DER encoded bytes.
   *
   * @param encoded The encoded form.
   * @throws IOException If the bytes are not a valid DER construct.
   */
  public X500DistinguishedName(byte[] encoded) throws IOException
  {
    this(new ByteArrayInputStream(encoded));
  }

  /**
   * Create a new X500DistinguishedName from the DER encoded bytes.
   *
   * @param encoded The encoded form.
   * @throws IOException If the bytes are not a valid DER construct.
   */
  public X500DistinguishedName(InputStream encoded) throws IOException
  {
    parseDER(encoded);
  }

  // Instance methods.
  // ------------------------------------------------------------------------

  public boolean equals(Object o)
  {
    return 
      (commonName != null &&
       commonName.equals(((X500DistinguishedName) o).commonName)) &&
      (country != null &&
       country.equals(((X500DistinguishedName) o).country)) &&
      (locality != null &&
       locality.equals(((X500DistinguishedName) o).locality)) &&
      (orgUnit != null &&
       orgUnit.equals(((X500DistinguishedName) o).orgUnit)) &&
      (organization != null &&
       organization.equals(((X500DistinguishedName) o).organization)) &&
      (street != null &&
       street.equals(((X500DistinguishedName) o).street)) &&
      (state != null &&
       state.equals(((X500DistinguishedName) o).state)) &&
      (domainComponent != null &&
       domainComponent.equals(((X500DistinguishedName) o).domainComponent)) &&
      (title != null &&
       title.equals(((X500DistinguishedName) o).title)) &&
      (dnQualifier != null &&
       dnQualifier.equals(((X500DistinguishedName) o).dnQualifier)) &&
      (surname != null &&
       surname.equals(((X500DistinguishedName) o).surname)) &&
      (givenName != null &&
       givenName.equals(((X500DistinguishedName) o).givenName)) &&
      (initials != null &&
       initials.equals(((X500DistinguishedName) o).initials)) &&
      (generation != null &&
       generation.equals(((X500DistinguishedName) o).generation)) &&
      (email != null &&
       email.equals(((X500DistinguishedName) o).email)) &&
      (userid != null &&
       userid.equals(((X500DistinguishedName) o).userid));
  }

  public byte[] getEncoded()
  {
    if (encoded == null)
      encoded = encodeDER();
    return (byte[]) encoded.clone();
  }

  private static String quote(String str)
  {
    if (str.indexOf(" ")  > 0 || str.indexOf("\f") > 0 ||
        str.indexOf("\n") > 0 || str.indexOf("\r") > 0 ||
        str.indexOf("\t") > 0)
      str = '"' + str + '"';
    // XXX needs regex
    //return str.replaceAll("([,+\"\\<>;])", "\\\1");
    return str;
  }

  public String toRFC1779()
  {
    if (nameRFC1779 != null)
      return nameRFC1779;
    StringBuffer buf = new StringBuffer();
    if (commonName != null)
      buf.append("CN=").append(quote(commonName)).append(", ");
    if (country != null)
      buf.append("C=").append(quote(country)).append(", ");
    if (locality != null)
      buf.append("L=").append(quote(locality)).append(", ");
    if (orgUnit != null)
      buf.append("OU=").append(quote(orgUnit)).append(", ");
    if (organization != null)
      buf.append("O=").append(quote(organization)).append(", ");
    if (street != null)
      buf.append("STREET=").append(quote(street)).append(", ");
    if (state != null)
      buf.append("ST=").append(quote(state)).append(", ");
    if (title != null)
      buf.append(T).append("=").append(quote(title)).append(", ");
    if (dnQualifier != null)
      buf.append(DNQ).append("=").append(quote(dnQualifier)).append(", ");
    if (surname != null)
      buf.append(NAME).append("=").append(quote(surname)).append(", ");
    if (givenName != null)
      buf.append(GIVENNAME).append("=").append(quote(givenName)).append(", ");
    if (initials != null)
      buf.append(INITIALS).append("=").append(quote(initials)).append(", ");
    if (generation != null)
      buf.append(GENERATION).append("=").append(quote(generation)).append(", ");
    if (email != null)
      buf.append(EMAIL).append("=").append(quote(email)).append(", ");
    if (domainComponent != null)
      buf.append(DC).append("=").append(quote(domainComponent)).append(", ");
    if (userid != null)
      buf.append(UID).append("=").append(quote(userid)).append(", ");
    // XXX escapes
    return (nameRFC1779 = buf.substring(0, buf.length()-2));
  }

  public String toRFC2253()
  {
    if (nameRFC2253 != null)
      return nameRFC2253;
    StringBuffer buf = new StringBuffer();
    if (commonName != null)
      buf.append("CN=").append(quote(commonName)).append(",");
    if (country != null)
      buf.append("C=").append(quote(country)).append(",");
    if (locality != null)
      buf.append("L=").append(quote(locality)).append(",");
    if (orgUnit != null)
      buf.append("OU=").append(quote(orgUnit)).append(",");
    if (organization != null)
      buf.append("O=").append(quote(organization)).append(",");
    if (street != null)
      buf.append("STREET=").append(quote(street)).append(",");
    if (state != null)
      buf.append("ST=").append(quote(state)).append(",");
    if (title != null)
      buf.append(T).append("=").append(quote(title)).append(",");
    if (dnQualifier != null)
      buf.append(DNQ).append("=").append(quote(dnQualifier)).append(",");
    if (surname != null)
      buf.append(NAME).append("=").append(quote(surname)).append(",");
    if (givenName != null)
      buf.append(GIVENNAME).append("=").append(quote(givenName)).append(",");
    if (initials != null)
      buf.append(INITIALS).append("=").append(quote(initials)).append(",");
    if (generation != null)
      buf.append(GENERATION).append("=").append(quote(generation)).append(",");
    if (email != null)
      buf.append(EMAIL).append("=").append(quote(email)).append(",");
    if (domainComponent != null)
      buf.append(DC).append("=").append(quote(domainComponent)).append(",");
    if (userid != null)
      buf.append(UID).append("=").append(quote(userid)).append(",");
    // XXX escapes.
    return (nameRFC2253 = buf.substring(0, buf.length()-1));
  }

  public String toCanonical()
  {
    if (nameCanonical != null)
      return nameCanonical;
    nameCanonical = toRFC2253();
    return nameCanonical; // XXX canonicalize
  }

  public String getCommonName()
  {
    return commonName;
  }

  public String getCountry()
  {
    return country;
  }

  public String getLocality()
  {
    return locality;
  }

  public String getOrganizationalUnit()
  {
    return orgUnit;
  }

  public String getOrganization()
  {
    return organization;
  }

  public String getStreet()
  {
    return street;
  }

  public String getState()
  {
    return state;
  }

  public String getTitle()
  {
    return title;
  }

  public String getDNQualifier()
  {
    return dnQualifier;
  }

  public String getSurname()
  {
    return surname;
  }

  public String getGivenName()
  {
    return givenName;
  }

  public String getInitials()
  {
    return initials;
  }

  public String getGeneration()
  {
    return generation;
  }

  public String getEmail()
  {
    return email;
  }

  public String getDomain()
  {
    return domainComponent;
  }

  public String getUserID()
  {
    return userid;
  }

  // Own methods.
  // ------------------------------------------------------------------------

  private static String unquote(String str)
  {
    if (str.startsWith("\"") && str.endsWith("\""))
      str = str.substring(1, str.length()-1);
    // XXX needs regex
    //return str.replaceAll("\\([,+\"\\<>;])", "\1");
    return str;
  }

  private void parseDN(String name, boolean rfc2253)
  {
    if (name.length() == 0)
      throw new IllegalArgumentException("zero-length distinguished name");
    StreamTokenizer parse = new StreamTokenizer(new StringReader(name));
    parse.resetSyntax();
    parse.wordChars('\000', '~');
    parse.ordinaryChar('#');
    parse.ordinaryChar(',');
    parse.ordinaryChar('=');
    parse.ordinaryChar('<');
    parse.ordinaryChar('>');
    parse.ordinaryChar(';');
    parse.ordinaryChar('\\');
    parse.quoteChar('"');
    String attrib = null;
    String value = null;
    int token, lastToken = ',';
    while (true)
      {
        try
          {
            token = parse.nextToken();
          }
        catch (IOException ioe)
          {
            throw new IllegalArgumentException();
          }
        switch (token)
          {
            case StreamTokenizer.TT_WORD:
              if (lastToken == ',' || lastToken == '+' ||
                  (!rfc2253 && lastToken == ';'))
                attrib = parse.sval.trim();
              else if (lastToken == '=')
                value = unquote(parse.sval.trim());
              else
                throw new IllegalArgumentException();
              break;
            case '"':
              if (lastToken == '=')
                value = parse.sval;
              else
                throw new IllegalArgumentException();
              break;
            case ';':
              if (rfc2253)
                throw new IllegalArgumentException();
            case ',':
            case '+':
              if (attrib == null || value == null)
                throw new IllegalArgumentException("extraneous separator");
              try
                {
                  setAttribute(new OID(attrib), value);
                }
              catch (Exception x)
                {
                  setAttribute(attrib, value);
                }
              attrib = null;
              value = null;
              break;
            case '=':
              break;
            case StreamTokenizer.TT_EOF:
              return;
            default:
              throw new IllegalArgumentException("unknown token " + (char)token
                + " (" + token + ")");
          }
        lastToken = token;
      }
  }

  private void parseDER(InputStream in) throws IOException
  {
    DERReader der = new DERReader(in);
    DERValue name = der.read();
    if (!name.isConstructed())
      throw new ASN1ParsingException("badly formed Name");
    int len = 0;
    while (len < name.getLength())
      {
        DERValue rdn = der.read();
        if (rdn.getValue() != DER.CONSTRUCTED_VALUE)
          throw new ASN1ParsingException("badly formed RDNSequence");
        int len2 = 0;
        while (len2 < rdn.getLength())
          {
            DERValue atav = der.read();
            if (atav.getValue() != DER.CONSTRUCTED_VALUE)
              throw new ASN1ParsingException(
                "badly formed AttributeTypeAndValue");
            OID atype = (OID) der.read().getValue();
            String aval = (String) der.read().getValue();
            setAttribute(atype, aval);
            len2 += 1 + atav.getLength()
                 + DERWriter.definiteEncodingSize(atav.getLength());
          }
        len += len2 + 1 + DERWriter.definiteEncodingSize(name.getLength());
      }
  }

  private byte[] encodeDER()
  {
    try
      {
        LinkedList name = new LinkedList();
        if (commonName != null)
          {
            HashSet rdn = new HashSet();
            LinkedList atav = new LinkedList();
            atav.add(new DERValue(DER.OBJECT_IDENTIFIER, CN));
            atav.add(new DERValue(DER.PRINTABLE_STRING, commonName));
            rdn.add(new DERValue(DER.CONSTRUCTED | DER.SEQUENCE, atav));
            name.add(new DERValue(DER.CONSTRUCTED | DER.SET, rdn));
          }
        if (country != null)
          {
            HashSet rdn = new HashSet();
            LinkedList atav = new LinkedList();
            atav.add(new DERValue(DER.OBJECT_IDENTIFIER, C));
            atav.add(new DERValue(DER.PRINTABLE_STRING, country));
            rdn.add(new DERValue(DER.CONSTRUCTED | DER.SEQUENCE, atav));
            name.add(new DERValue(DER.CONSTRUCTED | DER.SET, rdn));
          }
        if (locality != null)
          {
            HashSet rdn = new HashSet();
            LinkedList atav = new LinkedList();
            atav.add(new DERValue(DER.OBJECT_IDENTIFIER, L));
            atav.add(new DERValue(DER.PRINTABLE_STRING, locality));
            rdn.add(new DERValue(DER.CONSTRUCTED | DER.SEQUENCE, atav));
            name.add(new DERValue(DER.CONSTRUCTED | DER.SET, rdn));
          }
        if (orgUnit != null)
          {
            HashSet rdn = new HashSet();
            LinkedList atav = new LinkedList();
            atav.add(new DERValue(DER.OBJECT_IDENTIFIER, OU));
            atav.add(new DERValue(DER.PRINTABLE_STRING, orgUnit));
            rdn.add(new DERValue(DER.CONSTRUCTED | DER.SEQUENCE, atav));
            name.add(new DERValue(DER.CONSTRUCTED | DER.SET, rdn));
          }
        if (organization != null)
          {
            HashSet rdn = new HashSet();
            LinkedList atav = new LinkedList();
            atav.add(new DERValue(DER.OBJECT_IDENTIFIER, O));
            atav.add(new DERValue(DER.PRINTABLE_STRING, organization));
            rdn.add(new DERValue(DER.CONSTRUCTED | DER.SEQUENCE, atav));
            name.add(new DERValue(DER.CONSTRUCTED | DER.SET, rdn));
          }
        if (street != null)
          {
            HashSet rdn = new HashSet();
            LinkedList atav = new LinkedList();
            atav.add(new DERValue(DER.OBJECT_IDENTIFIER, STREET));
            atav.add(new DERValue(DER.PRINTABLE_STRING, street));
            rdn.add(new DERValue(DER.CONSTRUCTED | DER.SEQUENCE, atav));
            name.add(new DERValue(DER.CONSTRUCTED | DER.SET, rdn));
          }
        if (state != null)
          {
            HashSet rdn = new HashSet();
            LinkedList atav = new LinkedList();
            atav.add(new DERValue(DER.OBJECT_IDENTIFIER, ST));
            atav.add(new DERValue(DER.PRINTABLE_STRING, state));
            rdn.add(new DERValue(DER.CONSTRUCTED | DER.SEQUENCE, atav));
            name.add(new DERValue(DER.CONSTRUCTED | DER.SET, rdn));
          }
        if (title != null)
          {
            HashSet rdn = new HashSet();
            LinkedList atav = new LinkedList();
            atav.add(new DERValue(DER.OBJECT_IDENTIFIER, T));
            atav.add(new DERValue(DER.PRINTABLE_STRING, title));
            rdn.add(new DERValue(DER.CONSTRUCTED | DER.SEQUENCE, atav));
            name.add(new DERValue(DER.CONSTRUCTED | DER.SET, rdn));
          }
        if (dnQualifier != null)
          {
            HashSet rdn = new HashSet();
            LinkedList atav = new LinkedList();
            atav.add(new DERValue(DER.OBJECT_IDENTIFIER, DNQ));
            atav.add(new DERValue(DER.PRINTABLE_STRING, dnQualifier));
            rdn.add(new DERValue(DER.CONSTRUCTED | DER.SEQUENCE, atav));
            name.add(new DERValue(DER.CONSTRUCTED | DER.SET, rdn));
          }
        if (surname != null)
          {
            HashSet rdn = new HashSet();
            LinkedList atav = new LinkedList();
            atav.add(new DERValue(DER.OBJECT_IDENTIFIER, NAME));
            atav.add(new DERValue(DER.PRINTABLE_STRING, surname));
            rdn.add(new DERValue(DER.CONSTRUCTED | DER.SEQUENCE, atav));
            name.add(new DERValue(DER.CONSTRUCTED | DER.SET, rdn));
          }
        if (givenName != null)
          {
            HashSet rdn = new HashSet();
            LinkedList atav = new LinkedList();
            atav.add(new DERValue(DER.OBJECT_IDENTIFIER, GIVENNAME));
            atav.add(new DERValue(DER.PRINTABLE_STRING, givenName));
            rdn.add(new DERValue(DER.CONSTRUCTED | DER.SEQUENCE, atav));
            name.add(new DERValue(DER.CONSTRUCTED | DER.SET, rdn));
          }
        if (initials != null)
          {
            HashSet rdn = new HashSet();
            LinkedList atav = new LinkedList();
            atav.add(new DERValue(DER.OBJECT_IDENTIFIER, INITIALS));
            atav.add(new DERValue(DER.PRINTABLE_STRING, initials));
            rdn.add(new DERValue(DER.CONSTRUCTED | DER.SEQUENCE, atav));
            name.add(new DERValue(DER.CONSTRUCTED | DER.SET, rdn));
          }
        if (generation != null)
          {
            HashSet rdn = new HashSet();
            LinkedList atav = new LinkedList();
            atav.add(new DERValue(DER.OBJECT_IDENTIFIER, GENERATION));
            atav.add(new DERValue(DER.PRINTABLE_STRING, generation));
            rdn.add(new DERValue(DER.CONSTRUCTED | DER.SEQUENCE, atav));
            name.add(new DERValue(DER.CONSTRUCTED | DER.SET, rdn));
          }
        if (email != null)
          {
            HashSet rdn = new HashSet();
            LinkedList atav = new LinkedList();
            atav.add(new DERValue(DER.OBJECT_IDENTIFIER, EMAIL));
            atav.add(new DERValue(DER.PRINTABLE_STRING, email));
            rdn.add(new DERValue(DER.CONSTRUCTED | DER.SEQUENCE, atav));
            name.add(new DERValue(DER.CONSTRUCTED | DER.SET, rdn));
          }
        if (domainComponent != null)
          {
            HashSet rdn = new HashSet();
            LinkedList atav = new LinkedList();
            atav.add(new DERValue(DER.OBJECT_IDENTIFIER, DC));
            atav.add(new DERValue(DER.PRINTABLE_STRING, domainComponent));
            rdn.add(new DERValue(DER.CONSTRUCTED | DER.SEQUENCE, atav));
            name.add(new DERValue(DER.CONSTRUCTED | DER.SET, rdn));
          }
        if (userid != null)
          {
            HashSet rdn = new HashSet();
            LinkedList atav = new LinkedList();
            atav.add(new DERValue(DER.OBJECT_IDENTIFIER, UID));
            atav.add(new DERValue(DER.PRINTABLE_STRING, userid));
            rdn.add(new DERValue(DER.CONSTRUCTED | DER.SEQUENCE, atav));
            name.add(new DERValue(DER.CONSTRUCTED | DER.SET, rdn));
          }
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        DERWriter.write(out, new DERValue(DER.CONSTRUCTED|DER.SEQUENCE, name));
        return out.toByteArray();
      }
    catch (IOException ioe)
      {
        throw new Error(ioe);
      }
  }

  private void setAttribute(String atype, String aval)
  {
    if (atype.equals("CN"))
      commonName = aval;
    else if (atype.equals("C"))
      country = aval;
    else if (atype.equals("L"))
      locality = aval;
    else if (atype.equals("ST"))
      state = aval;
    else if (atype.equals("STREET"))
      street = aval;
    else if (atype.equals("O"))
      organization = aval;
    else if (atype.equals("OU"))
      orgUnit = aval;
    else if (atype.equals("T"))
      title = aval;
    else if (atype.equals("DNQ") || atype.equals("DNQUALIFIER"))
      dnQualifier = aval;
    else if (atype.equals("SURNAME"))
      surname = aval;
    else if (atype.equals("GIVENNAME"))
      givenName = aval;
    else if (atype.equals("INITIALS"))
      initials = aval;
    else if (atype.equals("GENERATION"))
      generation = aval;
    else if (atype.equals("EMAILADDRESS"))
      email = aval;
    else if (atype.equals("DC"))
      domainComponent = aval;
    else if (atype.equals("UID"))
      userid = aval;
    else
      throw new IllegalArgumentException("unknown attribute " + atype);
  }

  private void setAttribute(OID atype, String aval)
  {
    if (atype.equals(CN))
      commonName = aval;
    else if (atype.equals(C))
      country = aval;
    else if (atype.equals(L))
      locality = aval;
    else if (atype.equals(ST))
      state = aval;
    else if (atype.equals(STREET))
      street = aval;
    else if (atype.equals(O))
      organization = aval;
    else if (atype.equals(OU))
      orgUnit = aval;
    else if (atype.equals(T))
      title = aval;
    else if (atype.equals(DNQ))
      dnQualifier = aval;
    else if (atype.equals(NAME))
      surname = aval;
    else if (atype.equals(GIVENNAME))
      givenName = aval;
    else if (atype.equals(INITIALS))
      initials = aval;
    else if (atype.equals(GENERATION))
      generation = aval;
    else if (atype.equals(EMAIL))
      email = aval;
    else if (atype.equals(DC))
      domainComponent = aval;
    else if (atype.equals(UID))
      userid = aval;
    else
      throw new IllegalArgumentException("unknown attribute " + atype);
  }
}
