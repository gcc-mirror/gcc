/* GSSName.java -- a name interface for GSS.
   Copyright (C) 2004 Free Software Foundation, Inc.

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

/* The documentation comments of this class are derived from the text
   of RFC 2853:  Generic Security Service API Version 2: Java Bindings.
   That document is covered under the following license notice:

Copyright (C) The Internet Society (2000).  All Rights Reserved.

This document and translations of it may be copied and furnished to
others, and derivative works that comment on or otherwise explain it
or assist in its implementation may be prepared, copied, published and
distributed, in whole or in part, without restriction of any kind,
provided that the above copyright notice and this paragraph are
included on all such copies and derivative works.  However, this
document itself may not be modified in any way, such as by removing
the copyright notice or references to the Internet Society or other
Internet organizations, except as needed for the purpose of developing
Internet standards in which case the procedures for copyrights defined
in the Internet Standards process must be followed, or as required to
translate it into languages other than English.

The limited permissions granted above are perpetual and will not be
revoked by the Internet Society or its successors or assigns.

This document and the information contained herein is provided on an
"AS IS" basis and THE INTERNET SOCIETY AND THE INTERNET ENGINEERING
TASK FORCE DISCLAIMS ALL WARRANTIES, EXPRESS OR IMPLIED, INCLUDING BUT
NOT LIMITED TO ANY WARRANTY THAT THE USE OF THE INFORMATION HEREIN
WILL NOT INFRINGE ANY RIGHTS OR ANY IMPLIED WARRANTIES OF
MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. */


package org.ietf.jgss;

/**
 * <p>This interface encapsulates a single GSS-API principal entity.
 * Different name formats and their definitions are identified with
 * universal Object Identifiers (Oids).  The format of the names can be
 * derived based on the unique oid of its namespace type.</p>
 *
 * <h3>Example Code</h3>
 *
 * <pre>
GSSManager mgr = GSSManager.getInstance();

// create a host based service name
GSSName name = mgr.createName("service@host",
                              GSSName.NT_HOSTBASED_SERVICE);

Oid krb5 = new Oid("1.2.840.113554.1.2.2");

GSSName mechName = name.canonicalize(krb5);

// the above two steps are equivalent to the following
GSSName mechName = mgr.createName("service@host",
                                  GSSName.NT_HOSTBASED_SERVICE, krb5);

// perform name comparison
if (name.equals(mechName))
  print("Names are equal.");

// obtain textual representation of name and its printable
// name type
print(mechName.toString() +
      mechName.getStringNameType().toString());

// export and re-import the name
byte [] exportName = mechName.export();

// create a new name object from the exported buffer
GSSName newName = mgr.createName(exportName,
                                 GSSName.NT_EXPORT_NAME);
</pre>
 */
public interface GSSName
{

  // Constants.
  // -------------------------------------------------------------------------

  /**
   * <p>Name type for representing an anonymous entity. It represents the
   * following value: <code>{ 1(iso), 3(org), 6(dod), 1(internet), 5(security),
   * 6(nametypes), 3(gss-anonymous-name) }</code>.</p>
   */
  Oid NT_ANONYMOUS = new Oid(new int[] { 1, 3, 6, 1, 5, 6, 3 });

  /**
   * <p>Name type used to indicate an exported name produced by the export
   * method. It represents the following value: <code>{ 1(iso), 3(org), 6(dod),
   * 1(internet), 5(security), 6(nametypes), 4(gss-api-exported-name)
   * }</code>.</p>
   */
  Oid NT_EXPORT_NAME = new Oid(new int[] { 1, 3, 6, 1, 5, 6, 4 });

  /**
   * <p>Oid indicating a host-based service name form.  It is used to
   * represent services associated with host computers.  This name form is
   * constructed using two elements, "service" and "hostname", as follows:</p>
   *
   * <blockquote><code>service@hostname</code></blockquote>
   *
   * <p>Values for the "service" element are registered with the IANA. It
   * represents the following value: <code>{ 1(iso), 3(org), 6(dod),
   * 1(internet), 5(security), 6(nametypes), 2(gss-host-based-services)
   * }</code>.</p>
   */
  Oid NT_HOSTBASED_SERVICE = new Oid(new int[] { 1, 3, 6, 1, 5, 6, 2 });

  /**
   * <p>Name type to indicate a numeric user identifier corresponding to a
   * user on a local system. (e.g. Uid).  It represents the following
   * value: <code>{ iso(1) member-body(2) United States(840) mit(113554)
   * infosys(1) gssapi(2) generic(1) machine_uid_name(2) }</code>.</p>
   */
  Oid NT_MACHINE_UID_NAME = new Oid(new int[] { 1, 2, 840, 113554, 1, 2, 1, 2 });

  /**
   * <p>Name type to indicate a string of digits representing the numeric
   * user identifier of a user on a local system. It represents the
   * following value: <code>{ iso(1) member-body(2) United States(840)
   * mit(113554) infosys(1) gssapi(2) generic(1) string_uid_name(3)
   * }</code>.</p>
   */
  Oid NT_STRING_UID_NAME = new Oid(new int[] { 1, 2, 840, 113554, 1, 2, 1, 3 });

  /**
   * <p>Name type to indicate a named user on a local system.  It represents
   * the following value: <code>{ iso(1) member-body(2) United States(840)
   * mit(113554) infosys(1) gssapi(2) generic(1) user_name(1) }</code>.</p>
   */
  Oid NT_USER_NAME = new Oid(new int[] { 1, 2, 840, 113554, 1, 2, 1, 1 });

  // Instance methods.
  // -------------------------------------------------------------------------

  /**
   * Compares two GSSName objects to determine whether they refer to the
   * same entity.  This method may throw a {@link GSSException} when the
   * names cannot be compared.  If either of the names represents an
   * anonymous entity, the method will return <code>false</code>.
   *
   * @param another GSSName object to compare with.
   * @return True if this name equals the other, and if neither name
   *         represents an anonymous entity.
   * @throws GSSException If the names cannot be compared.
   */
  boolean equals(GSSName another) throws GSSException;

  /**
   * A variation of the {@link #equals(org.ietf.jgss.GSSName)} method that
   * is provided to override the {@link Object#equals(java.lang.Object)}
   * method that the implementing class will inherit.  The behavior is
   * exactly the same as that in the other equals method except that no
   * {@link GSSException} is thrown; instead, <code>false</code> will be
   * returned in the situation where an error occurs. (Note that the Java
   * language specification requires that two objects that are equal
   * according to the {@link Object#equals(java.lang.Object)} method must
   * return the same integer when the {@link hashCode()} method is called
   * on them.
   *
   * @param another GSSName object to compare with.
   * @return True if this name equals the other, if neither name
   *         represents an anonymous entity, or if an error occurs.
   */
  boolean equals(Object another);

  /**
   * Creates a mechanism name (MN) from an arbitrary internal name.  This
   * is equivalent to using the factory methods {@link
   * GSSManager#createName(java.lang.String,org.ietf.jgss.Oid,org.ietf.jgss.Oid)}
   * or {@link
   * GSSManager#createName(byte[],org.ietf.jgss.Oid,org.ietf.jgss.Oid)}.
   *
   * @param mech The oid for the mechanism for which the canonical form
   *             of the name is requested.
   * @return The mechanism name.
   * @throws GSSException If this operation fails.
   */
  GSSName canonicalize(Oid mech) throws GSSException;

  /**
   * Returns a canonical contiguous byte representation of a mechanism
   * name (MN), suitable for direct, byte by byte comparison by
   * authorization functions.  If the name is not an MN, implementations
   * may throw a {@link GSSException} with the {@link GSSException#NAME_NOT_MN}
   * status code.  If an implementation chooses not to throw an exception,
   * it should use some system specific default mechanism to canonicalize
   * the name and then export it. The format of the header of the output
   * buffer is specified in <a
   * href="http://www.ietf.org/rfc/rfc2743.txt">RFC 2743</a>.
   *
   * @return The exported name.
   * @throws GSSException If the name is not an MN and the implementation
   *         throws an exception for this case.
   */
  byte[] export() throws GSSException;

  /**
   * Returns a textual representation of the GSSName object.  To retrieve
   * the printed name format, which determines the syntax of the returned
   * string, the {@link #getStringNameType()} method can be used.
   *
   * @return The textual representation of the GSSName object.
   */
  String toString();

  /**
   * Returns the oid representing the type of name returned through the
   * {@link #toString()} method.  Using this oid, the syntax of the printable
   * name can be determined.
   *
   * @return The name type.
   * @throws GSSException If this operation fails.
   */
  Oid getStringNameType() throws GSSException;

  /**
   * Tests if this name object represents an anonymous entity.  Returns
   * <code>true</code> if this is an anonymous name.
   *
   * @return True if this name represents an anonymous entity.
   */
  boolean isAnonymous();

  /**
   * Tests if this name object contains only one mechanism element and is
   * thus a mechanism name as defined by <a
   * href="http://www.ietf.org/rfc/rfc2743.txt">RFC 2743</a>.
   *
   * @return True if this name is a mechanism name.
   */
  boolean isMN();
}
