/* TaggedProfile.java --
   Copyright (C) 2005, 2006 Free Software Foundation, Inc.

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


package org.omg.IOP;

import org.omg.CORBA.portable.IDLEntity;

import java.io.Serializable;

/**
* The TaggedProfile if part of the {@link IOR}, defining a single specific
* aspect of the object related information. The content of profile depends
* on this information. It is represented here just as an array of
* bytes. The OMG currently defines three types of the tagged profile:
* <ul>
* <li>The Internet profile, identified by {@link TAG_INTERNET_IOP},
* supports the Internet Inter-ORB Protocol.
* </li>
* <li>The Multiple Components profile, identified by
* {@link TAG_MULTIPLE_COMPONENTS}, may be used to carry various IOR
* tagged components.
* <li>
* </li>
* <li>The SCCP IOP profile (described in OMG CORBA/IN Interworking
* specification).</li>
* </ul>
*
* The tagged profile may have its internal tagged components. The examples
* of the possible components inside the tag are {@link TAG_CODE_SETS},
* {@link TAG_ALTERNATE_IIOP_ADDRESS},  {@link TAG_JAVA_CODEBASE},
* {@link TAG_ORB_TYPE} and {@link TAG_POLICIES}. The complete list can only
* be found in OMG specification. Some of them occur only once
* (in the same TaggedProfile), others can be repeated.
*
* @see TaggedProfileHolder
* @see TaggedProfileHelper
*
* @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
*/
public final class TaggedProfile
  implements IDLEntity, Serializable
{
  /**
   * Use serialVersionUID (v1.4) for interoperability.
   */
  private static final long serialVersionUID = -461232684387903343L;

  /**
   * The integer tag identifier, typically one of TAG_INTERNET_IOP.value or
   * TAG_MULTIPLE_COMPONENTS.value.
   */
  public int tag;

  /**
   * The profile_data, represented here in the form of the array of bytes.
   */
  public byte[] profile_data;

  /**
   * Create the unitialised instance, assigning to
   * the all fields java default values.
   */
  public TaggedProfile()
  {
  }

  /**
   * Create the instance, initialising the fields to the given values.
   */
  public TaggedProfile(int a_tag, byte[] a_profile_data)
  {
    tag = a_tag;
    profile_data = a_profile_data;
  }
}