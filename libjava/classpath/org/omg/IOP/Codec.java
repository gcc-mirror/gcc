/* Codec.java --
   Copyright (C) 2005 Free Software Foundation, Inc.

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

/**
 * <p>
 * Codec provides means to encode IDL data types into the byte arrays.
 * Some parts of the CORBA message may contain such abstracted (encapsulated)
 * byte arrays, holding arbitrary information. The encoding and decoding
 * operations are defined separately in {@link CodecOperations}.
 * The Codec for {@link ENCODING_CDR_ENCAPS} v 1.0 - 1.2 is required by OMG.
 * Vendors can implement additional Codec's, driven by alternative algorithms.
 * </p>
 * <p>
 * The {@link ENCODING_CDR_ENCAPS} Codec, returned by the {@link CodecFactory},
 * is a local object. It is not possible to get its stringified reference,
 * to send it over CDR streams or invoke the methods remotely.
 * </p>
 * <p>
 * Codec is obtained from {@link CodecFactory}. CodecFactory is returned by
 * <code>ORB.resolve_initial_references("CodecFactory")</code>.
 * </p>
 * @specnote The ENCODING_CDR_ENCAPS Codec is local in both Suns
 * (at least till 1.4 inclusive) an this implementation.
 *
 *
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public interface Codec
  extends CodecOperations, IDLEntity, org.omg.CORBA.Object
{
}