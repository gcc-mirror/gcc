/* Minor.java --
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


package gnu.CORBA;


/**
 * Provides information and operations, related to about the 20 bit vendor minor
 * code Id. This code is included into all CORBA system exceptions and is also
 * transferred to remote side.
 * 
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public interface Minor
{
  // Note: MARSHAL done.

  /* MARSHAL */

  /**
   * The GNU Classpath VMCID. The last 12 bits can be used to mark up to 4096
   * possible exceptions.
   */
  int vendor = 0x47430000;

  /*
   * Minor codes form MARSHAL exception.
   */

  /**
   * The message being received is not a GIOP message. It does not start from
   * the expected magic sequence byte[] { 'G', 'I', 'O', 'P' }.
   */
  int Giop = 1 | vendor;

  /**
   * The unexpected IOException while reading or writing the GIOP message header
   * or the subsequent request or response header
   */
  int Header = 2 | vendor;

  /**
   * The data stream ended before reading all expected values from it. This
   * usually means that the CORBA message is corrupted, but may also indicate
   * that the server expects the remote method being invoked to have more or
   * different parameters.
   */
  int EOF = 3 | vendor;

  /**
   * The unexpected IOException while reading or writing the data via Commond
   * Data Representation stream.
   */
  int CDR = 5 | vendor;

  /**
   * The unexpected IOException while reading or writing the Value type.
   */
  int Value = 6 | vendor;

  /**
   * The unexpected IOException while handling request forwarding.
   */
  int Forwarding = 7 | vendor;

  /**
   * The unexpected IOException while handling data encapsulation, tagged
   * components, tagged profiles, etc.
   */
  int Encapsulation = 8 | vendor;

  /**
   * The unexpected IOException while inserting or extracting data to/from the
   * Any or DynamicAny.
   */
  int Any = 9 | vendor;

  /**
   * The unexpected UserException in the context where it cannot be handled and
   * must be converted to the SystemException.
   */
  int UserException = 10 | vendor;

  /**
   * While the operation could formally be applied to the target, the OMG
   * standard states that it is actually not applicable. For example, some CORBA
   * objects like POA are always local and should not be passed to or returned
   * from the remote side.
   */
  int Inappropriate = 11 | vendor;

  /**
   * When reading data, it was discovered that size of the data structure like
   * string, sequence or character is written as the negative number.
   */
  int Negative = 12 | vendor;

  /**
   * Reference to non-existing node in the data grapth while reading the value
   * types.
   */
  int Graph = 14 | vendor;

  /**
   * Unexpected exception was thrown from the IDL type helper while handling the
   * object of this type as a boxed value.
   */
  int Boxed = 15 | vendor;

  /**
   * Unable to instantiate an value type object while reading it from the
   * stream.
   */
  int Instantiation = 16 | vendor;

  /**
   * The header tag of the value type being read from the CDR stream contains an
   * unexpected value outside 0x7fffff00 .. 0x7fffffff and also not null and not
   * an indirection.
   */
  int ValueHeaderTag = 17 | vendor;

  /**
   * The header tag flags of the value type being read from the CDR stream make
   * the invalid combination (for instance, 0x7fffff04).
   */
  int ValueHeaderFlags = 18 | vendor;

  /**
   * The value type class, written on the wire, is not compatible with the
   * expected class, passed as a parameter to the InputStream.read_value.
   */
  int ClassCast = 19 | vendor;

  /**
   * Positive or otherwise invalid indirection offset when reading the data
   * graph of the value type.
   */
  int Offset = 20 | vendor;

  /**
   * Errors while reading the chunked value type.
   */
  int Chunks = 21 | vendor;

  /**
   * No means are provided to write this value type.
   */
  int UnsupportedValue = 22 | vendor;

  /**
   * The value factory, required for the operation being invoked, is not
   * registered with this ORB.
   */
  int Factory = 23 | vendor;

  /**
   * Unsupported object addressing method in GIOP request header.
   */
  int UnsupportedAddressing = 24 | vendor;

  /**
   * Invalid stringified object reference (IOR).
   */
  int IOR = 25 | vendor;

  /**
   * Problems with converting between stubs, ties, interfaces and
   * implementations.
   */
  int TargetConversion = 26 | vendor;

  /**
   * Problems with reading or writing the fields of the value type object.
   */
  int ValueFields = 27 | vendor;

  /**
   * The instance of the value type is not serializable.
   */
  int NonSerializable = 28 | vendor;

  /* BAD_OPERATION */

  /**
   * The remote side requested to invoke the method that is not available on
   * that target (client and server probably disagree in the object definition).
   */
  int Method = 0 | vendor;

  /**
   * Failed to activate the inactive object.
   */
  int Activation = 10 | vendor;

  /*
   * Any - Attempt to extract from the Any value of the different type that was
   * stored into that Any.
   */

  /* ClassCast - Unable to narrow the object into stub. */

  /**
   * The policies, applying to ORB or POA prevent the requested operation.
   */
  int Policy = 11 | vendor;

  /**
   * Socket related errors like failure to open socket on the expected port,
   * failure to get a free port when required and so on.
   */
  int Socket = 12 | vendor;

  /**
   * The passed value for enumeration is outside the valid range for that
   * enumeration.
   */
  int Enumeration = 14 | vendor;

  /**
   * The passed policy code is outside the valid range of the possible policies
   * for the given policy type.
   */
  int PolicyType = 15 | vendor;
  
  /* NO_RESOURCES */
  
  /**
   * Unable to get a free port for a new socket. Proably too many objects under
   * unsuitable POA policy.
   */
  int Ports = 20 | vendor;
  
  /**
   * Too many parallel calls (too many parallel threads). The thread control
   * prevents malicios client from knocking the server out by suddenly
   * submitting large number of requests.
   */
  int Threads = 21 | vendor;
  
  /**
   * The IOR starts with file://, http:// or ftp://, but this local or remote
   * resource is not accessible.
   */
  int Missing_IOR = 22 | vendor;

}
