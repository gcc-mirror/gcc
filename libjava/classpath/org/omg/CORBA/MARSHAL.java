/* MARSHAL.java --
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


package org.omg.CORBA;

import java.io.Serializable;

/**
 * Means that some request or reply from the network has a wrong size or is
 * structurally invalid. In GNU Classpath, this exception may have the following
 * minor codes (the high 20 bits being Classpath VMCID):
 * 
 * <table border="1">
 * <tr>
 * <th>Hex</th>
 * <th>Dec</th>
 * <th>Minor</th>
 * <th>Name</th>
 * <th>Case</th>
 * </tr>
 * <tr>
 * <td>47430001</td>
 * <td>1195573249</td>
 * <td>1</td>
 * <td>Giop</td>
 * <td>The message being received is not a GIOP message. It does not start from
 * the expected magic sequence byte[] { 'G', 'I', 'O', 'P' }.</td>
 * </tr>
 * <tr>
 * <td>47430002</td>
 * <td>1195573250</td>
 * <td>2</td>
 * <td>Header</td>
 * <td>The unexpected IOException while reading or writing the GIOP message
 * header or the subsequent request or response header</td>
 * </tr>
 * <tr>
 * <td>47430003</td>
 * <td>1195573251</td>
 * <td>3</td>
 * <td>EOF</td>
 * <td>The data stream ended before reading all expected values from it. This
 * usually means that the CORBA message is corrupted, but may also indicate that
 * the server expects the remote method being invoked to have more or different
 * parameters</td>
 * </tr>
 * <tr>
 * <td>47430005</td>
 * <td>1195573253</td>
 * <td>5</td>
 * <td>CDR</td>
 * <td>The unexpected IOException while reading or writing the data via Commond
 * Data Representation stream</td>
 * </tr>
 * <tr>
 * <td>47430006</td>
 * <td>1195573254 </td>
 * <td>6</td>
 * <td>Value</td>
 * <td>The unexpected IOException while reading or writing the Value type.
 * </td>
 * </tr>
 * <tr>
 * <td>47430007 </td>
 * <td>1195573255</td>
 * <td>7</td>
 * <td>Forwarding</td>
 * <td>The unexpected IOException while handling request forwarding.</td>
 * </tr>
 * <tr>
 * <td>47430008</td>
 * <td>1195573256</td>
 * <td>8</td>
 * <td>Encapsulation </td>
 * <td>The unexpected IOException while handling data encapsulation, tagged
 * components, tagged profiles, etc.</td>
 * </tr>
 * <tr>
 * <td>47430009</td>
 * <td>1195573257</td>
 * <td>9 </td>
 * <td>Any</td>
 * <td>The unexpected IOException while inserting or extracting data to/from
 * the Any.</td>
 * </tr>
 * <tr>
 * <td>4743000a</td>
 * <td>1195573258 </td>
 * <td>10</td>
 * <td>UserException</td>
 * <td>The unexpected UserException in the context where it cannot be handled
 * as such and must be converted to the SystemException. </td>
 * </tr>
 * <tr>
 * <td>4743000b</td>
 * <td>1195573259</td>
 * <td>11</td>
 * <td>Inappropriate</td>
 * <td>While the operation could formally be applied to the target, the OMG
 * standard states that it is actually not applicable. For example, some CORBA
 * objects like POA are always local and should not be passed to or returned
 * from the remote side.</td>
 * </tr>
 * <tr>
 * <td>4743000c</td>
 * <td>1195573260</td>
 * <td>12</td>
 * <td>Negative</td>
 * <td>When reading data, it was discovered that size of the data structure
 * like string, sequence or character is written as the negative number.</td>
 * </tr>
 * <tr>
 * <td>4743000e</td>
 * <td>1195573262 </td>
 * <td>14</td>
 * <td>Graph</td>
 * <td>Reference to non-existing node in the data grapth while reading the
 * value types.</td>
 * </tr>
 * <tr>
 * <td>4743000f</td>
 * <td>1195573263</td>
 * <td>15</td>
 * <td>Boxed</td>
 * <td>Unexpected exception was thrown from the IDL type helper while handling
 * the object of this type as a boxed value.</td>
 * </tr>
 * <tr>
 * <td>47430010</td>
 * <td>1195573264</td>
 * <td>16</td>
 * <td>Instantiation</td>
 * <td>Unable to instantiate an value type object while reading it from the
 * stream.</td>
 * </tr>
 * <tr>
 * <td>47430011</td>
 * <td>1195573265</td>
 * <td>17</td>
 * <td>ValueHeaderTag</td>
 * <td>The header tag of the value type being read from the CDR stream contains
 * an unexpected value outside 0x7fffff00 .. 0x7fffffff and also not null and
 * not an indirection.</td>
 * </tr>
 * <tr>
 * <td>47430012</td>
 * <td>1195573266</td>
 * <td>18</td>
 * <td>ValueHeaderFlags</td>
 * <td>The header tag flags of the value type being read from the CDR stream
 * make the invalid combination (for instance, 0x7fffff04).</td>
 * </tr>
 * <tr>
 * <td>47430013</td>
 * <td>1195573267</td>
 * <td>19</td>
 * <td>ClassCast</td>
 * <td>The value type class, written on the wire, is not compatible with the
 * expected class, passed as a parameter to the InputStream.read_value.</td>
 * </tr>
 * <tr>
 * <td>47430014</td>
 * <td>1195573268</td>
 * <td>20</td>
 * <td>Offset</td>
 * <td>Positive or otherwise invalid indirection offset when reading the data
 * graph of the value type.</td>
 * </tr>
 * <tr>
 * <td>47430015</td>
 * <td>1195573269</td>
 * <td>21</td>
 * <td>Chunks</td>
 * <td>Errors while reading the chunked value type.</td>
 * </tr>
 * <tr>
 * <td>47430016</td>
 * <td>1195573270</td>
 * <td>22</td>
 * <td>UnsupportedValue</td>
 * <td>No means are provided to read or write this value type (not Streamable,
 * not CustomMarshal, not Serializable, no factory, no helper.</td>
 * </tr>
 * <tr> 
 * <td>47430017</td>
 * <td>1195573271</td>
 * <td>23</td>
 * <td>Factory</td>
 * <td>The value factory, required for the operation being invoked, is not
 * registered with this ORB.</td>
 * </tr>
 * <tr>
 * <td>47430018</td>
 * <td>1195573272</td>
 * <td>24</td>
 * 
 * <td>UnsupportedAddressing</td>
 * <td>Unsupported object addressing method in GIOP request header.</td>
 * </tr>
 * <tr>
 * <td>47430019</td>
 * <td>1195573273</td>
 * <td>25</td>
 * <td>IOR</td>
 * <td>Invalid object reference (IOR).</td>
 * </tr>
 * <tr>
 * <td>4743001a</td>
 * <td>1195573274</td>
 * <td>26</td>
 * <td>TargetConversion</td>
 * <td>Problems with converting between stubs, ties, interfaces and
 * implementations.</td>
 * 
 * </tr>
 * <tr>
 * <td>4743001b</td>
 * <td>1195573275</td>
 * <td>27</td>
 * <td>ValueFields</td>
 * <td>Problems with reading or writing the fields of the value type object
 * </td>
 * </tr>
 * <tr>
 * <td>4743001c</td>
 * <td>1195573276</td>
 * <td>28</td>
 * <td>NonSerializable</td>
 * <td>The instance of the value type, passed using RMI over IIOP, is not
 * serializable</td>
 * </tr>
 * </table>
 * 
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class MARSHAL
  extends SystemException
  implements Serializable
{
  /**
   * Use serialVersionUID for interoperability.
   */
  private static final long serialVersionUID = 7416408250336395546L;

  /**
   * Creates a MARSHAL with the default minor code of 0, completion state
   * COMPLETED_NO and the given explaining message.
   * 
   * @param reasom the explaining message.
   */
  public MARSHAL(String message)
  {
    super(message, 0, CompletionStatus.COMPLETED_NO);
  }

  /**
   * Creates MARSHAL with the default minor code of 0 and a completion state
   * COMPLETED_NO.
   */
  public MARSHAL()
  {
    super("", 0, CompletionStatus.COMPLETED_NO);
  }

  /**
   * Creates a MARSHAL exception with the specified minor code and completion
   * status.
   * 
   * @param minor_code additional error code.
   * @param is_completed the method completion status.
   */
  public MARSHAL(int minor_code, CompletionStatus is_completed)
  {
    super("", minor_code, is_completed);
  }

  /**
   * Created MARSHAL exception, providing full information.
   * 
   * @param reason explaining message.
   * @param minor_code additional error code (the "minor").
   * @param is_completed the method completion status.
   */
  public MARSHAL(String reason, int minor_code, CompletionStatus is_completed)
  {
    super(reason, minor_code, is_completed);
  }
}
