/* PrinterStateReasons.java --
   Copyright (C) 2004, 2005 Free Software Foundation, Inc.

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


package javax.print.attribute.standard;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import javax.print.attribute.PrintServiceAttribute;

/**
 * The <code>PrinterStateReasons</code> attribute provides the set of 
 * additional informations available about the current state of the printer 
 * device.
 * <p>
 * The attribute is basically a map with <code>PrinterStateReason</code>
 * objects as keys associated with their severity level as 
 * <code>Severity</code> instances. The IPP keyword value can be 
 * constructed as follows: <br>
 * <code>reason.toString() + '-' + severity.toString()</code> 
 * </p>
 * <p>
 * <b>IPP Compatibility:</b> PrinterStateReasons is an IPP 1.1 attribute.
 * </p>
 * @see javax.print.attribute.standard.PrinterState
 * @see javax.print.attribute.standard.PrinterStateReason
 * @see javax.print.attribute.standard.Severity
 * 
 * @author Michael Koch (konqueror@gmx.de)
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public final class PrinterStateReasons extends HashMap
  implements PrintServiceAttribute
{
  private static final long serialVersionUID = -3731791085163619457L;

  /**
   * Constructs an empty <code>PrinterStateReasons</code> attribute.
   */  
  public PrinterStateReasons()
  {
    super();
  }

  /**
   * Constructs an empty <code>PrinterStateReasons</code> attribute
   * with the given initial capacity and load factor.
   * 
   * @param initialCapacity the intial capacity.
   * @param loadFactor the load factor of the underlying HashMap.
   * 
   * @throws IllegalArgumentException if initialCapacity &lt; 0
   * @throws IllegalArgumentException if initialCapacity or loadFactor &lt; 0
   */
  public PrinterStateReasons(int initialCapacity, float loadFactor)
  {
    super(initialCapacity, loadFactor);
  }

  /**
   * Constructs an empty <code>PrinterStateReasons</code> attribute
   * with the given initial capacity and the default load factor.
   * 
   * @param initialCapacity the intial capacity.
   * 
   * @throws IllegalArgumentException if initialCapacity &lt; 0
   */
  public PrinterStateReasons(int initialCapacity)
  {
    super(initialCapacity);
  }

  /**
   * Constructs a <code>PrinterStateReasons</code> attribute
   * with the given content of the map.
   * 
   * @param map the map for the initial values with the same 
   * <code>PrinterStateReason</code> to <code>Severity</code> mappings.
   * 
   * @throws NullPointerException if map or any key/value is <code>null</code>.
   * @throws ClassCastException if values of map are not of type 
   * <code>PrinterStateReason</code> and keys are not of type 
   * <code>Severity</code>.
   */
  public PrinterStateReasons(Map map)
  {
    super(map.size(), 0.75f);
    Iterator it = map.entrySet().iterator();
    while (it.hasNext())
      {
        Map.Entry entry = (Map.Entry) it.next();
        put(entry.getKey(), entry.getValue());
      }
  }

  /**
   * Constructs an unmodifiable view of the contained printer state reasons
   * associated with the given severity level.
   * 
   * @param severity the severity level for the constructed set.
   * @return The set of printer state reasons.
   */
  public Set printerStateReasonSet(Severity severity)
  {
    if (severity == null)
      throw new NullPointerException("severity is null");
    
    HashSet set = new HashSet();
    Iterator it = entrySet().iterator();
    while (it.hasNext())
      {
        Map.Entry entry = (Map.Entry) it.next();
        if (entry.getValue().equals(severity))
          set.add(entry.getKey());
      }
    
    return Collections.unmodifiableSet(set);
  }
  
  /**
   * Puts the given reason object associated with the given severity object
   * into the set.
   * 
   * @param reason the reason of type <code>PrinterStateReason</code>.
   * @param severity the severity of the reason of type <code>Severity</code>.
   * 
   * @return The previously associated severity of the reason or 
   * <code>null</code> if the reason object was not in the map before.
   * 
   * @throws NullPointerException if any of the values is <code>null</code>.
   * @throws ClassCastException if reason is not a 
   * <code>PrinterStateReason</code> and severity is not a 
   * <code>Severity</code> instance.
   */
  public Object put(Object reason, Object severity)
  {
    if (reason == null)
      throw new NullPointerException("reason is null");    
    if (severity == null)
      throw new NullPointerException("severity is null");
    
    return put((PrinterStateReason) reason, (Severity) severity);
  }   
  
  /**
   * Returns category of this class.
   *
   * @return The class <code>PrintStateReasons</code> itself.
   */
  public Class getCategory()
  {
    return PrinterStateReasons.class;
  }

  /**
   * Returns the name of this attribute.
   *
   * @return The name "printer-state-reasons".
   */
  public String getName()
  {
    return "printer-state-reasons";
  }
}
