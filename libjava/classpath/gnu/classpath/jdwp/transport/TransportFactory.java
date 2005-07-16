/* TransportFactory.java -- Factory for transports
   Copyright (C) 2005 Free Software Foundation

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
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */


package gnu.classpath.jdwp.transport;

import java.util.HashMap;

/**
 * A factory class that constructs transports for use by
 * the JDWP back-end.
 *
 * @author Keith Seitz  <keiths@redhat.com>
 */
public class TransportFactory
{
  // Keyword in configspec that specifies transport method
  private static final String _TRANSPORT_PROPERTY = "transport";

  // A single transport method mapping
  private static class TransportMethod
  {
    String name;
    Class clazz;
    public TransportMethod (String name, Class clazz)
    {
      this.name = name;
      this.clazz = clazz;
    }
  }

  // List of all supported transport methods
  private static TransportMethod[] _transportMethods = new TransportMethod[]
  {
    new TransportMethod (SocketTransport.NAME, SocketTransport.class)
    //new TransportMethod (ShmemTransport.NAME, ShmemTransport.class)
  };

  /**
   * Get a transport configured as specified in the properties
   *
   * @param   properties  a <code>HashMap</code> specifying the JDWP
   *                      configuration properties
   * @returns             the created and configured transport
   * @throws  TransportException for invalid configurations
   */
  public static ITransport newInstance (HashMap properties)
    throws TransportException
  {
    String name = null;
    if (properties != null)
      name = (String) properties.get (_TRANSPORT_PROPERTY);
    if (name == null)
      throw new TransportException ("no transport specified");

    for (int i = 0; i < _transportMethods.length; ++i)
      {
	if (_transportMethods[i].name.equals (name))
	  {
	    try
	      {
		ITransport t;
		t = (ITransport) _transportMethods[i].clazz.newInstance ();
		t.configure (properties);
		return t;
	      }
	    catch (TransportException te)
	      {
		throw te;
	      }
	    catch (Exception e)
	      {
		throw new TransportException (e);
	      }
	  }
      }

    throw new TransportException ("transport \"" + name + "\" not found");
  }
}
