/* gnu.java.beans.decoder.DecoderContext
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

package gnu.java.beans.decoder;

import java.beans.XMLDecoder;
import java.util.ArrayList;
import java.util.Iterator;

/** DecoderContext is a Context implementation which allows access to
 * the XMLDecoder instance itself. This is used for the &lt;java&gt; tag.
 *
 * @author Robert Schuster
 */
public class DecoderContext extends AbstractContext
{
  private XMLDecoder decoder;

  public DecoderContext(XMLDecoder xmlDecoder)
  {
    decoder = xmlDecoder;
  }

  private ArrayList objects = new ArrayList();

  /* (non-Javadoc)
   * @see gnu.java.beans.decoder.Context#addObject(java.lang.Object)
   */
  public void addParameterObject(Object o) throws AssemblyException
  {
    objects.add(o);
  }

  /* (non-Javadoc)
   * @see gnu.java.beans.decoder.Context#reportStatement()
   */
  public void notifyStatement(Context outerContext) throws AssemblyException
  {
  }

  /* (non-Javadoc)
   * @see gnu.java.beans.decoder.Context#endContext(gnu.java.beans.decoder.Context)
   */
  public Object endContext(Context outerContext) throws AssemblyException
  {
    return decoder;
  }

  /* (non-Javadoc)
   * @see gnu.java.beans.decoder.Context#subContextFailed()
   */
  public boolean subContextFailed()
  {
    return false;
  }

  /* (non-Javadoc)
   * @see gnu.java.beans.decoder.Context#set(int, java.lang.Object)
   */
  public void set(int index, Object o) throws AssemblyException
  {
    throw new AssemblyException(new IllegalArgumentException("Set method is not allowed in decoder context."));
  }

  /* (non-Javadoc)
   * @see gnu.java.beans.decoder.Context#get(int)
   */
  public Object get(int index) throws AssemblyException
  {
    throw new AssemblyException(new IllegalArgumentException("Get method is not allowed in decoder context."));
  }

  /* (non-Javadoc)
   * @see gnu.java.beans.decoder.Context#getResult()
   */
  public Object getResult()
  {
    return decoder;
  }

  /** Returns an Iterator that retrieves the assembled objects.
   * 
   * @return An Iterator retrieving assembled objects.
   */
  public Iterator iterator()
  {
    return objects.iterator();
  }
  
}
