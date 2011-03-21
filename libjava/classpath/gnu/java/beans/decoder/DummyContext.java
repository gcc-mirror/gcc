/* gnu.java.beans.decoder.DummyContext
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

package gnu.java.beans.decoder;

/** The DummyContext is used as the Context implementation for the DummyHandler. It
 * just prevents having a null-reference.
 *
 * <p>When the implementation is correct none of this class' methods
 * (except <code>notifyStatement()</code>) is called.</p>
 *
 * @author Robert Schuster
 */
public class DummyContext extends AbstractContext
{
    /* (non-Javadoc)
     * @see gnu.java.beans.decoder.Context#addObject(java.lang.Object)
     */
    public void addParameterObject(Object o) throws AssemblyException
    {
        fail();
    }

    /* (non-Javadoc)
     * @see gnu.java.beans.decoder.Context#reportStatement()
     */
    public void notifyStatement(Context outerContext) throws AssemblyException
    {
        // intentionally ignored
    }

    /* (non-Javadoc)
     * @see gnu.java.beans.decoder.Context#endContext(gnu.java.beans.decoder.Context)
     */
    public Object endContext(Context outerContext) throws AssemblyException
    {
        fail();
        return null;
    }

    /* (non-Javadoc)
     * @see gnu.java.beans.decoder.Context#subContextFailed()
     */
    public boolean subContextFailed()
    {
        fail();
        return false;
    }

    /* (non-Javadoc)
     * @see gnu.java.beans.decoder.Context#set(int, java.lang.Object)
     */
    public void set(int index, Object o) throws AssemblyException
    {
        fail();
    }

    /* (non-Javadoc)
     * @see gnu.java.beans.decoder.Context#get(int)
     */
    public Object get(int index) throws AssemblyException
    {
        fail();
        return null;
    }

    /* (non-Javadoc)
     * @see gnu.java.beans.decoder.Context#getResult()
     */
    public Object getResult()
    {
        fail();
        return null;
    }

    private void fail()
    {
        throw new InternalError("Invoking the DummyContext is not expected"
                                + " - Please file a bug report at"
                                + " http://www/gnu.org/software/classpath/.");
    }
}
