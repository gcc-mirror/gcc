/* gnu.java.beans.decoder.DummyHandler
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

import java.beans.ExceptionListener;

import org.xml.sax.Attributes;

/** An ElementHandler implementation that is used as an artificial root
 * element. This avoids having to check for a null element.
 *
 * @author Robert Schuster
 */
class DummyHandler implements ElementHandler
{
    /* (non-Javadoc)
     * @see gnu.java.beans.decoder.ElementHandler#start(org.xml.sax.Attributes, java.beans.ExceptionListener)
     */
    public void start(
        Attributes attributes,
        ExceptionListener exceptionListener)
    {
        fail();
    }

    /* (non-Javadoc)
     * @see gnu.java.beans.decoder.ElementHandler#end(java.beans.ExceptionListener)
     */
    public void end(ExceptionListener exceptionListener)
    {
        fail();
    }

    /* (non-Javadoc)
     * @see gnu.java.beans.decoder.ElementHandler#characters(char[], int, int)
     */
    public void characters(char[] ch, int start, int length)
    {
        fail();
    }

    /* (non-Javadoc)
     * @see gnu.java.beans.decoder.ElementHandler#isSubelementAllowed(java.lang.String)
     */
    public boolean isSubelementAllowed(String subElementName)
    {
        return true;
    }

    /* (non-Javadoc)
     * @see gnu.java.beans.decoder.ElementHandler#instantiateClass(java.lang.String)
     */
    public Class instantiateClass(String className)
        throws ClassNotFoundException
    {
        fail();
	return null;
    }

    /* (non-Javadoc)
     * @see gnu.java.beans.decoder.ElementHandler#reportStatement(java.beans.ExceptionListener)
     */
    public void notifyStatement(ExceptionListener exceptionListener)
    {
        // ignore
    }

    /* (non-Javadoc)
     * @see gnu.java.beans.decoder.ElementHandler#hasFailed()
     */
    public boolean hasFailed()
    {
        return false;
    }

    /* (non-Javadoc)
     * @see gnu.java.beans.decoder.ElementHandler#getContext()
     */
    public Context getContext()
    {
        return new DummyContext();
    }

    /* (non-Javadoc)
     * @see gnu.java.beans.decoder.ElementHandler#contextFailed()
     */
    public void notifyContextFailed()
    {
        fail();
    }

    /* (non-Javadoc)
     * @see gnu.java.beans.decoder.ElementHandler#putObject(java.lang.String, java.lang.Object)
     */
    public void putObject(String objectId, Object o)
    {
        fail();
    }

    /* (non-Javadoc)
     * @see gnu.java.beans.decoder.ElementHandler#getObject(java.lang.String)
     */
    public Object getObject(String objectId)
    {
        fail();
	return null;
    }

    public ElementHandler getParent()
    {
        fail();
	return null;
    }

    private void fail()
    {
        throw new InternalError("Invoking the DummyHandler is not expected"
			        + " - Please file a bug report at "
				+ " http://www.gnu.org/software/classpath/.");
    }
}
