/* URIResolver.java -- 
   Copyright (C) 2004, 2005  Free Software Foundation, Inc.

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
package javax.xml.transform;

/**
 * Callback that can resolve a URI into source XML for transformation.
 * Equivalent to the SAX EntityResolver.
 * 
 * @author (a href='mailto:dog@gnu.org'>Chris Burdess</a)
 */
public interface URIResolver
{

  /**
   * Resolves an <code>xsl:include</code>, <code>xsl:import</code>, or
   * XPath <code>document()</code> function.
   * @param href the URI to resolve (relative or absolute)
   * @param base the base URI relative to which the URI is to be resolved
   * @return a source, or null if the resource could not be found
   */
  public Source resolve(String href, String base)
    throws TransformerException;

}
