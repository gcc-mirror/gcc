/* CSSParserCallback.java -- Callback for parsing CSS
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package gnu.javax.swing.text.html.css;

/**
 * Defines the callback that is used by the CSSParser to notify the
 * backend of the parsing process.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public interface CSSParserCallback
{

  /**
   * Signals the beginning of a statement.
   *
   * A CSS statement is build up like follows:
   * <pre>
   * <selector> {
   *   ... declarations...
   * }
   * </pre>
   *
   * After startStatement(), the callback will receive zero to n callbacks
   * to declaration, followed by an endStatement() call.
   *
   * @param selector the selector of the statement.
   */
  void startStatement(Selector[] selector);

  /**
   * Signals the end of a statement.
   */
  void endStatement();

  /**
   * Signals the parsing of one declaration, which defines a mapping
   * from a property to a value.
   *
   * @param property the property
   * @param value the value
   */
  void declaration(String property, String value);

}
