/* GeneratedNaturalLanguageSupported.java -- 
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


package gnu.javax.print.ipp.attribute.supported;

import gnu.javax.print.ipp.attribute.NaturalLanguageSyntax;

import javax.print.attribute.SupportedValuesAttribute;

/**
 * GeneratedNaturalLanguageSupported attribute as described 
 * in RFC 2911 section 4.4.20 provides the natural languages
 * which are supported by the IPP implementation to be used 
 * in the name and text syntax attribute types.
 * 
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public final class GeneratedNaturalLanguageSupported 
  extends NaturalLanguageSyntax 
  implements SupportedValuesAttribute
{

  /**
   * Creates a <code>GeneratedNaturalLanguageSupported</code> object.
   * 
   * @param value the charset string value.
   */
  public GeneratedNaturalLanguageSupported(String value)
  {
    super(value);
  }

  /**
   * Returns category of this class.
   *
   * @return The class <code>GeneratedNaturalLanguageSupported</code> itself.
   */
  public Class getCategory()
  {
    return GeneratedNaturalLanguageSupported.class;
  }

  /**
   * Returns the name of this attribute.
   *
   * @return The name "generated-natural-language-supported".
   */
  public String getName()
  {
    return "generated-natural-language-supported";
  }

}
