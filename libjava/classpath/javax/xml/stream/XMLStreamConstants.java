/* XMLStreamConstants.java -- 
   Copyright (C) 2005  Free Software Foundation, Inc.

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

package javax.xml.stream;

/**
 * STaX constants.
 */
public interface XMLStreamConstants
{

  /**
   * A start element event.
   */
  static final int START_ELEMENT = 1;

  /**
   * An end element event.
   */
  static final int END_ELEMENT = 2;

  /**
   * A processing instruction event.
   */
  static final int PROCESSING_INSTRUCTION = 3;

  /**
   * A text event.
   */
  static final int CHARACTERS = 4;

  /**
   * A comment event.
   */
  static final int COMMENT = 5;

  /**
   * An ignorable whitespace event.
   */
  static final int SPACE = 6;

  /**
   * A start document event.
   */
  static final int START_DOCUMENT = 7;

  /**
   * An end document event.
   */
  static final int END_DOCUMENT = 8;

  /**
   * An entity reference event.
   */
  static final int ENTITY_REFERENCE = 9;

  /**
   * An attribute event.
   */
  static final int ATTRIBUTE = 10;

  /**
   * A DOCTYPE declaration event.
   */
  static final int DTD = 11;

  /**
   * A CDATA section event.
   */
  static final int CDATA = 12;

  /**
   * A namespace event.
   */
  static final int NAMESPACE = 13;

  /**
   * A start-entity event.
   */
  static final int START_ENTITY = 14;

  /**
   * An end-entity event.
   */
  static final int END_ENTITY = 15;
  
  /**
   * A notation declaration event.
   */
  static final int NOTATION_DECLARATION = 16;

  /**
   * An entity declaration event.
   */
  static final int ENTITY_DECLARATION = 17;
  
}

