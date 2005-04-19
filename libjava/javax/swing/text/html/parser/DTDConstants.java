/* DTDConstants.java --
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


package javax.swing.text.html.parser;

/**
 * <p>This class defines the SGML basic types, used for describing HTML 4.01
 * at {@link http://www.w3.org/TR/html4/types.html }. Not all constants,
 * defined here, are actually used in HTML 4.01 SGML specification. Some others
 * are defined just as part of the required implementation.
 * </p>
 * <p>
 * If you need more information about SGML DTD documents,
 * the author suggests to read SGML tutorial on
 * {@link http://www.w3.org/TR/WD-html40-970708/intro/sgmltut.html}.
 * We also recommend Goldfarb C.F (1991) <i>The SGML Handbook</i>,
 * Oxford University Press, 688 p, ISBN: 0198537379.
 * </p>
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public interface DTDConstants
{
  /* ----- The data types, used in HTML 4.01 SGML definition: ---- */

  /**
   * The CDATA (Character data) constant, specifes the content model,
   * consisting of characters only. In SGML for HTML 4.01, the character
   * entities must be replaced by characters, the line feeds must be
   * ignored and any number of the subsequent carriage returns or tabs
   * must be replaced by a single space.
   */
  int CDATA = 1;

  /**
   *  The EMPTY constant, means the element with no content.
   */
  int EMPTY = 17;

  /**
   * The ID constant, means that the token is the unique identifier.
   * This identifier can be referenced by attribute with value of IDREF.
   * The identifier must begin with letter, followed by any number of
   * letters, digits, hyphens, underscores, colons and periods.
   */
  int ID = 4;

  /**
   *  The IDREF constant, specifies reference to a valid ID within
   * the document.
   */
  int IDREF = 5;

  /**
   *  The IDREFS constant, a space separated list of IDREFs
   */
  int IDREFS = 6;

  /**
   * The NAME constant, means the token that
   * must begin with letter, followed by any number of
   * letters, digits, hyphens, underscores, colons and periods.
   */
  int NAME = 7;

  /**
   *  The NAMES constant, specifies a space separated of NAMEs.
   */
  int NAMES = 8;

  /**
   * The NMTOKEN constant, specifies the attribute, consisting of
   * characters that can be either digits or alphabetic characters).
   */
  int NMTOKEN = 9;

  /**
   * The NMTOKENS constant, specifies a list of NMTOKENs.
   */
  int NMTOKENS = 10;

  /**
   *  The NOTATION constant, a previously defined data type.
   */
  int NOTATION = 11;

  /**
   * The NUMBER constant (means that the attribute consists of at least
   * one decimal digit).
   */
  int NUMBER = 12;

  /**
   *  The NUMBERS constant, specifies a space separated list of NUMBERs.
   */
  int NUMBERS = 13;

  /**
   *  The NUTOKEN constant.
   */
  int NUTOKEN = 14;

  /**
   *  The NUTOKENS constant.
   */
  int NUTOKENS = 15;

  /* -------
     The entity scope constants.
     As these four constants are combined with the bitwise OR,
     they are defined in the hexadecimal notation.
     The reason of setting the two bits at once (for PUBLIC and SYSTEM)
     is probably historical.                                      ----- */

  /**
   * The PUBLIC constant, specifies the public entity. The PUBLIC entities
   * are assumed to be known to many systems so that a full declaration
   * need not be transmitted. For example,
   * &lt;!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.0//EN"&gt;
   */
  int PUBLIC = 0xA;

  /**
   * The SYSTEM constant, specifies the system entitiy. The system entities
   * are assumed to be known but require the clear identifer
   * (like the file path), where they can be found in the system.
   * For example, <code>
   * &lt;DOCTYPE html SYSTEM "/path/to/file.dtd"&gt; </code>.
   */
  int SYSTEM = 0x11;

  /**
   * The PARAMETER constant, specifies that entity is only valid
   * inside SGML DTD scope.
   */
  int PARAMETER = 0x40000;

  /**
   * The GENERAL constant, specifies theat the entity is valid in the
   * whole HTML document scope.
   */
  int GENERAL = 0x10000;

  /* ---- The constants, defining if the element attribute is required,
     fixed or implied.  ---- */

  /**
   * The attribute modifier #REQUIRED constant, indicates that the
   * value must be supplied.
   */
  int REQUIRED = 2;

  /**
   * The attribute modifier #FIXED constant, means that the attribute has
   * the fixed value that cannot be changed.
   */
  int FIXED = 1;

  /**
   * The attribute modifier #IMPLIED constant,
   * indicating that for this attribute the user agent must provide
   * the value itself.
   */
  int IMPLIED = 5;

  /**
   * The attribute modifier #CURRENT constant, specifies the value
   * that at any point in the document is the last value supplied for
   * that element. A value is required to be supplied for the first
   * occurrence of an element
   */
  int CURRENT = 3;

  /**
   * The attribute modifier #CONREF constant, specifies the IDREF value of
   * the reference to content in another location of the document.
   * The element with this attribute is empty, the content from
   * that another location must be used instead.
   */
  int CONREF = 4;

  /* ----- Constants, defining if the element
     start and end tags are required. ---- */

  /**
   *  The STARTTAG, meaning that the element needs a starting tag.
   */
  int STARTTAG = 13;

  /**
   *  The ENDTAG constant, meaning that the element needs a closing tag.
   */
  int ENDTAG = 14;

  /* ----- Other constants: ----- */

  /**
   * The ANY constant, specifies
   * an attribute, consisting from arbitrary characters.
   */
  int ANY = 19;

  /**
   *  The DEFAULT constant, specifies the default value.
   */
  int DEFAULT = 131072;

  /**
   *  The ENTITIES constant (list of ENTITYes)
   */
  int ENTITIES = 3;

  /**
   *  The ENTITY constant, meaning the numeric or symbolic name of some
   * HTML data.
   */
  int ENTITY = 2;

  /**
   *  The MD constant.
   */
  int MD = 16;

  /**
   *  The MODEL constant.
   */
  int MODEL = 18;

  /**
   * The MS constant.
   */
  int MS = 15;

  /**
   * The PI (Processing Instruction) constant, specifies a processing
   * instruction. Processing instructions are used to embed information
   * intended for specific applications.
   */
  int PI = 12;

  /**
   * The RCDATA constant (Entity References and Character Data), specifies
   * the content model, consisting of characters AND entities. The
   * "&lt;" is threated as an ordinary character, but
   * "<code>&amp;name;</code>" still means the general entity with
   *  the given name.
   */
  int RCDATA = 16;

  /**
   * The SDATA constant. Means that the value contains the entity name
   * and the replacement value of a character entity reference.
   */
  int SDATA = 11;
}
