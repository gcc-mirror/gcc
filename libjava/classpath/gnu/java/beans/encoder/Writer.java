/* Writer.java -- Writing interface for XML persistence.
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


package gnu.java.beans.encoder;

/** A <code>Writer</code> represents a simplified interface to an XML
 * writer that is used for the XML persistence mechanism.
 *
 * <p>Its sole purpose is to allow multiple backends which may remove
 * the need to have certain APIs in the classpath. Eg. it is possible
 * to write a stripped down XML Writer that does not rely on SAX, StAX
 * or DOM APIs.</p>
 *
 * <p>The caller may assume that every action is done immediately. However
 * it is possible that the underlying implementation uses buffering streams.
 * To make sure the data is written call the {@link flush} method.</p>
 *
 * <p>The <code>Writer</code> implementation should care about the formatting
 * of the XML stream making it possible to generate three types of formats using
 * a special method invocation chain.</p>
 *
 * <p>Write
 * <code>
 * &lt;element/&gt;
 * </code>
 * by issuing <code>write("element", true)</code> (or any of the other
 * write-variants that allows specifying the <code>isEmpty</code> argument)
 * and <code>writeEnd(true)</code>.</p>
 *
 * <p>Write
 * <code>
 * &lt;element&gt;body&lt;/element&gt;
 * </code>
 * by issuing <code>writeNoChildren("element", "body")</code> and <code>writeNoChildrenEnd()</code>.</p>
 *
 * <p>
 * Write
 * <code>
 * &lt;element&gt;
 *   &lt;child1/&gt;
 *   &lt;child2/&gt;
 *   ...
 * &lt;element/&gt;
 * </code>
 * by issuing <code>write("element", false)</code> (or any of the other
 * write-variants that allows specifying the <code>isEmpty</code> argument)
 * and <code>writeEnd(false)</code>.</p>
 *
 * <p>Note: It is important that the values of <code>isEmpty</code> and
 * <code>wasEmpty</code> match. Otherwise strange things might happen to
 * the layout.</p>
 *
 * @author Robert Schuster (robertschuster@fsfe.org)
 *
 */
public interface Writer
{
  // TODO: This interface's design is not the best. Feel free to
  // improve it as you like.

  /** Writes the XML preamble. */
  void writePreamble();

  /** Writes the end of an XML tag.
   *
   * <p>If your tag has not generated any body text or child
   * elements provide <code>true</code> as the argument to generate
   * more space efficient variant of the tag.>/p>
   *
   * @param wasEmpty Whether the tag was empty or not.
   */
  void writeEnd(boolean wasEmpty);

  /** Writes an XML tag without any attributes.
   *
   * @param tagName The name of the tag to write.
   * @param empty Whether the element has child elements.
   */
  void write(String tagName, boolean empty);

  /** Writes an XML tag with one attribute name and value.
   *
   * @param tagName The name of the tag to write.
   * @param attributeName The name of attribute.
   * @param attributeValue The attribute's value.
   * @param empty Whether the element has child elements.
   */
  void write(String tagName, String attributeName, String attributeValue, boolean empty);

  /** Writes an XML tag with multiple attributes and a body text.
   *
   * @param tagName The name of the tag to write.
   * @param value The element's body content.
   * @param attributeNames A set of attribute names.
   * @param attributeValues A set of attribute values.
   * @param empty Whether the element has child elements.
   */
  void write(String tagName, String value, String[] attributeNames,
             String[] attributeValues, boolean empty);

  /** Writes an XML tag with multiple attributes without a body text.
   *
   * @param tagName The name of the tag to write.
   * @param attributeNames A set of attribute names.
   * @param attributeValues A set of attribute values.
   * @param empty Whether the element has child elements.
   */
  void write(String tagName, String[] attributeNames, String[] attributeValues, boolean empty);

  /** Writes an XML tag with no attributes but with a body text
   * that may have child elements.
   *
   * @param tagName The name of the tag to write.
   * @param value The element's body content.
   */
  void write(String tagName, String value);

  /** Writes an XML tag with no attributes but with a body text
   * that does not have child elements.
   *
   * @param tagName The name of the tag to write.
   * @param value The element's body content.
   */
  void writeNoChildren(String tagName, String value);

  /** Writes the end of an XML tag that has no child elements.
   *
   * <p>Must be used in combination with {@link writeNoChildren} only.</p>
   */
  void writeEndNoChildren();

  /** Forces the implementation to write some data.
   */
  void flush();

  /** Closes the writer.
   */
  void close();
}
