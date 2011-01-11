/* Assembly.java --
   Copyright (C) 2003, 2006 Free Software Foundation, Inc.

This file is a part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
USA

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
exception statement from your version.  */


package gnu.javax.crypto.assembly;

import java.util.Map;

/**
 * An <code>Assembly</code> is a construction consisting of a chain of
 * {@link Transformer} elements; each wired in pre- or post- transformation
 * mode. This chain is terminated by one <code>LoopbackTransformer</code>
 * element.
 * <p>
 * Once constructed, and correctly initialised, the bulk of the methods
 * available on the <code>Assembly</code> are delegated to the <i>head</i> of
 * the {@link Transformer} chain of the <code>Assembly</code>.
 *
 * @see Transformer
 */
public class Assembly
{
  public static final String DIRECTION = "gnu.crypto.assembly.assembly.direction";

  /** Flag that tells if the instance is initialised or not; and if yes how. */
  private Direction wired;

  /** The first Transformer in the chain. */
  private Transformer head;

  /**
   * Trivial constructor that sets the <i>chain</i> to a
   * <code>LoopbackTransformer</code>.
   */
  public Assembly()
  {
    super();

    wired = null;
    head = new LoopbackTransformer();
  }

  /**
   * Adds the designated {@link Transformer} and signals that it should operate
   * in pre-processing mode; i.e. it should apply its internal transformation
   * algorithm on the input data stream, <b>before</b> it passes that stream to
   * the next element in the <i>chain</i>.
   *
   * @param t the {@link Transformer} to add at the head of the current chain.
   * @throws IllegalArgumentException if the designated {@link Transformer} has
   *           a non-null tail; i.e. it is already an element of a chain.
   */
  public void addPreTransformer(Transformer t)
  {
    wireTransformer(t, Operation.PRE_PROCESSING);
  }

  /**
   * Adds the designated {@link Transformer} and signals that it should operate
   * in post-processing mode; i.e. it should apply its internal transformation
   * algorithm on the input data stream, <b>after</b> it passes that stream to
   * the next element in the <i>chain</i>.
   *
   * @param t the {@link Transformer} to add at the head of the current chain.
   * @throws IllegalArgumentException if the designated {@link Transformer} has
   *           a non-null tail; i.e. it is already an element of a chain.
   */
  public void addPostTransformer(Transformer t)
  {
    wireTransformer(t, Operation.POST_PROCESSING);
  }

  /**
   * Initialises the <code>Assembly</code> for operation with specific
   * characteristics.
   *
   * @param attributes a set of name-value pairs that describes the desired
   *          future behaviour of this instance.
   * @throws IllegalStateException if the instance is already initialised.
   */
  public void init(Map attributes) throws TransformerException
  {
    if (wired != null)
      throw new IllegalStateException();
    Direction flow = (Direction) attributes.get(DIRECTION);
    if (flow == null)
      flow = Direction.FORWARD;
    attributes.put(Transformer.DIRECTION, flow);
    head.init(attributes);
    wired = flow;
  }

  /**
   * Resets the <code>Assembly</code> for re-initialisation and use with other
   * characteristics. This method always succeeds.
   */
  public void reset()
  {
    head.reset();
    wired = null;
  }

  /**
   * Convenience method that calls the method with same name and three
   * arguments, using a byte array of length <code>1</code> whose contents are
   * the designated byte.
   *
   * @param b the byte to process.
   * @return the result of transformation.
   * @throws IllegalStateException if the instance is not initialised.
   * @throws TransformerException if a transformation-related exception occurs
   *           during the operation.
   * @see #update(byte[], int, int)
   */
  public byte[] update(byte b) throws TransformerException
  {
    return update(new byte[] { b }, 0, 1);
  }

  /**
   * Convenience method that calls the method with same name and three
   * arguments. All bytes in <code>in</code>, starting from index position
   * <code>0</code> are considered.
   *
   * @param in the input data bytes.
   * @return the result of transformation.
   * @throws IllegalStateException if the instance is not initialised.
   * @throws TransformerException if a transformation-related exception occurs
   *           during the operation.
   * @see #update(byte[], int, int)
   */
  public byte[] update(byte[] in) throws TransformerException
  {
    return update(in, 0, in.length);
  }

  /**
   * Processes a designated number of bytes from a given byte array.
   *
   * @param in the input data bytes.
   * @param offset index of <code>in</code> from which to start considering
   *          data.
   * @param length the count of bytes to process.
   * @return the result of transformation.
   * @throws IllegalStateException if the instance is not initialised.
   * @throws TransformerException if a transformation-related exception occurs
   *           during the operation.
   */
  public byte[] update(byte[] in, int offset, int length)
      throws TransformerException
  {
    if (wired == null)
      throw new IllegalStateException();
    return head.update(in, offset, length);
  }

  /**
   * Convenience method that calls the method with same name and three arguments
   * using a 0-long byte array.
   *
   * @return the result of transformation.
   * @throws IllegalStateException if the instance is not initialised.
   * @throws TransformerException if a transformation-related exception occurs
   *           during the operation.
   * @see #lastUpdate(byte[], int, int)
   */
  public byte[] lastUpdate() throws TransformerException
  {
    return lastUpdate(new byte[0], 0, 0);
  }

  /**
   * Convenience method that calls the method with same name and three
   * arguments, using a byte array of length <code>1</code> whose contents are
   * the designated byte.
   *
   * @param b the byte to process.
   * @return the result of transformation.
   * @throws IllegalStateException if the instance is not initialised.
   * @throws TransformerException if a transformation-related exception occurs
   *           during the operation.
   * @see #lastUpdate(byte[], int, int)
   */
  public byte[] lastUpdate(byte b) throws TransformerException
  {
    return lastUpdate(new byte[] { b }, 0, 1);
  }

  /**
   * Convenience method that calls the method with same name and three
   * arguments. All bytes in <code>in</code>, starting from index position
   * <code>0</code> are considered.
   *
   * @param in the input data bytes.
   * @return the result of transformation.
   * @throws IllegalStateException if the instance is not initialised.
   * @throws TransformerException if a transformation-related exception occurs
   *           during the operation.
   * @see #lastUpdate(byte[], int, int)
   */
  public byte[] lastUpdate(byte[] in) throws TransformerException
  {
    return lastUpdate(in, 0, in.length);
  }

  /**
   * Processes a designated number of bytes from a given byte array and signals,
   * at the same time, that this is the last <i>push</i> operation for this
   * <code>Assembly</code>.
   *
   * @param in the input data bytes.
   * @param offset index of <code>in</code> from which to start considering
   *          data.
   * @param length the count of bytes to process.
   * @return the result of transformation.
   * @throws IllegalStateException if the instance is not initialised.
   * @throws TransformerException if a transformation-related exception occurs
   *           during the operation.
   */
  public byte[] lastUpdate(byte[] in, int offset, int length)
      throws TransformerException
  {
    if (wired == null)
      throw new IllegalStateException();
    byte[] result = head.lastUpdate(in, offset, length);
    reset();
    return result;
  }

  private void wireTransformer(Transformer t, Operation mode)
  {
    if (t.tail != null)
      throw new IllegalArgumentException();
    t.setMode(mode);
    t.tail = head;
    head = t;
  }
}
