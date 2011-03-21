/* Transformer.java --
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

import gnu.javax.crypto.pad.IPad;

import java.io.ByteArrayOutputStream;
import java.util.Map;

/**
 * A <code>Transformer</code> is an abstract representation of a two-way
 * <i>transformation</i> that can be chained together with other instances of
 * this type. Examples of such transformations in this library are:
 * {@link Cascade} cipher, {@link gnu.javax.crypto.pad.IPad} algorithm, and a
 * ZLib-based deflater/inflater algorithm. A special implementation of a
 * <code>Transformer</code> to close a chain is also provided.
 * <p>
 * A <code>Transformer</code> is characterised by the followings:
 * <ul>
 * <li>It can be chained to other instances, to form an {@link Assembly}.</li>
 * <li>When configured in an {@link Assembly}, it can be set to apply its
 * internal transformation on the input data stream before (pre-processing) or
 * after (post-processing) passing the input data to the next element in the
 * chain. Note that the same type <code>Transformer</code> can be used as
 * either in pre-processing or a post-processing modes.</li>
 * <li>A special transformer --<code>LoopbackTransformer</code>-- is used
 * to close the chain.</li>
 * <li>A useful type of <code>Transformer</code> --one we're interested in--
 * has internal buffers. The distinction between a casual push (update)
 * operation and the last one allows to correctly flush any intermediate bytes
 * that may exist in those buffers.</li>
 * </ul>
 * <p>
 * To allow wiring <code>Transformer</code> instances together, a
 * <i>minimal-output-size</i> in bytes is necessary. The trivial case of a
 * value of <code>1</code> for such attribute practically means that no output
 * buffering, from the previous element, is needed --which is independant of
 * buffering the input if the <code>Transformer</code> implementation itself
 * is block-based.
 *
 * @see CascadeTransformer
 * @see PaddingTransformer
 * @see DeflateTransformer
 */
public abstract class Transformer
{
  public static final String DIRECTION = "gnu.crypto.assembly.transformer.direction";

  protected Direction wired;

  protected Operation mode;

  protected Transformer tail = null;

  protected ByteArrayOutputStream inBuffer = new ByteArrayOutputStream(2048);

  protected ByteArrayOutputStream outBuffer = new ByteArrayOutputStream(2048);

  /** Trivial protected constructor. */
  protected Transformer()
  {
    super();

    this.wired = null;
  }

  public static final Transformer getCascadeTransformer(Cascade cascade)
  {
    return new CascadeTransformer(cascade);
  }

  public static final Transformer getPaddingTransformer(IPad padding)
  {
    return new PaddingTransformer(padding);
  }

  public static final Transformer getDeflateTransformer()
  {
    return new DeflateTransformer();
  }

  /**
   * Sets the operational mode of this <code>Transformer</code>.
   *
   * @param mode the processing mode this <code>Transformer</code> is required
   *          to operate in.
   * @throws IllegalStateException if this instance has already been assigned an
   *           operational mode.
   */
  public void setMode(final Operation mode)
  {
    if (this.mode != null)
      throw new IllegalStateException();
    this.mode = mode;
  }

  /**
   * Returns <code>true</code> if this <code>Transformer</code> was wired in
   * pre-processing mode; <code>false</code> otherwise.
   *
   * @return <code>true</code> if this <code>Transformer</code> has been
   *         wired in pre-processing mode; <code>false</code> otherwise.
   * @throws IllegalStateException if this instance has not yet been assigned an
   *           operational <i>type</i>.
   */
  public boolean isPreProcessing()
  {
    if (mode == null)
      throw new IllegalStateException();
    return (mode == Operation.PRE_PROCESSING);
  }

  /**
   * Returns <code>true</code> if this <code>Transformer</code> was wired in
   * post-processing mode; <code>false</code> otherwise.
   *
   * @return <code>true</code> if this <code>Transformer</code> has been
   *         wired in post-processing mode; <code>false</code> otherwise.
   * @throws IllegalStateException if this instance has not yet been assigned an
   *           operational <i>type</i>.
   */
  public boolean isPostProcessing()
  {
    return ! isPreProcessing();
  }

  /**
   * Initialises the <code>Transformer</code> for operation with specific
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
    wired = flow;
    inBuffer.reset();
    outBuffer.reset();
    tail.init(attributes); // initialise tail first
    initDelegate(attributes); // initialise this instance
  }

  /**
   * Returns the block-size of this <code>Transformer</code>. A value of
   * <code>1</code> indicates that this instance is block-agnostic.
   *
   * @return the current minimal required block size.
   */
  public int currentBlockSize()
  {
    if (wired == null)
      throw new IllegalStateException();
    return delegateBlockSize();
  }

  /**
   * Resets the <code>Transformer</code> for re-initialisation and use with
   * other characteristics. This method always succeeds.
   */
  public void reset()
  {
    resetDelegate();
    wired = null;
    inBuffer.reset();
    outBuffer.reset();
    tail.reset(); // reset tail last
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
   * Convenience method that calls the same method with three arguments. All
   * bytes in <code>in</code>, starting from index position <code>0</code>
   * are considered.
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
    byte[] result = (wired == Direction.FORWARD ? forwardUpdate(in, offset, length)
                                                : inverseUpdate(in, offset, length));
    return result;
  }

  /**
   * Convenience method that calls the same method with three arguments. A
   * zero-long byte array is used.
   *
   * @return the result of transformation.
   * @throws IllegalStateException if the instance is not initialised.
   * @throws TransformerException if a transformation-related exception occurs
   *           during the operation.
   * @see #lastUpdate(byte[], int, int)
   */
  public byte[] lastUpdate() throws TransformerException
  {
    byte[] result = (wired == Direction.FORWARD ? lastForwardUpdate()
                                                : lastInverseUpdate());
    if (inBuffer.size() != 0) // we still have some buffered bytes
      throw new TransformerException("lastUpdate(): input buffer not empty");
    return result;
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
   * Convenience method that calls the same method with three arguments. All
   * bytes in <code>in</code>, starting from index position <code>0</code>
   * are considered.
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
   * at the same time, that this is the last <i>push</i> operation on this
   * <code>Transformer</code>.
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
    byte[] result = update(in, offset, length);
    byte[] rest = lastUpdate();
    if (rest.length > 0)
      {
        byte[] newResult = new byte[result.length + rest.length];
        System.arraycopy(result, 0, newResult, 0, result.length);
        System.arraycopy(rest, 0, newResult, result.length, rest.length);
        result = newResult;
      }
    return result;
  }

  private byte[] forwardUpdate(byte[] in, int off, int len)
      throws TransformerException
  {
    return (isPreProcessing() ? preTransform(in, off, len)
                              : postTransform(in, off, len));
  }

  private byte[] inverseUpdate(byte[] in, int off, int len)
      throws TransformerException
  {
    return (isPreProcessing() ? postTransform(in, off, len)
                              : preTransform(in, off, len));
  }

  private byte[] preTransform(byte[] in, int off, int len)
      throws TransformerException
  {
    byte[] result = updateDelegate(in, off, len);
    result = tail.update(result);
    return result;
  }

  private byte[] postTransform(byte[] in, int off, int len)
      throws TransformerException
  {
    byte[] result = tail.update(in, off, len);
    result = updateDelegate(result, 0, result.length);
    return result;
  }

  private byte[] lastForwardUpdate() throws TransformerException
  {
    return (isPreProcessing() ? preLastTransform() : postLastTransform());
  }

  private byte[] lastInverseUpdate() throws TransformerException
  {
    return (isPreProcessing() ? postLastTransform() : preLastTransform());
  }

  private byte[] preLastTransform() throws TransformerException
  {
    byte[] result = lastUpdateDelegate();
    result = tail.lastUpdate(result);
    return result;
  }

  private byte[] postLastTransform() throws TransformerException
  {
    byte[] result = tail.lastUpdate();
    result = updateDelegate(result, 0, result.length);
    byte[] rest = lastUpdateDelegate();
    if (rest.length > 0)
      {
        byte[] newResult = new byte[result.length + rest.length];
        System.arraycopy(result, 0, newResult, 0, result.length);
        System.arraycopy(rest, 0, newResult, result.length, rest.length);
        result = newResult;
      }
    return result;
  }

  abstract void initDelegate(Map attributes) throws TransformerException;

  abstract int delegateBlockSize();

  abstract void resetDelegate();

  abstract byte[] updateDelegate(byte[] in, int off, int len)
      throws TransformerException;

  abstract byte[] lastUpdateDelegate() throws TransformerException;
}
