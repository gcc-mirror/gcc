/* Stage.java --
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

import gnu.javax.crypto.mode.IMode;

import java.security.InvalidKeyException;
import java.util.Map;
import java.util.Set;

/**
 * A <i>Stage</i> in a Cascade Cipher.
 * <p>
 * Each stage may be either an implementation of a Block Cipher Mode of
 * Operation ({@link IMode}) or another Cascade Cipher ({@link Cascade}).
 * Each stage has also a <i>natural</i> operational direction when constructed
 * for inclusion within a {@link Cascade}. This <i>natural</i> direction
 * dictates how data flows from one stage into another when stages are chained
 * together in a cascade. One can think of a stage and its natural direction as
 * the specification of how to wire the stage into the chain. The following
 * diagrams may help understand the paradigme. The first shows two stages
 * chained each with a {@link Direction#FORWARD} direction.
 *
 * <pre>
 *            FORWARD         FORWARD
 *        +------+       +-------+
 *        |      |       |       |
 *        |  +--in --+   |   +--in --+
 *     ---+  | Stage |   |   | Stage |  +---
 *           +--out--+   |   +--out--+  |
 *               |       |       |      |
 *               +-------+       +------+
 * </pre>
 *
 * <p>
 * The second diagram shows two stages, one in a {@link Direction#FORWARD}
 * direction, while the other is wired in a {@link Direction#REVERSED}
 * direction.
 *
 * <pre>
 *            FORWARD         REVERSED
 *        +------+               +------+
 *        |      |               |      |
 *        |  +--in --+       +--in --+  |
 *     ---+  | Stage |       | Stage |  +---
 *           +--out--+       +--out--+
 *               |               |
 *               +---------------+
 * </pre>
 *
 * @see ModeStage
 * @see CascadeStage
 */
public abstract class Stage
{
  public static final String DIRECTION = "gnu.crypto.assembly.stage.direction";

  protected Direction forward;

  protected Direction wired;

  protected Stage(Direction forwardDirection)
  {
    super();

    this.forward = forwardDirection;
    this.wired = null;
  }

  public static final Stage getInstance(IMode mode, Direction forwardDirection)
  {
    return new ModeStage(mode, forwardDirection);
  }

  public static final Stage getInstance(Cascade cascade,
                                        Direction forwardDirection)
  {
    return new CascadeStage(cascade, forwardDirection);
  }

  /**
   * Returns the {@link Set} of supported block sizes for this
   * <code>Stage</code>. Each element in the returned {@link Set} is an
   * instance of {@link Integer}.
   *
   * @return a {@link Set} of supported block sizes.
   */
  public abstract Set blockSizes();

  /**
   * Initialises the stage for operation with specific characteristics.
   *
   * @param attributes a set of name-value pairs that describes the desired
   *          future behaviour of this instance.
   * @throws IllegalStateException if the instance is already initialised.
   * @throws InvalidKeyException if the key data is invalid.
   */
  public void init(Map attributes) throws InvalidKeyException
  {
    if (wired != null)
      throw new IllegalStateException();
    Direction flow = (Direction) attributes.get(DIRECTION);
    if (flow == null)
      {
        flow = Direction.FORWARD;
        attributes.put(DIRECTION, flow);
      }
    initDelegate(attributes);
    wired = flow;
  }

  /**
   * Returns the currently set block size for the stage.
   *
   * @return the current block size for this stage.
   * @throws IllegalStateException if the instance is not initialised.
   */
  public abstract int currentBlockSize() throws IllegalStateException;

  /**
   * Resets the stage for re-initialisation and use with other characteristics.
   * This method always succeeds.
   */
  public void reset()
  {
    resetDelegate();
    wired = null;
  }

  /**
   * Processes exactly one block of <i>plaintext</i> (if initialised in the
   * {@link Direction#FORWARD} state) or <i>ciphertext</i> (if initialised in
   * the {@link Direction#REVERSED} state).
   *
   * @param in the plaintext.
   * @param inOffset index of <code>in</code> from which to start considering
   *          data.
   * @param out the ciphertext.
   * @param outOffset index of <code>out</code> from which to store result.
   * @throws IllegalStateException if the instance is not initialised.
   */
  public void update(byte[] in, int inOffset, byte[] out, int outOffset)
  {
    if (wired == null)
      throw new IllegalStateException();
    updateDelegate(in, inOffset, out, outOffset);
  }

  /**
   * Conducts a simple <i>correctness</i> test that consists of basic symmetric
   * encryption / decryption test(s) for all supported block and key sizes of
   * underlying block cipher(s) wrapped by Mode leafs. The test also includes
   * one (1) variable key Known Answer Test (KAT) for each block cipher.
   *
   * @return <code>true</code> if the implementation passes simple
   *         <i>correctness</i> tests. Returns <code>false</code> otherwise.
   */
  public abstract boolean selfTest();

  abstract void initDelegate(Map attributes) throws InvalidKeyException;

  abstract void resetDelegate();

  abstract void updateDelegate(byte[] in, int inOffset, byte[] out,
                               int outOffset);
}
