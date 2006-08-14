/* Cascade.java -- 
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

import java.math.BigInteger;
import java.security.InvalidKeyException;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;
import java.util.Set;

/**
 * A <i>Cascade</i> Cipher is the concatenation of two or more block ciphers
 * each with independent keys. Plaintext is input to the first stage; the output
 * of stage <code>i</code> is input to stage <code>i + 1</code>; and the
 * output of the last stage is the <i>Cascade</i>'s ciphertext output.
 * <p>
 * In the simplest case, all stages in a <code>Cascade</code> have <i>k</i>-bit
 * keys, and the stage inputs and outputs are all n-bit quantities. The stage
 * ciphers may differ (general cascade of ciphers), or all be identical (cascade
 * of identical ciphers).
 * <p>
 * The term "block ciphers" used above refers to implementations of
 * {@link gnu.javax.crypto.mode.IMode}, including the
 * {@link gnu.javax.crypto.mode.ECB} mode which basically exposes a
 * symmetric-key block cipher algorithm as a <i>Mode</i> of Operations.
 * <p>
 * References:
 * <ol>
 * <li><a href="http://www.cacr.math.uwaterloo.ca/hac">[HAC]</a>: Handbook of
 * Applied Cryptography.<br>
 * CRC Press, Inc. ISBN 0-8493-8523-7, 1997<br>
 * Menezes, A., van Oorschot, P. and S. Vanstone.</li>
 * </ol>
 */
public class Cascade
{
  public static final String DIRECTION = "gnu.crypto.assembly.cascade.direction";

  /** The map of Stages chained in this cascade. */
  protected HashMap stages;

  /** The ordered list of Stage UIDs to their attribute maps. */
  protected LinkedList stageKeys;

  /** The current operational direction of this instance. */
  protected Direction wired;

  /** The curently set block-size for this instance. */
  protected int blockSize;

  public Cascade()
  {
    super();

    stages = new HashMap(3);
    stageKeys = new LinkedList();
    wired = null;
    blockSize = 0;
  }

  /**
   * Returns the Least Common Multiple of two integers.
   * 
   * @param a the first integer.
   * @param b the second integer.
   * @return the LCM of <code>abs(a)</code> and <code>abs(b)</code>.
   */
  private static final int lcm(int a, int b)
  {
    BigInteger A = BigInteger.valueOf(a * 1L);
    BigInteger B = BigInteger.valueOf(b * 1L);
    return A.multiply(B).divide(A.gcd(B)).abs().intValue();
  }

  /**
   * Adds to the end of the current chain, a designated {@link Stage}.
   * 
   * @param stage the {@link Stage} to append to the chain.
   * @return a unique identifier for this stage, within this cascade.
   * @throws IllegalStateException if the instance is already initialised.
   * @throws IllegalArgumentException if the designated stage is already in the
   *           chain, or it has incompatible characteristics with the current
   *           elements already in the chain.
   */
  public Object append(Stage stage) throws IllegalArgumentException
  {
    return insert(size(), stage);
  }

  /**
   * Adds to the begining of the current chain, a designated {@link Stage}.
   * 
   * @param stage the {@link Stage} to prepend to the chain.
   * @return a unique identifier for this stage, within this cascade.
   * @throws IllegalStateException if the instance is already initialised.
   * @throws IllegalArgumentException if the designated stage is already in the
   *           chain, or it has incompatible characteristics with the current
   *           elements already in the chain.
   */
  public Object prepend(Stage stage) throws IllegalArgumentException
  {
    return insert(0, stage);
  }

  /**
   * Inserts a {@link Stage} into the current chain, at the specified index
   * (zero-based) position.
   * 
   * @param stage the {@link Stage} to insert into the chain.
   * @return a unique identifier for this stage, within this cascade.
   * @throws IllegalArgumentException if the designated stage is already in the
   *           chain, or it has incompatible characteristics with the current
   *           elements already in the chain.
   * @throws IllegalStateException if the instance is already initialised.
   * @throws IndexOutOfBoundsException if <code>index</code> is less than
   *           <code>0</code> or greater than the current size of this
   *           cascade.
   */
  public Object insert(int index, Stage stage) throws IllegalArgumentException,
      IndexOutOfBoundsException
  {
    if (stages.containsValue(stage))
      throw new IllegalArgumentException();
    if (wired != null || stage == null)
      throw new IllegalStateException();
    if (index < 0 || index > size())
      throw new IndexOutOfBoundsException();
    // check that there is a non-empty set of common block-sizes
    Set set = stage.blockSizes();
    if (stages.isEmpty())
      {
        if (set.isEmpty())
          throw new IllegalArgumentException("1st stage with no block sizes");
      }
    else
      {
        Set common = this.blockSizes();
        common.retainAll(set);
        if (common.isEmpty())
          throw new IllegalArgumentException("no common block sizes found");
      }
    Object result = new Object();
    stageKeys.add(index, result);
    stages.put(result, stage);
    return result;
  }

  /**
   * Returns the current number of stages in this chain.
   * 
   * @return the current count of stages in this chain.
   */
  public int size()
  {
    return stages.size();
  }

  /**
   * Returns an {@link Iterator} over the stages contained in this instance.
   * Each element of this iterator is a concrete implementation of a {@link
   * Stage}.
   * 
   * @return an {@link Iterator} over the stages contained in this instance.
   *         Each element of the returned iterator is a concrete instance of a
   *         {@link Stage}.
   */
  public Iterator stages()
  {
    LinkedList result = new LinkedList();
    for (Iterator it = stageKeys.listIterator(); it.hasNext();)
      result.addLast(stages.get(it.next()));
    return result.listIterator();
  }

  /**
   * Returns the {@link Set} of supported block sizes for this
   * <code>Cascade</code> that are common to all of its chained stages. Each
   * element in the returned {@link Set} is an instance of {@link Integer}.
   * 
   * @return a {@link Set} of supported block sizes common to all the stages of
   *         the chain.
   */
  public Set blockSizes()
  {
    HashSet result = null;
    for (Iterator it = stages.values().iterator(); it.hasNext();)
      {
        Stage aStage = (Stage) it.next();
        if (result == null) // first time
          result = new HashSet(aStage.blockSizes());
        else
          result.retainAll(aStage.blockSizes());
      }
    return result == null ? Collections.EMPTY_SET : result;
  }

  /**
   * Initialises the chain for operation with specific characteristics.
   * 
   * @param attributes a set of name-value pairs that describes the desired
   *          future behaviour of this instance.
   * @throws IllegalStateException if the chain, or any of its stages, is
   *           already initialised.
   * @throws InvalidKeyException if the intialisation data provided with the
   *           stage is incorrect or causes an invalid key to be generated.
   * @see Direction#FORWARD
   * @see Direction#REVERSED
   */
  public void init(Map attributes) throws InvalidKeyException
  {
    if (wired != null)
      throw new IllegalStateException();
    Direction flow = (Direction) attributes.get(DIRECTION);
    if (flow == null)
      flow = Direction.FORWARD;
    int optimalSize = 0;
    for (Iterator it = stageKeys.listIterator(); it.hasNext();)
      {
        Object id = it.next();
        Map attr = (Map) attributes.get(id);
        attr.put(Stage.DIRECTION, flow);
        Stage stage = (Stage) stages.get(id);
        stage.init(attr);
        optimalSize = optimalSize == 0 ? stage.currentBlockSize()
                                       : lcm(optimalSize,
                                             stage.currentBlockSize());
      }
    if (flow == Direction.REVERSED) // reverse order
      Collections.reverse(stageKeys);
    wired = flow;
    blockSize = optimalSize;
  }

  /**
   * Returns the currently set block size for the chain.
   * 
   * @return the current block size for the chain.
   * @throws IllegalStateException if the instance is not initialised.
   */
  public int currentBlockSize()
  {
    if (wired == null)
      throw new IllegalStateException();
    return blockSize;
  }

  /**
   * Resets the chain for re-initialisation and use with other characteristics.
   * This method always succeeds.
   */
  public void reset()
  {
    for (Iterator it = stageKeys.listIterator(); it.hasNext();)
      ((Stage) stages.get(it.next())).reset();
    if (wired == Direction.REVERSED) // reverse it back
      Collections.reverse(stageKeys);
    wired = null;
    blockSize = 0;
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
    int stageBlockSize, j, i = stages.size();
    for (Iterator it = stageKeys.listIterator(); it.hasNext();)
      {
        Stage stage = (Stage) stages.get(it.next());
        stageBlockSize = stage.currentBlockSize();
        for (j = 0; j < blockSize; j += stageBlockSize)
          stage.update(in, inOffset + j, out, outOffset + j);
        i--;
        if (i > 0)
          System.arraycopy(out, outOffset, in, inOffset, blockSize);
      }
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
  public boolean selfTest()
  {
    for (Iterator it = stageKeys.listIterator(); it.hasNext();)
      {
        if (! ((Stage) stages.get(it.next())).selfTest())
          return false;
      }
    return true;
  }
}
