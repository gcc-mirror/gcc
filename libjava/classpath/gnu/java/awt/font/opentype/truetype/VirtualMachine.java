/* VirtualMachine.java -- Virtual machine for TrueType bytecodes.
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

package gnu.java.awt.font.opentype.truetype;

import gnu.java.lang.CPStringBuilder;

import java.awt.FontFormatException;
import java.awt.geom.AffineTransform;
import java.nio.ByteBuffer;
import java.nio.ShortBuffer;


/**
 * A virtual machine for interpreting TrueType bytecodes.
 *
 * <p><b>Lack of Thread Safety:</b> The virtual machine is
 * intentionally <i>not</i> safe to access from multiple concurrent
 * threads. Synchronization needs to be performed externally. Usually,
 * the font has already obtained a lock before calling the scaler,
 * which in turn calls the VM. It would be wasteful to acquire
 * additional locks for the VM.
 *
 * <p><b>Implementation Status:</b> The current implementation can
 * execute pre-programs of fonts, but it does not yet actually move
 * any points. Control flow and arithmeti instructions are
 * implemented, but most geometric instructions are not working
 * yet. So, the VirtualMachine class is currently a no-op.  However,
 * not very much is missing. You are more than welcome to complete the
 * implementation.
 *
 * <p><b>Patents:</b> Apple Computer holds three United States Patents
 * for the mathematical algorithms that are used by TrueType
 * instructions. The monopoly granted by these patents will expire in
 * October 2009. Before the expiration date, a license must be
 * obtained from Apple Computer to use the patented technology inside
 * the United States. For other countries, different dates might
 * apply, or no license might be needed.
 *
 * <p>The default build of this class does not use the patented
 * algorithms.  If you have obtained a license from Apple, or if the
 * patent protection has expired, or if no license is required for
 * your contry, you can set a flag in the source file which will
 * enable the use of the patented mathematical algorithms.</p>
 *
 * <p>The relevant patents are listed subsequently.</p>
 *
 * <p><ol><li>United States Patent 5155805, <i>Method and Apparatus
 * for Moving Control Points in Displaying Digital Typeface on Raster
 * Output Devices,</i> invented by Sampo Kaasila, assigned to Apple
 * Computer. Filing date: May 8, 1989. Date of patent: October 13,
 * 1992.</li>
 *
 * <li>United States Patent 5159668, <i>Method and Apparatus for
 * Manipulating Outlines in Improving Digital Typeface on Raster
 * Output Devices,</i> invented by Sampo Kaasila, assigned to Apple
 * Computer. Filing date: May 8, 1989. Date of patent: October 27,
 * 1992.</li>
 *
 * <li>United States Patent 5325479, <i>Method and Apparatus for
 * Moving Control Points in Displaying Digital Typeface on Raster
 * Output Devices,</i> invented by Sampo Kaasila, assigned to Apple
 * Computer. Filing date: May 28, 1989. Date of patent: June 28, 1994
 * (with a statement that &#x201c;[t]he portion of the term of this
 * patent subsequent to Oct. 13, 2009 has been
 * disclaimed&#x201d;).</li></ol>
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
class VirtualMachine
{
  /**
   * Indicates whether or not to perform hinting operations that are
   * protected by a number of US patents, two of which will expire on
   * October 13, 2009, and one of which will expire on October 27,
   * 2009.
   */
  private final static boolean PATENTED_HINTING = false;


  /**
   * Indicates whether the execution of the Virtual Machine is traced
   * to System.out.
   */
  private final static boolean TRACE_EXECUTION = false;


  /**
   * The value 1 in 2-dot-14 fixed notation.
   */
  private static final short ONE_214 = 0x4000; // 1 << 14


  /**
   * The storage area of the virtual machine.
   */
  private final int[] storage;


  /**
   * The stack. The stack grows from bottom to top, so
   * <code>sp[0]</code> gets used before <code>sp[1]</code>.
   */
  private int[] stack;


  /**
   * The maximum number of stack elements.
   */
  private final int maxStackElements;


  /**
   * The current stack pointer of the virtual machine.
   */
  private int sp;


  /**
   * fdefBuffer[i] is the buffer that contains the TrueType
   * instructions of function #i. Most of the time, functions are
   * defined in the font program, but a font may also re-define
   * functions in its CVT program.
   */
  private ByteBuffer[] fdefBuffer;


  /**
   * fdefEntryPoint[i] is the position in fdefBuffer[i] where the
   * first TrueType instruction after the FDEF is located.
   */
  private int[] fdefEntryPoint;


  /**
   * The original Control Value Table, sometimes abbreviated as CVT.
   * The table contains signed 16-bit FUnits. Some fonts have no CVT,
   * in which case the field will be <code>null</code>.
   */
  private ShortBuffer controlValueTable;


  /**
   * The scaled values inside the control value table.
   */
  private int[] cvt;


  /**
   * A value that is used by rounding operations to compensate for dot
   * gain.
   */
  private int engineCompensation = 0;


  /**
   * The contents of the font&#x2019;s <code>fpgm</code> table, or
   * <code>null</code> after the font program has been executed once.
   */
  private ByteBuffer fontProgram;


  /**
   * The <code>prep</code> table of the font, which contains a program
   * that is executed whenever the point size or the device transform
   * have changed.  This program is called pre-program because it gets
   * executed before the instructions of the individual glyphs.  If
   * the font does not contain a pre-program, the value of this field
   * is <code>null</code>.
   */
  private ByteBuffer preProgram;


  /**
   * The number of points in the Twilight Zone.
   */
  private int numTwilightPoints;


  /**
   * The current point size of the scaled font. The value is in Fixed
   * 26.6 notation.
   */
  private int pointSize; // 26.6

  private AffineTransform deviceTransform;

  private int scaleX, scaleY, shearX, shearY; // 26.6


  /**
   * Indicates whether or not scan-line conversion will use
   * anti-aliasing (with gray levels). Font programs can ask for this
   * value with the <code>GETINFO</code> instruction, and some
   * programs may behave differently according to this setting.
   */
  private boolean antialiased;


  /* Graphics State. FIXME: Move this to its own class? Some
   * documentation would not hurt, either.
   */
  private int cvtCutIn; // 26.6
  private int deltaBase; // uint32
  private int deltaShift; // uint32
  private short freeX; // 2.14
  private short freeY; // 2.14
  private int loop; // int
  private int minimumDistance; // 26.6
  private short projX; // 2.14
  private short projY; // 2.14
  private short dualX; // 2.14
  private short dualY; // 2.14
  private int rp0, rp1, rp2; // point numbers
  private boolean scanControl;
  private int scanType;
  private int singleWidthValue; // 26.6
  private Zone zp0, zp1, zp2;

  private Zone twilightZone;
  private Zone glyphZone;


  /**
   * Indicates whether or not the instructions that are associated
   * with individual glyphs shall be executed.  Set as a side effect
   * of executing the pre-program when the point size, device
   * transform or some other relevant parameter have changed.
   */
  private boolean executeGlyphInstructions;


  /**
   * Indicates whether to ignore any modifications to the control
   * value table that the font&#x2019;s pre-program might have
   * performed.  Set as a side effect of executing the pre-program
   * when the point size, device transform or some other relevant
   * parameter have changed.
   */
  private boolean ignoreCVTProgram;


  /**
   * The length of the space between rounded values. A value
   * of zero means that rounding has been switched off.
   */
  private int roundPeriod; // 26.6


  /**
   * The offset of the rounded values from multiples of
   * <code>roundPeriod</code>.
   */
  private int roundPhase; // 26.6


  private int roundThreshold; // 26.6


  /**
   * A cache for the number of pixels per EM. The value is a normal
   * integer, not a fixed point notation.
   *
   * @see #getPixelsPerEM()
   */
  private int cachedPixelsPerEM;


  /**
   * The number of font units per EM.
   */
  private int unitsPerEm;


  /**
   * Constructs a new Virtual Machine for executing TrueType
   * instructions.
   *
   * @param unitsPerEm the number of font units in one typographic
   * em.
   *
   * @param preProgram the <code>prep</code> table of the font, which
   * contains a program that is executed whenever the point size or
   * the device transform have changed.  This program is called
   * pre-program because it gets executed before the instructions of
   * the individual glyphs.  If the font does not contain a
   * pre-program, pass <code>null</code>.
   */
  VirtualMachine(int unitsPerEm,
                 ByteBuffer maxp,
                 ByteBuffer controlValueTable,
                 ByteBuffer fontProgram,
                 ByteBuffer preProgram)
    throws FontFormatException
  {
    int maxStorage, numFunctionDefs, maxInstructionDefs;

    if (maxp.getInt(0) != 0x00010000)
      throw new FontFormatException("unsupported maxp version");

    this.unitsPerEm = unitsPerEm;
    maxStorage = maxp.getChar(18);

    /* FreeType says that there exist some broken fonts (like
     * "Keystrokes MT") that contain function defs, but have a zero
     * value in their maxp table.
     */
    numFunctionDefs = maxp.getChar(20);
    if (numFunctionDefs == 0)
      numFunctionDefs = 64;
    fdefBuffer = new ByteBuffer[numFunctionDefs];
    fdefEntryPoint = new int[numFunctionDefs];

    /* Read the contents of the Control Value Table. */
    if (controlValueTable != null)
      this.controlValueTable = controlValueTable.asShortBuffer();

    maxInstructionDefs = maxp.getChar(22);
    maxStackElements = maxp.getChar(24);
    storage = new int[maxStorage];
    this.fontProgram = fontProgram;
    this.preProgram = preProgram;
    numTwilightPoints = maxp.getChar(16);
  }


  /**
   * Sets the graphics state to default values.
   */
  private void resetGraphicsState()
  {
    /* The freedom, projection and dual vector default to the x axis. */
    freeX = projX = dualX = ONE_214;
    freeY = projY = dualX = 0;
    cachedPixelsPerEM = 0;

    cvtCutIn = 68; // 17/16 in 26.6 notation
    deltaBase = 9;
    deltaShift = 3;
    loop = 1;
    minimumDistance = Fixed.ONE;
    singleWidthValue = 0;
    rp0 = rp1 = rp2 = 0;
    scanControl = false;
    scanType = 2;
    zp0 = zp1 = zp2 = getZone(1);

    setRoundingMode(Fixed.ONE, 0x48); // round to grid
  }


  /**
   * Reloads the control value table and scales each entry from font
   * units to pixel values.
   */
  private void reloadControlValueTable()
  {
    /* Some TrueType fonts have no control value table. */
    if (controlValueTable == null)
      return;

    /* Read in the Control Value Table. */
    if (cvt == null)
      cvt = new int[controlValueTable.capacity()];

    /* Scale the entries. */
    for (int i = 0; i < cvt.length; i++)
      cvt[i] = funitsToPixels(controlValueTable.get(i));
  }


  /**
   * Scales a value from font unites to pixels.
   *
   * @return the scaled value.
   */
  private int funitsToPixels(int funits)
  {
    return (int) (((long) funits * scaleY + (unitsPerEm>>1))
                  / unitsPerEm);
  }


  /**
   * Sets up the virtual machine for the specified parameters.  If
   * there is no change to the last set-up, the method will quickly
   * return. Otherwise, the font&#x2019;s pre-program will be
   * executed.
   *
   * @param pointSize the point size of the scaled font.
   *
   * @param deviceTransform an affine transformation which gets
   * applied in addition to scaling by <code>pointSize</code>.  Font
   * programs can separately inquire about the point size.  For this
   * reason, it is not recommended to pre-multiply the point size to
   * the device transformation.
   *
   * @param antialiased <code>true</code> if the scan-line conversion
   * algorithm will use gray levels to give a smoother appearance,
   * <code>false</code> otherwise.  Font programs can ask for this
   * value with the <code>GETINFO</code> instruction, and some
   * programs may behave differently according to this setting.
   */
  public boolean setup(double pointSize,
                       AffineTransform deviceTransform,
                       boolean antialiased)
  {
    boolean changeCTM;
    int pointSize_Fixed;

    if (stack == null)
      stack = new int[maxStackElements];

    if (twilightZone == null)
      twilightZone = new Zone(numTwilightPoints);

    /* If the font program has not yet been executed, do so. */
    if (fontProgram != null)
    {
      resetGraphicsState();
      sp = -1;
      execute(fontProgram, 0);
      fontProgram = null; // prevent further execution
    }

    /* Determine whether the transformation matrix has changed. */
    pointSize_Fixed = Fixed.valueOf(pointSize);
    changeCTM = ((pointSize_Fixed != this.pointSize)
                 || !deviceTransform.equals(this.deviceTransform)
                 || (antialiased != this.antialiased));

    if (changeCTM)
    {
      this.pointSize = pointSize_Fixed;
      this.deviceTransform = deviceTransform;
      this.antialiased = antialiased;
      scaleX = (int) (deviceTransform.getScaleX() * pointSize * 64);
      scaleY = (int) (deviceTransform.getScaleY() * pointSize * 64);
      shearX = (int) (deviceTransform.getShearX() * pointSize * 64);
      shearY = (int) (deviceTransform.getShearY() * pointSize * 64);

      resetGraphicsState();
      reloadControlValueTable();
      executeGlyphInstructions = true;
      ignoreCVTProgram = false;

      if (preProgram != null)
      {
        sp = -1;
        execute(preProgram, 0);
        if (ignoreCVTProgram)
          reloadControlValueTable();
      }
    }

    return executeGlyphInstructions;
  }


  /**
   * Executes a stream of TrueType instructions.
   */
  private void execute(ByteBuffer instructions, int pos)
  {
    instructions.position(pos);

    // FIXME: SECURITY: Possible denial-of-service attack
    // via instructions that have an endless loop.
    while (instructions.hasRemaining()
           && executeInstruction(instructions))
      ;
  }


  /**
   * Writes a textual description of the current TrueType instruction,
   * including the top stack elements, to <code>System.out</code>.
   * This is useful for debugging.
   *
   * @param inst the instruction stream, positioned at the current
   * instruction.
   */
  private void dumpInstruction(ByteBuffer inst)
  {
    CPStringBuilder sbuf = new CPStringBuilder(40);
    int pc = inst.position();
    int bcode = inst.get(pc) & 0xff;
    int count;
    int delta;

    char pcPrefix = 'c';
    for (int i = 0; i < fdefBuffer.length; i++)
    {
      if (fdefBuffer[i] == inst)
      {
        pcPrefix = 'f';
        break;
      }
    }
    sbuf.append(pcPrefix);


    sbuf.append(getHex((short) inst.position()));
    sbuf.append(": ");
    sbuf.append(getHex((byte) bcode));
    sbuf.append("  ");
    sbuf.append(INST_NAME[bcode]);

    if (bcode == 0x40) // NPUSHB
    {
      count = inst.get(pc + 1) & 0xff;
      sbuf.append(" (");
      sbuf.append(count);
      sbuf.append(") ");
      for (int i = 0; i < count; i++)
      {
        if (i > 0)
          sbuf.append(" ");
        sbuf.append('$');
        sbuf.append(getHex(inst.get(pc + 2 + i)));
      }
    }
    if (bcode == 0x41) // NPUSHW
    {
      count = inst.get(pc + 1) & 0xff;
      sbuf.append(" (");
      sbuf.append(count);
      sbuf.append(") ");
      for (int i = 0; i < count; i++)
      {
        if (i > 0)
          sbuf.append(' ');
        sbuf.append('$');
        sbuf.append(getHex(inst.getShort(pc + 2 + 2*i)));
      }
    }
    else
    {
      count = getInstructionLength(bcode) - 1;
      for (int i = 0; i < count; i++)
      {
        sbuf.append(" $");
        sbuf.append(getHex(inst.get(pc + 1 + i)));
      }
    }

    while (sbuf.length() < 30)
      sbuf.append(' ');
    sbuf.append('|');
    sbuf.append(sp + 1);
    sbuf.append("| ");
    for (int i = sp; i >= Math.max(0, sp - 5); i = i - 1)
    {
      if (i < sp)
        sbuf.append(" ");
      if ((stack[i] >> 16) != 0)
        sbuf.append(getHex((short) (stack[i] >> 16)));
      sbuf.append(getHex((short) stack[i]));
    }
    System.out.println(sbuf);
  }


  private static char getNibble(int i, int rightShift)
  {
    i = (i >> rightShift) & 15;
    if (i < 10)
      return (char) (i + '0');
    else
      return (char) (i + 'a' - 10);
  }


  private static String getHex(byte b)
  {
    char[] a = new char[2];
    a[0] = getNibble(b, 4);
    a[1] = getNibble(b, 0);
    return new String(a);
  }


  private static String getHex(short b)
  {
    char[] a = new char[4];
    a[0] = getNibble(b, 12);
    a[1] = getNibble(b, 8);
    a[2] = getNibble(b, 4);
    a[3] = getNibble(b, 0);
    return new String(a);
  }


  /**
   * Skips any instructions until the specified opcode has been
   * encoutered.
   *
   * @param inst the current instruction stream. After the call,
   * the position of <code>inst</code> is right after the first
   * occurence of <code>opcode</code>.
   *
   * @param opcode1 the opcode for which to look.
   *
   * @param opcode2 another opcode for which to look. Pass -1
   * if only <code>opcode1</code> would terminate skipping.
   *
   * @param illegalCode1 an opcode that must not be encountered
   * while skipping. Pass -1 if any opcode is acceptable.
   *
   * @param illegalCode2 another opcode that must not be encountered
   * while skipping. Pass -1 to perform no check.
   *
   * @param handleNestedIfClauses <code>true</code> to handle
   * nested <code>IF [ELSE] EIF</code> clauses, <code>false</code>
   * to ignore them. From the TrueType specification document,
   * one would think that nested if clauses would not be valid,
   * but they do appear in some fonts.
   *
   * @throws IllegalStateException if <code>illegalCode1</code> or
   * <code>illegalCode2</code> has been encountered while skipping.
   */
  private static void skipAfter(ByteBuffer inst,
                                int opcode1, int opcode2,
                                int illegalCode1, int illegalCode2,
                                boolean handleNestedIfClauses)
  {
    int pos = inst.position();
    int curOpcode;
    int instLen;
    int nestingLevel = 0; // increased inside IF [ELSE] EIF sequences

    while (true)
    {
      curOpcode = inst.get(pos) & 0xff;
      instLen = getInstructionLength(curOpcode);

      if (false && TRACE_EXECUTION)
      {
        for (int i = 0; i < nestingLevel; i++)
          System.out.print("--");
        System.out.print("--" + pos + "-" + INST_NAME[curOpcode]);
        if (nestingLevel > 0)
          System.out.print(", ifNestingLevel=" + nestingLevel);
        System.out.println();
      }

      if (curOpcode == 0x40) // NPUSHB
        pos += 1 + (inst.get(pos + 1) & 0xff);
      else if (curOpcode == 0x41) // NPUSHW
        pos += 1 + 2 * (inst.get(pos + 1) & 0xff);
      else
        pos += instLen;

      if ((nestingLevel == 0)
          && ((curOpcode == opcode1) || (curOpcode == opcode2)))
        break;

      if (handleNestedIfClauses)
      {
        if (curOpcode == /* IF */ 0x58)
          ++nestingLevel;
        else if (curOpcode == /* EIF */ 0x59)
          --nestingLevel;
      }

      if ((nestingLevel < 0)
          || (curOpcode == illegalCode1)
          || (curOpcode == illegalCode2))
        throw new IllegalStateException();
    }

    inst.position(pos);
  }


  /**
   * Returns the number of bytes that a TrueType instruction occupies.
   *
   * @param opcode the instruction.
   *
   * @return the number of bytes occupied by the instructions and its
   * operands. For <code>NPUSHB</code> and <code>NPUSHW</code>, where
   * the instruction length depends on the first operand byte, the
   * result is -1.
   */
  private static int getInstructionLength(int opcode)
  {
    /* NPUSHB, NPUSHW --> see following byte */
    if ((opcode == 0x40) || (opcode == 0x41))
      return -1;

    /* PUSHB[0] .. PUSHB[7] --> 2, 3, 4, 5, 6, 7, 8, 9 */
    if ((opcode >= 0xb0) && (opcode <= 0xb7))
      return opcode - 0xae;

    /* PUSHW[0] .. PUSHW[7] --> 3, 5, 6, 7, 11, 13, 15, 17*/
    if ((opcode >= 0xb8) && (opcode <= 0xbf))
      return 1 + ((opcode - 0xb7) << 1);

    return 1;
  }


  /**
   * Executes a single TrueType instruction. This is the core
   * routine of the Virtual Machine.
   *
   * @return <code>true</code> if another instruction shall be
   * executed in the same call frame; <code>false</code> if the
   * current call frame shall be popped.
   */
  private boolean executeInstruction(ByteBuffer inst)
  {
    if (TRACE_EXECUTION)
      dumpInstruction(inst);

    int i, count, e1, e2, e3, e4, x, y;
    int bcode = inst.get() & 0xff;

    switch (bcode)
    {
    case 0x00: // SVTCA[0], Set freedom and proj. Vectors To Coord. Axis [y]
      setFreedomVector((short) 0, ONE_214);
      setProjectionVector((short) 0, ONE_214);
      break;

    case 0x01: // SVTCA[1], Set freedom and proj. Vectors To Coord. Axis [x]
      setFreedomVector(ONE_214, (short) 0);
      setProjectionVector(ONE_214, (short) 0);
      break;

    case 0x02: // SPVTCA[0], Set Projection Vector To Coordinate Axis [y]
      setProjectionVector((short) 0, ONE_214);
      break;

    case 0x03: // SPVTCA[1], Set Projection Vector To Coordinate Axis [x]
      setProjectionVector(ONE_214, (short) 0);
      break;

    case 0x0c: // GPV, Get Projection Vector
      stack[++sp] = projX;
      stack[++sp] = projY;
      break;

    case 0x0d: // GPV, Get Freedom Vector
      stack[++sp] = freeX;
      stack[++sp] = freeY;
      break;

    case 0x0F: // ISECT, move point p to the InterSECTION of two lines
      sp -= 4;
      handleISECT(stack[sp], stack[sp+1], stack[sp+2],
                  stack[sp+3], stack[sp+4]);
      break;

    case 0x10: // SRP0, Set Reference Point 0
      rp0 = stack[sp--];
      break;

    case 0x11: // SRP1, Set Reference Point 1
      rp1 = stack[sp--];
      break;

    case 0x12: // SRP2, Set Reference Point 2
      rp2 = stack[sp--];
      break;

    case 0x13: // SZP0, Set Zone Pointer 0
      zp0 = getZone(stack[sp--]);
      break;

    case 0x14: // SZP1, Set Zone Pointer 1
      zp1 = getZone(stack[sp--]);
      break;

    case 0x15: // SZP2, Set Zone Pointer 2
      zp2 = getZone(stack[sp--]);
      break;

    case 0x16: // SZPS, Set Zone PointerS
      zp0 = zp1 = zp2 = getZone(stack[sp--]);
      break;

    case 0x17: // SLOOP, Set LOOP variable
      loop = stack[sp--];
      break;

    case 0x18: // RTG, Round To Grid
      setRoundingMode(Fixed.ONE, 0x48);
      break;

    case 0x19: // RTHG, Round To Half Grid
      setRoundingMode(Fixed.ONE, 0x68);
      break;

    case 0x1a: // SMD, Set Minimum Distance
      minimumDistance = stack[sp--];
      break;

    case 0x1B: // ELSE, ELSE clause
      skipAfter(inst,
                /* look for: EIF, -- */ 0x59, -1,
                /* illegal: --, -- */   -1, -1,
                /* handle nested if clauses */ true);
      break;

    case 0x1C: // JMPR, JuMP Relative
      inst.position(inst.position() - 1 + stack[sp--]);
      break;

    case 0x1D: // SCVTCI, Set Control Value Table Cut-In
      cvtCutIn = stack[sp--];
      break;

    case 0x1F: // SSW, Set Single Width
      singleWidthValue = stack[sp--];
      break;

    case 0x20: // DUP, DUPlicate top stack element
      e1 = stack[sp];
      stack[++sp] = e1;
      break;

    case 0x21: // POP, POP top stack element
      sp--;
      break;

    case 0x22: // CLEAR, CLEAR the stack
      sp = -1;
      break;

    case 0x23: // SWAP, SWAP the top two elements on the stack
      e1 = stack[sp--];
      e2 = stack[sp];
      stack[sp] = e1;
      stack[++sp] = e2;
      break;

    case 0x24: // DEPTH, DEPTH of the stack
      stack[++sp] = sp + 1;
      break;

    case 0x25: // CINDEX, Copy the INDEXed element to the top of the stack
      stack[sp] = stack[sp - stack[sp]];
      break;

    case 0x26: // MINDEX, Move the INDEXed element to the top of the stack
      i = stack[sp];
      e1 = stack[sp - i];
      System.arraycopy(/* src */ stack,  /* srcPos */ sp - i + 1,
                       /* dest */ stack, /* destPos*/ sp - i,
                       /* length */ i - 1);
      --sp;
      stack[sp] = e1;
      break;

    case 0x2a: // LOOPCALL, LOOP and CALL function
      i = stack[sp--];
      count = stack[sp--];
      e1 = inst.position();
      e2 = sp;
      for (int j = 0; j < count; j++)
        execute(fdefBuffer[i], fdefEntryPoint[i]);
      inst.position(e1);
      break;

    case 0x2B: // CALL, CALL function
      i = stack[sp--];
      e1 = inst.position();
      e2 = sp;
      execute(fdefBuffer[i], fdefEntryPoint[i]);
      inst.position(e1);
      break;

    case 0x2C: // FDEF, Function DEFinition
      i = stack[sp--];
      fdefBuffer[i] = inst;
      fdefEntryPoint[i] = inst.position();
      skipAfter(inst,
                /* look for: ENDF */ 0x2d,
                /* look for: --- */ -1,
                /* illegal: IDEF */ 0x89,
                /* illegal: FDEF */ 0x2c,
                /* do not handle nested if clauses */ false);
      break;

    case 0x2D: // ENDF, END Function definition
      /* Pop the current stack frame. */
      return false;

    case 0x2e: // MDAP[0], Move Direct Absolute Point
      handleMDAP(stack[sp--], /* round */ false);
      break;

    case 0x2f: // MDAP[1], Move Direct Absolute Point
      handleMDAP(stack[sp--], /* round */ true);
      break;

    case 0x39: // IP, Interpolate Point by the last relative stretch
      handleIP();
      break;

    case 0x3d: // RTDG, Round To Double Grid
      setRoundingMode(Fixed.ONE, 0x08);
      roundThreshold = roundThreshold / 64; // period/128
      break;

    case 0x3e: // MIAP[0], Move Indirect Absolute Point
      e1 = stack[sp--];
      handleMIAP(e1, stack[sp--], /* round */ false);
      break;

    case 0x3f: // MIAP[1], Move Indirect Absolute Point
      e1 = stack[sp--];
      handleMIAP(e1, stack[sp--], /* round */ true);
      break;

    case 0x40: // NPUSHB
      count = inst.get() & 0xff;
      for (i = 0; i < count; i++)
        stack[++sp] = inst.get() & 0xff;
      break;

    case 0x41: // NPUSHW
      count = inst.get() & 0xff;
      for (i = 0; i < count; i++)
        stack[++sp] = inst.getShort();
      break;

    case 0x42: // WS, Write Store
      e1 = stack[sp--]; i = stack[sp--];
      storage[i] = e1;
      break;

    case 0x43: // RS, Read Store
      stack[sp] = storage[stack[sp]];
      break;

    case 0x44: // WCVTP, Write Control Value Table in Pixel units
      e1 = stack[sp--];
      i = stack[sp--];
      if (i < cvt.length)
        cvt[i] = e1;
      break;

    case 0x45: // RCVT, Read Control Value Table entry
      if (stack[sp] < cvt.length)
        stack[sp] = cvt[stack[sp]];
      else
        stack[sp] = 0;
      break;

    case 0x46: // GC[0], Get Coordinate projected onto the projection vector
      stack[sp] = getProjection(zp2, stack[sp]);
      break;

    case 0x47: // GC[1], Get Coordinate projected onto the projection vector
      stack[sp] = getOriginalProjection(zp2, stack[sp]);
      break;

    case 0x4B: // MPPEM, Measure Pixels Per EM
      stack[++sp] = getPixelsPerEM();
      break;

    case 0x4c: // MPS, Measure Point Size
      /* FreeType2 returns pixels per em here, because they think that
       * the point size would be irrelevant in a given font program.
       * This is extremely surprising, because the appearance of good
       * fonts _should_ change with point size. For example, a good
       * font should be wider at small point sizes, and the holes
       * inside glyphs ("Punzen" in German, I do not know the correct
       * English expression) should be larger. Note that this change
       * of appearance is dependent on point size, _not_ the
       * resolution of the display device.
       */
      stack[++sp] = pointSize;
      break;

    case 0x4f: // DEBUG, DEBUG call
      sp--;
      break;

    case 0x50: // LT, Less Than
      e1 = stack[sp--];
      stack[sp] = (stack[sp] < e1) ? 1 : 0;
      break;

    case 0x51: // LTEQ, Greater Than or EQual
      e1 = stack[sp--];
      stack[sp] = (stack[sp] <= e1) ? 1 : 0;
      break;

    case 0x52: // GT, Greater Than
      e1 = stack[sp--];
      stack[sp] = (stack[sp] > e1) ? 1 : 0;
      break;

    case 0x53: // GTEQ, Greater Than or EQual
      e1 = stack[sp--];
      stack[sp] = (stack[sp] >= e1) ? 1 : 0;
      break;

    case 0x54: // EQ, EQual
      e1 = stack[sp--];
      stack[sp] = (stack[sp] == e1) ? 1 : 0;
      break;

    case 0x55: // NEQ, Not EQual
      e1 = stack[sp--];
      stack[sp] = (stack[sp] != e1) ? 1 : 0;
      break;

    case 0x58: // IF, IF test
      if (stack[sp--] == 0)
        skipAfter(inst,
                  /* look for: ELSE */ 0x1B,
                  /* look for: EIF */  0x59,
                  /* illegal: -- */    -1,
                  /* illegal: -- */    -1,
                  /* handle nested if clauses */ true);
      break;

    case 0x59: // EIF, End IF
      // Do nothing.
      break;

    case 0x5A: // AND
      e1 = stack[sp--];
      stack[sp] = ((e1 != 0) && (stack[sp] != 0)) ? 1 : 0;
      break;

    case 0x5B: // OR
      e1 = stack[sp--];
      stack[sp] = ((e1 != 0) || (stack[sp] != 0)) ? 1 : 0;
      break;

    case 0x5C: // NOT
      stack[sp] = (stack[sp] != 0) ? 0 : 1;
      break;

    case 0x5e: // SDB, Set Delta Base in the graphics state
      deltaBase = stack[sp--];
      break;

    case 0x5f: // SDS, Set Delta Shift in the graphics state
      deltaShift = stack[sp--];
      break;

    case 0x60: // ADD
      e1 = stack[sp--];
      stack[sp] += e1;
      break;

    case 0x61: // SUB, SUBtract
      e1 = stack[sp--];
      stack[sp] -= e1;
      break;

    case 0x62: // DIV, DIVide
      e1 = stack[sp--];
      stack[sp] = Fixed.div(e1, stack[sp]);
      break;

    case 0x63:  // MUL, MULtiply
      e1 = stack[sp--];
      stack[sp] = Fixed.mul(e1, stack[sp]);
      break;

    case 0x64: // ABS, ABSolute value
      stack[sp] = Math.abs(stack[sp]);
      break;

    case 0x65: // NEG, NEGate
      stack[sp] = -stack[sp];
      break;

    case 0x66: // FLOOR
      stack[sp] = Fixed.floor(stack[sp]);
      break;

    case 0x67: // CEILING
      stack[sp] = Fixed.ceil(stack[sp]);
      break;

    case 0x68: // ROUND[0] -- round grey distance
      stack[sp] = round(stack[sp], /* no engine compensation */ 0);
      break;

    case 0x69: // ROUND[1] -- round black distance
      stack[sp] = round(stack[sp], -engineCompensation);
      break;

    case 0x6a: // ROUND[2] -- round white distance
      stack[sp] = round(stack[sp], engineCompensation);
      break;

    case 0x6b: // ROUND[3] -- round distance (not yet defined)
      stack[sp] = round(stack[sp], /* no engine compensation */ 0);
      break;

    case 0x6c: // NROUND[0] -- compensate grey distance
      stack[sp] = nround(stack[sp], 0);
      break;

    case 0x6d: // NROUND[1] -- compensate black distance
      stack[sp] = nround(stack[sp], -engineCompensation);
      break;

    case 0x6e: // NROUND[2] -- compensate white distance
      stack[sp] = nround(stack[sp], engineCompensation);
      break;

    case 0x6f: // NROUND[3] -- compensate distance (not yet defined)
      stack[sp] = nround(stack[sp], 0);
      break;

    case 0x70: // WCVTF, Write Control Value Table in Funits
      e1 = stack[sp--];
      cvt[stack[sp--]] = e1 * getPixelsPerEM();
      break;

    case 0x73: // DELTAC1, DELTA exception C1
      count = stack[sp--];
      sp -= 2 * count;
      deltaC(stack, sp + 1, count, 0);
      break;

    case 0x74: // DELTAC2, DELTA exception C2
      count = stack[sp--];
      sp -= 2 * count;
      deltaC(stack, sp + 1, count, 16);
      break;

    case 0x75: // DELTAC3, DELTA exception C3
      count = stack[sp--];
      sp -= 2 * count;
      deltaC(stack, sp + 1, count, 32);
      break;

    case 0x76: // SROUND, Super ROUND
      setRoundingMode(Fixed.ONE, stack[sp--]);
      break;

    case 0x77: // S45ROUND, Super ROUND 45 degrees
      setRoundingMode(/* sqrt(2)/2 */ 0x2d, stack[sp--]);
      break;

    case 0x78: // JROT, Jump Relative On True
      e1 = stack[sp--];
      i = inst.position() - 1 + stack[sp--];
      if (e1 != 0)
        inst.position(i);
      break;

    case 0x79: // JROF, Jump Relative On False
      e1 = stack[sp--];
      i = inst.position() - 1 + stack[sp--];
      if (e1 == 0)
        inst.position(i);
      break;

    case 0x7a: // ROFF, Round OFF
      roundPeriod = 0;
      break;

    case 0x7c: // RUTG, Round Up To Grid
      setRoundingMode(Fixed.ONE, 0x40);
      break;

    case 0x7d: // RDTG, Round Down To Grid
      setRoundingMode(Fixed.ONE, 0x40);
      roundThreshold = 0;
      break;

    case 0x7e: // SANGW, Set ANGle Weight (no-op according to TrueType spec)
    case 0x7f: // AA, Adjust Angle (no-op according to TrueType spec)
      sp--;
      break;

    case 0x85: // SCANCTRL, SCAN conversion ConTRoL
      e1 = stack[sp--];
      int ppemThreshold = e1 & 255;
      scanControl = false;
      boolean ppemCondition = (ppemThreshold == 255)
        || ((ppemThreshold != 0) && (getPixelsPerEM() > ppemThreshold));
      if (((e1 & (1<<8)) != 0) && ppemCondition)
        scanControl = true;
      if (((e1 & (1<<9)) != 0) && isRotated())
        scanControl = true;
      if (((e1 & (1<<10)) != 0) && isStretched())
        scanControl = true;
      if (((e1 & (1<<11)) != 0) && !ppemCondition)
        scanControl = false;
      if (((e1 & (1<<12)) != 0) && !isRotated())
        scanControl = false;
      if (((e1 & (1<<13)) != 0) && !isStretched())
        scanControl = false;
      break;

    case 0x88: // GETINFO, GET INFOrmation
      e1 = 0;
      if ((stack[sp] & 1) != 0) // ask for rasterizer version
        e1 |= 35; // "Microsoft Rasterizer version 1.7" (grayscale-capable)
      if (((stack[sp] & 2) != 0) && isRotated())
        e1 |= 1 << 8; // bit 8: glyph has been rotated
      if (((stack[sp] & 4) != 0) && isStretched())
        e1 |= 1 << 9; // bit 9: glyph has been stretched
      if (((stack[sp] & 32) != 0) && antialiased)
        e1 |= 1 << 12; // bit 12: antialiasing is active
      stack[sp] = e1;
      break;

    case 0x8a: // ROLL, ROLL the top three stack elements
      e1 = stack[sp - 2];
      stack[sp - 2] = stack[sp - 1];
      stack[sp - 1] = stack[sp];
      stack[sp] = e1;
      break;

    case 0x8b: // MAX, MAXimum of top two stack elements
      e1 = stack[sp--];
      stack[sp] = Math.max(e1, stack[sp]);
      break;

    case 0x8c: // MIN, MINimum of top two stack elements
      e1 = stack[sp--];
      stack[sp] = Math.min(e1, stack[sp]);
      break;

    case 0x8d: // SCANTYPE
      scanType = stack[sp--];
      break;

    case 0x8e: // INSTCTRL, INSTRuction execution ConTRoL
      e1 = stack[sp--]; // selector
      e2 = stack[sp--]; // value
      switch (e1)
      {
      case 1:
        executeGlyphInstructions = (e2 == 0);
        break;

      case 2:
        ignoreCVTProgram = (e2 != 0);
        break;
      }
      break;

    case 0xb0: // PUSHB[0]
    case 0xb1: // PUSHB[1]
    case 0xb2: // PUSHB[2]
    case 0xb3: // PUSHB[3]
    case 0xb4: // PUSHB[4]
    case 0xb5: // PUSHB[5]
    case 0xb6: // PUSHB[6]
    case 0xb7: // PUSHB[7]
      count = bcode - 0xb0 + 1;
      for (i = 0; i < count; i++)
        stack[++sp] = inst.get() & 0xff;
      break;

    case 0xb8: // PUSHW[0]
    case 0xb9: // PUSHW[1]
    case 0xba: // PUSHW[2]
    case 0xbb: // PUSHW[3]
    case 0xbc: // PUSHW[4]
    case 0xbd: // PUSHW[5]
    case 0xbe: // PUSHW[6]
    case 0xbf: // PUSHW[7]
      count = bcode - 0xb8 + 1;
      for (i = 0; i < count; i++)
        stack[++sp] = inst.getShort();
      break;

      // MIRPxxxx, Move Indirect Relative Point
    case 0xe0: case 0xe1: case 0xe2: case 0xe3:
    case 0xe4: case 0xe5: case 0xe6: case 0xe7:
    case 0xe8: case 0xe9: case 0xea: case 0xeb:
    case 0xec: case 0xed: case 0xee: case 0xef:
    case 0xf0: case 0xf1: case 0xf2: case 0xf3:
    case 0xf4: case 0xf5: case 0xf6: case 0xf7:
    case 0xf8: case 0xf9: case 0xfa: case 0xfb:
    case 0xfc: case 0xfd: case 0xfe: case 0xff:
      e1 = stack[sp--];
      handleMIRP(bcode, /* point */ e1, /* cvtIndex */ stack[sp--]);
      break;

    default:
      throw new IllegalStateException();
    }

    return true;
  }


  /**
   * Sets the rounding mode.
   *
   * @param period the grid period in fixed-point notation, such as
   * {@link Fixed#ONE} for the <code>SROUND</code> instruction or
   * <code>sqrt(2)/2</code> for the <code>S45ROUND</code> instruction.
   *
   * @param mode a byte whose bits are set according to the TrueType
   * specification for SROUND and S45ROUND parameters.
   */
  private void setRoundingMode(int period, int mode)
  {
    /* Set the period. */
    switch ((mode & 0xc0) >> 6)
    {
    case 0:
      roundPeriod = period / 2;
      break;

    case 2:
      roundPeriod = period * 2;
      break;

    default:
      roundPeriod = period;
      break;
    }

    /* Set the phase. */
    switch ((mode & 0x30) >> 4)
    {
    case 0:
      roundPhase = 0;
      break;

    case 1:
      roundPhase = roundPeriod >> 2;  // period/4
      break;

    case 2:
      roundPhase = roundPeriod >> 1; // period/2
      break;

    case 3:
      roundPhase = (roundPeriod >> 1) + (roundPeriod >> 2); // period * 3/4
      break;
    }

    /* Set the threshold. */
    int threshold = mode & 0x0f;
    if (threshold == 0)
      roundThreshold = roundPeriod - Fixed.ONE;
    else
      roundThreshold = ((threshold - 4) * roundPeriod) / 8;
  }



  /**
   * Implements the DELTAC instructions. These instructions check
   * whether the current number of pixels per em is contained in an
   * exception table.  If it is, a delta value is determined, and the
   * specified entry in the Control Value Table is modified according
   * to the delta.
   *
   * @param pairs the delta table. Because the delta table is on
   * the stack, callers usually just want to pass the stack array.
   *
   * @param offset the offset of the first pair in <code>pairs</code>.
   *
   * @param numPairs the number of pairs.
   *
   * @param base 0 for <code>DELTAC1</code>, 16 for <code>DELTAC2</code>,
   * or 32 for <code>DELTAC2</code>.
   *
   * @see <a href=
   * "http://developer.apple.com/fonts/TTRefMan/RM05/Chap5.html#DELTAC1"
   * >Apple&#x2019;s documentation for <code>DELTAC1</code></a>, <a href=
   * "http://developer.apple.com/fonts/TTRefMan/RM05/Chap5.html#DELTAC2"
   * ><code>DELTAC2</code></a>, and <a href=
   * "http://developer.apple.com/fonts/TTRefMan/RM05/Chap5.html#DELTAC3"
   * ><code>DELTAC3</code></a>
   */
  private void deltaC(int[] pairs, int offset, int numPairs, int base)
  {
    int arg, relativePpem;
    int ppemTrigger = getPixelsPerEM() - (deltaBase + base);
    int delta, cvtIndex, rightShift;
    for (int i = 0; i < numPairs; i++)
    {
      arg = pairs[offset + 2 * i];
      relativePpem = (arg >> 4) & 15;
      if (relativePpem == ppemTrigger)
      {
        delta = (arg & 15) - 8;
        if (delta >= 0)
          ++delta;

        rightShift = deltaShift - 6;
        if (rightShift > 0)
          delta = delta >> rightShift;
        else if (rightShift < 0)
          delta = delta << (-rightShift);
        cvt[pairs[offset + 2 * i + 1]] += delta;

        break;
      }
    }
  }


  private Zone getZone(int zoneNumber)
  {
    return (zoneNumber == 0) ? twilightZone : glyphZone;
  }


  /**
   * Projects the specified vector along the current projection
   * vector.
   *
   * @param x the x component of the input vector, in 26.6 fixed-point
   * notation.
   *
   * @param y the y component of the input vector, in 26.6 fixed-point
   * notation.
   *
   * @return the projected distance, in 26.6 fixed-point notation.
   */
  private int getProjection(int x, int y)
  {
    return (int) (((((long) x) * projX + ((long) y) * projY)) >> 14);
  }


  /**
   * Projects the specified vector along the current dual projection
   * vector.
   *
   * @param x the x component of the input vector, in 26.6 fixed-point
   * notation.
   *
   * @param y the y component of the input vector, in 26.6 fixed-point
   * notation.
   *
   * @return the projected distance, in 26.6 fixed-point notation.
   */
  private int getDualProjection(int x, int y)
  {
    return (int) (((((long) x) * dualX + ((long) y) * dualY)) >> 14);
  }


  private int getProjection(Zone zone, int point)
  {
    return getProjection(zone.getX(point), zone.getY(point));
  }


  private int getOriginalProjection(Zone zone, int point)
  {
    return getDualProjection(zone.getOriginalX(point),
                             zone.getOriginalY(point));
  }


  private void handleISECT(int a0, int a1, int b0, int b1, int p)
  {
    System.out.println("FIXME: Unimplemented ISECT " + p);
  }


  private static int muldiv(int a, int b, int c)
  {
    int s;
    s = a; a = Math.abs(a);
    s ^= b; b = Math.abs(b);
    s ^= c; c = Math.abs(c);
    a = (int) ((((long) a) * b + (c>>1)) / c);
    return (s < 0) ? -a : a;
  }


  private int getFreeDotProj()
  {
    int result;

    result = ((((int) projX) * freeX) << 2)
      + ((((int) projY) * freeY) << 2);

    /* FIXME: This seems somewhat bogus. Need to contact the
     * developers of FreeType.
     */
    if (Math.abs(result) < 0x4000000)
      result = 0x40000000;
    return result;
  }


  private void movePoint(Zone zone, int point, int distance)
  {
    int freeDotProj = getFreeDotProj();
    int c;

    if (freeX != 0)
    {
      c = zone.getX(point);
      c += muldiv(distance, freeX << 16, freeDotProj);
      zone.setX(point, c, /* touch */ true);
    }

    if (freeY != 0)
    {
      c = zone.getY(point);
      c += muldiv(distance, freeY << 16, freeDotProj);
      zone.setY(point, c, /* touch */ true);
    }

    if (TRACE_EXECUTION)
    {
      System.out.println("point[" + point + "] moved to "
                         + Fixed.toString(zone.getX(point),
                                          zone.getY(point)));
      dumpVectors();
    }
  }

  private void dumpVectors()
  {
    System.out.println("  proj=" + Fixed.toString(projX>>8, projY>>8)
                       + ", free=" + Fixed.toString(freeX>>8, freeY>>8));
  }


  private void handleIP()
  {
    // Implementation taken from FreeType.
    int p, org_a, org_b, org_x, cur_a, cur_b, cur_x, distance;
    int freeDotProj;

    org_a = getOriginalProjection(zp0, rp1);
    cur_a = getProjection(zp0, rp1);

    org_b = getOriginalProjection(zp1, rp2);
    cur_b = getProjection(zp1, rp2);

    while (--loop >= 0)
    {
      p = stack[sp--];
      org_x = getOriginalProjection(zp2, p);
      cur_x = getProjection(zp2, p);

      if (((org_a <= org_b) && (org_x <= org_a))
          || ((org_a > org_b) && (org_x >= org_a)))
        distance = (cur_a - org_a) + (org_x - cur_x);
      else if (((org_a <= org_b) && (org_x >= org_b))
               || ((org_a > org_b) && (org_x < org_b)))
        distance = (cur_b - org_b) + (org_x - cur_x);
      else
        distance = muldiv(cur_b - cur_a, org_x - org_a, org_b - org_a)
          + (cur_a - cur_x);
      movePoint(zp2, p, distance);
    }
    loop = 1;
  }


  private void handleMDAP(int point, boolean round)
  {
    System.out.println("FIXME: Unimplemented MDAP: point "
                       + point + "/" + zp0);
  }


  private void handleMIAP(int cvtIndex, int point, boolean round)
  {
    int previousPos, pos;

    previousPos = getProjection(zp0, point);
    pos = cvt[cvtIndex];

    if (round)
    {
      if (Math.abs(pos - previousPos) > cvtCutIn)
        pos = previousPos;
      pos = round(pos, /* no engine compensation */ 0);
    }
    movePoint(zp0, point, pos - previousPos);
    rp0 = rp1 = point;
  }


  private void handleMIRP(int bcode, int point, int cvtIndex)
  {
    System.out.println("FIXME: Unimplemented mirp " + point + ", " + cvtIndex);
  }



  private int round(int distance, int compensation)
  {
    int result;

    if (roundPeriod == 0)
      return nround(distance, compensation);

    if (distance >= 0)
    {
      result = distance + compensation - roundPhase + roundThreshold;
      result &= -roundPeriod; // truncate to the next lowest periodic value
      return Math.max(result, 0) + roundPhase;
    }
    else
    {
      result = compensation - roundPhase + roundThreshold - distance;
      result &= -roundPeriod;
      return Math.max(-result, 0) - roundPhase;
    }
  }


  private static int nround(int distance, int compensation)
  {
    if (distance >= 0)
      return Math.max(distance + compensation, 0);
    else
      return Math.min(distance - compensation, 0);
  }


  /**
   * Determines whether the current glyph is rotated.
   *
   * @return <code>false</code> if the shearing factors for the
   * <i>x</i> and <i>y</i> axes are zero; <code>true</code> if they
   * are non-zero.
   */
  private boolean isRotated()
  {
    return (shearX != 0) || (shearY != 0);
  }


  /**
   * Determines whether the current glyph is stretched.
   *
   * @return <code>false</code> if the scaling factors for the
   * <i>x</i> and <i>y</i> axes are are equal; <code>true</code> if
   * they differ.
   */
  private boolean isStretched()
  {
    return scaleX != scaleY;
  }


  /**
   * Returns how many pixels there are per EM, in direction of the
   * current projection vector. The result is a normal integer,
   * not a Fixed.
   */
  private int getPixelsPerEM()
  {
    if (cachedPixelsPerEM == 0)
    {
      cachedPixelsPerEM = Fixed.intValue(Fixed.vectorLength(
        applyCTM_x(projX >> 8, projY >> 8),
        applyCTM_y(projX >> 8, projY >> 8)));
    }

    return cachedPixelsPerEM;
  }


  private void setProjectionVector(short x, short y)
  {
    if (PATENTED_HINTING)
    {
      if ((x != projX) || (y != projY))
        cachedPixelsPerEM = 0;

      projX = x;
      projY = y;
    }
  }


  private void setFreedomVector(short x, short y)
  {
    if (PATENTED_HINTING)
    {
      freeX = x;
      freeY = y;
    }
  }


  private void setDualVector(short x, short y)
  {
    if (PATENTED_HINTING)
    {
      dualX = x;
      dualY = y;
    }
  }


  private int applyCTM_x(int x, int y)
  {
    return (int) (((long) scaleX * x + (long) shearX * y) >> 6);
  }

  private int applyCTM_y(int x, int y)
  {
    return (int) (((long) shearY * x + (long) scaleY * y) >> 6);
  }


  private static final String[] INST_NAME =
  {
    /* 00 */ "SVTCA[0]", "SVTCA[1]", "SPVTCA[0]", "SPVTCA[1]",
    /* 04 */ "INST_04", "INST_05", "INST_06", "INST_07",
    /* 08 */ "INST_08", "INST_09", "INST_0A", "INST_0B",
    /* 0c */ "GPV", "GFV", "INST_0E", "ISECT",
    /* 10 */ "SRP0", "SRP1", "SRP2", "SZP0",
    /* 14 */ "SZP1", "SZP2", "SZPS", "SLOOP",
    /* 18 */ "RTG", "RTHG", "SMD", "ELSE",
    /* 1c */ "JMPR", "SCVTCI", "INST_1E", "SSW",
    /* 20 */ "DUP", "POP", "CLEAR", "SWAP",
    /* 24 */ "DEPTH", "CINDEX", "MINDEX", "INST_27",
    /* 28 */ "INST_28", "INST_29", "LOOPCALL", "CALL",
    /* 2c */ "FDEF", "ENDF", "MDAP[0]", "MDAP[1]",
    /* 30 */ "IUP[0]", "IUP[1]", "SHP[0]", "SHP[1]",
    /* 34 */ "INST_34", "INST_35", "INST_36", "INST_37",
    /* 38 */ "INST_38", "IP", "INST_3A", "INST_3B",
    /* 3c */ "INST_3C", "RTDG", "MIAP[0]", "MIAP[1]",
    /* 40 */ "NPUSHB", "NPUSHW", "WS", "RS",
    /* 44 */ "WCVTP", "RCVT", "GC[0]", "GC[1]",
    /* 48 */ "INST_48", "INST_49", "INST_4A", "MPPEM",
    /* 4c */ "MPS", "FLIPON", "FLIPOFF", "DEBUG",
    /* 50 */ "LT", "LTEQ", "GT", "GTEQ",
    /* 54 */ "EQ", "NEQ", "INST_56", "INST_57",
    /* 58 */ "IF", "EIF", "AND", "OR",
    /* 5c */ "NOT", "INST_5D", "SDB", "SDS",
    /* 60 */ "ADD", "SUB", "DIV", "MUL",
    /* 64 */ "ABS", "NEG", "FLOOR", "CEILING",
    /* 68 */ "ROUND[0]", "ROUND[1]", "ROUND[2]", "ROUND[3]",
    /* 6c */ "NROUND[0]", "NROUND[1]", "NROUND[2]", "NROUND[3]",
    /* 70 */ "WCVTF", "INST_71", "INST_72", "DELTAC1",
    /* 74 */ "DELTAC2", "DELTAC3", "SROUND", "S45ROUND",
    /* 78 */ "JROT", "JROF", "ROFF", "INST_7B",
    /* 7c */ "RUTG", "RDTG", "SANGW", "AA",
    /* 80 */ "FLIPPT", "FLIPRGON", "FLIPRGOFF", "INST_83",
    /* 84 */ "INST_84", "SCANCTRL", "INST_86", "INST_87",
    /* 88 */ "GETINFO", "INST_89", "ROLL", "MAX",
    /* 8c */ "MIN", "SCANTYPE", "INSTCTRL", "INST_8F",
    /* 90 */ "INST_90", "INST_91", "INST_92", "INST_93",
    /* 94 */ "INST_94", "INST_95", "INST_96", "INST_97",
    /* 98 */ "INST_98", "INST_99", "INST_9A", "INST_9B",
    /* 9c */ "INST_9C", "INST_9D", "INST_9E", "INST_9F",
    /* a0 */ "INST_A0", "INST_A1", "INST_A2", "INST_A3",
    /* a4 */ "INST_A4", "INST_A5", "INST_A6", "INST_A7",
    /* a8 */ "INST_A8", "INST_A9", "INST_AA", "INST_AB",
    /* ac */ "INST_AC", "INST_AD", "INST_AE", "INST_AF",
    /* b0 */ "PUSHB[0]", "PUSHB[1]", "PUSHB[2]", "PUSHB[3]",
    /* b4 */ "PUSHB[4]", "PUSHB[5]", "PUSHB[6]", "PUSHB[7]",
    /* b8 */ "PUSHW[0]", "PUSHW[1]", "PUSHW[2]", "PUSHW[3]",
    /* bc */ "PUSHW[4]", "PUSHW[5]", "PUSHW[6]", "PUSHW[7]",
    /* c0 */ "INST_C0", "INST_C1", "INST_C2", "INST_C3",
    /* c4 */ "INST_C4", "INST_C5", "INST_C6", "INST_C7",
    /* c8 */ "INST_C8", "INST_C9", "INST_CA", "INST_CB",
    /* cc */ "INST_CC", "INST_CD", "INST_CE", "INST_CF",
    /* d0 */ "INST_D0", "INST_D1", "INST_D2", "INST_D3",
    /* d4 */ "INST_D4", "INST_D5", "INST_D6", "INST_D7",
    /* d8 */ "INST_D8", "INST_D9", "INST_DA", "INST_DB",
    /* dc */ "INST_DC", "INST_DD", "INST_DE", "INST_DF",
    /* e0 */ "MIRP00000", "MIRP00001", "MIRP00010", "MIRP00011",
    /* e4 */ "MIRP00100", "MIRP00101", "MIRP00110", "MIRP00111",
    /* e8 */ "MIRP01000", "MIRP01001", "MIRP01010", "MIRP01011",
    /* ec */ "MIRP01100", "MIRP01101", "MIRP01110", "MIRP01111",
    /* f0 */ "MIRP10000", "MIRP10001", "MIRP10010", "MIRP10011",
    /* f4 */ "MIRP10100", "MIRP10101", "MIRP10110", "MIRP10111",
    /* f8 */ "MIRP11000", "MIRP11001", "MIRP11010", "MIRP11011",
    /* fc */ "MIRP11100", "MIRP11101", "MIRP11110", "MIRP11111"
  };
}
