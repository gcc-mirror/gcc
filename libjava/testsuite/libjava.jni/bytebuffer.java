// Test to make sure JNI implementation catches exceptions.

import java.nio.*;

public class bytebuffer
{
  static
  {
    System.loadLibrary("bytebuffer");
  }

  public static native void testByteBuffer(ByteBuffer bb);
  public static native void testCharBuffer(CharBuffer b);
  public static native void testDoubleBuffer(DoubleBuffer b);
  public static native void testFloatBuffer(FloatBuffer b);
  public static native void testIntBuffer(IntBuffer b);
  public static native void testLongBuffer(LongBuffer b);
  public static native void testShortBuffer(ShortBuffer b);

  public static void main(String[] args)
  {
    ByteBuffer bb = ByteBuffer.allocate(1024);
    testByteBuffer(bb);
    testCharBuffer(bb.asCharBuffer());
    testDoubleBuffer(bb.asDoubleBuffer());
    testFloatBuffer(bb.asFloatBuffer());
    testIntBuffer(bb.asIntBuffer());
    testLongBuffer(bb.asLongBuffer());
    testShortBuffer(bb.asShortBuffer());

    testCharBuffer(CharBuffer.allocate(1024));
    testDoubleBuffer(DoubleBuffer.allocate(1024));
    testFloatBuffer(FloatBuffer.allocate(1024));
    testIntBuffer(IntBuffer.allocate(1024));
    testLongBuffer(LongBuffer.allocate(1024));
    testShortBuffer(ShortBuffer.allocate(1024));
  }
}
