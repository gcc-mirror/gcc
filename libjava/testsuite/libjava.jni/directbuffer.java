// Test to make sure JNI implementation catches exceptions.

import java.nio.*;

public class directbuffer
{
  static
  {
    System.loadLibrary("directbuffer");
  }

  public static native ByteBuffer createDirectByteBuffer();
  
  public static native void testDirectByteBuffer(ByteBuffer bb, int len);
  public static native void testCharBuffer(CharBuffer b, int len);
  public static native void testDoubleBuffer(DoubleBuffer b, int len);
  public static native void testFloatBuffer(FloatBuffer b, int len);
  public static native void testIntBuffer(IntBuffer b, int len);
  public static native void testLongBuffer(LongBuffer b, int len);
  public static native void testShortBuffer(ShortBuffer b, int len);

  public static void main(String[] args)
  {
    ByteBuffer bb = createDirectByteBuffer();
    CharBuffer cb = bb.asCharBuffer();
    DoubleBuffer db = bb.asDoubleBuffer();
    FloatBuffer fb = bb.asFloatBuffer();
    IntBuffer ib = bb.asIntBuffer();
    LongBuffer lb = bb.asLongBuffer();
    ShortBuffer sb = bb.asShortBuffer();

    testDirectByteBuffer(bb, 1024);
    testCharBuffer(cb, 512);
    testDoubleBuffer(db, 128);
    testFloatBuffer(fb, 256);
    testIntBuffer(ib, 256);
    testLongBuffer(lb, 128);
    testShortBuffer(sb, 512);
  }
}
