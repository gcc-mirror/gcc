public class PR19295 implements myInterface {
        public long tagBits = 0;

        public final boolean isArrayType() {
            return (tagBits & IsArrayType) != 0;
        }
}

abstract class blah {
	public final static int Bit1 = 0x2;
}

interface myInterface {
        long IsArrayType = blah.Bit1;
}

