/*
* Instance initializers are now turned into a new function instinit$ and called
* from the ctor. SAVE_EXPRs can't be easily shared.
*
* Contributed by Adam King <aking@dreammechanics.com>
*
*/

public class Matrix4f
{
        public float m00;
        public float m01;
        public float m02;
        public float m03;
        public float m10;
        public float m11;
        public float m12;
        public float m13;
        public float m20;
        public float m21;
        public float m22;
        public float m23;
        public float m30;
        public float m31;
        public float m32;
        public float m33;

        private float scale;

        static boolean initialized;
        static Matrix4f xmat;
        static Matrix4f ymat;
        static Matrix4f zmat;
        static Matrix4f tempMatrix1;
        static Matrix4f tempMatrix2;

        {
                if( !initialized )
                {
		    System.out.println ("not initialized");
                        initialized = true;
                        xmat = new Matrix4f();
                        ymat = new Matrix4f();
                        zmat = new Matrix4f();
                        tempMatrix1 = new Matrix4f();
                        tempMatrix2 = new Matrix4f();
                }
		else { System.out.println ("initialized"); }
		    
        }

        public Matrix4f()
        {
                m00 = 1;
                m01 = 0;
                m02 = 0;
                m03 = 0;
                m10 = 0;
                m11 = 1;
                m12 = 0;
                m13 = 0;
                m20 = 0;
                m21 = 0;
                m22 = 1;
                m23 = 0;
                m30 = 0;
                m31 = 0;
                m32 = 0;
                m33 = 1;
        }

        public Matrix4f( float v[] )
        {
                m00 = v[0];
                m01 = v[1];
                m02 = v[2];
                m03 = v[3];
                m10 = v[4];
                m11 = v[5];
                m12 = v[6];
                m13 = v[7];
                m20 = v[8];
                m21 = v[9];
                m22 = v[10];
                m23 = v[11];
                m30 = v[12];
                m31 = v[13];
                m32 = v[14];
                m33 = v[15];
        }

        public Matrix4f( float m00, float m01, float m02, float m03,
                float m10, float m11, float m12, float m13, 
                float m20, float m21, float m22, float m23, 
                float m30, float m31, float m32, float m33 )
        {
                this.m00 = m00;
                this.m01 = m01;
                this.m02 = m02;
                this.m03 = m03;
                this.m10 = m10;
                this.m11 = m11;
                this.m12 = m12;
                this.m13 = m13;
                this.m20 = m20;
                this.m21 = m21;
                this.m22 = m22;
                this.m23 = m23;
                this.m30 = m30;
                this.m31 = m31;
                this.m32 = m32;
                this.m33 = m33;
        }

        public static void main( String[] args )
        {
	    System.out.println( "Test main..." );
	    new Matrix4f ();
	    new Matrix4f (0,0,0,0,
			  0,0,0,0,
			  0,0,0,0,
			  0,0,0,0);
	    new Matrix4f (new float [] {0,0,0,0,
					0,0,0,0,
					0,0,0,0,
					0,0,0,0});
        }
}

