// Regression test for PR java/16249.

class PR16249
{
    static void bug(byte[] iCode, int pc)
    {
        while (pc < 100) {
            try {
                switch (iCode[pc] & 0xff) {
                    case 666:
                        iCode[++pc] = 1;
                }
            }
            catch (Throwable ex) {
            }
        }
    }
}
