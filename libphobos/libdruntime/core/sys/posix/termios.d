/**
 * D header file for POSIX.
 *
 * Copyright: Copyright Sean Kelly 2005 - 2009.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Sean Kelly, Alex Rønne Petersen
 * Standards: The Open Group Base Specifications Issue 6, IEEE Std 1003.1, 2004 Edition
 */

/*          Copyright Sean Kelly 2005 - 2009.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
module core.sys.posix.termios;

private import core.sys.posix.config;
public import core.sys.posix.sys.types; // for pid_t

version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

version (Posix):
extern (C):

nothrow:
@nogc:

//
// Required
//
/*
cc_t
speed_t
tcflag_t

NCCS

struct termios
{
    tcflag_t   c_iflag;
    tcflag_t   c_oflag;
    tcflag_t   c_cflag;
    tcflag_t   c_lflag;
    cc_t[NCCS] c_cc;
}

VEOF
VEOL
VERASE
VINTR
VKILL
VMIN
VQUIT
VSTART
VSTOP
VSUSP
VTIME

BRKINT
ICRNL
IGNBRK
IGNCR
IGNPAR
INLCR
INPCK
ISTRIP
IXOFF
IXON
PARMRK

OPOST

B0
B50
B75
B110
B134
B150
B200
B300
B600
B1200
B1800
B2400
B4800
B9600
B19200
B38400

CSIZE
    CS5
    CS6
    CS7
    CS8
CSTOPB
CREAD
PARENB
PARODD
HUPCL
CLOCAL

ECHO
ECHOE
ECHOK
ECHONL
ICANON
IEXTEN
ISIG
NOFLSH
TOSTOP

TCSANOW
TCSADRAIN
TCSAFLUSH

TCIFLUSH
TCIOFLUSH
TCOFLUSH

TCIOFF
TCION
TCOOFF
TCOON

speed_t cfgetispeed(in termios*);
speed_t cfgetospeed(in termios*);
int     cfsetispeed(termios*, speed_t);
int     cfsetospeed(termios*, speed_t);
int     tcdrain(int);
int     tcflow(int, int);
int     tcflush(int, int);
int     tcgetattr(int, termios*);
int     tcsendbreak(int, int);
int     tcsetattr(int, int, in termios*);
*/

version (CRuntime_Glibc)
{
    alias ubyte cc_t;
    alias uint  speed_t;
    alias uint  tcflag_t;

    enum NCCS   = 32;

    struct termios
    {
        tcflag_t   c_iflag;
        tcflag_t   c_oflag;
        tcflag_t   c_cflag;
        tcflag_t   c_lflag;
        cc_t       c_line;
        cc_t[NCCS] c_cc;
        speed_t    c_ispeed;
        speed_t    c_ospeed;
    }

    enum VEOF       = 4;
    enum VEOL       = 11;
    enum VERASE     = 2;
    enum VINTR      = 0;
    enum VKILL      = 3;
    enum VMIN       = 6;
    enum VQUIT      = 1;
    enum VSTART     = 8;
    enum VSTOP      = 9;
    enum VSUSP      = 10;
    enum VTIME      = 5;

    enum BRKINT     = 0x0000002; // 0000002
    enum ICRNL      = 0x0000100; // 0000400
    enum IGNBRK     = 0x0000001; // 0000001
    enum IGNCR      = 0x0000080; // 0000200
    enum IGNPAR     = 0x0000004; // 0000004
    enum INLCR      = 0x0000040; // 0000100
    enum INPCK      = 0x0000010; // 0000020
    enum ISTRIP     = 0x0000020; // 0000040
    enum IXOFF      = 0x0001000; // 0010000
    enum IXON       = 0x0000400; // 0002000
    enum PARMRK     = 0x0000008; // 0000010

    enum OPOST      = 0x0000001; // 0000001

    enum B0         = 0x0000000; // 0000000
    enum B50        = 0x0000001; // 0000001
    enum B75        = 0x0000002; // 0000002
    enum B110       = 0x0000003; // 0000003
    enum B134       = 0x0000004; // 0000004
    enum B150       = 0x0000005; // 0000005
    enum B200       = 0x0000006; // 0000006
    enum B300       = 0x0000007; // 0000007
    enum B600       = 0x0000008; // 0000010
    enum B1200      = 0x0000009; // 0000011
    enum B1800      = 0x000000A; // 0000012
    enum B2400      = 0x000000B; // 0000013
    enum B4800      = 0x000000C; // 0000014
    enum B9600      = 0x000000D; // 0000015
    enum B19200     = 0x000000E; // 0000016
    enum B38400     = 0x000000F; // 0000017

    enum CSIZE      = 0x0000030; // 0000060
    enum   CS5      = 0x0000000; // 0000000
    enum   CS6      = 0x0000010; // 0000020
    enum   CS7      = 0x0000020; // 0000040
    enum   CS8      = 0x0000030; // 0000060
    enum CSTOPB     = 0x0000040; // 0000100
    enum CREAD      = 0x0000080; // 0000200
    enum PARENB     = 0x0000100; // 0000400
    enum PARODD     = 0x0000200; // 0001000
    enum HUPCL      = 0x0000400; // 0002000
    enum CLOCAL     = 0x0000800; // 0004000

    enum ECHO       = 0x0000008; // 0000010
    enum ECHOE      = 0x0000010; // 0000020
    enum ECHOK      = 0x0000020; // 0000040
    enum ECHONL     = 0x0000040; // 0000100
    enum ICANON     = 0x0000002; // 0000002
    enum IEXTEN     = 0x0008000; // 0100000
    enum ISIG       = 0x0000001; // 0000001
    enum NOFLSH     = 0x0000080; // 0000200
    enum TOSTOP     = 0x0000100; // 0000400

    enum TCSANOW    = 0;
    enum TCSADRAIN  = 1;
    enum TCSAFLUSH  = 2;

    enum TCIFLUSH   = 0;
    enum TCOFLUSH   = 1;
    enum TCIOFLUSH  = 2;

    enum TCIOFF     = 2;
    enum TCION      = 3;
    enum TCOOFF     = 0;
    enum TCOON      = 1;

    speed_t cfgetispeed(in termios*);
    speed_t cfgetospeed(in termios*);
    int     cfsetispeed(termios*, speed_t);
    int     cfsetospeed(termios*, speed_t);
    int     tcdrain(int);
    int     tcflow(int, int);
    int     tcflush(int, int);
    int     tcgetattr(int, termios*);
    int     tcsendbreak(int, int);
    int     tcsetattr(int, int, in termios*);
}
else version (Darwin)
{
    alias ubyte cc_t;
    alias c_ulong  speed_t;
    alias c_ulong  tcflag_t;

    enum NCCS   = 20;

    struct termios
    {
        tcflag_t   c_iflag;
        tcflag_t   c_oflag;
        tcflag_t   c_cflag;
        tcflag_t   c_lflag;
        cc_t[NCCS] c_cc;
        speed_t    c_ispeed;
        speed_t    c_ospeed;
    }

    enum VEOF       = 0;
    enum VEOL       = 1;
    enum VERASE     = 3;
    enum VINTR      = 8;
    enum VKILL      = 5;
    enum VMIN       = 16;
    enum VQUIT      = 9;
    enum VSTART     = 12;
    enum VSTOP      = 13;
    enum VSUSP      = 10;
    enum VTIME      = 17;

    enum BRKINT     = 0x0000002;
    enum ICRNL      = 0x0000100;
    enum IGNBRK     = 0x0000001;
    enum IGNCR      = 0x0000080;
    enum IGNPAR     = 0x0000004;
    enum INLCR      = 0x0000040;
    enum INPCK      = 0x0000010;
    enum ISTRIP     = 0x0000020;
    enum IXOFF      = 0x0000400;
    enum IXON       = 0x0000200;
    enum PARMRK     = 0x0000008;

    enum OPOST      = 0x0000001;

    enum B0         = 0;
    enum B50        = 50;
    enum B75        = 75;
    enum B110       = 110;
    enum B134       = 134;
    enum B150       = 150;
    enum B200       = 200;
    enum B300       = 300;
    enum B600       = 600;
    enum B1200      = 1200;
    enum B1800      = 1800;
    enum B2400      = 2400;
    enum B4800      = 4800;
    enum B9600      = 9600;
    enum B19200     = 19200;
    enum B38400     = 38400;

    enum CSIZE      = 0x0000300;
    enum   CS5      = 0x0000000;
    enum   CS6      = 0x0000100;
    enum   CS7      = 0x0000200;
    enum   CS8      = 0x0000300;
    enum CSTOPB     = 0x0000400;
    enum CREAD      = 0x0000800;
    enum PARENB     = 0x0001000;
    enum PARODD     = 0x0002000;
    enum HUPCL      = 0x0004000;
    enum CLOCAL     = 0x0008000;

    enum ECHO       = 0x00000008;
    enum ECHOE      = 0x00000002;
    enum ECHOK      = 0x00000004;
    enum ECHONL     = 0x00000010;
    enum ICANON     = 0x00000100;
    enum IEXTEN     = 0x00000400;
    enum ISIG       = 0x00000080;
    enum NOFLSH     = 0x80000000;
    enum TOSTOP     = 0x00400000;

    enum TCSANOW    = 0;
    enum TCSADRAIN  = 1;
    enum TCSAFLUSH  = 2;

    enum TCIFLUSH   = 1;
    enum TCOFLUSH   = 2;
    enum TCIOFLUSH  = 3;

    enum TCIOFF     = 3;
    enum TCION      = 4;
    enum TCOOFF     = 1;
    enum TCOON      = 2;

    speed_t cfgetispeed(in termios*);
    speed_t cfgetospeed(in termios*);
    int     cfsetispeed(termios*, speed_t);
    int     cfsetospeed(termios*, speed_t);
    int     tcdrain(int);
    int     tcflow(int, int);
    int     tcflush(int, int);
    int     tcgetattr(int, termios*);
    int     tcsendbreak(int, int);
    int     tcsetattr(int, int, in termios*);

}
else version (FreeBSD)
{
    alias ubyte cc_t;
    alias uint  speed_t;
    alias uint  tcflag_t;

    enum NCCS   = 20;

    struct termios
    {
        tcflag_t   c_iflag;
        tcflag_t   c_oflag;
        tcflag_t   c_cflag;
        tcflag_t   c_lflag;
        cc_t[NCCS] c_cc;
        speed_t    c_ispeed;
        speed_t    c_ospeed;
    }

    enum VEOF       = 0;
    enum VEOL       = 1;
    enum VERASE     = 3;
    enum VINTR      = 8;
    enum VKILL      = 5;
    enum VMIN       = 16;
    enum VQUIT      = 9;
    enum VSTART     = 12;
    enum VSTOP      = 13;
    enum VSUSP      = 10;
    enum VTIME      = 17;

    enum BRKINT     = 0x0000002;
    enum ICRNL      = 0x0000100;
    enum IGNBRK     = 0x0000001;
    enum IGNCR      = 0x0000080;
    enum IGNPAR     = 0x0000004;
    enum INLCR      = 0x0000040;
    enum INPCK      = 0x0000010;
    enum ISTRIP     = 0x0000020;
    enum IXOFF      = 0x0000400;
    enum IXON       = 0x0000200;
    enum PARMRK     = 0x0000008;

    enum OPOST      = 0x0000001;

    enum B0         = 0;
    enum B50        = 50;
    enum B75        = 75;
    enum B110       = 110;
    enum B134       = 134;
    enum B150       = 150;
    enum B200       = 200;
    enum B300       = 300;
    enum B600       = 600;
    enum B1200      = 1200;
    enum B1800      = 1800;
    enum B2400      = 2400;
    enum B4800      = 4800;
    enum B9600      = 9600;
    enum B19200     = 19200;
    enum B38400     = 38400;

    enum CSIZE      = 0x0000300;
    enum   CS5      = 0x0000000;
    enum   CS6      = 0x0000100;
    enum   CS7      = 0x0000200;
    enum   CS8      = 0x0000300;
    enum CSTOPB     = 0x0000400;
    enum CREAD      = 0x0000800;
    enum PARENB     = 0x0001000;
    enum PARODD     = 0x0002000;
    enum HUPCL      = 0x0004000;
    enum CLOCAL     = 0x0008000;

    enum ECHO       = 0x00000008;
    enum ECHOE      = 0x00000002;
    enum ECHOK      = 0x00000004;
    enum ECHONL     = 0x00000010;
    enum ICANON     = 0x00000100;
    enum IEXTEN     = 0x00000400;
    enum ISIG       = 0x00000080;
    enum NOFLSH     = 0x80000000;
    enum TOSTOP     = 0x00400000;

    enum TCSANOW    = 0;
    enum TCSADRAIN  = 1;
    enum TCSAFLUSH  = 2;

    enum TCIFLUSH   = 1;
    enum TCOFLUSH   = 2;
    enum TCIOFLUSH  = 3;

    enum TCIOFF     = 3;
    enum TCION      = 4;
    enum TCOOFF     = 1;
    enum TCOON      = 2;

    speed_t cfgetispeed(in termios*);
    speed_t cfgetospeed(in termios*);
    int     cfsetispeed(termios*, speed_t);
    int     cfsetospeed(termios*, speed_t);
    int     tcdrain(int);
    int     tcflow(int, int);
    int     tcflush(int, int);
    int     tcgetattr(int, termios*);
    int     tcsendbreak(int, int);
    int     tcsetattr(int, int, in termios*);
}
else version (DragonFlyBSD)
{
    alias ubyte cc_t;
    alias uint  speed_t;
    alias uint  tcflag_t;

    enum NCCS   = 20;

    struct termios
    {
        tcflag_t   c_iflag;
        tcflag_t   c_oflag;
        tcflag_t   c_cflag;
        tcflag_t   c_lflag;
        cc_t[NCCS] c_cc;
        speed_t    c_ispeed;
        speed_t    c_ospeed;
    }

    enum VEOF       = 0;
    enum VEOL       = 1;
    enum VERASE     = 3;
    enum VINTR      = 8;
    enum VKILL      = 5;
    enum VMIN       = 16;
    enum VQUIT      = 9;
    enum VSTART     = 12;
    enum VSTOP      = 13;
    enum VSUSP      = 10;
    enum VTIME      = 17;

    enum BRKINT     = 0x0000002;
    enum ICRNL      = 0x0000100;
    enum IGNBRK     = 0x0000001;
    enum IGNCR      = 0x0000080;
    enum IGNPAR     = 0x0000004;
    enum INLCR      = 0x0000040;
    enum INPCK      = 0x0000010;
    enum ISTRIP     = 0x0000020;
    enum IXOFF      = 0x0000400;
    enum IXON       = 0x0000200;
    enum PARMRK     = 0x0000008;

    enum OPOST      = 0x0000001;

    enum B0         = 0;
    enum B50        = 50;
    enum B75        = 75;
    enum B110       = 110;
    enum B134       = 134;
    enum B150       = 150;
    enum B200       = 200;
    enum B300       = 300;
    enum B600       = 600;
    enum B1200      = 1200;
    enum B1800      = 1800;
    enum B2400      = 2400;
    enum B4800      = 4800;
    enum B9600      = 9600;
    enum B19200     = 19200;
    enum B38400     = 38400;

    enum CSIZE      = 0x0000300;
    enum   CS5      = 0x0000000;
    enum   CS6      = 0x0000100;
    enum   CS7      = 0x0000200;
    enum   CS8      = 0x0000300;
    enum CSTOPB     = 0x0000400;
    enum CREAD      = 0x0000800;
    enum PARENB     = 0x0001000;
    enum PARODD     = 0x0002000;
    enum HUPCL      = 0x0004000;
    enum CLOCAL     = 0x0008000;

    enum ECHO       = 0x00000008;
    enum ECHOE      = 0x00000002;
    enum ECHOK      = 0x00000004;
    enum ECHONL     = 0x00000010;
    enum ICANON     = 0x00000100;
    enum IEXTEN     = 0x00000400;
    enum ISIG       = 0x00000080;
    enum NOFLSH     = 0x80000000;
    enum TOSTOP     = 0x00400000;

    enum TCSANOW    = 0;
    enum TCSADRAIN  = 1;
    enum TCSAFLUSH  = 2;

    enum TCIFLUSH   = 1;
    enum TCOFLUSH   = 2;
    enum TCIOFLUSH  = 3;

    enum TCIOFF     = 3;
    enum TCION      = 4;
    enum TCOOFF     = 1;
    enum TCOON      = 2;

    speed_t cfgetispeed(in termios*);
    speed_t cfgetospeed(in termios*);
    int     cfsetispeed(termios*, speed_t);
    int     cfsetospeed(termios*, speed_t);
    int     tcdrain(int);
    int     tcflow(int, int);
    int     tcflush(int, int);
    int     tcgetattr(int, termios*);
    int     tcsendbreak(int, int);
    int     tcsetattr(int, int, in termios*);
}
else version (NetBSD)
{
    alias ubyte cc_t;
    alias uint  speed_t;
    alias uint  tcflag_t;

    enum NCCS   = 20;

    struct termios
    {
        tcflag_t   c_iflag;
        tcflag_t   c_oflag;
        tcflag_t   c_cflag;
        tcflag_t   c_lflag;
        cc_t[NCCS] c_cc;
        speed_t    c_ispeed;
        speed_t    c_ospeed;
    }

    enum VEOF       = 0;
    enum VEOL       = 1;
    enum VERASE     = 3;
    enum VINTR      = 8;
    enum VKILL      = 5;
    enum VMIN       = 16;
    enum VQUIT      = 9;
    enum VSTART     = 12;
    enum VSTOP      = 13;
    enum VSUSP      = 10;
    enum VTIME      = 17;

    enum BRKINT     = 0x0000002;
    enum ICRNL      = 0x0000100;
    enum IGNBRK     = 0x0000001;
    enum IGNCR      = 0x0000080;
    enum IGNPAR     = 0x0000004;
    enum INLCR      = 0x0000040;
    enum INPCK      = 0x0000010;
    enum ISTRIP     = 0x0000020;
    enum IXOFF      = 0x0000400;
    enum IXON       = 0x0000200;
    enum PARMRK     = 0x0000008;

    enum OPOST      = 0x0000001;

    enum B0         = 0;
    enum B50        = 50;
    enum B75        = 75;
    enum B110       = 110;
    enum B134       = 134;
    enum B150       = 150;
    enum B200       = 200;
    enum B300       = 300;
    enum B600       = 600;
    enum B1200      = 1200;
    enum B1800      = 1800;
    enum B2400      = 2400;
    enum B4800      = 4800;
    enum B9600      = 9600;
    enum B19200     = 19200;
    enum B38400     = 38400;

    enum CSIZE      = 0x0000300;
    enum   CS5      = 0x0000000;
    enum   CS6      = 0x0000100;
    enum   CS7      = 0x0000200;
    enum   CS8      = 0x0000300;
    enum CSTOPB     = 0x0000400;
    enum CREAD      = 0x0000800;
    enum PARENB     = 0x0001000;
    enum PARODD     = 0x0002000;
    enum HUPCL      = 0x0004000;
    enum CLOCAL     = 0x0008000;

    enum ECHO       = 0x00000008;
    enum ECHOE      = 0x00000002;
    enum ECHOK      = 0x00000004;
    enum ECHONL     = 0x00000010;
    enum ICANON     = 0x00000100;
    enum IEXTEN     = 0x00000400;
    enum ISIG       = 0x00000080;
    enum NOFLSH     = 0x80000000;
    enum TOSTOP     = 0x00400000;

    enum TCSANOW    = 0;
    enum TCSADRAIN  = 1;
    enum TCSAFLUSH  = 2;

    enum TCIFLUSH   = 1;
    enum TCOFLUSH   = 2;
    enum TCIOFLUSH  = 3;

    enum TCIOFF     = 3;
    enum TCION      = 4;
    enum TCOOFF     = 1;
    enum TCOON      = 2;

    speed_t cfgetispeed(in termios*);
    speed_t cfgetospeed(in termios*);
    int     cfsetispeed(termios*, speed_t);
    int     cfsetospeed(termios*, speed_t);
    int     tcdrain(int);
    int     tcflow(int, int);
    int     tcflush(int, int);
    int     tcgetattr(int, termios*);
    int     tcsendbreak(int, int);
    int     tcsetattr(int, int, in termios*);
}
else version (Solaris)
{
    alias tcflag_t = uint;
    alias cc_t = ubyte;
    alias speed_t = uint;

    enum NCCS   = 19;

    struct termios
    {
        tcflag_t   c_iflag;    /* input modes */
        tcflag_t   c_oflag;    /* output modes */
        tcflag_t   c_cflag;    /* control modes */
        tcflag_t   c_lflag;    /* line discipline modes */
        cc_t[NCCS] c_cc;       /* control chars */
    }

    /* control characters */
    enum VINTR  = 0;
    enum VQUIT  = 1;
    enum VERASE = 2;
    enum VKILL  = 3;
    enum VEOF   = 4;
    enum VEOL   = 5;
    enum VMIN   = 4;
    enum VTIME  = 5;
    enum VSTART = 8;
    enum VSTOP  = 9;
    enum VSUSP  = 10;

    /* input modes */
    enum IGNBRK = 0x000001;
    enum BRKINT = 0x000002;
    enum IGNPAR = 0x000004;
    enum PARMRK = 0x000008;
    enum INPCK  = 0x000010;
    enum ISTRIP = 0x000020;
    enum INLCR  = 0x000040;
    enum IGNCR  = 0x000080;
    enum ICRNL  = 0x000100;
    enum IXON   = 0x000400;
    enum IXOFF  = 0x001000;

    /* output modes */
    enum OPOST  = 0x000001;

    /* control modes */
    enum CSIZE  = 0x000030;
    enum CS5    = 0x000000;
    enum CS6    = 0x000010;
    enum CS7    = 0x000020;
    enum CS8    = 0x000030;
    enum CSTOPB = 0x000040;
    enum CREAD  = 0x000080;
    enum PARENB = 0x000100;
    enum PARODD = 0x000200;
    enum HUPCL  = 0x000400;
    enum CLOCAL = 0x000800;

    enum CRTSCTS = 0x10000000;

    /* line discipline 0 modes */
    enum ISIG   = 0x000001;
    enum ICANON = 0x000002;
    enum ECHO   = 0x000008;
    enum ECHOE  = 0x000010;
    enum ECHOK  = 0x000020;
    enum ECHONL = 0x000040;
    enum NOFLSH = 0x000080;
    enum TOSTOP = 0x000100;

    enum ECHOCTL = 0x000200;
    enum ECHOPRT = 0x000400;
    enum ECHOKE  = 0x000800;

    enum IEXTEN = 0x008000;  /* POSIX flag - enable POSIX extensions */

    enum _TIOC      = ('T'<<8);
    enum TCSANOW    = (_TIOC|14);
    enum TCSADRAIN  = (_TIOC|15);
    enum TCSAFLUSH  = (_TIOC|16);

    /* termios option flags */
    enum TCIFLUSH   = 0;  /* flush data received but not read */
    enum TCOFLUSH   = 1;  /* flush data written but not transmitted */
    enum TCIOFLUSH  = 2;  /* flush both data both input and output queues */

    enum TCOOFF     = 0;  /* suspend output */
    enum TCOON      = 1;  /* restart suspended output */
    enum TCIOFF     = 2;  /* suspend input */
    enum TCION      = 3;  /* restart suspended input */

    /* Speeds */
    enum B0      = 0;
    enum B50     = 1;
    enum B75     = 2;
    enum B110    = 3;
    enum B134    = 4;
    enum B150    = 5;
    enum B200    = 6;
    enum B300    = 7;
    enum B600    = 8;
    enum B1200   = 9;
    enum B1800   = 10;
    enum B2400   = 11;
    enum B4800   = 12;
    enum B9600   = 13;
    enum B19200  = 14;
    enum B38400  = 15;
    enum B57600  = 16;
    enum B76800  = 17;
    enum B115200 = 18;
    enum B153600 = 19;
    enum B230400 = 20;
    enum B307200 = 21;
    enum B460800 = 22;
    enum B921600 = 23;

    /*
     * POSIX termios functions
     * These functions get mapped into ioctls.
     */
    speed_t cfgetospeed(in termios*);
    int     cfsetospeed(termios*, speed_t);
    speed_t cfgetispeed(in termios*);
    int     cfsetispeed(termios*, speed_t);
    int     tcgetattr(int, termios*);
    int     tcsetattr(int, int, in termios*);
    int     tcsendbreak(int, int);
    int     tcdrain(int);
    int     tcflush(int, int);
    int     tcflow(int, int);
}
else version (CRuntime_UClibc)
{
    alias ubyte cc_t;
    alias uint  speed_t;
    alias uint  tcflag_t;

    enum NCCS   = 32;

    struct termios
    {
        tcflag_t   c_iflag;
        tcflag_t   c_oflag;
        tcflag_t   c_cflag;
        tcflag_t   c_lflag;
        cc_t       c_line;
        cc_t[NCCS] c_cc;
        speed_t    c_ispeed;
        speed_t    c_ospeed;
    }

    enum VINTR      = 0;
    enum VQUIT      = 1;
    enum VERASE     = 2;
    enum VKILL      = 3;
    enum VEOF       = 4;
    enum VTIME      = 5;
    enum VMIN       = 6;
    enum VSWTC      = 7;
    enum VSTART     = 8;
    enum VSTOP      = 9;
    enum VSUSP      = 10;
    enum VEOL       = 11;
    enum VREPRINT   = 12;
    enum VDISCARD   = 13;
    enum VWERASE    = 14;
    enum VLNEXT     = 15;
    enum VEOL2      = 16;

    enum BRKINT     = 0x0000002; // 0000002
    enum ICRNL      = 0x0000100; // 0000400
    enum IGNBRK     = 0x0000001; // 0000001
    enum IGNCR      = 0x0000080; // 0000200
    enum IGNPAR     = 0x0000004; // 0000004
    enum INLCR      = 0x0000040; // 0000100
    enum INPCK      = 0x0000010; // 0000020
    enum ISTRIP     = 0x0000020; // 0000040
    enum IXOFF      = 0x0001000; // 0010000
    enum IXON       = 0x0000400; // 0002000
    enum PARMRK     = 0x0000008; // 0000010

    enum OPOST      = 0x0000001; // 0000001

    enum B0         = 0x0000000; // 0000000
    enum B50        = 0x0000001; // 0000001
    enum B75        = 0x0000002; // 0000002
    enum B110       = 0x0000003; // 0000003
    enum B134       = 0x0000004; // 0000004
    enum B150       = 0x0000005; // 0000005
    enum B200       = 0x0000006; // 0000006
    enum B300       = 0x0000007; // 0000007
    enum B600       = 0x0000008; // 0000010
    enum B1200      = 0x0000009; // 0000011
    enum B1800      = 0x000000A; // 0000012
    enum B2400      = 0x000000B; // 0000013
    enum B4800      = 0x000000C; // 0000014
    enum B9600      = 0x000000D; // 0000015
    enum B19200     = 0x000000E; // 0000016
    enum B38400     = 0x000000F; // 0000017

    enum CSIZE      = 0x0000030; // 0000060
    enum   CS5      = 0x0000000; // 0000000
    enum   CS6      = 0x0000010; // 0000020
    enum   CS7      = 0x0000020; // 0000040
    enum   CS8      = 0x0000030; // 0000060
    enum CSTOPB     = 0x0000040; // 0000100
    enum CREAD      = 0x0000080; // 0000200
    enum PARENB     = 0x0000100; // 0000400
    enum PARODD     = 0x0000200; // 0001000
    enum HUPCL      = 0x0000400; // 0002000
    enum CLOCAL     = 0x0000800; // 0004000

    enum ECHO       = 0x0000008; // 0000010
    enum ECHOE      = 0x0000010; // 0000020
    enum ECHOK      = 0x0000020; // 0000040
    enum ECHONL     = 0x0000040; // 0000100
    enum ICANON     = 0x0000002; // 0000002
    enum IEXTEN     = 0x0008000; // 0100000
    enum ISIG       = 0x0000001; // 0000001
    enum NOFLSH     = 0x0000080; // 0000200
    enum TOSTOP     = 0x0000100; // 0000400

    enum TCSANOW    = 0;
    enum TCSADRAIN  = 1;
    enum TCSAFLUSH  = 2;

    enum TCIFLUSH   = 0;
    enum TCOFLUSH   = 1;
    enum TCIOFLUSH  = 2;

    enum TCIOFF     = 2;
    enum TCION      = 3;
    enum TCOOFF     = 0;
    enum TCOON      = 1;

    speed_t cfgetispeed(in termios*);
    speed_t cfgetospeed(in termios*);
    int     cfsetispeed(termios*, speed_t);
    int     cfsetospeed(termios*, speed_t);
    int     tcdrain(int);
    int     tcflow(int, int);
    int     tcflush(int, int);
    int     tcgetattr(int, termios*);
    int     tcsendbreak(int, int);
    int     tcsetattr(int, int, in termios*);
}

//
// XOpen (XSI)
//
/*
IXANY

ONLCR
OCRNL
ONOCR
ONLRET
OFILL
NLDLY
    NL0
    NL1
CRDLY
    CR0
    CR1
    CR2
    CR3
TABDLY
    TAB0
    TAB1
    TAB2
    TAB3
BSDLY
    BS0
    BS1
VTDLY
    VT0
    VT1
FFDLY
    FF0
    FF1

pid_t   tcgetsid(int);
*/

version (CRuntime_Glibc)
{
    enum IXANY      = 0x0000800; // 0004000

    enum ONLCR      = 0x0000004; // 0000004
    enum OCRNL      = 0x0000008; // 0000010
    enum ONOCR      = 0x0000010; // 0000020
    enum ONLRET     = 0x0000020; // 0000040
    enum OFILL      = 0x0000040; // 0000100
    enum NLDLY      = 0x0000100; // 0000400
    enum   NL0      = 0x0000000; // 0000000
    enum   NL1      = 0x0000100; // 0000400
    enum CRDLY      = 0x0000600; // 0003000
    enum   CR0      = 0x0000000; // 0000000
    enum   CR1      = 0x0000200; // 0001000
    enum   CR2      = 0x0000400; // 0002000
    enum   CR3      = 0x0000600; // 0003000
    enum TABDLY     = 0x0001800; // 0014000
    enum   TAB0     = 0x0000000; // 0000000
    enum   TAB1     = 0x0000800; // 0004000
    enum   TAB2     = 0x0001000; // 0010000
    enum   TAB3     = 0x0001800; // 0014000
    enum BSDLY      = 0x0002000; // 0020000
    enum   BS0      = 0x0000000; // 0000000
    enum   BS1      = 0x0002000; // 0020000
    enum VTDLY      = 0x0004000; // 0040000
    enum   VT0      = 0x0000000; // 0000000
    enum   VT1      = 0x0004000; // 0040000
    enum FFDLY      = 0x0008000; // 0100000
    enum   FF0      = 0x0000000; // 0000000
    enum   FF1      = 0x0008000; // 0100000

    pid_t   tcgetsid(int);
}

else version (Darwin)
{
    enum IXANY      = 0x00000800;

    enum ONLCR      = 0x00000002;
    enum OCRNL      = 0x00000010;
    enum ONOCR      = 0x00000020;
    enum ONLRET     = 0x00000040;
    enum OFILL      = 0x00000080;
    enum NLDLY      = 0x00000300;
    enum   NL0      = 0x00000000;
    enum   NL1      = 0x00000100;
    enum CRDLY      = 0x00003000;
    enum   CR0      = 0x00000000;
    enum   CR1      = 0x00001000;
    enum   CR2      = 0x00002000;
    enum   CR3      = 0x00003000;
    enum TABDLY     = 0x00000c04;
    enum   TAB0     = 0x00000000;
    enum   TAB1     = 0x00000400;
    enum   TAB2     = 0x00000800;
    enum   TAB3     = 0x00000004;
    enum BSDLY      = 0x00008000;
    enum   BS0      = 0x00000000;
    enum   BS1      = 0x00008000;
    enum VTDLY      = 0x00010000;
    enum   VT0      = 0x00000000;
    enum   VT1      = 0x00010000;
    enum FFDLY      = 0x00004000;
    enum   FF0      = 0x00000000;
    enum   FF1      = 0x00004000;

    pid_t tcgetsid (int);
}

else version (FreeBSD)
{
    enum IXANY      = 0x00000800;

    enum ONLCR      = 0x00000002;
    enum OCRNL      = 0x00000010;
    enum ONOCR      = 0x00000020;
    enum ONLRET     = 0x00000040;
    //enum OFILL
    //enum NLDLY
    //enum     NL0
    //enum     NL1
    //enum CRDLY
    //enum     CR0
    //enum     CR1
    //enum     CR2
    //enum     CR3
    enum TABDLY     = 0x00000004;
    enum     TAB0   = 0x00000000;
    //enum     TAB1
    //enum     TAB2
    enum     TAB3   = 0x00000004;
    //enum BSDLY
    //enum     BS0
    //enum     BS1
    //enum VTDLY
    //enum     VT0
    //enum     VT1
    //enum FFDLY
    //enum     FF0
    //enum     FF1

    pid_t   tcgetsid(int);
}
else version (DragonFlyBSD)
{
    enum IXANY      = 0x00000800;

    enum ONLCR      = 0x00000002;
    enum OCRNL      = 0x00000010;
    enum ONOCR      = 0x00000020;
    enum ONLRET     = 0x00000040;
    //enum OFILL
    //enum NLDLY
    //enum     NL0
    //enum     NL1
    //enum CRDLY
    //enum     CR0
    //enum     CR1
    //enum     CR2
    //enum     CR3
    enum TABDLY     = 0x00000004;
    enum     TAB0   = 0x00000000;
    //enum     TAB1
    //enum     TAB2
    enum     TAB3   = 0x00000004;
    //enum BSDLY
    //enum     BS0
    //enum     BS1
    //enum VTDLY
    //enum     VT0
    //enum     VT1
    //enum FFDLY
    //enum     FF0
    //enum     FF1

    pid_t   tcgetsid(int);
}
else version (NetBSD)
{
    enum IXANY      = 0x00000800;

    enum ONLCR      = 0x00000002;
    enum OCRNL      = 0x00000010;
    enum ONOCR      = 0x00000020;
    enum ONLRET     = 0x00000040;
    //enum OFILL
    //enum NLDLY
    //enum     NL0
    //enum     NL1
    //enum CRDLY
    //enum     CR0
    //enum     CR1
    //enum     CR2
    //enum     CR3
    enum TABDLY     = 0x00000004;
    enum     TAB0   = 0x00000000;
    //enum     TAB1
    //enum     TAB2
    enum     TAB3   = 0x00000004;
    //enum BSDLY
    //enum     BS0
    //enum     BS1
    //enum VTDLY
    //enum     VT0
    //enum     VT1
    //enum FFDLY
    //enum     FF0
    //enum     FF1

    pid_t   tcgetsid(int);
}
else version (Solaris)
{
    enum IXANY      = 0x0000800;

    enum ONLCR      = 0x0000004;
    enum OCRNL      = 0x0000008;
    enum ONOCR      = 0x0000010;
    enum ONLRET     = 0x0000020;
    enum OFILL      = 0x0000040;
    enum OFDEL      = 0x0000080;
    enum NLDLY      = 0x0000100;
    enum NL0        = 0x0000000;
    enum NL1        = 0x0000100;
    enum CRDLY      = 0x0000600;
    enum CR0        = 0x0000000;
    enum CR1        = 0x0000200;
    enum CR2        = 0x0000400;
    enum CR3        = 0x0000600;
    enum TABDLY     = 0x0001800;
    enum TAB0       = 0x0000000;
    enum TAB1       = 0x0000800;
    enum TAB2       = 0x0001000;
    enum TAB3       = 0x0001800;
    enum BSDLY      = 0x0002000;
    enum BS0        = 0x0000000;
    enum BS1        = 0x0002000;
    enum VTDLY      = 0x0004000;
    enum VT0        = 0x0000000;
    enum VT1        = 0x0004000;
    enum FFDLY      = 0x0008000;
    enum FF0        = 0x0000000;
    enum FF1        = 0x0008000;
    enum XCASE      = 0x0000004;

    pid_t   tcgetsid(int);
}
else version (CRuntime_UClibc)
{
    enum IXANY      = 0x0000800; // 0004000

    enum ONLCR      = 0x0000004; // 0000004
    enum OCRNL      = 0x0000008; // 0000010
    enum ONOCR      = 0x0000010; // 0000020
    enum ONLRET     = 0x0000020; // 0000040
    enum OFILL      = 0x0000040; // 0000100
    enum NLDLY      = 0x0000100; // 0000400
    enum   NL0      = 0x0000000; // 0000000
    enum   NL1      = 0x0000100; // 0000400
    enum CRDLY      = 0x0000600; // 0003000
    enum   CR0      = 0x0000000; // 0000000
    enum   CR1      = 0x0000200; // 0001000
    enum   CR2      = 0x0000400; // 0002000
    enum   CR3      = 0x0000600; // 0003000
    enum TABDLY     = 0x0001800; // 0014000
    enum   TAB0     = 0x0000000; // 0000000
    enum   TAB1     = 0x0000800; // 0004000
    enum   TAB2     = 0x0001000; // 0010000
    enum   TAB3     = 0x0001800; // 0014000
    enum BSDLY      = 0x0002000; // 0020000
    enum   BS0      = 0x0000000; // 0000000
    enum   BS1      = 0x0002000; // 0020000
    enum VTDLY      = 0x0004000; // 0040000
    enum   VT0      = 0x0000000; // 0000000
    enum   VT1      = 0x0004000; // 0040000
    enum FFDLY      = 0x0008000; // 0100000
    enum   FF0      = 0x0000000; // 0000000
    enum   FF1      = 0x0008000; // 0100000

    pid_t   tcgetsid(int);
}
