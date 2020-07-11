/**
 * D header file for POSIX.
 *
 * License: $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 */

module core.sys.posix.sys.ttycom;

import core.sys.posix.sys.ioccom;
import core.sys.posix.termios;
import core.sys.posix.sys.time;

version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

version (Posix):

nothrow @nogc:

version (Darwin)
{
    struct winsize {
        ushort  ws_row;     // rows, in characters
        ushort  ws_col;     // columns, in characters
        ushort  ws_xpixel;  // horizontal size, pixels
        ushort  ws_ypixel;  // vertical size, pixels
    }

    // Serial/TTY ioctl's
    enum uint TIOCMODG =  _IOR!(int)('t', 3);  // get modem control state
    enum uint TIOCMODS =  _IOW!(int)('t', 4);  // set modem control state
    enum uint       TIOCM_LE  = 0x001;          // line enable
    enum uint       TIOCM_DTR = 0x002;          // data terminal ready
    enum uint       TIOCM_RTS = 0x004;          // request to send
    enum uint       TIOCM_ST  = 0x008;          // secondary transmit
    enum uint       TIOCM_SR  = 0x010;          // secondary receive
    enum uint       TIOCM_CTS = 0x020;          // clear to send
    enum uint       TIOCM_CAR = 0x040;          // carrier detect
    enum uint       TIOCM_CD  = TIOCM_CAR;
    enum uint       TIOCM_RNG = 0x080;          // ring
    enum uint       TIOCM_RI  = TIOCM_RNG;
    enum uint       TIOCM_DSR = 0x100;          // data set ready
                            // 8-10 compat
    enum uint TIOCEXCL  = _IO('t', 13);        // set exclusive use of tty
    enum uint TIOCNXCL  = _IO('t', 14);        // reset exclusive use of tty
                            // 15 unused
    enum uint TIOCFLUSH = _IOW!(int)('t', 16); // flush buffers
                            // 17-18 compat
    enum uint TIOCGETA  = _IOR!(termios)('t', 19); // get termios struct
    enum uint TIOCSETA  = _IOW!(termios)('t', 20); // set termios struct
    enum uint TIOCSETAW = _IOW!(termios)('t', 21); // drain output, set
    enum uint TIOCSETAF = _IOW!(termios)('t', 22); // drn out, fls in, set
    enum uint TIOCGETD  = _IOR!(int)('t', 26); // get line discipline
    enum uint TIOCSETD  = _IOW!(int)('t', 27); // set line discipline
    enum uint TIOCIXON  = _IO('t', 129);       // internal input VSTART
    enum uint TIOCIXOFF = _IO('t', 128);       // internal input VSTOP
                            // 127-124 compat
    enum uint TIOCSBRK  = _IO('t', 123);       // set break bit
    enum uint TIOCCBRK  = _IO('t', 122);       // clear break bit
    enum uint TIOCSDTR  = _IO('t', 121);       // set data terminal ready
    enum uint TIOCCDTR  = _IO('t', 120);       // clear data terminal ready
    enum uint TIOCGPGRP = _IOR!(int)('t', 119); // get pgrp of tty
    enum uint TIOCSPGRP = _IOW!(int)('t', 118); // set pgrp of tty
                            // 117-116 compat
    enum uint TIOCOUTQ  = _IOR!(int)('t', 115); // output queue size
    enum uint TIOCSTI   = _IOW!(char)('t', 114);// simulate terminal input
    enum uint TIOCNOTTY = _IO('t', 113);        // void tty association
    enum uint TIOCPKT   = _IOW!(int)('t', 112); // pty: set/clear packet mode
    enum uint   TIOCPKT_DATA       = 0x00;     // data packet
    enum uint   TIOCPKT_FLUSHREAD  = 0x01;     // flush packet
    enum uint   TIOCPKT_FLUSHWRITE = 0x02;     // flush packet
    enum uint   TIOCPKT_STOP       = 0x04;     // stop output
    enum uint   TIOCPKT_START      = 0x08;     // start output
    enum uint   TIOCPKT_NOSTOP     = 0x10;     // no more ^S, ^Q
    enum uint   TIOCPKT_DOSTOP     = 0x20;     // now do ^S ^Q
    enum uint   TIOCPKT_IOCTL      = 0x40;     // state change of pty driver
    enum uint TIOCSTOP   = _IO('t', 111);      // stop output, like ^S
    enum uint TIOCSTART  = _IO('t', 110);      // start output, like ^Q
    enum uint TIOCMSET   = _IOW!(int)('t', 109); // set all modem bits
    enum uint TIOCMBIS   = _IOW!(int)('t', 108); // bis modem bits
    enum uint TIOCMBIC   = _IOW!(int)('t', 107); // bic modem bits
    enum uint TIOCMGET   = _IOR!(int)('t', 106); // get all modem bits
    enum uint TIOCREMOTE = _IOW!(int)('t', 105); // remote input editing
    enum uint TIOCGWINSZ = _IOR!(winsize)('t', 104);  // get window size
    enum uint TIOCSWINSZ = _IOW!(winsize)('t', 103);  // set window size
    enum uint TIOCUCNTL  = _IOW!(int)('t', 102); // pty: set/clr usr cntl mode
    enum uint TIOCSTAT   = _IO('t', 101);      // simulate ^T status message
    enum uint   UIOCCMD(n) = _IO('u', n);      // usr cntl op "n"
    enum uint TIOCSCONS = _IO('t', 99);        // 4.2 compatibility
    enum uint TIOCCONS  = _IOW!(int)('t', 98); // become virtual console
    enum uint TIOCSCTTY = _IO('t', 97);        // become controlling tty
    enum uint TIOCEXT   = _IOW!(int)('t', 96); // pty: external processing
    enum uint TIOCSIG   =   _IO('t', 95);      // pty: generate signal
    enum uint TIOCDRAIN =   _IO('t', 94);      // wait till output drained
    enum uint TIOCMSDTRWAIT =  _IOW!(int)('t', 91);  // modem: set wait on close
    enum uint TIOCMGDTRWAIT =  _IOR!(int)('t', 90);  // modem: get wait on close
    enum uint TIOCTIMESTAMP =  _IOR!(timeval)('t', 89);   // enable/get timestamp
                                                          // of last input event
    enum uint TIOCDCDTIMESTAMP = _IOR!(timeval)('t', 88); // enable/get timestamp
                                                          // of last DCd rise
    enum uint TIOCSDRAINWAIT   = _IOW!(int)('t', 87); // set ttywait timeout
    enum uint TIOCGDRAINWAIT   = _IOR!(int)('t', 86); // get ttywait timeout
    enum uint TIOCDSIMICROCODE = _IO('t', 85);        // download microcode to
                                                      // DSI Softmodem
    enum uint TIOCPTYGRANT  =  _IO('t', 84);   // grantpt(3)
    enum uint TIOCPTYGNAME  =  _IOC(IOC_OUT, 't', 83, 128); // ptsname(3)
    enum uint TIOCPTYUNLK   =  _IO('t', 82);   // unlockpt(3)

    enum uint TTYDISC  = 0;       // termios tty line discipline
    enum uint TABLDISC = 3;       // tablet discipline
    enum uint SLIPDISC = 4;       // serial IP discipline
    enum uint PPPDISC  = 5;       // PPP discipline
}
else version (FreeBSD)
{
    struct winsize {
        ushort  ws_row;     // rows, in characters
        ushort  ws_col;     // columns, in characters
        ushort  ws_xpixel;  // horizontal size, pixels
        ushort  ws_ypixel;  // vertical size, pixels
    }

    // Serial/TTY ioctl's
                                               // 0-2 compat
                                               // 3-7 unused
                                               // 8-10 compat
                                               // 11-12 unused
    enum uint TIOCEXCL  = _IO('t', 13);        // set exclusive use of tty
    enum uint TIOCNXCL  = _IO('t', 14);        // reset exclusive use of tty
    enum uint TIOCGPTN  = _IOR!(int)('t', 15); // get pts number
    enum uint TIOCFLUSH = _IOW!(int)('t', 16); // flush buffers
                            // 17-18 compat
    enum uint TIOCGETA  = _IOR!(termios)('t', 19); // get termios struct
    enum uint TIOCSETA  = _IOW!(termios)('t', 20); // set termios struct
    enum uint TIOCSETAW = _IOW!(termios)('t', 21); // drain output, set
    enum uint TIOCSETAF = _IOW!(termios)('t', 22); // drn out, fls in, set
                            // 23-25 unused
    enum uint TIOCGETD  = _IOR!(int)('t', 26); // get line discipline
    enum uint TIOCSETD  = _IOW!(int)('t', 27); // set line discipline
    enum uint TIOCPTMASTER = _IO('t', 28);     // pts master validation
                            // 29-85 unused
    enum uint TIOCGDRAINWAIT = _IOR!(int)('t', 86); // get ttywait timeout
    enum uint TIOCSDRAINWAIT = _IOW!(int)('t', 87); // set ttywait timeout
                            // 88 unused
                            // 89-91 conflicts: tun and tap
    enum uint TIOCTIMESTAMP = _IOR!(timeval)('t', 89); // enable/get timestamp of last input event
    enum uint TIOCMGDTRWAIT = _IOR!(int)('t', 90); // modem: get wait on close
    enum uint TIOCMSDTRWAIT = _IOW!(int)('t', 91); // modem: set wait on close
                            // 92-93 tun and tap
                            // 94-97 conflicts: tun and tap
    enum uint TIOCDRAIN = _IO('t', 94); // wait till output drained
    enum uint TIOCSIG   = _IOWINT('t', 95); // pty: generate signal
    enum uint TIOCEXT   = _IOW!(int)('t', 96); // pty: external processing
    enum uint TIOCSCTTY = _IO('t', 97);        // become controlling tty
    enum uint TIOCCONS  = _IOW!(int)('t', 98); // become virtual console
    enum uint TIOCGSID  = _IOR!(int)('t', 99); // get session id
                            // 100 unused
    enum uint TIOCSTAT  = _IO('t', 101);       // simulate ^T status message
    enum uint TIOCUCNTL = _IOW!(int)('t', 102); // pty: set/clr usr cntl mode
    enum uint   UIOCCMD(n) = _IO('u', n);       // usr cntl op "n"
    enum uint TIOCSWINSZ = _IOW!(winsize)('t', 103); // set window size
    enum uint TIOCGWINSZ = _IOR!(winsize)('t', 104); // get window size
                            // 105 unused
    enum uint TIOCMGET  = _IOR!(int)('t', 106); // get all modem bits
    enum uint   TIOCM_LE  = 0x01;               // line enable
    enum uint   TIOCM_DTR = 0x02;               // data terminal ready
    enum uint   TIOCM_RTS = 0x04;               // request to send
    enum uint   TIOCM_ST  = 0x08;               // secondary transmit
    enum uint   TIOCM_SR  = 0x10;               // secondary receive
    enum uint   TIOCM_CTS = 0x20;               // clear to send
    enum uint   TIOCM_DCD = 0x40;               // data carrier detect
    enum uint   TIOCM_RI  = 0x80;               // ring indicate
    enum uint   TIOCM_DSR = 0x100;              // data set ready
    enum uint   TIOCM_CD  = TIOCM_DCD;
    enum uint   TIOCM_CAR = TIOCM_DCD;
    enum uint   TIOCM_RNG = TIOCM_RI;
    enum uint TIOCMBIC  = _IOW!(int)('t', 107); // bic modem bits
    enum uint TIOCMBIS  = _IOW!(int)('t', 108); // bis modem bits
    enum uint TIOCMSET  = _IOW!(int)('t', 109); // set all modem bits
    enum uint TIOCSTART = _IO('t', 110);        // start output like ^Q
    enum uint TIOCSTOP  = _IO('t', 111);        // stop output like ^S
    enum uint TIOCPKT   = _IOW!(int)('t', 112); // pty: set/clr packet mode
    enum uint TIOCPKT_DATA       = 0x00;        // data packet
    enum uint TIOCPKT_FLUSHREAD  = 0x01;        // flush packet
    enum uint TIOCPKT_FLUSHWRITE = 0x02;        // flush packet
    enum uint TIOCPKT_STOP       = 0x04;        // stop output
    enum uint TIOCPKT_START      = 0x08;        // start output
    enum uint TIOCPKT_NOSTOP     = 0x10;        // no more ^S, ^Q
    enum uint TIOCPKT_DOSTOP     = 0x20;        // now do ^S, ^Q
    enum uint TIOCPKT_IOCTL      = 0x40;        // state change of pty driver
    enum uint TIOCNOTTY = _IO('t', 113);        // void tty association
    enum uint TIOCSTI   = _IOW!(char)('t', 114); // simulate terminal input
    enum uint TIOCOUTQ  = _IOR!(int)('t', 115); // output queue size
                             // 116-117 compat
    enum uint TIOCSPGRP = _IOW!(int)('t', 118); // set pgrp of tty
    enum uint TIOCGPGRP = _IOR!(int)('t', 119); // get pgrp of tty

    enum uint TIOCCDTR  = _IO('t', 120);       // clear data terminal ready
    enum uint TIOCSDTR  = _IO('t', 121);       // set data terminal ready
    enum uint TIOCCBRK  = _IO('t', 122);       // clear break bit
    enum uint TIOCSBRK  = _IO('t', 123);       // set break bit
                            // 124-127 compat

    enum uint TTYDISC  = 0;       // termios tty line discipline
    enum uint SLIPDISC = 4;       // serial IP discipline
    enum uint PPPDISC  = 5;       // PPP discipline
    enum uint NETGRAPHDISC = 6;   // Netgraph tty node discipline
    enum uint H4DISC   = 7;       // Netgraph Blutooth H4 discipline
}
