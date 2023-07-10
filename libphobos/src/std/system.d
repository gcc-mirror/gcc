// Written in the D programming language.

/**
 * Information about the target operating system, environment, and CPU.
 *
 *  Copyright: Copyright The D Language Foundation 2000 - 2011
 *  License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 *  Authors:   $(HTTP digitalmars.com, Walter Bright) and
               $(HTTP jmdavisprog.com, Jonathan M Davis)
 *  Source:    $(PHOBOSSRC std/system.d)
 */
module std.system;

immutable
{
    /++
        Operating system.

        Note:
            This is for cases where you need a value representing the OS at
            runtime. If you're doing something which should compile differently
            on different OSes, then please use `version (Windows)`,
            `version (linux)`, etc.

        See_Also:
            $(DDSUBLINK spec/version,PredefinedVersions, Predefined Versions)
      +/
    enum OS
    {
        win32 = 1, /// Microsoft 32 bit Windows systems
        win64,     /// Microsoft 64 bit Windows systems
        linux,     /// All Linux Systems, except for Android
        osx,       /// Mac OS X
        iOS,       /// iOS
        tvOS,      /// tvOS
        watchOS,   /// watchOS
        freeBSD,   /// FreeBSD
        netBSD,    /// NetBSD
        openBSD,   /// OpenBSD
        dragonFlyBSD, /// DragonFlyBSD
        solaris,   /// Solaris
        android,   /// Android
        otherPosix, /// Other Posix Systems
        unknown,   /// Unknown
    }

    /// The OS that the program was compiled for.
    version (Win32)        OS os = OS.win32;
    else version (Win64)   OS os = OS.win64;
    else version (Android) OS os = OS.android;
    else version (linux)   OS os = OS.linux;
    else version (OSX)     OS os = OS.osx;
    else version (iOS)     OS os = OS.iOS;
    else version (tvOS)    OS os = OS.tvOS;
    else version (watchOS) OS os = OS.watchOS;
    else version (FreeBSD) OS os = OS.freeBSD;
    else version (NetBSD)  OS os = OS.netBSD;
    else version (OpenBSD) OS os = OS.openBSD;
    else version (DragonFlyBSD) OS os = OS.dragonFlyBSD;
    else version (Posix)   OS os = OS.otherPosix;
    else OS os = OS.unknown;

    /++
        Byte order endianness.

        Note:
            This is intended for cases where you need to deal with endianness at
            runtime. If you're doing something which should compile differently
            depending on whether you're compiling on a big endian or little
            endian machine, then please use `version (BigEndian)` and
            `version (LittleEndian)`.

        See_Also:
            $(DDSUBLINK spec/version,PredefinedVersions, Predefined Versions)
      +/
    enum Endian
    {
        bigEndian,   /// Big endian byte order
        littleEndian /// Little endian byte order
    }

    /// The endianness that the program was compiled for.
    version (LittleEndian) Endian endian = Endian.littleEndian;
    else                  Endian endian = Endian.bigEndian;
    /++
        Instruction Set Architecture.

        Note:
            This is intended for cases where you need a value representing the
            instruction set architecture at runtime. If you're doing something
            which should compile differently depending on instruction set
            architecture, then please use `version (X86_64)`, `version (ARM)`,
            etc.

        See_Also:
            $(DDSUBLINK spec/version,PredefinedVersions, Predefined Versions)
      +/
    enum ISA
    {
        x86,   /// Intel and AMD 32-bit processors
        x86_64, /// Intel and AMD 64-bit processors
        arm, /// The ARM architecture (32-bit) (AArch32 et al)
        aarch64, /// The Advanced RISC Machine architecture (64-bit)
        asmJS, /// The asm.js intermediate programming language
        avr, /// 8-bit Atmel AVR Microcontrollers
        epiphany, /// The Epiphany architecture
        ppc, /// The PowerPC architecture, 32-bit
        ppc64, /// The PowerPC architecture, 64-bit
        ia64, /// The Itanium architecture (64-bit)
        mips32, /// The MIPS architecture, 32-bit
        mips64, /// The MIPS architecture, 64-bit
        msp430, /// The MSP430 architecture
        nvptx, /// The Nvidia Parallel Thread Execution (PTX) architecture, 32-bit
        nvptx64, /// The Nvidia Parallel Thread Execution (PTX) architecture, 64-bit
        riscv32, /// The RISC-V architecture, 32-bit
        riscv64, /// The RISC-V architecture, 64-bit
        sparc, /// The SPARC architecture, 32-bit
        sparc64, /// The SPARC architecture, 64-bit
        s390, /// The System/390 architecture, 32-bit
        systemZ, /// The System Z architecture, 64-bit
        hppa, /// The HP PA-RISC architecture, 32-bit
        hppa64, /// The HP PA-RISC architecture, 64-bit
        sh, /// The SuperH architecture, 32-bit
        webAssembly, /// The WebAssembly virtual ISA (instruction set architecture), 32-bit
        alpha, /// The Alpha architecture
        unknown, /// Unknown
    }

    /// The instruction set architecture that the program was compiled for.
    version (X86) ISA instructionSetArchitecture = ISA.x86;
    else version (X86_64)   ISA instructionSetArchitecture = ISA.x86_64;
    else version (ARM)      ISA instructionSetArchitecture = ISA.arm;
    else version (AArch64)  ISA instructionSetArchitecture = ISA.aarch64;
    else version (AsmJS)    ISA instructionSetArchitecture = ISA.asmJS;
    else version (AVR)      ISA instructionSetArchitecture = ISA.avr;
    else version (Epiphany) ISA instructionSetArchitecture = ISA.epiphany;
    else version (PPC)      ISA instructionSetArchitecture = ISA.ppc;
    else version (PPC64)    ISA instructionSetArchitecture = ISA.ppc64;
    else version (IA64)     ISA instructionSetArchitecture = ISA.ia64;
    else version (MIPS32)   ISA instructionSetArchitecture = ISA.mips32;
    else version (MIPS64)   ISA instructionSetArchitecture = ISA.mips64;
    else version (MSP430)   ISA instructionSetArchitecture = ISA.msp430;
    else version (NVPTX)    ISA instructionSetArchitecture = ISA.nvptx;
    else version (NVPTX64)  ISA instructionSetArchitecture = ISA.nvptx64;
    else version (RISCV32)  ISA instructionSetArchitecture = ISA.riscv32;
    else version (RISCV64)  ISA instructionSetArchitecture = ISA.riscv64;
    else version (SPARC)    ISA instructionSetArchitecture = ISA.sparc;
    else version (SPARC64)  ISA instructionSetArchitecture = ISA.sparc64;
    else version (S390)     ISA instructionSetArchitecture = ISA.s390;
    else version (SystemZ)  ISA instructionSetArchitecture = ISA.systemZ;
    else version (HPPA)     ISA instructionSetArchitecture = ISA.hppa;
    else version (HPPA64)   ISA instructionSetArchitecture = ISA.hppa64;
    else version (SH)       ISA instructionSetArchitecture = ISA.sh;
    else version (WebAssembly) ISA instructionSetArchitecture = ISA.webAssembly;
    else version (Alpha)    ISA instructionSetArchitecture = ISA.alpha;
    else ISA instructionSetArchitecture = ISA.unknown;
}

