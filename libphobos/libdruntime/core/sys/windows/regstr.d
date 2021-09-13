/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_regstr.d)
 */
module core.sys.windows.regstr;
version (Windows):
@system:

// TODO: fix possible conflict with shloj. Sort out NEC_98 issue.

import core.sys.windows.windef;

enum REGSTR_MAX_VALUE_LENGTH = 256;

enum {
    IT_COMPACT = 0,
    IT_TYPICAL,
    IT_PORTABLE,
    IT_CUSTOM // = 3
}

enum DOSOPTGF_DEFCLEAN = 1;

enum DOSOPTF_DEFAULT     = 0x01;
enum DOSOPTF_SUPPORTED   = 0x02;
enum DOSOPTF_ALWAYSUSE   = 0x04;
enum DOSOPTF_USESPMODE   = 0x08;
enum DOSOPTF_PROVIDESUMB = 0x10;
enum DOSOPTF_NEEDSETUP   = 0x20;
enum DOSOPTF_INDOSSTART  = 0x40;
enum DOSOPTF_MULTIPLE    = 0x80;

enum SUF_FIRSTTIME  = 0x0001;
enum SUF_EXPRESS    = 0x0002;
enum SUF_BATCHINF   = 0x0004;
enum SUF_CLEAN      = 0x0008;
enum SUF_INSETUP    = 0x0010;
enum SUF_NETSETUP   = 0x0020;
enum SUF_NETHDBOOT  = 0x0040;
enum SUF_NETRPLBOOT = 0x0080;
enum SUF_SBSCOPYOK  = 0x0100;

enum VPDF_DISABLEPWRMGMT       = 1;
enum VPDF_FORCEAPM10MODE       = 2;
enum VPDF_SKIPINTELSLCHECK     = 4;
enum VPDF_DISABLEPWRSTATUSPOLL = 8;

enum PCMCIA_OPT_HAVE_SOCKET  = 0x01;
enum PCMCIA_OPT_AUTOMEM      = 0x04;
enum PCMCIA_OPT_NO_SOUND     = 0x08;
enum PCMCIA_OPT_NO_AUDIO     = 0x10;
enum PCMCIA_OPT_NO_APMREMOVE = 0x20;

enum PCMCIA_DEF_MEMBEGIN   = 0x0C0000;
enum PCMCIA_DEF_MEMEND     = 0xFFFFFF;
enum PCMCIA_DEF_MEMLEN     = 0x001000;
enum PCMCIA_DEF_MIN_REGION = 0x010000;

enum {
    PCI_OPTIONS_USE_BIOS         = 1,
    PCI_OPTIONS_USE_IRQ_STEERING = 2
}

enum PCI_FLAG_NO_VIDEO_IRQ      = 0x0001;
enum PCI_FLAG_PCMCIA_WANT_IRQ   = 0x0002;
enum PCI_FLAG_DUAL_IDE          = 0x0004;
enum PCI_FLAG_NO_ENUM_AT_ALL    = 0x0008;
enum PCI_FLAG_ENUM_NO_RESOURCE  = 0x0010;
enum PCI_FLAG_NEED_DWORD_ACCESS = 0x0020;
enum PCI_FLAG_SINGLE_FUNCTION   = 0x0040;
enum PCI_FLAG_ALWAYS_ENABLED    = 0x0080;
enum PCI_FLAG_IS_IDE            = 0x0100;
enum PCI_FLAG_IS_VIDEO          = 0x0200;
enum PCI_FLAG_FAIL_START        = 0x0400;

enum size_t REGSTR_VAL_MAX_HCID_LEN = 1024;

enum REGDF_NOTDETIO        = 0x00000001;
enum REGDF_NOTDETMEM       = 0x00000002;
enum REGDF_NOTDETIRQ       = 0x00000004;
enum REGDF_NOTDETDMA       = 0x00000008;
enum REGDF_NOTDETALL       = REGDF_NOTDETIO | REGDF_NOTDETMEM | REGDF_NOTDETIRQ | REGDF_NOTDETDMA;
enum REGDF_NEEDFULLCONFIG  = 0x00000010;
enum REGDF_GENFORCEDCONFIG = 0x00000020;
enum REGDF_NODETCONFIG     = 0x00008000;
enum REGDF_CONFLICTIO      = 0x00010000;
enum REGDF_CONFLICTMEM     = 0x00020000;
enum REGDF_CONFLICTIRQ     = 0x00040000;
enum REGDF_CONFLICTDMA     = 0x00080000;
enum REGDF_CONFLICTALL     = REGDF_CONFLICTIO | REGDF_CONFLICTMEM | REGDF_CONFLICTIRQ | REGDF_CONFLICTDMA;
enum REGDF_MAPIRQ2TO9      = 0x00100000;
enum REGDF_NOTVERIFIED     = 0x80000000;

enum CONFIGFLAG_DISABLED       = 0x0001;
enum CONFIGFLAG_REMOVED        = 0x0002;
enum CONFIGFLAG_MANUAL_INSTALL = 0x0004;
enum CONFIGFLAG_IGNORE_BOOT_LC = 0x0008;
enum CONFIGFLAG_NET_BOOT       = 0x0010;
enum CONFIGFLAG_REINSTALL      = 0x0020;
enum CONFIGFLAG_FAILEDINSTALL  = 0x0040;
enum CONFIGFLAG_CANTSTOPACHILD = 0x0080;
enum CONFIGFLAG_OKREMOVEROM    = 0x0100;
enum CONFIGFLAG_NOREMOVEEXIT   = 0x0200;

enum CSCONFIGFLAG_DISABLED      = 1;
enum CSCONFIGFLAG_DO_NOT_CREATE = 2;
enum CSCONFIGFLAG_DO_NOT_START  = 4;
enum CSCONFIGFLAG_BITS          = 7;

enum DMSTATEFLAG_APPLYTOALL = 1;

enum NUM_RESOURCE_MAP = 256;

enum MF_FLAGS_EVEN_IF_NO_RESOURCE         = 1;
enum MF_FLAGS_NO_CREATE_IF_NO_RESOURCE    = 2;
enum MF_FLAGS_FILL_IN_UNKNOWN_RESOURCE    = 4;
enum MF_FLAGS_CREATE_BUT_NO_SHOW_DISABLED = 8;

enum EISAFLAG_NO_IO_MERGE   = 1;
enum EISAFLAG_SLOT_IO_FIRST = 2;

enum EISA_NO_MAX_FUNCTION   = 0xFF;

enum NUM_EISA_RANGES = 4;

enum APMMENUSUSPEND_DISABLED = 0;
enum APMMENUSUSPEND_ENABLED  = 1;
enum APMMENUSUSPEND_UNDOCKED = 2;
enum APMMENUSUSPEND_NOCHANGE = 128;

//#ifndef NEC_98
const TCHAR[]
    REGSTR_KEY_ISAENUM             = "ISAPnP",
    REGSTR_KEY_EISAENUM            = "EISA",
    REGSTR_VAL_EISA_RANGES         = "EISARanges",
    REGSTR_VAL_EISA_FUNCTIONS      = "EISAFunctions",
    REGSTR_VAL_EISA_FUNCTIONS_MASK = "EISAFunctionsMask",
    REGSTR_VAL_EISA_FLAGS          = "EISAFlags",
    REGSTR_VAL_EISA_SIMULATE_INT15 = "EISASimulateInt15";
// #else
// #define REGSTR_KEY_ISAENUM   TEXT("C98PnP")
// #define REGSTR_KEY_EISAENUM  TEXT("NESA")
// #define  REGSTR_VAL_EISA_RANGES  TEXT("NESARanges")
// #define  REGSTR_VAL_EISA_FUNCTIONS   TEXT("NESAFunctions")
// #define  REGSTR_VAL_EISA_FUNCTIONS_MASK  TEXT("NESAFunctionsMask")
// #define  REGSTR_VAL_EISA_FLAGS   TEXT("NESAFlags")
// #define  REGSTR_VAL_EISA_SIMULATE_INT15  TEXT("NESASimulateInt15")
// #endif

const TCHAR[]
    REGSTR_KEY_CLASS                     = `Class`,
    REGSTR_KEY_CONFIG                    = `Config`,
    REGSTR_KEY_ENUM                      = `Enum`,
    REGSTR_KEY_ROOTENUM                  = `Root`,
    REGSTR_KEY_BIOSENUM                  = `BIOS`,
    REGSTR_KEY_PCMCIAENUM                = `PCMCIA`,
    REGSTR_KEY_PCIENUM                   = `PCI`,
    REGSTR_KEY_LOGCONFIG                 = `LogConfig`,
    REGSTR_KEY_SYSTEMBOARD               = `*PNP0C01`,
    REGSTR_KEY_APM                       = `*PNP0C05`,
    REGSTR_KEY_INIUPDATE                 = `IniUpdate`,
    REG_KEY_INSTDEV                      = `Installed`,
    REGSTR_KEY_DOSOPTCDROM               = `CD-ROM`,
    REGSTR_KEY_DOSOPTMOUSE               = `MOUSE`,
    REGSTR_DEFAULT_INSTANCE              = `0000`,
    REGSTR_PATH_MOTHERBOARD              = REGSTR_KEY_SYSTEMBOARD ~ `\` ~ REGSTR_DEFAULT_INSTANCE,
    REGSTR_PATH_SETUP                    = `Software\Microsoft\Windows\CurrentVersion`,
    REGSTR_PATH_PIFCONVERT               = `Software\Microsoft\Windows\CurrentVersion\PIFConvert`,
    REGSTR_PATH_MSDOSOPTS                = `Software\Microsoft\Windows\CurrentVersion\MS-DOSOptions`,
    REGSTR_PATH_MSDOSEMU                 = `Software\Microsoft\Windows\CurrentVersion\MS-DOS Emulation`,
    REGSTR_PATH_NEWDOSBOX                = `Software\Microsoft\Windows\CurrentVersion\MS-DOS Emulation\AppCompat`,
    REGSTR_PATH_RUNONCE                  = `Software\Microsoft\Windows\CurrentVersion\RunOnce`,
    REGSTR_PATH_RUN                      = `Software\Microsoft\Windows\CurrentVersion\Run`,
    REGSTR_PATH_RUNSERVICESONCE          = `Software\Microsoft\Windows\CurrentVersion\RunServicesOnce`,
    REGSTR_PATH_RUNSERVICES              = `Software\Microsoft\Windows\CurrentVersion\RunServices`,

//#ifndef REGSTR_PATH_EXPLORER /* also in shlobj.h */
    REGSTR_PATH_EXPLORER                 = `Software\Microsoft\Windows\CurrentVersion\Explorer`,
//#endif

    REGSTR_PATH_DETECT                   = `Software\Microsoft\Windows\CurrentVersion\Detect`,
    REGSTR_PATH_APPPATHS                 = `Software\Microsoft\Windows\CurrentVersion\App Paths`,
    REGSTR_PATH_UNINSTALL                = `Software\Microsoft\Windows\CurrentVersion\Uninstall`,
    REGSTR_PATH_REALMODENET              = `Software\Microsoft\Windows\CurrentVersion\Network\Real Mode Net`,
    REGSTR_PATH_NETEQUIV                 = `Software\Microsoft\Windows\CurrentVersion\Network\Equivalent`,
    REGSTR_PATH_CVNETWORK                = `Software\Microsoft\Windows\CurrentVersion\Network`,
    REGSTR_PATH_IDCONFIGDB               = `System\CurrentControlSet\Control\IDConfigDB`,
    REGSTR_PATH_CLASS                    = `System\CurrentControlSet\Services\Class`,
    REGSTR_PATH_DISPLAYSETTINGS          = `Display\Settings`,
    REGSTR_PATH_FONTS                    = `Display\Fonts`,
    REGSTR_PATH_ENUM                     = `Enum`,
    REGSTR_PATH_ROOT                     = `Enum\Root`,
    REGSTR_PATH_SERVICES                 = `System\CurrentControlSet\Services`,
    REGSTR_PATH_VXD                      = `System\CurrentControlSet\Services\VxD`,
    REGSTR_PATH_IOS                      = `System\CurrentControlSet\Services\VxD\IOS`,
    REGSTR_PATH_VMM                      = `System\CurrentControlSet\Services\VxD\VMM`,
    REGSTR_PATH_VPOWERD                  = `System\CurrentControlSet\Services\VxD\VPOWERD`,
    REGSTR_PATH_VNETSUP                  = `System\CurrentControlSet\Services\VxD\VNETSUP`,
    REGSTR_PATH_NWREDIR                  = `System\CurrentControlSet\Services\VxD\NWREDIR`,
    REGSTR_PATH_NCPSERVER                = `System\CurrentControlSet\Services\NcpServer\Parameters`,
    REGSTR_PATH_IOARB                    = `System\CurrentControlSet\Services\Arbitrators\IOArb`,
    REGSTR_PATH_ADDRARB                  = `System\CurrentControlSet\Services\Arbitrators\AddrArb`,
    REGSTR_PATH_DMAARB                   = `System\CurrentControlSet\Services\Arbitrators\DMAArb`,
    REGSTR_PATH_IRQARB                   = `System\CurrentControlSet\Services\Arbitrators\IRQArb`,
    REGSTR_PATH_CODEPAGE                 = `System\CurrentControlSet\Control\Nls\Codepage`,
    REGSTR_PATH_FILESYSTEM               = `System\CurrentControlSet\Control\FileSystem`,
    REGSTR_PATH_FILESYSTEM_NOVOLTRACK    = `System\CurrentControlSet\Control\FileSystem\NoVolTrack`,
    REGSTR_PATH_CDFS                     = `System\CurrentControlSet\Control\FileSystem\CDFS`,
    REGSTR_PATH_WINBOOT                  = `System\CurrentControlSet\Control\WinBoot`,
    REGSTR_PATH_INSTALLEDFILES           = `System\CurrentControlSet\Control\InstalledFiles`,
    REGSTR_PATH_VMM32FILES               = `System\CurrentControlSet\Control\VMM32Files`,

    REGSTR_VAL_BITSPERPIXEL              = `BitsPerPixel`,
    REGSTR_VAL_RESOLUTION                = `Resolution`,
    REGSTR_VAL_DPILOGICALX               = `DPILogicalX`,
    REGSTR_VAL_DPILOGICALY               = `DPILogicalY`,
    REGSTR_VAL_DPIPHYSICALX              = `DPIPhysicalX`,
    REGSTR_VAL_DPIPHYSICALY              = `DPIPhysicalY`,
    REGSTR_VAL_REFRESHRATE               = `RefreshRate`,
    REGSTR_VAL_DISPLAYFLAGS              = `DisplayFlags`,
    REGSTR_PATH_CONTROLPANEL             = `Control Panel`,
    REGSTR_PATH_CONTROLSFOLDER           = `Software\Microsoft\Windows\CurrentVersion\Controls Folder`,
    REGSTR_VAL_DOSCP                     = `OEMCP`,
    REGSTR_VAL_WINCP                     = `ACP`,
    REGSTR_PATH_DYNA_ENUM                = `Config Manager\Enum`,
    REGSTR_VAL_HARDWARE_KEY              = `HardWareKey`,
    REGSTR_VAL_ALLOCATION                = `Allocation`,
    REGSTR_VAL_PROBLEM                   = `Problem`,
    REGSTR_VAL_STATUS                    = `Status`,
    REGSTR_VAL_DONTUSEMEM                = `DontAllocLastMem`,
    REGSTR_VAL_SYSTEMROOT                = `SystemRoot`,
    REGSTR_VAL_BOOTCOUNT                 = `BootCount`,
    REGSTR_VAL_REALNETSTART              = `RealNetStart`,
    REGSTR_VAL_MEDIA                     = `MediaPath`,
    REGSTR_VAL_CONFIG                    = `ConfigPath`,
    REGSTR_VAL_DEVICEPATH                = `DevicePath`,
    REGSTR_VAL_SRCPATH                   = `SourcePath`,
    REGSTR_VAL_OLDWINDIR                 = `OldWinDir`,
    REGSTR_VAL_SETUPFLAGS                = `SetupFlags`,
    REGSTR_VAL_REGOWNER                  = `RegisteredOwner`,
    REGSTR_VAL_REGORGANIZATION           = `RegisteredOrganization`,
    REGSTR_VAL_LICENSINGINFO             = `LicensingInfo`,
    REGSTR_VAL_OLDMSDOSVER               = `OldMSDOSVer`,
    REGSTR_VAL_FIRSTINSTALLDATETIME      = `FirstInstallDateTime`,
    REGSTR_VAL_INSTALLTYPE               = `InstallType`,
    REGSTR_VAL_WRAPPER                   = `Wrapper`,

    REGSTR_KEY_SETUP                     = `\Setup`,
    REGSTR_VAL_BOOTDIR                   = `BootDir`,
    REGSTR_VAL_WINBOOTDIR                = `WinbootDir`,
    REGSTR_VAL_WINDIR                    = `WinDir`,
    REGSTR_VAL_APPINSTPATH               = `AppInstallPath`,
    REGSTR_PATH_EBD                      = REGSTR_PATH_SETUP ~ REGSTR_KEY_SETUP ~ `\EBD`,
    REGSTR_KEY_EBDFILESLOCAL             = `EBDFilesLocale`,
    REGSTR_KEY_EBDFILESKEYBOARD          = `EBDFilesKeyboard`,
    REGSTR_KEY_EBDAUTOEXECBATLOCAL       = `EBDAutoexecBatLocale`,
    REGSTR_KEY_EBDAUTOEXECBATKEYBOARD    = `EBDAutoexecBatKeyboard`,
    REGSTR_KEY_EBDCONFIGSYSLOCAL         = `EBDConfigSysLocale`,
    REGSTR_KEY_EBDCONFIGSYSKEYBOARD      = `EBDConfigSysKeyboard`,
    REGSTR_VAL_MSDOSMODE                 = `MSDOSMode`,
    REGSTR_VAL_MSDOSMODEDISCARD          = `Discard`,
    REGSTR_VAL_DOSOPTGLOBALFLAGS         = `GlobalFlags`,
    REGSTR_VAL_DOSOPTFLAGS               = `Flags`,
    REGSTR_VAL_OPTORDER                  = `Order`,
    REGSTR_VAL_CONFIGSYS                 = `Config.Sys`,
    REGSTR_VAL_AUTOEXEC                  = `Autoexec.Bat`,
    REGSTR_VAL_STDDOSOPTION              = `StdOption`,
    REGSTR_VAL_DOSOPTTIP                 = `TipText`,

    REGSTR_VAL_DOSPAGER                  = `DOSPager`,
    REGSTR_VAL_VXDGROUPS                 = `VXDGroups`,
    REGSTR_VAL_VPOWERDFLAGS              = `Flags`,

    REGSTR_VAL_WORKGROUP                 = `Workgroup`,
    REGSTR_VAL_DIRECTHOST                = `DirectHost`,
    REGSTR_VAL_FILESHARING               = `FileSharing`,
    REGSTR_VAL_PRINTSHARING              = `PrintSharing`,
    REGSTR_VAL_FIRSTNETDRIVE             = `FirstNetworkDrive`,
    REGSTR_VAL_MAXCONNECTIONS            = `MaxConnections`,
    REGSTR_VAL_APISUPPORT                = `APISupport`,
    REGSTR_VAL_MAXRETRY                  = `MaxRetry`,
    REGSTR_VAL_MINRETRY                  = `MinRetry`,
    REGSTR_VAL_SUPPORTLFN                = `SupportLFN`,
    REGSTR_VAL_SUPPORTBURST              = `SupportBurst`,
    REGSTR_VAL_SUPPORTTUNNELLING         = `SupportTunnelling`,
    REGSTR_VAL_FULLTRACE                 = `FullTrace`,
    REGSTR_VAL_READCACHING               = `ReadCaching`,
    REGSTR_VAL_SHOWDOTS                  = `ShowDots`,
    REGSTR_VAL_GAPTIME                   = `GapTime`,
    REGSTR_VAL_SEARCHMODE                = `SearchMode`,
    REGSTR_VAL_SHELLVERSION              = `ShellVersion`,
    REGSTR_VAL_MAXLIP                    = `MaxLIP`,
    REGSTR_VAL_PRESERVECASE              = `PreserveCase`,
    REGSTR_VAL_OPTIMIZESFN               = `OptimizeSFN`,
    REGSTR_VAL_NCP_BROWSEMASTER          = `BrowseMaster`,
    REGSTR_VAL_NCP_USEPEERBROWSING       = `Use_PeerBrowsing`,
    REGSTR_VAL_NCP_USESAP                = `Use_Sap`,
    REGSTR_VAL_WIN31FILESYSTEM           = `Win31FileSystem`,
    REGSTR_VAL_PRESERVELONGNAMES         = `PreserveLongNames`,
    REGSTR_VAL_DRIVEWRITEBEHIND          = `DriveWriteBehind`,
    REGSTR_VAL_ASYNCFILECOMMIT           = `AsyncFileCommit`,
    REGSTR_VAL_PATHCACHECOUNT            = `PathCache`,
    REGSTR_VAL_NAMECACHECOUNT            = `NameCache`,
    REGSTR_VAL_CONTIGFILEALLOC           = `ContigFileAllocSize`,
    REGSTR_VAL_VOLIDLETIMEOUT            = `VolumeIdleTimeout`,
    REGSTR_VAL_BUFFIDLETIMEOUT           = `BufferIdleTimeout`,
    REGSTR_VAL_BUFFAGETIMEOUT            = `BufferAgeTimeout`,
    REGSTR_VAL_NAMENUMERICTAIL           = `NameNumericTail`,
    REGSTR_VAL_READAHEADTHRESHOLD        = `ReadAheadThreshold`,
    REGSTR_VAL_DOUBLEBUFFER              = `DoubleBuffer`,
    REGSTR_VAL_SOFTCOMPATMODE            = `SoftCompatMode`,
    REGSTR_VAL_DRIVESPINDOWN             = `DriveSpinDown`,
    REGSTR_VAL_FORCEPMIO                 = `ForcePMIO`,
    REGSTR_VAL_FORCERMIO                 = `ForceRMIO`,
    REGSTR_VAL_LASTBOOTPMDRVS            = `LastBootPMDrvs`,
    REGSTR_VAL_VIRTUALHDIRQ              = `VirtualHDIRQ`,
    REGSTR_VAL_SRVNAMECACHECOUNT         = `ServerNameCacheMax`,
    REGSTR_VAL_SRVNAMECACHE              = `ServerNameCache`,
    REGSTR_VAL_SRVNAMECACHENETPROV       = `ServerNameCacheNumNets`,
    REGSTR_VAL_AUTOMOUNT                 = `AutoMountDrives`,
    REGSTR_VAL_COMPRESSIONMETHOD         = `CompressionAlgorithm`,
    REGSTR_VAL_COMPRESSIONTHRESHOLD      = `CompressionThreshold`,
    REGSTR_VAL_CDCACHESIZE               = `CacheSize`,
    REGSTR_VAL_CDPREFETCH                = `Prefetch`,
    REGSTR_VAL_CDPREFETCHTAIL            = `PrefetchTail`,
    REGSTR_VAL_CDRAWCACHE                = `RawCache`,
    REGSTR_VAL_CDEXTERRORS               = `ExtendedErrors`,
    REGSTR_VAL_CDSVDSENSE                = `SVDSense`,
    REGSTR_VAL_CDSHOWVERSIONS            = `ShowVersions`,
    REGSTR_VAL_CDCOMPATNAMES             = `MSCDEXCompatNames`,
    REGSTR_VAL_CDNOREADAHEAD             = `NoReadAhead`,
    REGSTR_VAL_SCSI                      = `SCSI\`,
    REGSTR_VAL_ESDI                      = `ESDI\`,
    REGSTR_VAL_FLOP                      = `FLOP\`,
    REGSTR_VAL_DISK                      = `GenDisk`,
    REGSTR_VAL_CDROM                     = `GenCD`,
    REGSTR_VAL_TAPE                      = `TAPE`,
    REGSTR_VAL_SCANNER                   = `SCANNER`,
    REGSTR_VAL_FLOPPY                    = `FLOPPY`,
    REGSTR_VAL_SCSITID                   = `SCSITargetID`,
    REGSTR_VAL_SCSILUN                   = `SCSILUN`,
    REGSTR_VAL_REVLEVEL                  = `RevisionLevel`,
    REGSTR_VAL_PRODUCTID                 = `ProductId`,
    REGSTR_VAL_PRODUCTTYPE               = `ProductType`,
    REGSTR_VAL_DEVTYPE                   = `DeviceType`,
    REGSTR_VAL_REMOVABLE                 = `Removable`,
    REGSTR_VAL_CURDRVLET                 = `CurrentDriveLetterAssignment`,
    REGSTR_VAL_USRDRVLET                 = `UserDriveLetterAssignment`,
    REGSTR_VAL_SYNCDATAXFER              = `SyncDataXfer`,
    REGSTR_VAL_AUTOINSNOTE               = `AutoInsertNotification`,
    REGSTR_VAL_DISCONNECT                = `Disconnect`,
    REGSTR_VAL_INT13                     = `Int13`,
    REGSTR_VAL_PMODE_INT13               = `PModeInt13`,
    REGSTR_VAL_USERSETTINGS              = `AdapterSettings`,
    REGSTR_VAL_NOIDE                     = `NoIDE`,
    REGSTR_VAL_DISKCLASSNAME             = `DiskDrive`,
    REGSTR_VAL_CDROMCLASSNAME            = `CDROM`,
    REGSTR_VAL_FORCELOAD                 = `ForceLoadPD`,
    REGSTR_VAL_FORCEFIFO                 = `ForceFIFO`,
    REGSTR_VAL_FORCECL                   = `ForceChangeLine`,
    REGSTR_VAL_NOUSECLASS                = `NoUseClass`,
    REGSTR_VAL_NOINSTALLCLASS            = `NoInstallClass`,
    REGSTR_VAL_NODISPLAYCLASS            = `NoDisplayClass`,
    REGSTR_VAL_SILENTINSTALL             = `SilentInstall`,
    REGSTR_KEY_PCMCIA_CLASS              = `PCMCIA`,
    REGSTR_KEY_SCSI_CLASS                = `SCSIAdapter`,
    REGSTR_KEY_PORTS_CLASS               = `ports`,
    REGSTR_KEY_MEDIA_CLASS               = `MEDIA`,
    REGSTR_KEY_DISPLAY_CLASS             = `Display`,
    REGSTR_KEY_KEYBOARD_CLASS            = `Keyboard`,
    REGSTR_KEY_MOUSE_CLASS               = `Mouse`,
    REGSTR_KEY_MONITOR_CLASS             = `Monitor`,
    REGSTR_VAL_PCMCIA_OPT                = `Options`,
    REGSTR_VAL_PCMCIA_MEM                = `Memory`,
    REGSTR_VAL_PCMCIA_ALLOC              = `AllocMemWin`,
    REGSTR_VAL_PCMCIA_ATAD               = `ATADelay`,
    REGSTR_VAL_PCMCIA_SIZ                = `MinRegionSize`,
    REGSTR_VAL_P1284MDL                  = `Model`,
    REGSTR_VAL_P1284MFG                  = `Manufacturer`,
    REGSTR_VAL_ISAPNP                    = `ISAPNP`,
    REGSTR_VAL_ISAPNP_RDP_OVERRIDE       = `RDPOverRide`,
    REGSTR_VAL_PCI                       = `PCI`,
    REGSTR_PCI_OPTIONS                   = `Options`,
    REGSTR_PCI_DUAL_IDE                  = `PCIDualIDE`,

    REGSTR_KEY_CRASHES                   = `Crashes`,
    REGSTR_KEY_DANGERS                   = `Dangers`,
    REGSTR_KEY_DETMODVARS                = `DetModVars`,
    REGSTR_KEY_NDISINFO                  = `NDISInfo`,
    REGSTR_VAL_PROTINIPATH               = `ProtIniPath`,
    REGSTR_VAL_RESOURCES                 = `Resources`,
    REGSTR_VAL_CRASHFUNCS                = `CrashFuncs`,
    REGSTR_VAL_CLASS                     = `Class`,
    REGSTR_VAL_DEVDESC                   = `DeviceDesc`,
    REGSTR_VAL_BOOTCONFIG                = `BootConfig`,
    REGSTR_VAL_DETFUNC                   = `DetFunc`,
    REGSTR_VAL_DETFLAGS                  = `DetFlags`,
    REGSTR_VAL_COMPATIBLEIDS             = `CompatibleIDs`,
    REGSTR_VAL_DETCONFIG                 = `DetConfig`,
    REGSTR_VAL_VERIFYKEY                 = `VerifyKey`,
    REGSTR_VAL_COMINFO                   = `ComInfo`,
    REGSTR_VAL_INFNAME                   = `InfName`,
    REGSTR_VAL_CARDSPECIFIC              = `CardSpecific`,
    REGSTR_VAL_NETOSTYPE                 = `NetOSType`,
    REGSTR_DATA_NETOS_NDIS               = `NDIS`,
    REGSTR_DATA_NETOS_ODI                = `ODI`,
    REGSTR_DATA_NETOS_IPX                = `IPX`,
    REGSTR_VAL_MFG                       = `Mfg`,
    REGSTR_VAL_SCAN_ONLY_FIRST           = `ScanOnlyFirstDrive`,
    REGSTR_VAL_SHARE_IRQ                 = `ForceIRQSharing`,
    REGSTR_VAL_NONSTANDARD_ATAPI         = `NonStandardATAPI`,
    REGSTR_VAL_IDE_FORCE_SERIALIZE       = `ForceSerialization`,
    REGSTR_VAL_HWREV                     = `HWRevision`,
    REGSTR_VAL_ENABLEINTS                = `EnableInts`,

    REGSTR_VAL_APMBIOSVER                = `APMBiosVer`,
    REGSTR_VAL_APMFLAGS                  = `APMFlags`,
    REGSTR_VAL_SLSUPPORT                 = `SLSupport`,
    REGSTR_VAL_MACHINETYPE               = `MachineType`,
    REGSTR_VAL_SETUPMACHINETYPE          = `SetupMachineType`,
    REGSTR_MACHTYPE_UNKNOWN              = `Unknown`,
    REGSTR_MACHTYPE_IBMPC                = `IBM PC`,
    REGSTR_MACHTYPE_IBMPCJR              = `IBM PCjr`,
    REGSTR_MACHTYPE_IBMPCCONV            = `IBM PC Convertible`,
    REGSTR_MACHTYPE_IBMPCXT              = `IBM PC/XT`,
    REGSTR_MACHTYPE_IBMPCXT_286          = `IBM PC/XT 286`,
    REGSTR_MACHTYPE_IBMPCAT              = `IBM PC/AT`,
    REGSTR_MACHTYPE_IBMPS2_25            = `IBM PS/2-25`,
    REGSTR_MACHTYPE_IBMPS2_30_286        = `IBM PS/2-30 286`,
    REGSTR_MACHTYPE_IBMPS2_30            = `IBM PS/2-30`,
    REGSTR_MACHTYPE_IBMPS2_50            = `IBM PS/2-50`,
    REGSTR_MACHTYPE_IBMPS2_50Z           = `IBM PS/2-50Z`,
    REGSTR_MACHTYPE_IBMPS2_55SX          = `IBM PS/2-55SX`,
    REGSTR_MACHTYPE_IBMPS2_60            = `IBM PS/2-60`,
    REGSTR_MACHTYPE_IBMPS2_65SX          = `IBM PS/2-65SX`,
    REGSTR_MACHTYPE_IBMPS2_70            = `IBM PS/2-70`,
    REGSTR_MACHTYPE_IBMPS2_P70           = `IBM PS/2-P70`,
    REGSTR_MACHTYPE_IBMPS2_70_80         = `IBM PS/2-70/80`,
    REGSTR_MACHTYPE_IBMPS2_80            = `IBM PS/2-80`,
    REGSTR_MACHTYPE_IBMPS2_90            = `IBM PS/2-90`,
    REGSTR_MACHTYPE_IBMPS1               = `IBM PS/1`,
    REGSTR_MACHTYPE_PHOENIX_PCAT         = `Phoenix PC/AT Compatible`,
    REGSTR_MACHTYPE_HP_VECTRA            = `HP Vectra`,
    REGSTR_MACHTYPE_ATT_PC               = `AT&T PC`,
    REGSTR_MACHTYPE_ZENITH_PC            = `Zenith PC`,
    REGSTR_VAL_APMMENUSUSPEND            = `APMMenuSuspend`,

    REGSTR_VAL_BUSTYPE                   = `BusType`,
    REGSTR_VAL_CPU                       = `CPU`,
    REGSTR_VAL_NDP                       = `NDP`,
    REGSTR_VAL_PNPBIOSVER                = `PnPBIOSVer`,
    REGSTR_VAL_PNPSTRUCOFFSET            = `PnPStrucOffset`,
    REGSTR_VAL_PCIBIOSVER                = `PCIBIOSVer`,
    REGSTR_VAL_HWMECHANISM               = `HWMechanism`,
    REGSTR_VAL_LASTPCIBUSNUM             = `LastPCIBusNum`,
    REGSTR_VAL_CONVMEM                   = `ConvMem`,
    REGSTR_VAL_EXTMEM                    = `ExtMem`,
    REGSTR_VAL_COMPUTERNAME              = `ComputerName`,
    REGSTR_VAL_BIOSNAME                  = `BIOSName`,
    REGSTR_VAL_BIOSVERSION               = `BIOSVersion`,
    REGSTR_VAL_BIOSDATE                  = `BIOSDate`,
    REGSTR_VAL_MODEL                     = `Model`,
    REGSTR_VAL_SUBMODEL                  = `Submodel`,
    REGSTR_VAL_REVISION                  = `Revision`,
    REGSTR_VAL_FIFODEPTH                 = `FIFODepth`,
    REGSTR_VAL_RDINTTHRESHOLD            = `RDIntThreshold`,
    REGSTR_VAL_WRINTTHRESHOLD            = `WRIntThreshold`,
    REGSTR_VAL_PRIORITY                  = `Priority`,
    REGSTR_VAL_DRIVER                    = `Driver`,
    REGSTR_VAL_FUNCDESC                  = `FunctionDesc`,
    REGSTR_VAL_FORCEDCONFIG              = `ForcedConfig`,
    REGSTR_VAL_CONFIGFLAGS               = `ConfigFlags`,
    REGSTR_VAL_CSCONFIGFLAGS             = `CSConfigFlags`,

    REGSTR_VAL_ROOT_DEVNODE              = `HTREE\ROOT\0`,
    REGSTR_VAL_RESERVED_DEVNODE          = `HTREE\RESERVED\0`,
    REGSTR_PATH_READDATAPORT             = REGSTR_KEY_ISAENUM ~ `\ReadDataPort\0`,
    REGSTR_PATH_MULTI_FUNCTION           = `MF`,
    REGSTR_VAL_RESOURCE_MAP              = `ResourceMap`,
    REGSTR_PATH_CHILD_PREFIX             = `Child`,
    REGSTR_VAL_MF_FLAGS                  = `MFFlags`,
    REGSTR_VAL_DRVDESC                   = `DriverDesc`,
    REGSTR_VAL_DEVLOADER                 = `DevLoader`,
    REGSTR_VAL_STATICVXD                 = `StaticVxD`,
    REGSTR_VAL_PROPERTIES                = `Properties`,
    REGSTR_VAL_MANUFACTURER              = `Manufacturer`,
    REGSTR_VAL_EXISTS                    = `Exists`,
    REGSTR_VAL_CMENUMFLAGS               = `CMEnumFlags`,
    REGSTR_VAL_CMDRIVFLAGS               = `CMDrivFlags`,
    REGSTR_VAL_ENUMERATOR                = `Enumerator`,
    REGSTR_VAL_DEVICEDRIVER              = `DeviceDriver`,
    REGSTR_VAL_PORTNAME                  = `PortName`,
    REGSTR_VAL_INFPATH                   = `InfPath`,
    REGSTR_VAL_INFSECTION                = `InfSection`,
    REGSTR_VAL_POLLING                   = `Polling`,
    REGSTR_VAL_DONTLOADIFCONFLICT        = `DontLoadIfConflict`,
    REGSTR_VAL_PORTSUBCLASS              = `PortSubClass`,
    REGSTR_VAL_NETCLEAN                  = `NetClean`,
    REGSTR_VAL_IDE_NO_SERIALIZE          = `IDENoSerialize`,
    REGSTR_VAL_NOCMOSORFDPT              = `NoCMOSorFDPT`,
    REGSTR_VAL_COMVERIFYBASE             = `COMVerifyBase`,
    REGSTR_KEY_OVERRIDE                  = `Override`,
    REGSTR_VAL_CONFIGMG                  = `CONFIGMG`,
    REGSTR_VAL_SYSDM                     = `SysDM`,
    REGSTR_VAL_SYSDMFUNC                 = `SysDMFunc`,
    REGSTR_VAL_PRIVATE                   = `Private`,
    REGSTR_VAL_PRIVATEFUNC               = `PrivateFunc`,
    REGSTR_VAL_DETECT                    = `Detect`,
    REGSTR_VAL_DETECTFUNC                = `DetectFunc`,
    REGSTR_VAL_ASKFORCONFIG              = `AskForConfig`,
    REGSTR_VAL_ASKFORCONFIGFUNC          = `AskForConfigFunc`,
    REGSTR_VAL_WAITFORUNDOCK             = `WaitForUndock`,
    REGSTR_VAL_WAITFORUNDOCKFUNC         = `WaitForUndockFunc`,
    REGSTR_VAL_REMOVEROMOKAY             = `RemoveRomOkay`,
    REGSTR_VAL_REMOVEROMOKAYFUNC         = `RemoveRomOkayFunc`,
    REGSTR_VAL_CURCONFIG                 = `CurrentConfig`,
    REGSTR_VAL_FRIENDLYNAME              = `FriendlyName`,
    REGSTR_VAL_CURRENTCONFIG             = `CurrentConfig`,
    REGSTR_VAL_MAP                       = `Map`,
    REGSTR_VAL_ID                        = `CurrentID`,
    REGSTR_VAL_DOCKED                    = `CurrentDockedState`,
    REGSTR_VAL_CHECKSUM                  = `CurrentChecksum`,
    REGSTR_VAL_HWDETECT                  = `HardwareDetect`,
    REGSTR_VAL_INHIBITRESULTS            = `InhibitResults`,
    REGSTR_VAL_PROFILEFLAGS              = `ProfileFlags`,
    REGSTR_KEY_PCMCIA                    = `PCMCIA\`,
    REGSTR_KEY_PCUNKNOWN                 = `UNKNOWN_MANUFACTURER`,
    REGSTR_VAL_PCSSDRIVER                = `Driver`,
    REGSTR_KEY_PCMTD                     = `MTD-`,
    REGSTR_VAL_PCMTDRIVER                = `MTD`,
    REGSTR_VAL_HARDWAREID                = `HardwareID`,
    REGSTR_VAL_INSTALLER                 = `Installer`,
    REGSTR_VAL_INSICON                   = `Icon`,
    REGSTR_VAL_ENUMPROPPAGES             = `EnumPropPages`,
    REGSTR_VAL_BASICPROPERTIES           = `BasicProperties`,
    REGSTR_VAL_PRIVATEPROBLEM            = `PrivateProblem`,
    REGSTR_KEY_CURRENT                   = `Current`,
    REGSTR_KEY_DEFAULT                   = `Default`,
    REGSTR_KEY_MODES                     = `Modes`,
    REGSTR_VAL_MODE                      = `Mode`,
    REGSTR_VAL_BPP                       = `BPP`,
    REGSTR_VAL_HRES                      = `HRes`,
    REGSTR_VAL_VRES                      = `VRes`,
    REGSTR_VAL_FONTSIZE                  = `FontSize`,
    REGSTR_VAL_DRV                       = `drv`,
    REGSTR_VAL_GRB                       = `grb`,
    REGSTR_VAL_VDD                       = `vdd`,
    REGSTR_VAL_VER                       = `Ver`,
    REGSTR_VAL_MAXRES                    = `MaxResolution`,
    REGSTR_VAL_DPMS                      = `DPMS`,
    REGSTR_VAL_RESUMERESET               = `ResumeReset`,
    REGSTR_VAL_DESCRIPTION               = `Description`,
    REGSTR_KEY_SYSTEM                    = `System`,
    REGSTR_KEY_USER                      = `User`,
    REGSTR_VAL_DPI                       = `dpi`,
    REGSTR_VAL_PCICOPTIONS               = `PCICOptions`,

    REGSTR_VAL_PCICIRQMAP                = `PCICIRQMap`,
    REGSTR_PATH_APPEARANCE               = `Control Panel\Appearance`,
    REGSTR_PATH_LOOKSCHEMES              = `Control Panel\Appearance\Schemes`,
    REGSTR_VAL_CUSTOMCOLORS              = `CustomColors`,
    REGSTR_PATH_SCREENSAVE               = `Control Panel\Desktop`,
    REGSTR_VALUE_USESCRPASSWORD          = `ScreenSaveUsePassword`,
    REGSTR_VALUE_SCRPASSWORD             = `ScreenSave_Data`,
    REGSTR_VALUE_LOWPOWERTIMEOUT         = `ScreenSaveLowPowerTimeout`,
    REGSTR_VALUE_POWEROFFTIMEOUT         = `ScreenSavePowerOffTimeout`,
    REGSTR_VALUE_LOWPOWERACTIVE          = `ScreenSaveLowPowerActive`,
    REGSTR_VALUE_POWEROFFACTIVE          = `ScreenSavePowerOffActive`,
    REGSTR_PATH_WINDOWSAPPLETS           = `Software\Microsoft\Windows\CurrentVersion\Applets`,
    REGSTR_PATH_SYSTRAY                  = `Software\Microsoft\Windows\CurrentVersion\Applets\SysTray`,
    REGSTR_VAL_SYSTRAYSVCS               = `Services`,
    REGSTR_VAL_SYSTRAYBATFLAGS           = `PowerFlags`,
    REGSTR_VAL_SYSTRAYPCCARDFLAGS        = `PCMCIAFlags`,
    REGSTR_PATH_NETWORK_USERSETTINGS     = `Network`,
    REGSTR_KEY_NETWORK_PERSISTENT        = `\Persistent`,
    REGSTR_KEY_NETWORK_RECENT            = `\Recent`,
    REGSTR_VAL_REMOTE_PATH               = `RemotePath`,
    REGSTR_VAL_USER_NAME                 = `UserName`,
    REGSTR_VAL_PROVIDER_NAME             = `ProviderName`,
    REGSTR_VAL_CONNECTION_TYPE           = `ConnectionType`,
    REGSTR_VAL_UPGRADE                   = `Upgrade`,
    REGSTR_KEY_LOGON                     = `\Logon`,
    REGSTR_VAL_MUSTBEVALIDATED           = `MustBeValidated`,
    REGSTR_VAL_RUNLOGINSCRIPT            = `ProcessLoginScript`,
    REGSTR_KEY_NETWORKPROVIDER           = `\NetworkProvider`,
    REGSTR_PATH_NW32NETPROVIDER          =REGSTR_PATH_SERVICES ~ `\NWNP32` ~ REGSTR_KEY_NETWORKPROVIDER,
    REGSTR_PATH_MS32NETPROVIDER          =REGSTR_PATH_SERVICES ~ `\MSNP32` ~ REGSTR_KEY_NETWORKPROVIDER,
    REGSTR_VAL_AUTHENT_AGENT             = `AuthenticatingAgent`,
    REGSTR_VAL_PREFREDIR                 = `PreferredRedir`,
    REGSTR_VAL_AUTOSTART                 = `AutoStart`,
    REGSTR_VAL_AUTOLOGON                 = `AutoLogon`,
    REGSTR_VAL_NETCARD                   = `Netcard`,
    REGSTR_VAL_TRANSPORT                 = `Transport`,
    REGSTR_VAL_DYNAMIC                   = `Dynamic`,
    REGSTR_VAL_TRANSITION                = `Transition`,
    REGSTR_VAL_STATICDRIVE               = `StaticDrive`,
    REGSTR_VAL_LOADHI                    = `LoadHi`,
    REGSTR_VAL_LOADRMDRIVERS             = `LoadRMDrivers`,
    REGSTR_VAL_SETUPN                    = `SetupN`,
    REGSTR_VAL_SETUPNPATH                = `SetupNPath`,
    REGSTR_VAL_WRKGRP_FORCEMAPPING       = `WrkgrpForceMapping`,
    REGSTR_VAL_WRKGRP_REQUIRED           = `WrkgrpRequired`,
    REGSTR_PATH_CURRENT_CONTROL_SET      = `System\CurrentControlSet\Control`,
    REGSTR_VAL_CURRENT_USER              = `Current User`,
    REGSTR_PATH_PWDPROVIDER              = `System\CurrentControlSet\Control\PwdProvider`,
    REGSTR_VAL_PWDPROVIDER_PATH          = `ProviderPath`,
    REGSTR_VAL_PWDPROVIDER_DESC          = `Description`,
    REGSTR_VAL_PWDPROVIDER_CHANGEPWD     = `ChangePassword`,
    REGSTR_VAL_PWDPROVIDER_CHANGEPWDHWND = `ChangePasswordHwnd`,
    REGSTR_VAL_PWDPROVIDER_GETPWDSTATUS  = `GetPasswordStatus`,
    REGSTR_VAL_PWDPROVIDER_ISNP          = `NetworkProvider`,
    REGSTR_VAL_PWDPROVIDER_CHANGEORDER   = `ChangeOrder`,
    REGSTR_PATH_POLICIES                 = `Software\Microsoft\Windows\CurrentVersion\Policies`,
    REGSTR_PATH_UPDATE                   = `System\CurrentControlSet\Control\Update`,
    REGSTR_VALUE_ENABLE                  = `Enable`,
    REGSTR_VALUE_VERBOSE                 = `Verbose`,
    REGSTR_VALUE_NETPATH                 = `NetworkPath`,
    REGSTR_VALUE_DEFAULTLOC              = `UseDefaultNetLocation`,
    REGSTR_KEY_NETWORK                   = `Network`,
// [Redefined] REGSTR_KEY_SYSTEM         = `System`)
    REGSTR_KEY_PRINTERS                  = `Printers`,
    REGSTR_KEY_WINOLDAPP                 = `WinOldApp`,
    REGSTR_VAL_NOFILESHARING             = `NoFileSharing`,
    REGSTR_VAL_NOPRINTSHARING            = `NoPrintSharing`,
    REGSTR_VAL_NOFILESHARINGCTRL         = `NoFileSharingControl`,
    REGSTR_VAL_NOPRINTSHARINGCTRL        = `NoPrintSharingControl`,
    REGSTR_VAL_HIDESHAREPWDS             = `HideSharePwds`,
    REGSTR_VAL_DISABLEPWDCACHING         = `DisablePwdCaching`,
    REGSTR_VAL_ALPHANUMPWDS              = `AlphanumPwds`,
    REGSTR_VAL_NETSETUP_DISABLE          = `NoNetSetup`,
    REGSTR_VAL_NETSETUP_NOCONFIGPAGE     = `NoNetSetupConfigPage`,
    REGSTR_VAL_NETSETUP_NOIDPAGE         = `NoNetSetupIDPage`,
    REGSTR_VAL_NETSETUP_NOSECURITYPAGE   = `NoNetSetupSecurityPage`,
    REGSTR_VAL_SYSTEMCPL_NOVIRTMEMPAGE   = `NoVirtMemPage`,
    REGSTR_VAL_SYSTEMCPL_NODEVMGRPAGE    = `NoDevMgrPage`,
    REGSTR_VAL_SYSTEMCPL_NOCONFIGPAGE    = `NoConfigPage`,
    REGSTR_VAL_SYSTEMCPL_NOFILESYSPAGE   = `NoFileSysPage`,
    REGSTR_VAL_DISPCPL_NODISPCPL         = `NoDispCPL`,
    REGSTR_VAL_DISPCPL_NOBACKGROUNDPAGE  = `NoDispBackgroundPage`,
    REGSTR_VAL_DISPCPL_NOSCRSAVPAGE      = `NoDispScrSavPage`,
    REGSTR_VAL_DISPCPL_NOAPPEARANCEPAGE  = `NoDispAppearancePage`,
    REGSTR_VAL_DISPCPL_NOSETTINGSPAGE    = `NoDispSettingsPage`,
    REGSTR_VAL_SECCPL_NOSECCPL           = `NoSecCPL`,
    REGSTR_VAL_SECCPL_NOPWDPAGE          = `NoPwdPage`,
    REGSTR_VAL_SECCPL_NOADMINPAGE        = `NoAdminPage`,
    REGSTR_VAL_SECCPL_NOPROFILEPAGE      = `NoProfilePage`,
    REGSTR_VAL_PRINTERS_HIDETABS         = `NoPrinterTabs`,
    REGSTR_VAL_PRINTERS_NODELETE         = `NoDeletePrinter`,
    REGSTR_VAL_PRINTERS_NOADD            = `NoAddPrinter`,
    REGSTR_VAL_WINOLDAPP_DISABLED        = `Disabled`,
    REGSTR_VAL_WINOLDAPP_NOREALMODE      = `NoRealMode`,
    REGSTR_VAL_NOENTIRENETWORK           = `NoEntireNetwork`,
    REGSTR_VAL_NOWORKGROUPCONTENTS       = `NoWorkgroupContents`,
    REGSTR_VAL_MINPWDLEN                 = `MinPwdLen`,
    REGSTR_VAL_PWDEXPIRATION             = `PwdExpiration`,
    REGSTR_VAL_WIN31PROVIDER             = `Win31Provider`,
    REGSTR_VAL_DISABLEREGTOOLS           = `DisableRegistryTools`,
    REGSTR_PATH_WINLOGON                 = `Software\Microsoft\Windows\CurrentVersion\Winlogon`,
    REGSTR_VAL_LEGALNOTICECAPTION        = `LegalNoticeCaption`,
    REGSTR_VAL_LEGALNOTICETEXT           = `LegalNoticeText`,
    REGSTR_VAL_RESTRICTRUN               = `RestrictRun`,
    REGSTR_KEY_POL_USERS                 = `Users`,
    REGSTR_KEY_POL_COMPUTERS             = `Computers`,
    REGSTR_KEY_POL_USERGROUPS            = `UserGroups`,
    REGSTR_KEY_POL_DEFAULT               = `.default`,
    REGSTR_KEY_POL_USERGROUPDATA         = `GroupData\UserGroups\Priority`,
    REGSTR_PATH_TIMEZONE                 = `System\CurrentControlSet\Control\TimeZoneInformation`,
    REGSTR_VAL_TZBIAS                    = `Bias`,
    REGSTR_VAL_TZDLTBIAS                 = `DaylightBias`,
    REGSTR_VAL_TZSTDBIAS                 = `StandardBias`,
    REGSTR_VAL_TZACTBIAS                 = `ActiveTimeBias`,
    REGSTR_VAL_TZDLTFLAG                 = `DaylightFlag`,
    REGSTR_VAL_TZSTDSTART                = `StandardStart`,
    REGSTR_VAL_TZDLTSTART                = `DaylightStart`,
    REGSTR_VAL_TZDLTNAME                 = `DaylightName`,
    REGSTR_VAL_TZSTDNAME                 = `StandardName`,
    REGSTR_VAL_TZNOCHANGESTART           = `NoChangeStart`,
    REGSTR_VAL_TZNOCHANGEEND             = `NoChangeEnd`,
    REGSTR_VAL_TZNOAUTOTIME              = `DisableAutoDaylightTimeSet`,
    REGSTR_PATH_FLOATINGPOINTPROCESSOR   = `HARDWARE\DESCRIPTION\System\FloatingPointProcessor`,
    REGSTR_PATH_FLOATINGPOINTPROCESSOR0  = `HARDWARE\DESCRIPTION\System\FloatingPointProcessor\0`,
    REGSTR_PATH_COMPUTRNAME              = `System\CurrentControlSet\Control\ComputerName\ComputerName`,
    REGSTR_VAL_COMPUTRNAME               = `ComputerName`,
    REGSTR_PATH_SHUTDOWN                 = `System\CurrentControlSet\Control\Shutdown`,
    REGSTR_VAL_FORCEREBOOT               = `ForceReboot`,
    REGSTR_VAL_SETUPPROGRAMRAN           = `SetupProgramRan`,
    REGSTR_VAL_DOES_POLLING              = `PollingSupportNeeded`,
    REGSTR_PATH_KNOWNDLLS                = `System\CurrentControlSet\Control\SessionManager\KnownDLLs`,
    REGSTR_PATH_KNOWN16DLLS              = `System\CurrentControlSet\Control\SessionManager\Known16DLLs`,
    REGSTR_PATH_CHECKVERDLLS             = `System\CurrentControlSet\Control\SessionManager\CheckVerDLLs`,
    REGSTR_PATH_WARNVERDLLS              = `System\CurrentControlSet\Control\SessionManager\WarnVerDLLs`,
    REGSTR_PATH_HACKINIFILE              = `System\CurrentControlSet\Control\SessionManager\HackIniFiles`,
    REGSTR_PATH_CHECKBADAPPS             = `System\CurrentControlSet\Control\SessionManager\CheckBadApps`,
    REGSTR_PATH_APPPATCH                 = `System\CurrentControlSet\Control\SessionManager\AppPatches`,
    REGSTR_PATH_KNOWNVXDS                = `System\CurrentControlSet\Control\SessionManager\KnownVxDs`,
    REGSTR_VAL_UNINSTALLER_DISPLAYNAME   = `DisplayName`,
    REGSTR_VAL_UNINSTALLER_COMMANDLINE   = `UninstallString`,
    REGSTR_PATH_DESKTOP                  = REGSTR_PATH_SCREENSAVE,
    REGSTR_PATH_MOUSE                    = `Control Panel\Mouse`,
    REGSTR_PATH_KEYBOARD                 = `Control Panel\Keyboard`,
    REGSTR_PATH_COLORS                   = `Control Panel\Colors`,
    REGSTR_PATH_SOUND                    = `Control Panel\Sound`,
    REGSTR_PATH_METRICS                  = `Control Panel\Desktop\WindowMetrics`,
    REGSTR_PATH_ICONS                    = `Control Panel\Icons`,
    REGSTR_PATH_CURSORS                  = `Control Panel\Cursors`,
    REGSTR_PATH_CHECKDISK                = `Software\Microsoft\Windows\CurrentVersion\Applets\Check Drive`,
    REGSTR_PATH_CHECKDISKSET             = `Settings`,
    REGSTR_PATH_CHECKDISKUDRVS           = `NoUnknownDDErrDrvs`,
    REGSTR_PATH_FAULT                    = `Software\Microsoft\Windows\CurrentVersion\Fault`,
    REGSTR_VAL_FAULT_LOGFILE             = `LogFile`,
    REGSTR_PATH_AEDEBUG                  = `Software\Microsoft\Windows NT\CurrentVersion\AeDebug`,
    REGSTR_VAL_AEDEBUG_DEBUGGER          = `Debugger`,
    REGSTR_VAL_AEDEBUG_AUTO              = `Auto`,
    REGSTR_PATH_GRPCONV                  = `Software\Microsoft\Windows\CurrentVersion\GrpConv`,
    REGSTR_VAL_REGITEMDELETEMESSAGE      = `Removal Message`,
    REGSTR_PATH_LASTCHECK                = `Software\Microsoft\Windows\CurrentVersion\Explorer\LastCheck`,
    REGSTR_PATH_LASTOPTIMIZE             = `Software\Microsoft\Windows\CurrentVersion\Explorer\LastOptimize`,
    REGSTR_PATH_LASTBACKUP               = `Software\Microsoft\Windows\CurrentVersion\Explorer\LastBackup`,
    REGSTR_PATH_CHKLASTCHECK             = `Software\Microsoft\Windows\CurrentVersion\Applets\Check Drive\LastCheck`,
    REGSTR_PATH_CHKLASTSURFAN            = `Software\Microsoft\Windows\CurrentVersion\Applets\Check Drive\LastSurfaceAnalysis`,
    REGSTR_KEY_SHARES                    = `Software\Microsoft\Windows\CurrentVersion\Network\LanMan`,
    REGSTR_VAL_SHARES_FLAGS              = `Flags`,
    REGSTR_VAL_SHARES_TYPE               = `Type`,
    REGSTR_VAL_SHARES_PATH               = `Path`,
    REGSTR_VAL_SHARES_REMARK             = `Remark`,
    REGSTR_VAL_SHARES_RW_PASS            = `Parm1`,
    REGSTR_VAL_SHARES_RO_PASS            = `Parm2`,
    REGSTR_PATH_PRINT                    = `System\CurrentControlSet\Control\Print`,
    REGSTR_PATH_PRINTERS                 = `System\CurrentControlSet\Control\Print\Printers`,
    REGSTR_PATH_PROVIDERS                = `System\CurrentControlSet\Control\Print\Providers`,
    REGSTR_PATH_MONITORS                 = `System\CurrentControlSet\Control\Print\Monitors`,
    REGSTR_PATH_ENVIRONMENTS             = `System\CurrentControlSet\Control\Print\Environments`,
    REGSTR_VAL_START_ON_BOOT             = `StartOnBoot`,
    REGSTR_VAL_PRINTERS_MASK             = `PrintersMask`,
    REGSTR_VAL_DOS_SPOOL_MASK            = `DOSSpoolMask`,
    REGSTR_KEY_CURRENT_ENV               = `\Windows 4.0`,
    REGSTR_KEY_DRIVERS                   = `\Drivers`,
    REGSTR_KEY_PRINT_PROC                = `\Print Processors`,
    REGSTR_PATH_EVENTLABELS              = `AppEvents\EventLabels`,
    REGSTR_PATH_SCHEMES                  = `AppEvents\Schemes`,
    REGSTR_PATH_APPS                     = REGSTR_PATH_SCHEMES ~ `\Apps`,
    REGSTR_PATH_APPS_DEFAULT             = REGSTR_PATH_SCHEMES ~ `\Apps\.Default`,
    REGSTR_PATH_NAMES                    = REGSTR_PATH_SCHEMES ~ `\Names`,
    REGSTR_PATH_MULTIMEDIA               = REGSTR_PATH_SETUP ~ `\Multimedia`,
    REGSTR_PATH_MULTIMEDIA_AUDIO         = `Software\Microsoft\Multimedia\Audio`,
    REGSTR_PATH_MEDIARESOURCES           = REGSTR_PATH_CURRENT_CONTROL_SET ~ `\MediaResources`,
    REGSTR_PATH_MEDIAPROPERTIES          = REGSTR_PATH_CURRENT_CONTROL_SET ~ `\MediaProperties`,
    REGSTR_PATH_PRIVATEPROPERTIES        = REGSTR_PATH_MEDIAPROPERTIES ~ `\PrivateProperties`,
    REGSTR_PATH_PUBLICPROPERTIES         = REGSTR_PATH_MEDIAPROPERTIES ~ `\PublicProperties`,
    REGSTR_PATH_JOYOEM                   = REGSTR_PATH_PRIVATEPROPERTIES ~ `\Joystick\OEM`,
    REGSTR_PATH_JOYCONFIG                = REGSTR_PATH_MEDIARESOURCES ~ `\Joystick`,
    REGSTR_KEY_JOYCURR                   = `CurrentJoystickSettings`,
    REGSTR_KEY_JOYSETTINGS               = `JoystickSettings`,
    REGSTR_VAL_JOYUSERVALUES             = `JoystickUserValues`,
    REGSTR_VAL_JOYCALLOUT                = `JoystickCallout`,
    REGSTR_VAL_JOYNCONFIG                = `Joystick%dConfiguration`,
    REGSTR_VAL_JOYNOEMNAME               = `Joystick%dOEMName`,
    REGSTR_VAL_JOYNOEMCALLOUT            = `Joystick%dOEMCallout`,
    REGSTR_VAL_JOYOEMCALLOUT             = `OEMCallout`,
    REGSTR_VAL_JOYOEMNAME                = `OEMName`,
    REGSTR_VAL_JOYOEMDATA                = `OEMData`,
    REGSTR_VAL_JOYOEMXYLABEL             = `OEMXYLabel`,
    REGSTR_VAL_JOYOEMZLABEL              = `OEMZLabel`,
    REGSTR_VAL_JOYOEMRLABEL              = `OEMRLabel`,
    REGSTR_VAL_JOYOEMPOVLABEL            = `OEMPOVLabel`,
    REGSTR_VAL_JOYOEMULABEL              = `OEMULabel`,
    REGSTR_VAL_JOYOEMVLABEL              = `OEMVLabel`,
    REGSTR_VAL_JOYOEMTESTMOVEDESC        = `OEMTestMoveDesc`,
    REGSTR_VAL_JOYOEMTESTBUTTONDESC      = `OEMTestButtonDesc`,
    REGSTR_VAL_JOYOEMTESTMOVECAP         = `OEMTestMoveCap`,
    REGSTR_VAL_JOYOEMTESTBUTTONCAP       = `OEMTestButtonCap`,
    REGSTR_VAL_JOYOEMTESTWINCAP          = `OEMTestWinCap`,
    REGSTR_VAL_JOYOEMCALCAP              = `OEMCalCap`,
    REGSTR_VAL_JOYOEMCALWINCAP           = `OEMCalWinCap`,
    REGSTR_VAL_JOYOEMCAL1                = `OEMCal1`,
    REGSTR_VAL_JOYOEMCAL2                = `OEMCal2`,
    REGSTR_VAL_JOYOEMCAL3                = `OEMCal3`,
    REGSTR_VAL_JOYOEMCAL4                = `OEMCal4`,
    REGSTR_VAL_JOYOEMCAL5                = `OEMCal5`,
    REGSTR_VAL_JOYOEMCAL6                = `OEMCal6`,
    REGSTR_VAL_JOYOEMCAL7                = `OEMCal7`,
    REGSTR_VAL_JOYOEMCAL8                = `OEMCal8`,
    REGSTR_VAL_JOYOEMCAL9                = `OEMCal9`,
    REGSTR_VAL_JOYOEMCAL10               = `OEMCal10`,
    REGSTR_VAL_JOYOEMCAL11               = `OEMCal11`,
    REGSTR_VAL_JOYOEMCAL12               = `OEMCal12`;

enum {
    DTRESULTOK,
    DTRESULTFIX,
    DTRESULTPROB,
    DTRESULTPART
}

//#ifndef NEC_98
enum PCIC_DEFAULT_IRQMASK = 0x4EB8;
//#else
//#define PCIC_DEFAULT_IRQMASK  0x1468
//#endif
enum PCIC_DEFAULT_NUMSOCKETS = 0;

struct DSKTLSYSTEMTIME {
    WORD wYear;
    WORD wMonth;
    WORD wDayOfWeek;
    WORD wDay;
    WORD wHour;
    WORD wMinute;
    WORD wSecond;
    WORD wMilliseconds;
    WORD wResult;
}
alias DSKTLSYSTEMTIME* PDSKTLSYSTEMTIME, LPDSKTLSYSTEMTIME;
