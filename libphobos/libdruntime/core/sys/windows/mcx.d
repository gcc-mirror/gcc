/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * Authors: Stewart Gordon
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC core/sys/windows/_mcx.d)
 */
module core.sys.windows.mcx;
version (Windows):

import core.sys.windows.windef;

enum DWORD
    DIALOPTION_BILLING  =  64,
    DIALOPTION_QUIET    = 128,
    DIALOPTION_DIALTONE = 256;

enum DWORD
    MDMVOLFLAG_LOW    = 1,
    MDMVOLFLAG_MEDIUM = 2,
    MDMVOLFLAG_HIGH   = 4;

enum : DWORD {
    MDMVOL_LOW    = 0,
    MDMVOL_MEDIUM = 1,
    MDMVOL_HIGH   = 2
}

enum DWORD
    MDMSPKRFLAG_OFF       = 1,
    MDMSPKRFLAG_DIAL      = 2,
    MDMSPKRFLAG_ON        = 4,
    MDMSPKRFLAG_CALLSETUP = 8;

enum : DWORD {
    MDMSPKR_OFF,
    MDMSPKR_DIAL,
    MDMSPKR_ON,
    MDMSPKR_CALLSETUP
}

enum DWORD
    MDM_COMPRESSION      = 0x0001,
    MDM_ERROR_CONTROL    = 0x0002,
    MDM_FORCED_EC        = 0x0004,
    MDM_CELLULAR         = 0x0008,
    MDM_FLOWCONTROL_HARD = 0x0010,
    MDM_FLOWCONTROL_SOFT = 0x0020,
    MDM_CCITT_OVERRIDE   = 0x0040,
    MDM_SPEED_ADJUST     = 0x0080,
    MDM_TONE_DIAL        = 0x0100,
    MDM_BLIND_DIAL       = 0x0200,
    MDM_V23_OVERRIDE     = 0x0400;

struct MODEMDEVCAPS {
    DWORD dwActualSize;
    DWORD dwRequiredSize;
    DWORD dwDevSpecificOffset;
    DWORD dwDevSpecificSize;
    DWORD dwModemProviderVersion;
    DWORD dwModemManufacturerOffset;
    DWORD dwModemManufacturerSize;
    DWORD dwModemModelOffset;
    DWORD dwModemModelSize;
    DWORD dwModemVersionOffset;
    DWORD dwModemVersionSize;
    DWORD dwDialOptions;
    DWORD dwCallSetupFailTimer;
    DWORD dwInactivityTimeout;
    DWORD dwSpeakerVolume;
    DWORD dwSpeakerMode;
    DWORD dwModemOptions;
    DWORD dwMaxDTERate;
    DWORD dwMaxDCERate;
    BYTE  _abVariablePortion;

    BYTE* abVariablePortion() return { return &_abVariablePortion; }
}
alias MODEMDEVCAPS* PMODEMDEVCAPS, LPMODEMDEVCAPS;

struct MODEMSETTINGS {
    DWORD dwActualSize;
    DWORD dwRequiredSize;
    DWORD dwDevSpecificOffset;
    DWORD dwDevSpecificSize;
    DWORD dwCallSetupFailTimer;
    DWORD dwInactivityTimeout;
    DWORD dwSpeakerVolume;
    DWORD dwSpeakerMode;
    DWORD dwPreferredModemOptions;
    DWORD dwNegotiatedModemOptions;
    DWORD dwNegotiatedDCERate;
    BYTE  _abVariablePortion;

    BYTE* abVariablePortion() return { return &_abVariablePortion; }
}
alias MODEMSETTINGS* PMODEMSETTINGS, LPMODEMSETTINGS;
