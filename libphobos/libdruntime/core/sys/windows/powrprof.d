/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * Authors: Stewart Gordon
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_powrprof.d)
 */
module core.sys.windows.powrprof;
version (Windows):
@system:
pragma(lib, "powrprof");

import core.sys.windows.windef;
import core.sys.windows.ntdef;

// FIXME: look up Windows version support

enum ULONG
    EnableSysTrayBatteryMeter =  1,
    EnableMultiBatteryDisplay =  2,
    EnablePasswordLogon       =  4,
    EnableWakeOnRing          =  8,
    EnableVideoDimDisplay     = 16;

enum UINT NEWSCHEME = -1;

struct GLOBAL_MACHINE_POWER_POLICY {
    ULONG              Revision;
    SYSTEM_POWER_STATE LidOpenWakeAc;
    SYSTEM_POWER_STATE LidOpenWakeDc;
    ULONG              BroadcastCapacityResolution;
}
alias GLOBAL_MACHINE_POWER_POLICY* PGLOBAL_MACHINE_POWER_POLICY;

struct GLOBAL_USER_POWER_POLICY {
    ULONG               Revision;
    POWER_ACTION_POLICY PowerButtonAc;
    POWER_ACTION_POLICY PowerButtonDc;
    POWER_ACTION_POLICY SleepButtonAc;
    POWER_ACTION_POLICY SleepButtonDc;
    POWER_ACTION_POLICY LidCloseAc;
    POWER_ACTION_POLICY LidCloseDc;
    SYSTEM_POWER_LEVEL[NUM_DISCHARGE_POLICIES] DischargePolicy;
    ULONG GlobalFlags;
}
alias GLOBAL_USER_POWER_POLICY* PGLOBAL_USER_POWER_POLICY;

struct GLOBAL_POWER_POLICY {
    GLOBAL_USER_POWER_POLICY    user;
    GLOBAL_MACHINE_POWER_POLICY mach;
}
alias GLOBAL_POWER_POLICY* PGLOBAL_POWER_POLICY;

struct MACHINE_POWER_POLICY {
    ULONG               Revision;
    SYSTEM_POWER_STATE  MinSleepAc;
    SYSTEM_POWER_STATE  MinSleepDc;
    SYSTEM_POWER_STATE  ReducedLatencySleepAc;
    SYSTEM_POWER_STATE  ReducedLatencySleepDc;
    ULONG               DozeTimeoutAc;
    ULONG               DozeTimeoutDc;
    ULONG               DozeS4TimeoutAc;
    ULONG               DozeS4TimeoutDc;
    UCHAR               MinThrottleAc;
    UCHAR               MinThrottleDc;
    UCHAR[2]            pad1;
    POWER_ACTION_POLICY OverThrottledAc;
    POWER_ACTION_POLICY OverThrottledDc;
}
alias MACHINE_POWER_POLICY* PMACHINE_POWER_POLICY;

struct MACHINE_PROCESSOR_POWER_POLICY {
    ULONG Revision;
    PROCESSOR_POWER_POLICY ProcessorPolicyAc;
    PROCESSOR_POWER_POLICY ProcessorPolicyDc;
}
alias MACHINE_PROCESSOR_POWER_POLICY* PMACHINE_PROCESSOR_POWER_POLICY;

struct USER_POWER_POLICY {
   ULONG               Revision;
   POWER_ACTION_POLICY IdleAc;
   POWER_ACTION_POLICY IdleDc;
   ULONG               IdleTimeoutAc;
   ULONG               IdleTimeoutDc;
   UCHAR               IdleSensitivityAc;
   UCHAR               IdleSensitivityDc;
   UCHAR               ThrottlePolicyAc;
   UCHAR               ThrottlePolicyDc;
   SYSTEM_POWER_STATE  MaxSleepAc;
   SYSTEM_POWER_STATE  MaxSleepDc;
   ULONG[2]            Reserved;
   ULONG               VideoTimeoutAc;
   ULONG               VideoTimeoutDc;
   ULONG               SpindownTimeoutAc;
   ULONG               SpindownTimeoutDc;
   BOOLEAN             OptimizeForPowerAc;
   BOOLEAN             OptimizeForPowerDc;
   UCHAR               FanThrottleToleranceAc;
   UCHAR               FanThrottleToleranceDc;
   UCHAR               ForcedThrottleAc;
   UCHAR               ForcedThrottleDc;
}
alias USER_POWER_POLICY* PUSER_POWER_POLICY;

struct POWER_POLICY {
    USER_POWER_POLICY    user;
    MACHINE_POWER_POLICY mach;
}
alias POWER_POLICY* PPOWER_POLICY;

extern (Windows) {
    alias BOOLEAN function(UINT, DWORD, LPTSTR, DWORD, LPTSTR, PPOWER_POLICY,
      LPARAM) PWRSCHEMESENUMPROC;
    alias BOOLEAN function(POWER_ACTION, SYSTEM_POWER_STATE, ULONG, BOOLEAN)
      PFNNTINITIATEPWRACTION;

    NTSTATUS CallNtPowerInformation(POWER_INFORMATION_LEVEL, PVOID, ULONG,
      PVOID, ULONG);
    BOOLEAN CanUserWritePwrScheme();
    BOOLEAN DeletePwrScheme(UINT);
    BOOLEAN EnumPwrSchemes(PWRSCHEMESENUMPROC, LPARAM);
    BOOLEAN GetActivePwrScheme(PUINT);
    BOOLEAN GetCurrentPowerPolicies(PGLOBAL_POWER_POLICY, PPOWER_POLICY);
    BOOLEAN GetPwrCapabilities(PSYSTEM_POWER_CAPABILITIES);
    BOOLEAN GetPwrDiskSpindownRange(PUINT, PUINT);
    BOOLEAN IsAdminOverrideActive(PADMINISTRATOR_POWER_POLICY);
    BOOLEAN IsPwrHibernateAllowed();
    BOOLEAN IsPwrShutdownAllowed();
    BOOLEAN IsPwrSuspendAllowed();
    BOOLEAN ReadGlobalPwrPolicy(PGLOBAL_POWER_POLICY);
    BOOLEAN ReadProcessorPwrScheme(UINT, PMACHINE_PROCESSOR_POWER_POLICY);
    BOOLEAN ReadPwrScheme(UINT, PPOWER_POLICY);
    BOOLEAN SetActivePwrScheme(UINT, PGLOBAL_POWER_POLICY, PPOWER_POLICY);
    BOOLEAN SetSuspendState(BOOLEAN, BOOLEAN, BOOLEAN);
    BOOLEAN WriteGlobalPwrPolicy(PGLOBAL_POWER_POLICY);
    BOOLEAN WriteProcessorPwrScheme(UINT, PMACHINE_PROCESSOR_POWER_POLICY);
    BOOLEAN ValidatePowerPolicies(PGLOBAL_POWER_POLICY, PPOWER_POLICY);
    BOOLEAN WritePwrScheme(PUINT, LPTSTR, LPTSTR, PPOWER_POLICY);
}
