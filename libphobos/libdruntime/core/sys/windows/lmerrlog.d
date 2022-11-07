/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC core/sys/windows/_lmerrlog.d)
 */
module core.sys.windows.lmerrlog;
version (Windows):

// COMMENT: This appears to be only for Win16. All functions are deprecated.

import core.sys.windows.lmcons, core.sys.windows.windef;
import core.sys.windows.lmaudit; // for LPHLOG

enum ERRLOG_BASE=3100;
enum ERRLOG2_BASE=5700;
enum LOGFLAGS_FORWARD=0;
enum LOGFLAGS_BACKWARD=1;
enum LOGFLAGS_SEEK=2;
enum NELOG_Internal_Error=ERRLOG_BASE;
enum NELOG_Resource_Shortage=(ERRLOG_BASE+1);
enum NELOG_Unable_To_Lock_Segment=(ERRLOG_BASE+2);
enum NELOG_Unable_To_Unlock_Segment=(ERRLOG_BASE+3);
enum NELOG_Uninstall_Service=(ERRLOG_BASE+4);
enum NELOG_Init_Exec_Fail=(ERRLOG_BASE+5);
enum NELOG_Ncb_Error=(ERRLOG_BASE+6);
enum NELOG_Net_Not_Started=(ERRLOG_BASE+7);
enum NELOG_Ioctl_Error=(ERRLOG_BASE+8);
enum NELOG_System_Semaphore=(ERRLOG_BASE+9);
enum NELOG_Init_OpenCreate_Err=(ERRLOG_BASE+10);
enum NELOG_NetBios=(ERRLOG_BASE+11);
enum NELOG_SMB_Illegal=(ERRLOG_BASE+12);
enum NELOG_Service_Fail=(ERRLOG_BASE+13);
enum NELOG_Entries_Lost=(ERRLOG_BASE+14);
enum NELOG_Init_Seg_Overflow=(ERRLOG_BASE+20);
enum NELOG_Srv_No_Mem_Grow=(ERRLOG_BASE+21);
enum NELOG_Access_File_Bad=(ERRLOG_BASE+22);
enum NELOG_Srvnet_Not_Started=(ERRLOG_BASE+23);
enum NELOG_Init_Chardev_Err=(ERRLOG_BASE+24);
enum NELOG_Remote_API=(ERRLOG_BASE+25);
enum NELOG_Ncb_TooManyErr=(ERRLOG_BASE+26);
enum NELOG_Mailslot_err=(ERRLOG_BASE+27);
enum NELOG_ReleaseMem_Alert=(ERRLOG_BASE+28);
enum NELOG_AT_cannot_write=(ERRLOG_BASE+29);
enum NELOG_Cant_Make_Msg_File=(ERRLOG_BASE+30);
enum NELOG_Exec_Netservr_NoMem=(ERRLOG_BASE+31);
enum NELOG_Server_Lock_Failure=(ERRLOG_BASE+32);
enum NELOG_Msg_Shutdown=(ERRLOG_BASE+40);
enum NELOG_Msg_Sem_Shutdown=(ERRLOG_BASE+41);
enum NELOG_Msg_Log_Err=(ERRLOG_BASE+50);
enum NELOG_VIO_POPUP_ERR=(ERRLOG_BASE+51);
enum NELOG_Msg_Unexpected_SMB_Type=(ERRLOG_BASE+52);
enum NELOG_Wksta_Infoseg=(ERRLOG_BASE+60);
enum NELOG_Wksta_Compname=(ERRLOG_BASE+61);
enum NELOG_Wksta_BiosThreadFailure=(ERRLOG_BASE+62);
enum NELOG_Wksta_IniSeg=(ERRLOG_BASE+63);
enum NELOG_Wksta_HostTab_Full=(ERRLOG_BASE+64);
enum NELOG_Wksta_Bad_Mailslot_SMB=(ERRLOG_BASE+65);
enum NELOG_Wksta_UASInit=(ERRLOG_BASE+66);
enum NELOG_Wksta_SSIRelogon=(ERRLOG_BASE+67);
enum NELOG_Build_Name=(ERRLOG_BASE+70);
enum NELOG_Name_Expansion=(ERRLOG_BASE+71);
enum NELOG_Message_Send=(ERRLOG_BASE+72);
enum NELOG_Mail_Slt_Err=(ERRLOG_BASE+73);
enum NELOG_AT_cannot_read=(ERRLOG_BASE+74);
enum NELOG_AT_sched_err=(ERRLOG_BASE+75);
enum NELOG_AT_schedule_file_created=(ERRLOG_BASE+76);
enum NELOG_Srvnet_NB_Open=(ERRLOG_BASE+77);
enum NELOG_AT_Exec_Err=(ERRLOG_BASE+78);
enum NELOG_Lazy_Write_Err=(ERRLOG_BASE+80);
enum NELOG_HotFix=(ERRLOG_BASE+81);
enum NELOG_HardErr_From_Server=(ERRLOG_BASE+82);
enum NELOG_LocalSecFail1=(ERRLOG_BASE+83);
enum NELOG_LocalSecFail2=(ERRLOG_BASE+84);
enum NELOG_LocalSecFail3=(ERRLOG_BASE+85);
enum NELOG_LocalSecGeneralFail=(ERRLOG_BASE+86);
enum NELOG_NetWkSta_Internal_Error=(ERRLOG_BASE+90);
enum NELOG_NetWkSta_No_Resource=(ERRLOG_BASE+91);
enum NELOG_NetWkSta_SMB_Err=(ERRLOG_BASE+92);
enum NELOG_NetWkSta_VC_Err=(ERRLOG_BASE+93);
enum NELOG_NetWkSta_Stuck_VC_Err=(ERRLOG_BASE+94);
enum NELOG_NetWkSta_NCB_Err=(ERRLOG_BASE+95);
enum NELOG_NetWkSta_Write_Behind_Err=(ERRLOG_BASE+96);
enum NELOG_NetWkSta_Reset_Err=(ERRLOG_BASE+97);
enum NELOG_NetWkSta_Too_Many=(ERRLOG_BASE+98);
enum NELOG_Srv_Thread_Failure=(ERRLOG_BASE+104);
enum NELOG_Srv_Close_Failure=(ERRLOG_BASE+105);
enum NELOG_ReplUserCurDir=(ERRLOG_BASE+106);
enum NELOG_ReplCannotMasterDir=(ERRLOG_BASE+107);
enum NELOG_ReplUpdateError=(ERRLOG_BASE+108);
enum NELOG_ReplLostMaster=(ERRLOG_BASE+109);
enum NELOG_NetlogonAuthDCFail=(ERRLOG_BASE+110);
enum NELOG_ReplLogonFailed=(ERRLOG_BASE+111);
enum NELOG_ReplNetErr=(ERRLOG_BASE+112);
enum NELOG_ReplMaxFiles=(ERRLOG_BASE+113);
enum NELOG_ReplMaxTreeDepth=(ERRLOG_BASE+114);
enum NELOG_ReplBadMsg=(ERRLOG_BASE+115);
enum NELOG_ReplSysErr=(ERRLOG_BASE+116);
enum NELOG_ReplUserLoged=(ERRLOG_BASE+117);
enum NELOG_ReplBadImport=(ERRLOG_BASE+118);
enum NELOG_ReplBadExport=(ERRLOG_BASE+119);
enum NELOG_ReplSignalFileErr=(ERRLOG_BASE+120);
enum NELOG_DiskFT=(ERRLOG_BASE+121);
enum NELOG_ReplAccessDenied=(ERRLOG_BASE+122);
enum NELOG_NetlogonFailedPrimary=(ERRLOG_BASE+123);
enum NELOG_NetlogonPasswdSetFailed=(ERRLOG_BASE+124);
enum NELOG_NetlogonTrackingError=(ERRLOG_BASE+125);
enum NELOG_NetlogonSyncError=(ERRLOG_BASE+126);
enum NELOG_UPS_PowerOut=(ERRLOG_BASE+130);
enum NELOG_UPS_Shutdown=(ERRLOG_BASE+131);
enum NELOG_UPS_CmdFileError=(ERRLOG_BASE+132);
enum NELOG_UPS_CannotOpenDriver=(ERRLOG_BASE+133);
enum NELOG_UPS_PowerBack=(ERRLOG_BASE+134);
enum NELOG_UPS_CmdFileConfig=(ERRLOG_BASE+135);
enum NELOG_UPS_CmdFileExec=(ERRLOG_BASE+136);
enum NELOG_Missing_Parameter=(ERRLOG_BASE+150);
enum NELOG_Invalid_Config_Line=(ERRLOG_BASE+151);
enum NELOG_Invalid_Config_File=(ERRLOG_BASE+152);
enum NELOG_File_Changed=(ERRLOG_BASE+153);
enum NELOG_Files_Dont_Fit=(ERRLOG_BASE+154);
enum NELOG_Wrong_DLL_Version=(ERRLOG_BASE+155);
enum NELOG_Error_in_DLL=(ERRLOG_BASE+156);
enum NELOG_System_Error=(ERRLOG_BASE+157);
enum NELOG_FT_ErrLog_Too_Large=(ERRLOG_BASE+158);
enum NELOG_FT_Update_In_Progress=(ERRLOG_BASE+159);
enum NELOG_OEM_Code=(ERRLOG_BASE+199);
enum NELOG_NetlogonSSIInitError=ERRLOG2_BASE;
enum NELOG_NetlogonFailedToUpdateTrustList=(ERRLOG2_BASE+1);
enum NELOG_NetlogonFailedToAddRpcInterface=(ERRLOG2_BASE+2);
enum NELOG_NetlogonFailedToReadMailslot=(ERRLOG2_BASE+3);
enum NELOG_NetlogonFailedToRegisterSC=(ERRLOG2_BASE+4);
enum NELOG_NetlogonChangeLogCorrupt=(ERRLOG2_BASE+5);
enum NELOG_NetlogonFailedToCreateShare=(ERRLOG2_BASE+6);
enum NELOG_NetlogonDownLevelLogonFailed=(ERRLOG2_BASE+7);
enum NELOG_NetlogonDownLevelLogoffFailed=(ERRLOG2_BASE+8);
enum NELOG_NetlogonNTLogonFailed=(ERRLOG2_BASE+9);
enum NELOG_NetlogonNTLogoffFailed=(ERRLOG2_BASE+10);
enum NELOG_NetlogonPartialSyncCallSuccess=(ERRLOG2_BASE+11);
enum NELOG_NetlogonPartialSyncCallFailed=(ERRLOG2_BASE+12);
enum NELOG_NetlogonFullSyncCallSuccess=(ERRLOG2_BASE+13);
enum NELOG_NetlogonFullSyncCallFailed=(ERRLOG2_BASE+14);
enum NELOG_NetlogonPartialSyncSuccess=(ERRLOG2_BASE+15);
enum NELOG_NetlogonPartialSyncFailed=(ERRLOG2_BASE+16);
enum NELOG_NetlogonFullSyncSuccess=(ERRLOG2_BASE+17);
enum NELOG_NetlogonFullSyncFailed=(ERRLOG2_BASE+18);
enum NELOG_NetlogonAuthNoDomainController=(ERRLOG2_BASE+19);
enum NELOG_NetlogonAuthNoTrustLsaSecret=(ERRLOG2_BASE+20);
enum NELOG_NetlogonAuthNoTrustSamAccount=(ERRLOG2_BASE+21);
enum NELOG_NetlogonServerAuthFailed=(ERRLOG2_BASE+22);
enum NELOG_NetlogonServerAuthNoTrustSamAccount=(ERRLOG2_BASE+23);
enum NELOG_FailedToRegisterSC=(ERRLOG2_BASE+24);
enum NELOG_FailedToSetServiceStatus=(ERRLOG2_BASE+25);
enum NELOG_FailedToGetComputerName=(ERRLOG2_BASE+26);
enum NELOG_DriverNotLoaded=(ERRLOG2_BASE+27);
enum NELOG_NoTranportLoaded=(ERRLOG2_BASE+28);
enum NELOG_NetlogonFailedDomainDelta=(ERRLOG2_BASE+29);
enum NELOG_NetlogonFailedGlobalGroupDelta=(ERRLOG2_BASE+30);
enum NELOG_NetlogonFailedLocalGroupDelta=(ERRLOG2_BASE+31);
enum NELOG_NetlogonFailedUserDelta=(ERRLOG2_BASE+32);
enum NELOG_NetlogonFailedPolicyDelta=(ERRLOG2_BASE+33);
enum NELOG_NetlogonFailedTrustedDomainDelta=(ERRLOG2_BASE+34);
enum NELOG_NetlogonFailedAccountDelta=(ERRLOG2_BASE+35);
enum NELOG_NetlogonFailedSecretDelta=(ERRLOG2_BASE+36);
enum NELOG_NetlogonSystemError=(ERRLOG2_BASE+37);
enum NELOG_NetlogonDuplicateMachineAccounts=(ERRLOG2_BASE+38);
enum NELOG_NetlogonTooManyGlobalGroups=(ERRLOG2_BASE+39);
enum NELOG_NetlogonBrowserDriver=(ERRLOG2_BASE+40);
enum NELOG_NetlogonAddNameFailure=(ERRLOG2_BASE+41);
enum NELOG_RplMessages=(ERRLOG2_BASE+42);
enum NELOG_RplXnsBoot=(ERRLOG2_BASE+43);
enum NELOG_RplSystem=(ERRLOG2_BASE+44);
enum NELOG_RplWkstaTimeout=(ERRLOG2_BASE+45);
enum NELOG_RplWkstaFileOpen=(ERRLOG2_BASE+46);
enum NELOG_RplWkstaFileRead=(ERRLOG2_BASE+47);
enum NELOG_RplWkstaMemory=(ERRLOG2_BASE+48);
enum NELOG_RplWkstaFileChecksum=(ERRLOG2_BASE+49);
enum NELOG_RplWkstaFileLineCount=(ERRLOG2_BASE+50);
enum NELOG_RplWkstaBbcFile=(ERRLOG2_BASE+51);
enum NELOG_RplWkstaFileSize=(ERRLOG2_BASE+52);
enum NELOG_RplWkstaInternal=(ERRLOG2_BASE+53);
enum NELOG_RplWkstaWrongVersion=(ERRLOG2_BASE+54);
enum NELOG_RplWkstaNetwork=(ERRLOG2_BASE+55);
enum NELOG_RplAdapterResource=(ERRLOG2_BASE+56);
enum NELOG_RplFileCopy=(ERRLOG2_BASE+57);
enum NELOG_RplFileDelete=(ERRLOG2_BASE+58);
enum NELOG_RplFilePerms=(ERRLOG2_BASE+59);
enum NELOG_RplCheckConfigs=(ERRLOG2_BASE+60);
enum NELOG_RplCreateProfiles=(ERRLOG2_BASE+61);
enum NELOG_RplRegistry=(ERRLOG2_BASE+62);
enum NELOG_RplReplaceRPLDISK=(ERRLOG2_BASE+63);
enum NELOG_RplCheckSecurity=(ERRLOG2_BASE+64);
enum NELOG_RplBackupDatabase=(ERRLOG2_BASE+65);
enum NELOG_RplInitDatabase=(ERRLOG2_BASE+66);
enum NELOG_RplRestoreDatabaseFailure=(ERRLOG2_BASE+67);
enum NELOG_RplRestoreDatabaseSuccess=(ERRLOG2_BASE+68);
enum NELOG_RplInitRestoredDatabase=(ERRLOG2_BASE+69);
enum NELOG_NetlogonSessionTypeWrong=(ERRLOG2_BASE+70);

struct ERROR_LOG {
    DWORD el_len;
    DWORD el_reserved;
    DWORD el_time;
    DWORD el_error;
    LPWSTR el_name;
    LPWSTR el_text;
    LPBYTE el_data;
    DWORD el_data_size;
    DWORD el_nstrings;
}
alias ERROR_LOG* PERROR_LOG, LPERROR_LOG;

extern (Windows) {
    deprecated {
        NET_API_STATUS NetErrorLogClear(LPCWSTR, LPCWSTR, LPBYTE);
        NET_API_STATUS NetErrorLogRead(LPCWSTR, LPWSTR, LPHLOG, DWORD,
          LPDWORD, DWORD, DWORD, LPBYTE*, DWORD, LPDWORD, LPDWORD);
        NET_API_STATUS NetErrorLogWrite(LPBYTE, DWORD, LPCWSTR, LPBYTE,
          DWORD, LPBYTE, DWORD, LPBYTE);
    }
}
