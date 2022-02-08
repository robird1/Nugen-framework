/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.bgmcontrol.
 * IBgmConstant
 * Brief:
 *
 * Create Date: 2015¦~4¤ë17¤é
 * $Revision: 24486 $
 * $Author: VictorChen $
 * $Id: IBgmConstant.java 24486 2015-11-20 05:48:56Z VictorChen $
 */

package com.accu_chek.solo_m.rcapp.application.bgmcontrol;

public interface IBgmConstant
{
    final String ACTION_WAIT_STRIP = "wait_strip";
    final String ACTION_STRIP_INSERT = "strip_insert";
    final String ACTION_WAIT_BLOOD = "wait_blood";
    final String ACTION_bG_PROCESSING = "bg_processing";
    final String ACTION_bG_RESULT = "bg_result";
    final String KEY_BGMCONTROL = "key_BGMControl";
    final String ACTION_BG_ACK = "ACK";
    final String ACTION_BG_NAK = "NAK";
    final String ACTION_BG_EOT = "EOT";
    final String HI = "HI";
    final String LO = "LO";
    final String DATEFORMAT = "yyMMdd";
    final String TIMEFORMAT = "HHmmss";
    final String REMOVE_EARLY = "remove_eraly";
    final String REMOVE_WAIT_STRIP = "remove_wait_trip";
    final String CHECKBGMSTATUS = "CheckBgmStatus";
    final String POWERDOWNBGM = "PowerDownBgm";
    final String BGMEASUREMENT = "BgMeasurement";
    final String COMPOSECGCOMMAND = "ComposeCgCommand";
    final String HANDLEERROR = "HandleError";
    final String SETBGMDATETIME = "SetBgmDateTime";
    final String SETBGMTIME = "SetBgmTime";
    final String CHECKCODEKEY = "CheckCodeKey";
    final String GETSTRIPCOUNTER = "GetStripCounter";
    final String GETBGMTIME = "GetBgmTime";
    final String GETBGMDATETIME = "GetBgmDateTime";
    final String GETSTRIPCONNECTOR = "GetStripConnector";
    final String COMMUNICATIONTIMEOUT = "communicationtimeout";
    final String MDI = "MDI";

    final int READY_PIN_STATUS_VALUE = 0xffff;
    final int ZERO = 0;
    final int ONE = 1;
    final int TENS = 10;
    final int HUNDRED = 100;
    final int mExpectedByteLength = 1;
    final int READACK_VALUE = 2;
    final int WRITEACK_VALUE = 3;
    final int BYTEZERO = 0x00;

    final long BGM_CANCELCOMMAND_TIMEOUT = 4000L;
    final long BGM_WAIT_TIMEOUT = 2000L;
    final long BGM_WAIT_RDYPIN = 1500L;
    final long BGM_WAIT_RDYPIN_INTERRUPT = 1000L;
    final long BGM_COMMAND_WAIT_TIME = 2000L;
    //final long BGM_WAIT_BLOOD_TIME = 120000L;
    final long BGM_WAIT_BLOOD_TIME = 15000L;
    final long waitErrorHandlingTime = 20L;
    final long BLE_DELAY_TIME = 5000L;
    final long BGM_HARDWARE_PIN_TIMEOUT = 3000L;

    // BGM command hex define
    byte STX = 0x02;
    byte EOT = 0x04;
    byte ACK = 0x06;
    byte TAB = 0x09;
    byte NAK = 0x15;
    byte CR = 0x0d;
    byte ON_WAITSTRIP = 0x21;
    byte ON_STRIPINSER = 0x20;
    byte ON_WAITBLOOD = 0x23;
    byte ON_BG_PROCESSING = 0x30;
    byte cG_INRANGE = 0x30;

}
// (R20520 2015-10-01 07:04:11 DWYang)
// ----------------------------------------------------------------------------
// add updateCodeKey function.
// (R22634 2015-10-27 07:29:29 VictorChen)
// ----------------------------------------------------------------------------
// Refine code and comment.
// (R22634 2015-10-27 07:29:29 VictorChen)
// ----------------------------------------------------------------------------
// Refine bgtestflow constant naming.
// (R23451 2015-11-05 07:44:01 VictorChen)
// ----------------------------------------------------------------------------
// Refine code comment.
