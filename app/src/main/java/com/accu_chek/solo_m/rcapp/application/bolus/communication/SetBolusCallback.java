/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: SetBolus
 * Brief: FIXME
 *
 * Create Date: 11/05/2015
 * $Revision: 23382 $
 * $Author: LuyaHuang $
 * $Id: SetBolus.java 23382 2015-11-05 08:22:35Z LuyaHuang $
 */
package com.accu_chek.solo_m.rcapp.application.bolus.communication;


public interface SetBolusCallback
{
    void onSetBolusResponse(short bolusID);
    void onResponseCode(short requestOpCode, byte value);
}
/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */// [NSM-2889] Update Bolus Delivery Function
// (R23382 2015-11-05 04:22:35 LuyaHuang)
// ----------------------------------------------------------------------------
// [NSM-2889] Apply Patient Record storing to UI
// (R23382 2015-11-05 04:22:35 LuyaHuang)
// ----------------------------------------------------------------------------
// [NSM-2889] Apply Patient Record storing to UI
