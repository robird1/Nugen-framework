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
 * $Revision: 25095 $
 * $Author: LuyaHuang $
 * $Id: SetBolus.java 25095 2015-11-30 07:49:56Z LuyaHuang $
 */
package com.accu_chek.solo_m.rcapp.application.bolus.communication;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;

import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;

import com.accu_chek.solo_m.rcapp.application.ble.ResponsePack;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEResponseReceiver;
import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant;
import com.accu_chek.solo_m.rcapp.application.ble.constant.ResponseAction;
import com.accu_chek.solo_m.rcapp.application.ble.response.AttributeChangeNotification;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyFloat;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

public class SetBolus extends BolusCommunicationManager
{
    private final String mDebugTag = "SetBolus";
    private SetBolusCallback mCallback = null;
    
    private SetBolus()
    {}
    
    private static volatile SetBolus instance = null;
    
    public static SetBolus getInstance()
    {
        if(null == instance)
        {
            instance = new SetBolus();
        }
        
        return instance;
    }

    /**
     * 
     *
     * @param result
     */
    
    @Override
    public void onRequestCompleted(SafetyBoolean result)
    {
        if(SafetyBoolean.TRUE == result)
        {
            Debug.printD(mDebugTag, " SetBolus OK");
        }
        else
        {
            Debug.printD(mDebugTag, " SetBolus NG");
        }
        
    }

    /**
     * 
     *
     * @param context
     * @param intent
     */
    
    @Override
    public void onReceive(Context context, Intent intent)
    {
        String action = intent.getAction();
        
        Debug.printD(mDebugTag, "onReceive Start");
        
        if (ResponseAction.CommandResponse.BT_ATTR_NOTIF_IND.equalsIgnoreCase(action))
        {            
            ResponsePack pack = intent.getParcelableExtra(BLEResponseReceiver.KEY_RESPONSE_PACK);            
            AttributeChangeNotification response = (AttributeChangeNotification) pack.getResponse();
            int result = response.getResult().get().intValue();
            ByteBuffer buff = ByteBuffer.wrap(response.getData().getByteArray()).order(ByteOrder.LITTLE_ENDIAN);
            short opCode = buff.getShort();
            
            
            if((SET_BOLUS_RES_OP == opCode) && (CommsConstant.E2E_Result.RESULT_OK == result))
            {
                short bolusID = buff.getShort();
                
                byte counter = buff.get();
                short crc = buff.getShort();
                
                if(mCallback!=null)
                {
                    mCallback.onSetBolusResponse(bolusID);
                    mCallback = null;
                }
                
                Debug.printD(mDebugTag, "AttributeChangeNotification Start");
                
                Debug.printD(mDebugTag, "opCode = " + String.format("0x%x", opCode));

                Debug.printD(mDebugTag, "bolusID = " + String.format("0x%x", bolusID));
                
                Debug.printD(mDebugTag, "counter = " + String.format("0x%x", counter));
                Debug.printD(mDebugTag, "crc = " + String.format("0x%x", crc));
                
                Debug.printD(mDebugTag, "AttributeChangeNotification End");
                
                context.unregisterReceiver(this);
            }            
            else if((RESPONSE_CODE_OP == opCode) && (CommsConstant.E2E_Result.RESULT_OK == result))
            {
                short requestOpCode = buff.getShort();
                byte value = buff.get();
                byte counter = buff.get();
                short crc = buff.getShort();
                
                if(mCallback!=null)
                {
                    mCallback.onResponseCode(requestOpCode, value);
                    mCallback = null;
                }
                
                Debug.printD(mDebugTag, "AttributeChangeNotification Start");
                
                Debug.printD(mDebugTag, "opCode = " + String.format("0x%x", opCode));
                Debug.printD(mDebugTag, "requestOpCode = " + String.format("0x%x", requestOpCode));
                Debug.printD(mDebugTag, "value = " + String.format("0x%x", value));
                Debug.printD(mDebugTag, "counter = " + String.format("0x%x", counter));
                Debug.printD(mDebugTag, "crc = " + String.format("0x%x", crc));
                
                Debug.printD(mDebugTag, "AttributeChangeNotification End");
                
                context.unregisterReceiver(this);
            }
            else
            {
                
            }
        }

        Debug.printD(mDebugTag, "onReceive End");
    }
    
    public void setStandardTotalBolus(Context context, SafetyFloat fastAmount, SafetyNumber<Integer> delay, SetBolusCallback callback)
    {
        mCallback = callback;
        setStandardTotalBolus(context, fastAmount, delay);
    }
    
    public void setStandardTotalBolus(Context context, SafetyFloat fastAmount, SafetyNumber<Integer> delay)
    {
        CommonUtils.objectCheck(context, fastAmount, delay);
        
        Float f = 0.0f;
        String s = "0.0";
        
        SafetyNumber<Integer> type = new SafetyNumber<Integer>(BOLUS_TYPE_FAST, -BOLUS_TYPE_FAST);
        Integer flag = SET_BOLUS_FLAGS_DELAY_TIME_PRESENT | SET_BOLUS_FLAGS_DELIVERY_REASON_CORRECTION; 
        SafetyNumber<Integer> flags = new SafetyNumber<Integer>(flag, -flag);
        SafetyFloat extendedAmount = new SafetyFloat(f, s);
        SafetyNumber<Integer> duration = new SafetyNumber<Integer>(0, 0);
        
        setBolus(context, flags, type, fastAmount, extendedAmount, duration, delay);     
        
        IntentFilter i = new IntentFilter();
        i.addAction(AttributeChangeNotification.class.getName());
        
        context.registerReceiver(this, i);
    }

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
// (R24914 2015-11-26 03:06:26 LuyaHuang)
// ----------------------------------------------------------------------------
// [NSM-2889]
// 1. Enhance SetBolus
// 2. Enhance BolusDatabaseCOntroller
// 3. Fix BolusAdviser bug
