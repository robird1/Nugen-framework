/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: BolusDefaultAdviceSettingsController
 * Brief: FIXME
 *
 * Create Date: 10/16/2015
 * $Revision: 23382 $
 * $Author: LuyaHuang $
 * $Id: GetActiveBolusIDs.java 23382 2015-11-05 08:22:35Z LuyaHuang $
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
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

public class GetActiveBolusIDs extends BolusCommunicationManager
{
    private final String mDebugTag = "GetActiveBolusIDs"; 
    
    private GetActiveBolusIDs()
    {}
    
    private static volatile GetActiveBolusIDs instance = null;
    
    public static GetActiveBolusIDs getInstance()
    {
        if(null == instance)
        {
            instance = new GetActiveBolusIDs();
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
            Debug.printD(mDebugTag, "GetActiveBolusIDs OK");
        }
        else
        {
            Debug.printD(mDebugTag, "GetActiveBolusIDs NG");
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
            
            
            if((GET_ACTIVE_BOLUSE_IDS_RES_OP == opCode) && (CommsConstant.E2E_Result.RESULT_OK == result))
            {
                byte bolusNumber = buff.get();
                int readCounter = bolusNumber;
                short bolus1 = 0;
                short bolus2 = 0;
                short bolus3 = 0;
                short bolus4 = 0;
                
                if(readCounter >  0)
                {
                    bolus1 = buff.getShort();
                    readCounter--;
                }
                
                if(readCounter >  0)
                {
                    bolus2 = buff.getShort();
                    readCounter--;
                }
                
                if(readCounter >  0)
                {
                    bolus3 = buff.getShort();
                    readCounter--;
                }
                
                if(readCounter >  0)
                {
                    bolus4 = buff.getShort();
                    readCounter--;
                }
                
                byte counter = buff.get();
                short crc = buff.getShort();
                
                Debug.printD(mDebugTag, "AttributeChangeNotification Start");
                
                Debug.printD(mDebugTag, "opCode = " + String.format("0x%x", opCode));
                
                Debug.printD(mDebugTag, "bolusNumber = " + String.format("0x%x", bolusNumber));
                Debug.printD(mDebugTag, "bolus1 = " + String.format("0x%x", bolus1));
                Debug.printD(mDebugTag, "bolus2 = " + String.format("0x%x", bolus2));
                Debug.printD(mDebugTag, "bolus3 = " + String.format("0x%x", bolus3));
                Debug.printD(mDebugTag, "bolus4 = " + String.format("0x%x", bolus4));
                
                Debug.printD(mDebugTag, "counter = " + String.format("0x%x", counter));
                Debug.printD(mDebugTag, "crc = " + String.format("0x%x", crc));
                
                Debug.printD(mDebugTag, "AttributeChangeNotification End");
                
                context.unregisterReceiver(this);
            }            
            else if((IDD_READER_RESPONSE_CODE_OP == opCode) && (CommsConstant.E2E_Result.RESULT_OK == result))
            {
                short requestOpCode = buff.getShort();
                byte value = buff.get();
                byte counter = buff.get();
                short crc = buff.getShort();
                
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
    
    
    public void getActiveBoluseIDList(Context context)
    {
        CommonUtils.objectCheck(context);
        
        getActiveBoluseIDs(context);
        
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
