/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: GetActiveBolusDelivery
 * Brief: FIXME
 *
 * Create Date: 11/05/2015
 * $Revision: 23382 $
 * $Author: LuyaHuang $
 * $Id: GetActiveBolusDelivery.java 23382 2015-11-05 08:22:35Z LuyaHuang $
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
import com.accu_chek.solo_m.rcapp.application.safety.SFloat;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

public class GetActiveBolusDelivery extends BolusCommunicationManager
{
    private final String mDebugTag = "GetActiveBolusDelivery";
    
    private GetActiveBolusDelivery()
    {}
    
    private static volatile GetActiveBolusDelivery instance = null;
    
    public static GetActiveBolusDelivery getInstance()
    {
        if(null == instance)
        {
            instance = new GetActiveBolusDelivery();
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
            Debug.printD(mDebugTag, "GetActiveBolusDelivery OK");
        }
        else
        {
            Debug.printD(mDebugTag, "GetActiveBolusDelivery NG");
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
            
            
            if((GET_ACTIVE_BOLUSE_DELIVERY_RES_OP == opCode) && (CommsConstant.E2E_Result.RESULT_OK == result))
            {
                byte flags = buff.get();
                short bolusID = buff.getShort();
                byte type = buff.get();
                byte amountLSB = buff.get();
                byte amountMSB = buff.get();
                SFloat amount = new SFloat(amountLSB, amountMSB);
                short duration = buff.getShort();
                short delay = buff.getShort();
                
                byte counter = buff.get();
                short crc = buff.getShort();
                
                Debug.printD(mDebugTag, "AttributeChangeNotification Start");
                
                Debug.printD(mDebugTag, "opCode = " + String.format("0x%x", opCode));
                
                Debug.printD(mDebugTag, "flags = " + String.format("0x%x", flags));
                Debug.printD(mDebugTag, "bolusID = " + String.format("0x%x", bolusID));
                Debug.printD(mDebugTag, "type = " + String.format("0x%x", type));
                Debug.printD(mDebugTag, "amount = " + String.format("%.2f", amount.toDouble().get().floatValue()));
                Debug.printD(mDebugTag, "duration = " + String.format("%d", duration));
                Debug.printD(mDebugTag, "delay = " + String.format("%d", delay));
                
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
    
    
    public void getActiveBoluseDeliveryRemaining(Context context, SafetyNumber<Integer> bolusID)
    {
        CommonUtils.objectCheck(context);
        
        SafetyNumber<Integer> valueSelection = new SafetyNumber<Integer>(BOLUS_VALUE_REMAINING, -BOLUS_VALUE_REMAINING);
        
        getActiveBoluseDelivery(context, bolusID, valueSelection);
        
        IntentFilter i = new IntentFilter();
        i.addAction(AttributeChangeNotification.class.getName());
        
        context.registerReceiver(this, i);
    }
    
    public void getActiveBoluseDeliveryProgrammed(Context context, SafetyNumber<Integer> bolusID)
    {
        CommonUtils.objectCheck(context);
        
        SafetyNumber<Integer> valueSelection = new SafetyNumber<Integer>(BOLUS_VALUE_PROGRAMMED, -BOLUS_VALUE_PROGRAMMED);
        
        getActiveBoluseDelivery(context, bolusID, valueSelection);
        
        IntentFilter i = new IntentFilter();
        i.addAction(AttributeChangeNotification.class.getName());
        
        context.registerReceiver(this, i);
    }
    
    public void getActiveBoluseDeliveryDelivered(Context context, SafetyNumber<Integer> bolusID)
    {
        CommonUtils.objectCheck(context);
        
        SafetyNumber<Integer> valueSelection = new SafetyNumber<Integer>(BOLUS_VALUE_DELIVERED, -BOLUS_VALUE_DELIVERED);
        
        getActiveBoluseDelivery(context, bolusID, valueSelection);
        
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
