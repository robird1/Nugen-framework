/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.ble.response.BolusDelivery
 * Brief: 
 *
 * Create Date: 2015/12/1
 * $Revision: $
 * $Author: $
 * $Id: $
 */

package com.accu_chek.solo_m.rcapp.application.ble.response;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;

import org.apache.http.util.ByteArrayBuffer;

import android.content.Context;

import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SFloat;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyChannel;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.setting.NugenGeneralModel;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;

public class BolusDelivery
{

    private static final String KEY_CURRENT_BOLUS_DELIVERY = "key_current_bolus_delivery";
    
    private SafetyChannel<Integer> mFlag = null;
    
    private SafetyChannel<Integer> mBolusId = null;
    
    private SafetyChannel<Integer> mBolusType = null;
    
    private SFloat mBolusAmount = null;
    
    private SafetyChannel<Integer> mDuration = null;
    
    private SafetyChannel<Integer> mDelayTime = null;
    
    public static BolusDelivery parseBolusDelivery(Context context, SafetyByteArray data)
    {
        storeData(context, data);
        
        return parseDataFromArray(data);
    }
    
    public static BolusDelivery getCurrentBolusDelivery(Context context)
    {
        BolusDelivery result = new BolusDelivery();
        
        SafetyString currentBasal = NugenGeneralModel.getString(context, KEY_CURRENT_BOLUS_DELIVERY);
     
        if (null != currentBasal)
        {            
            ByteArrayBuffer buffer = new ByteArrayBuffer(0);
            
            for (String each : currentBasal.getString().split(","))
            {
                buffer.append(Integer.parseInt(each));
            }
            
            result = parseDataFromArray(new SafetyByteArray(buffer.toByteArray(), 
                    CRCTool.generateCRC16(buffer.toByteArray())));
        }
        else
        {
            // Apply to coding standard.
        }   
        
        return result;
    }
    
    private static BolusDelivery parseDataFromArray(SafetyByteArray data)
    {
        BolusDelivery result = new BolusDelivery();
        
        ByteBuffer buffer = ByteBuffer.wrap(data.getByteArray()).order(ByteOrder.LITTLE_ENDIAN);
        
        int flag = buffer.get();
        int bolusId = buffer.getShort();
        int bolusType = buffer.get();
        byte amountLo = buffer.get();
        byte amountHi = buffer.get();
        int duration = buffer.getShort();
        
        result.mFlag = new SafetyChannel<Integer>(CommonUtils.encodeCH1Value(flag),
                CommonUtils.encodeCH2Value(flag));
        result.mBolusId = new SafetyChannel<Integer>(CommonUtils.encodeCH1Value(bolusId),
                CommonUtils.encodeCH2Value(bolusId));
        result.mBolusType = new SafetyChannel<Integer>(CommonUtils.encodeCH1Value(bolusType),
                CommonUtils.encodeCH2Value(bolusType));
        result.mBolusAmount = new SFloat(amountLo, amountHi);
        result.mDuration = new SafetyChannel<Integer>(CommonUtils.encodeCH1Value(duration),
                CommonUtils.encodeCH2Value(duration));
        
        if (buffer.hasRemaining())
        {
            int delayTime = buffer.getShort();
            
            result.mDelayTime = new SafetyChannel<Integer>(CommonUtils.encodeCH1Value(delayTime),
                    CommonUtils.encodeCH2Value(delayTime));
        }
        else
        {
            // Apply to coding standard.
        }
        
        return result;
    }
    
    private static void storeData(Context context, SafetyByteArray data)
    {
        SafetyString result = new SafetyString();        
        StringBuilder builder = new StringBuilder();
        
        for (byte each : data.getByteArray())
        {
            builder.append(each);
            builder.append(",");
        }
        
        result.set(builder.toString(), CRCTool.generateCRC16(builder.toString().getBytes()));
        
        NugenGeneralModel.setString(context, KEY_CURRENT_BOLUS_DELIVERY, result);
    }
    
    public SafetyChannel<Integer> getFlag()
    {
        SafetyChannel<Integer> result = null;
        
        if (null != mFlag)
        {
            result = mFlag;
        }
        else
        {
            int value = 0;
            
            result = new SafetyChannel<Integer>(CommonUtils.encodeCH1Value(value),
                    CommonUtils.encodeCH2Value(value));
        }
        
        return result;
    }
    
    public SafetyChannel<Integer> getBolusId()
    {
        SafetyChannel<Integer> result = null;
        
        if (null != mBolusId)
        {
            result = mBolusId;
        }
        else
        {
            int value = 0;
            
            result = new SafetyChannel<Integer>(CommonUtils.encodeCH1Value(value),
                    CommonUtils.encodeCH2Value(value));
        }
        
        return result;
    }
    
    public SafetyChannel<Integer> getBolusType()
    {
        SafetyChannel<Integer> result = null;
        
        if (null != mBolusType)
        {
            result = mBolusType;
        }
        else
        {
            int value = 0;
            
            result = new SafetyChannel<Integer>(CommonUtils.encodeCH1Value(value),
                    CommonUtils.encodeCH2Value(value));
        }
        
        return result;
    }
    
    public SFloat getBolusAmount()
    {
        SFloat result = null;
        
        if (null != mBolusAmount)
        {
            result = mBolusAmount;
        }
        else
        {
            byte value = 0;
            
            result = new SFloat(value, value);
        }
        
        return result;
    }
    
    public SafetyChannel<Integer> getDuration()
    {
        SafetyChannel<Integer> result = null;
        
        if (null != mDuration)
        {
            result = mDuration;
        }
        else
        {
            int value = 0;
            
            result = new SafetyChannel<Integer>(CommonUtils.encodeCH1Value(value),
                    CommonUtils.encodeCH2Value(value));
        }
        
        return result;
    }
    
    public SafetyChannel<Integer> getDelayTime()
    {
        SafetyChannel<Integer> result = null;
        
        if (null != mDelayTime)
        {
            result = mDelayTime;
        }
        else
        {
            int value = 0;
            
            result = new SafetyChannel<Integer>(CommonUtils.encodeCH1Value(value),
                    CommonUtils.encodeCH2Value(value));
        }
        
        return result;
    }
    
}
