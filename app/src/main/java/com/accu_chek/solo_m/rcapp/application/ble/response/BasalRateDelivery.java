/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.ble.response.BasalRateDelivery
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

public class BasalRateDelivery
{
    public static final String KEY_CURRENT_BASAL_RATE_DELIVERY = "key_current_basal_rate_delivery";
    
    private SafetyChannel<Integer> mFlag = null;
    
    private SafetyChannel<Integer> mProfileNumber = null;
    
    private SFloat mCurrentValue = null;
    
    private SafetyChannel<Integer> mTBRType = null;
    
    private SFloat mTBRFactor = null;
    
    private SafetyChannel<Integer> mTBRDurationProgrammed = null;
    
    private SafetyChannel<Integer> mTBRDurationRemaining = null;
    
    public static BasalRateDelivery parseBasalRateDelivery(Context context, SafetyByteArray data)
    {        
        storeData(context, data);
        
        return parseDataFromArray(data);
    }
    
    public static BasalRateDelivery getCurrentBasalRateDelivery(Context context)
    {   
        BasalRateDelivery result = new BasalRateDelivery();
        
        SafetyString currentBasal = NugenGeneralModel.getString(context, KEY_CURRENT_BASAL_RATE_DELIVERY);
     
        if (null != currentBasal)
        {
            SafetyByteArray data = null;
            
            ByteArrayBuffer buffer = new ByteArrayBuffer(0);
            
            for (String each : currentBasal.getString().split(","))
            {
                buffer.append(Integer.parseInt(each));
            }
            
            data = new SafetyByteArray(buffer.toByteArray(), CRCTool.generateCRC16(buffer.toByteArray()));
            
            result = parseDataFromArray(data);
        }
        else
        {
            // Apply to coding standard.
        }
        
        return result;
    }
    
    private static BasalRateDelivery parseDataFromArray(SafetyByteArray data)
    {
        BasalRateDelivery result = new BasalRateDelivery();
        
        ByteBuffer buffer = ByteBuffer.wrap(data.getByteArray()).order(ByteOrder.LITTLE_ENDIAN);
        
        int flag = buffer.get();
        int profileNumber = buffer.get();
        
        result.mFlag = new SafetyChannel<Integer>(CommonUtils.encodeCH1Value(flag),
                CommonUtils.encodeCH2Value(flag));
        result.mProfileNumber = new SafetyChannel<Integer>(CommonUtils.encodeCH1Value(profileNumber),
                CommonUtils.encodeCH2Value(profileNumber));
        result.mCurrentValue = new SFloat(buffer.get(), buffer.get());
                
        if (buffer.hasRemaining())
        {
            int tbrType = buffer.get();
            byte tbrFactorLo = buffer.get();
            byte tbrFactorHi = buffer.get();
            int tbrDurationProgrammed = buffer.getShort();
            int tbrDurationRemaining = buffer.getShort();
            
            result.mTBRType = new SafetyChannel<Integer>(CommonUtils.encodeCH1Value(tbrType),
                    CommonUtils.encodeCH2Value(tbrType));
            result.mTBRFactor = new SFloat(tbrFactorLo, tbrFactorHi);
            result.mTBRDurationProgrammed = new SafetyChannel<Integer>(
                    CommonUtils.encodeCH1Value(tbrDurationProgrammed), 
                    CommonUtils.encodeCH2Value(tbrDurationProgrammed));
            result.mTBRDurationRemaining = new SafetyChannel<Integer>(
                    CommonUtils.encodeCH1Value(tbrDurationRemaining), 
                    CommonUtils.encodeCH2Value(tbrDurationRemaining));
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
        
        NugenGeneralModel.setString(context, KEY_CURRENT_BASAL_RATE_DELIVERY, result);
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
    
    public SafetyChannel<Integer> getProfileNumber()
    {
        SafetyChannel<Integer> result = null;
        
        if (null != mProfileNumber)
        {
            result = mProfileNumber;
        }
        else
        {
            int value = 0;
            
            result = new SafetyChannel<Integer>(CommonUtils.encodeCH1Value(value),
                    CommonUtils.encodeCH2Value(value));
        }
        
        return result;
    }
    
    public SFloat getCurrentValue()
    {
        SFloat result = null;
        
        if (null != mCurrentValue)
        {
            result = mCurrentValue;
        }
        else
        {
            byte value = 0;
            
            result = new SFloat(value, value);
        }
        
        return result;
    }
    
    public SafetyChannel<Integer> getTBRType()
    {
        SafetyChannel<Integer> result = null;
        
        if (null != mTBRType)
        {
            result = mTBRType;
        }
        else
        {
            int value = 0;
            
            result = new SafetyChannel<Integer>(CommonUtils.encodeCH1Value(value),
                    CommonUtils.encodeCH2Value(value));
        }
        
        return result;
    }
    
    public SFloat getTBRFactor()
    {
        SFloat result = null;
        
        if (null != mTBRFactor)
        {
            result = mTBRFactor;
        }
        else
        {
            byte value = 0;
            
            result = new SFloat(value, value);
        }
        
        return result;
    }
    
    public SafetyChannel<Integer> getTBRDurationProgrammed()
    {
        SafetyChannel<Integer> result = null;
        
        if (null != mTBRDurationProgrammed)
        {
            result = mTBRDurationProgrammed;
        }
        else
        {
            int value = 0;
            
            result = new SafetyChannel<Integer>(CommonUtils.encodeCH1Value(value),
                    CommonUtils.encodeCH2Value(value));
        }
        
        return result;
    }
    
    public SafetyChannel<Integer> getTBRDurationRemaining()
    {
        SafetyChannel<Integer> result = null;
        
        if (null != mTBRDurationRemaining)
        {
            result = mTBRDurationRemaining;
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
