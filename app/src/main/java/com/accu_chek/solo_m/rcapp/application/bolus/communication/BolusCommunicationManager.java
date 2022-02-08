/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: BolusCommunicationManager
 * Brief: FIXME
 *
 * Create Date: 11/05/2015
 * $Revision: 23914 $
 * $Author: LuyaHuang $
 * $Id: BolusCommunicationManager.java 23914 2015-11-12 02:27:37Z LuyaHuang $
 */
package com.accu_chek.solo_m.rcapp.application.bolus.communication;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;

import android.content.BroadcastReceiver;
import android.content.Context;

import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseCallback;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLERequestParameter;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SFloat;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyFloat;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;

public abstract class BolusCommunicationManager extends BroadcastReceiver implements ResponseCallback
{
    public static final int BOLUS_TYPE_FAST = 0x33;
    public static final int BOLUS_TYPE_EXTENDED = 0x3C;
    public static final int BOLUS_TYPE_MULTIWAVE = 0x55;
    
    public static final int BOLUS_VALUE_PROGRAMMED = 0x0F;
    public static final int BOLUS_VALUE_REMAINING = 0x33;
    public static final int BOLUS_VALUE_DELIVERED = 0x3C;
    
    public static final short RESPONSE_CODE_OP = 0x0F55;
    public static final int RESPONSE_CODE_VALUE_SIZE = 8;
    public static final byte RESPONSE_SUCCESS = 0x0F;
    public static final byte RESPONSE_OP_CODE_NOT_SUPPORTED = 0x70;
    public static final byte RESPONSE_INVALID_OPERAND = 0x71;
    public static final byte RESPONSE_PROCEDURE_NOT_COMPLETED = 0x72;
    public static final byte RESPONSE_PARAMETER_OUT_OF_RANGE = 0x73;
    public static final byte RESPONSE_PROCEDURE_NOT_APPLICABLE = 0x74;
    public static final byte RESPONSE_PLAUSIBILITY_CHECK_FAILED = 0x75;
    public static final byte RESPONSE_MAXIMUM_BOLUS_NUMBER_REACHED = 0x76;
    
    public static final short IDD_READER_RESPONSE_CODE_OP = 0x0303;
    
    public static final short SET_BOLUS_OP = 0x114B;
    public static final int SET_BOLUS_SIZE = 12;
    public static final byte SET_BOLUS_FLAGS_DELAY_TIME_PRESENT = 1;
    public static final byte SET_BOLUS_FLAGS_DELIVERY_REASON_CORRECTION  = 8;
    public static final byte SET_BOLUS_FLAGS_DELIVERY_REASON_MEAL = 0x10;
    
    public static final short SET_BOLUS_RES_OP = 0x1177;
    public static final int SET_BOLUS_RES_SIZE = 4;
    
    public static final short CANCEL_BOLUS_OP = 0x1178;
    public static final int CANCEL_BOLUS_SIZE = 4;
    
    public static final short CANCEL_BOLUS_RES_OP = 0x1187;
    public static final int CANCEL_BOLUS_RES_SIZE = 4;
    
    public static final short GET_AVAILABLE_BOLUSES_OP = 0x1188;
    public static final int GET_AVAILABLE_BOLUSES_SIZE = 2;
    
    public static final short GET_AVAILABLE_BOLUSES_RES_OP = 0x11B4;
    public static final int GET_AVAILABLE_BOLUSES_RES_SIZE = 6;
    
    public static final short GET_ACTIVE_BOLUSE_IDS_OP = 0x0330;
    public static final int GET_ACTIVE_BOLUSE_IDS_SIZE = 2;
    
    public static final short GET_ACTIVE_BOLUSE_IDS_RES_OP = 0x033F;
    public static final int GET_ACTIVE_BOLUSE_IDS_RES_SIZE = 3;
    
    public static final short GET_ACTIVE_BOLUSE_DELIVERY_OP = 0x0356;
    public static final int GET_ACTIVE_BOLUSE_DELIVERY_SIZE = 5;
    
    public static final short GET_ACTIVE_BOLUSE_DELIVERY_RES_OP = 0x0359;
    public static final int GET_ACTIVE_BOLUSE_DELIVERY_RES_SIZE = 12;
    
    protected final void setBolusDeliveryCommand(Context context, SafetyByteArray data, ResponseCallback callback)
    {
        byte isBonded = BLEController.getInstance(context).isBonded().getByte();
        byte isConnected = BLEController.getInstance(context).isConnected().getByte();
        BLERequestParameter parameter = new BLERequestParameter();
        parameter.setData(data);
        
        if((isBonded == SafetyBoolean.TRUE.getByte()) && (isConnected == SafetyBoolean.TRUE.getByte()))
        {
            BLEController.getInstance(context).sendIDDCommandCP(parameter, callback);
        }
       
    }
    
    protected final void setBolusStatusCommand(Context context, SafetyByteArray data, ResponseCallback callback)
    {
        byte isBonded = BLEController.getInstance(context).isBonded().getByte();
        byte isConnected = BLEController.getInstance(context).isConnected().getByte();
        BLERequestParameter parameter = new BLERequestParameter();
        parameter.setData(data);        
        
        if((isBonded == SafetyBoolean.TRUE.getByte()) && (isConnected == SafetyBoolean.TRUE.getByte()))
        {
            BLEController.getInstance(context).sendIDDStatusReaderCP(parameter, callback);
        }
        
    }
    
    protected final void setBolus(Context context, SafetyNumber<Integer> flags, SafetyNumber<Integer> type, SafetyFloat fastAmount, SafetyFloat extendedAmount, SafetyNumber<Integer> duration, SafetyNumber<Integer> delay)
    {
        
        byte[] data = null;
        SFloat insulinFast = new SFloat(fastAmount, 2);
        SFloat insulinExtened = new SFloat(extendedAmount, 2);
        
        float exValue = extendedAmount.getOriginal().floatValue();
        int zeroCompare = (int) (exValue * 100.0f);
        
        if(0 == zeroCompare)
        {
            insulinExtened = new SFloat((byte)0, (byte)0);
        }
        
        ByteBuffer buff = ByteBuffer.allocate(SET_BOLUS_SIZE);
        buff.order(ByteOrder.LITTLE_ENDIAN);
        buff.putShort(SET_BOLUS_OP);
        buff.put(flags.get().byteValue());
        buff.put(type.get().byteValue());
        buff.putShort(insulinFast.getValue().getOriginal().shortValue());
        buff.putShort(insulinExtened.getValue().getOriginal().shortValue());
        buff.putShort(duration.get().shortValue());
        buff.putShort(delay.get().shortValue());
            
        data = buff.array();
            
        setBolusDeliveryCommand(context, new SafetyByteArray(data,CRCTool.generateCRC16(data)), this);        
    }
    
    protected final void cancelBolus(Context context, SafetyNumber<Integer> bolusID)
    {
        byte[] data = null;
        ByteBuffer buff = ByteBuffer.allocate(CANCEL_BOLUS_SIZE);
        
        buff.order(ByteOrder.LITTLE_ENDIAN);
        buff.putShort(CANCEL_BOLUS_OP);
        buff.putShort(bolusID.get().shortValue());
        
        data = buff.array();
        
        setBolusDeliveryCommand(context, new SafetyByteArray(data,CRCTool.generateCRC16(data)), this); 
    }
    
    protected final void getAvailableBoluses(Context context)
    {
        byte[] data = null;
        ByteBuffer buff = ByteBuffer.allocate(GET_AVAILABLE_BOLUSES_SIZE);
        
        buff.order(ByteOrder.LITTLE_ENDIAN);
        buff.putShort(GET_AVAILABLE_BOLUSES_OP);
        
        data = buff.array();
        
        setBolusDeliveryCommand(context, new SafetyByteArray(data,CRCTool.generateCRC16(data)), this); 
    }
    
    protected final void getActiveBoluseIDs(Context context)
    {
        byte[] data = null;
        ByteBuffer buff = ByteBuffer.allocate(GET_ACTIVE_BOLUSE_IDS_SIZE);
        
        buff.order(ByteOrder.LITTLE_ENDIAN);
        buff.putShort(GET_ACTIVE_BOLUSE_IDS_OP);
        
        data = buff.array();
        
        setBolusStatusCommand(context, new SafetyByteArray(data,CRCTool.generateCRC16(data)), this); 
    }
    
    protected final void getActiveBoluseDelivery(Context context, SafetyNumber<Integer> bolusID, SafetyNumber<Integer> valueSelection)
    {
        byte[] data = null;
        ByteBuffer buff = ByteBuffer.allocate(GET_ACTIVE_BOLUSE_DELIVERY_SIZE);
        
        buff.order(ByteOrder.LITTLE_ENDIAN);
        buff.putShort(GET_ACTIVE_BOLUSE_DELIVERY_OP);
        buff.putShort(bolusID.get().shortValue());
        buff.put(valueSelection.get().byteValue());
        
        data = buff.array();
        
        setBolusStatusCommand(context, new SafetyByteArray(data,CRCTool.generateCRC16(data)), this); 
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
// [NSM-2889] Block Bolus Delivery when BLTE is no bonded and connected with Pump
