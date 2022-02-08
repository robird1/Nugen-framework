/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.ContinuaCommandController.handler.PowerStatus
 * Brief: 
 *
 * Create Date: 2015/3/19
 * $Revision: 24579 $
 * $Author: kevenwu $
 * $Id: PowerStatusHandler.java 24579 2015-11-23 03:00:24Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.typehandler;

import org.json.JSONException;
import org.json.JSONObject;

import android.os.BatteryManager;

import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants.PowerManagerConstants;
import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.model.ParseUtils;
import com.accu_chek.solo_m.rcapp.application.exception.ArgumentErrorException;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.settinginterface.NugenSettingModel;

/**
 * This class is used to handle the command for getting the power status.
 */
public class PowerStatusHandler implements IContinuaCommandHandler
{
    enum ChargingStatus
    {
        /**
         * Indicates the charging is full.
         */
        CHARGING_FULL(0x80, BatteryManager.BATTERY_STATUS_FULL),
        
        /**
         * Indicates the charging is on going.
         */
        CHARGING_TRICKLE(0x40, BatteryManager.BATTERY_STATUS_CHARGING),
        
        /**
         * Indicates the charging is stopped.
         */
        CHARGING_OFF(0x20, BatteryManager.BATTERY_STATUS_NOT_CHARGING);
        
        /**
         * The bit position of Continua defined.
         */
        public final int VALUE_OF_CONTINUA;
        
        /**
         * The value of device defined.
         */
        public final int VALUE_OF_DEVICE;
        
        /**
         * To define the bit position and value of each item.
         * 
         * @param bit : The bit position value.
         *        Range: Refer to the definition of PowerStatus.
         *        Unit: Integer.
         *        Scaling: 1.
         * @param value : The value which is defined in device.
         *        Range: Refer to the definition of PowerStatus.
         *        Unit: Integer.
         *        Scaling: 1.
         *        
         * see BIT_OF_CONTINUA [out]
         * see VALUE_OF_DEVICE [out]        
         */
        private ChargingStatus(int bit, int value)
        {
            VALUE_OF_CONTINUA = bit;
            VALUE_OF_DEVICE = value;
        }
        
        /**
         * Return the corresponding instance of PowerStatus according to the input value.
         *
         * @param value : The value which is used to recognize the correct PowerStatus.
         *        Range: -2^31 to (2^31)-1.
         *        Unit: Integer.
         *        Scaling: 1.
         *        
         * return PowerStatus [out]: The corresponding enumeration.
         *        Range: Valid object of PowerStatus.
         *        Unit: PowerStatus.
         *        Scaling: 1.
         * 
         * throws ArgumentErrorException if the value cannot be recognized.
         */
        public static ChargingStatus getStatusByValue(int value) throws ArgumentErrorException
        {
            ChargingStatus result = null;
            
            for (ChargingStatus status : ChargingStatus.values())
            {
                int valueOfStatus = status.VALUE_OF_DEVICE;
                
                if (valueOfStatus == value)
                {
                    result = status;
                }
            }
            
            if (null == result)
            {
                throw new ArgumentErrorException("This value [" + value 
                        + "] is not correct!");
            }
            
            return result;
        }
    }
    
    enum PowerStatus
    {
        /**
         * Indicates the USB is plugged.
         */
        ON_MAINS(0x8000, BatteryManager.BATTERY_PLUGGED_USB),
        
        /**
         * Indicates the power is on battery.
         */
        ON_BATTERY(0x4000, 0);
        
        /**
         * The bit position of Continua defined.
         */
        public final int VALUE_OF_CONTINUA;
        
        /**
         * The value of device defined.
         */
        public final int VALUE_OF_DEVICE;
        
        /**
         * To define the bit position and value of each item.
         * 
         * @param bit : The bit position value.
         *        Range: Refer to the definition of PowerStatus.
         *        Unit: Integer.
         *        Scaling: 1.
         * @param value : The value which is defined in device.
         *        Range: Refer to the definition of PowerStatus.
         *        Unit: Integer.
         *        Scaling: 1.
         *        
         * see BIT_OF_CONTINUA [out]
         * see VALUE_OF_DEVICE [out]        
         */
        private PowerStatus(int bit, int value)
        {
            VALUE_OF_CONTINUA = bit;
            VALUE_OF_DEVICE = value;
        }
        
        /**
         * Return the corresponding instance of PowerStatus according to the input value.
         *
         * @param value : The value which is used to recognize the correct PowerStatus.
         *        Range: -2^31 to (2^31)-1.
         *        Unit: Integer.
         *        Scaling: 1.
         *        
         * return PowerStatus [out]: The corresponding enumeration.
         *        Range: Valid object of PowerStatus.
         *        Unit: PowerStatus.
         *        Scaling: 1.
         * 
         * throws ArgumentErrorException if the value cannot be recognized.
         */
        public static PowerStatus getStatusByValue(int value) throws ArgumentErrorException
        {
            PowerStatus result = null;
            
            for (PowerStatus status : PowerStatus.values())
            {
                int valueOfStatus = status.VALUE_OF_DEVICE;
                
                if (valueOfStatus == value)
                {
                    result = status;
                }
            }
            
            if (null == result)
            {
                throw new ArgumentErrorException("This value [" + value 
                        + "] is not correct!");
            }
            
            return result;
        }
    }
    
    /**
     * Get the device power status from setting model and transfers it to Continua Agent.
     *
     * @param commandSet : The instance of Continua command set.
     *        Range: Valid object of ContinuaCommandSet.
     *        Unit: ContinuaCommandSet.
     *        Scaling: 1.
     *        
     * return void [out]: None
     */    
    @Override
    public void handleCommand(ContinuaCommandSet commandSet)
    {
        SafetyString batteryInfo = NugenSettingModel.getString(
                commandSet.getController().getContext(),
                PowerManagerConstants.BATTERY_INFO);
        int status = 0;
        
        try
        {
            int bitPositionOfPluged = 0;
            int bitPositionOfCharging = 0;
            
            JSONObject info = new JSONObject(batteryInfo.getString());
            
            bitPositionOfPluged = PowerStatus.getStatusByValue(
                    info.getInt(BatteryManager.EXTRA_PLUGGED)).VALUE_OF_CONTINUA;
            bitPositionOfCharging = ChargingStatus.getStatusByValue(
                    info.getInt(BatteryManager.EXTRA_STATUS)).VALUE_OF_CONTINUA;
            
            status = bitPositionOfPluged | bitPositionOfCharging;
        }
        catch (JSONException e)
        {
            // Keep status = 0
            e.printStackTrace();
        }
        catch (ArgumentErrorException e)
        {
            // Keep status = 0
            e.printStackTrace();
        }
        finally
        {
            byte[] statusInBytes = ParseUtils.parseInt16(status);
            
            commandSet.getController().setSegmentDataToAgent(
                    ContinuaCommand.POWER_STATUS, ParseUtils.appendCRC(statusInBytes));
        }
    }
}
