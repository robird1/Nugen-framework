/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.ReadBGReminders
 * Brief: 
 *
 * Create Date: 2015/7/1
 * $Revision: 24153 $
 * $Author: IvanHuang $
 * $Id: ReadBGReminders.java 24153 2015-11-16 05:16:05Z IvanHuang $
 */

package com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd;

import java.util.ArrayList;
import java.util.List;

import org.apache.http.util.ByteArrayBuffer;

import android.content.Context;

import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.IRPCCommandHandler;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCArgumentType;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCEventType;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCCommandInvocation;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCDataArguments;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCParseUtils;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.HammingDistance;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.settinginterface.NugenSettingModel;
import com.accu_chek.solo_m.rcapp.application.settinginterface.NugenUserSettingsConstants.UserSettingsKey;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;

public class ReadBGReminders implements IRPCCommandHandler
{
    
    /**
     * To read the high bG, low bG and after meal reminders from system and return to Agent as defined structure.
     * If the reminders can't be found, return application error to Agent.
     *
     * @param commandSet :The instance of Continua command set.
     *        Range: Valid object of ContinuaCommandSet.
     *        Unit: ContinuaCommandSet.
     *        Scaling: 1.
     * @param invocation : The data arguments of request RPC command.
     *        Range: Valid object of RPCCommandInvocation.
     *        Unit: RPCCommandInvocation.
     *        Scaling: 1.
     * 
     * return void [out]: None.
     */
    @Override
    public void handle(ContinuaCommandSet commandSet, RPCCommandInvocation invocation)
    {
        final short HIGH_BG = 0x0000;
        final short LOW_BG = 0x0001;
        final short AFTER_MEAL = 0x0002;
        
        SafetyByteArray response = null;
        List<RPCDataArguments> arguments = new ArrayList<RPCDataArguments>();
        
        Context context = commandSet.getController().getContext();
        
        SafetyNumber<Integer> status = null;
        SafetyNumber<Integer> threshold = null;
        SafetyNumber<Integer> duration = null;
        SafetyNumber<Integer> sound = null;
        
        
        status = NugenSettingModel.getInteger(context, UserSettingsKey.REMINDER_BG_AFTER_HIGH_STATUS);
        threshold = NugenSettingModel.getInteger(context, UserSettingsKey.REMINDER_BG_AFTER_HIGH_THRESHOLD);
        duration = NugenSettingModel.getInteger(context, UserSettingsKey.REMINDER_BG_AFTER_HIGH_DURATION);
        sound = NugenSettingModel.getInteger(context, UserSettingsKey.REMINDER_BG_AFTER_HIGH_SOUND);
        
        arguments.add(collectReminderData(HIGH_BG, status, threshold, duration, sound));
        
        
        status = NugenSettingModel.getInteger(context, UserSettingsKey.REMINDER_BG_AFTER_MEAL_STATUS);
        threshold = new SafetyNumber<Integer>(0, 0);
        duration = NugenSettingModel.getInteger(context, UserSettingsKey.REMINDER_BG_AFTER_MEAL_DURATION);
        sound = NugenSettingModel.getInteger(context, UserSettingsKey.REMINDER_BG_AFTER_MEAL_SOUND);
        
        arguments.add(collectReminderData(AFTER_MEAL, status, threshold, duration, sound));
        
        
        status = NugenSettingModel.getInteger(context, UserSettingsKey.REMINDER_BG_AFTER_LOW_STATUS);
        threshold = NugenSettingModel.getInteger(context, UserSettingsKey.REMINDER_BG_AFTER_LOW_THRESHOLD);
        duration = NugenSettingModel.getInteger(context, UserSettingsKey.REMINDER_BG_AFTER_LOW_DURATION);
        sound = NugenSettingModel.getInteger(context, UserSettingsKey.REMINDER_BG_AFTER_LOW_SOUND);
        
        arguments.add(collectReminderData(LOW_BG, status, threshold, duration, sound));
        
        
        response = RPCParseUtils.generateRPCResponse(RPCEventType.MDC_NOTI_RPC_COMMAND_RESPONSE, 
                arguments.toArray(new RPCDataArguments[arguments.size()]));
        
        commandSet.getController().setSegmentDataOfRPC(response);
    }
    
    /**
     * Convert the reminder property to Continua defined structure.
     * Structure RpcBgReminder:
     * {
     * Type,
     * Status,
     * Threshold,
     * Duration,
     * Sound
     * }
     * 
     * @param type : The type of reminder.
     *        Range: BG_HIGH, BG_LOW and AFTER_MEAL.
     *        Unit: Short.
     *        Scaling: 1.
     * @param status : The activation status.
     *        Range: Valid object of SafetyNumber<Integer>.
     *        Unit: SafetyNumber<Integer>.
     *        Scaling: 1.
     * @param threshold : The threshold of this reminder.
     *        Range: Valid object of SafetyNumber<Integer>.
     *        Unit: SafetyNumber<Integer>.
     *        Scaling: 1.
     * @param duration : The duration time of this reminder.
     *        Range: Valid object of SafetyNumber<Integer>.
     *        Unit: SafetyNumber<Integer>.
     *        Scaling: 1.
     * @param sound : The sound of this reminder.
     *        Range: Valid object of SafetyNumber<Integer>.
     *        Unit: SafetyNumber<Integer>.
     *        Scaling: 1.
     * 
     * return RPCDataArguments [out]: The argument contains all necessary data of reminders.
     */
    protected RPCDataArguments collectReminderData(short type, SafetyNumber<Integer> status,
            SafetyNumber<Integer> threshold, SafetyNumber<Integer> duration, SafetyNumber<Integer> sound)
    {        
        final int MINUTES_OF_HOUR = 60;
        final int ENABLE = 1;
        final int DISABLE = 2;
        
        RPCDataArguments result = new RPCDataArguments();
        ByteArrayBuffer buffer = new ByteArrayBuffer(0);
        byte[] array = null;
        
        byte[] typeInBytes = CRCTool.getBytes(type);
        
        CommonUtils.objectCheck(status, threshold, duration, sound);
        
        buffer.append(typeInBytes, 0, typeInBytes.length);
        
        switch (status.get())
        {
        case HammingDistance.SAFETY_NUMBER_VALUE_0063 :
            
            buffer.append(0);
            buffer.append(DISABLE);
            
            break;
        default :
            
            buffer.append(0);
            buffer.append(ENABLE);
            
            break;
        }
        
        
        // Threshold should be 2 bytes integer.
        for (byte each : CRCTool.getBytes(threshold.get().shortValue()))
        {
            buffer.append(each);
        }
        
        
        // Duration is in minutes, it should be converted to Continua defined structure.
        buffer.append(duration.get() / MINUTES_OF_HOUR);
        buffer.append(duration.get() % MINUTES_OF_HOUR);
        buffer.append(0);
        buffer.append(0);

        
        for (byte each : CRCTool.getBytes(sound.get().shortValue()))
        {
            buffer.append(each);
        }
        
        array = buffer.toByteArray();
        
        result.setType(RPCArgumentType.RPC_ARG_TYPE_BG_REMINDER);
        result.setLength(new SafetyNumber<Integer>(array.length, -array.length));
        result.setValue(new SafetyByteArray(array, CRCTool.generateCRC16(array)));

        return result;
    }
    
}
