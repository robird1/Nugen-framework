/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.ContinuaCommandController.devconfig.GlucoseStandard
 * Brief: 
 *
 * Create Date: 2015/3/23
 * $Revision: 20933 $
 * $Author: kevenwu $
 * $Id: GlucoseExtendWithoutRPC.java 20933 2015-10-05 08:34:57Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.confighandler;

import android.content.Context;

import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaConstants.ErrorCode;
import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaConstants.GlucoseSegmentId;
import com.accu_chek.solo_m.rcapp.application.continua.model.ParseUtils;
import com.accu_chek.solo_m.rcapp.application.continua.segment.glucose.BloodGlucose;
import com.accu_chek.solo_m.rcapp.application.continua.segment.glucose.ContextCarb;
import com.accu_chek.solo_m.rcapp.application.continua.segment.glucose.ContextMeal;
import com.accu_chek.solo_m.rcapp.application.continua.segment.glucose.ControlSolution;
import com.accu_chek.solo_m.rcapp.application.continua.segment.glucose.DeviceStatus;
import com.accu_chek.solo_m.rcapp.application.continua.segmenthandler.AllExtendWithoutRPCHandler;
import com.accu_chek.solo_m.rcapp.application.continua.segmenthandler.IContinuaSegmentHandler;
import com.accu_chek.solo_m.rcapp.application.continua.segmenthandler.SingleSegmentHandler;
import com.accu_chek.solo_m.rcapp.application.continua.typehandler.IContinuaCommandHandler.ContinuaCommand;
import com.accu_chek.solo_m.rcapp.application.exception.ArgumentErrorException;
import com.accu_chek.solo_m.rcapp.data.nugendata.DatabaseModel;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData.UrlType;

/**
 * This class is used to handle the command of Glucose Extend without RPC.
 */
public class GlucoseExtendWithoutRPC implements IContinuaConfigHandler
{
    
    /**
     * The Segment of glucose extend without RPC configuration.
     */
    enum Segment
    {
        /**
         * The instance of blood glucose segment.
         */
        BLOOD_GLUCOSE(GlucoseSegmentId.BLOOD_GLUCOSE, 
                new SingleSegmentHandler(BloodGlucose.class)),
        
        /**
         * The instance of control solution segment.
         */
        CONTROL_SOLUTION(GlucoseSegmentId.CONTROL_SOLUTION, 
                new SingleSegmentHandler(ControlSolution.class)),
        
        /**
         * The instance of device status segment.
         */
        DEVICE_STATUS(GlucoseSegmentId.DEVICE_STATUS, 
                new SingleSegmentHandler(DeviceStatus.class)),
        
        /**
         * The instance of context meal segment.
         */
        CONTEXT_MEAL(GlucoseSegmentId.CONTEXT_MEAL, 
                new SingleSegmentHandler(ContextMeal.class)),
        
        /**
         * The instance of context carbohydrate segment.
         */
        CONTEXT_CARB(GlucoseSegmentId.CONTEXT_CARBOHYDRATE,
                new SingleSegmentHandler(ContextCarb.class)),
        
        /**
         * The instance of get all segment.
         */
        ALL(GlucoseSegmentId.ALL, new AllExtendWithoutRPCHandler());
        
        /**
         * The segment id of this segment handler.
         */
        private final int mSegmentId;
        
        /**
         * The segment handler of this segment id.
         */
        private IContinuaSegmentHandler mHandler = null;
        
        /**
         * Constructs a new Segment and assign the segment id and segment handler.
         * 
         * @param segmentId : The segment id of this segment.
         *        Range: Refer to this enumeration definition.
         *        Unit: Integer.
         *        Scaling: 1.
         * @param handler : The segment handler of this segment.
         *        Range: Refer to this enumeration definition.
         *        Unit: IContinuaSegmentHandler.
         *        Scaling: 1.
         *        
         * see mSegmentId [out]
         * see mHandler [out]        
         */
        private Segment(int segmentId, IContinuaSegmentHandler handler)
        {
            mSegmentId = segmentId;
            mHandler = handler;
        }
        
        /**
         * Return the segment id of this segment.
         *
         * see mSegmentId [out]
         *
         * return int [out]: The segment id value of this segment.
         *         Range: Refer to this enumeration definition.
         *         Unit: Integer.
         *         Scaling: 1.
         */
        public int getSegmentId()
        {
            return mSegmentId;
        }
        
        /**
         * Return the segment handler of this enumeration.
         *
         * see mHandler [out]
         *
         * return IContinuaSegmentHandler [out]: Segment handler of this enumeration.
         *        Range: Valid object of IContinuaSegmentHandler.
         *        Unit: IContinuaSegmentHandler.
         *        Scaling: 1.
         */
        public IContinuaSegmentHandler getHandler()
        {
            return mHandler;
        }
        
        /**
         * Return the Segment by input segment id. If the id is not contained in
         * this enumeration, this function will throw ArgumentErrorException.
         *
         * @param segmentId : The input segment id.
         *        Range: Refer to this enumeration definition.
         *        Unit: Integer.
         *        Scaling: 1.
         * 
         * return Segment [out]: The instance of Segment.
         *         Range: Refer to this enumeration definition.
         *         Unit: Segment.
         *         Scaling: 1.
         *         
         * throws ArgumentErrorException if the segment id is not supported. 
         */
        public static Segment getSegment(int segmentId) throws ArgumentErrorException
        {
            Segment result = null;
            
            for (Segment segment : Segment.values())
            {
                int valueOfId = segment.getSegmentId();
                
                if (segmentId == valueOfId)
                {
                    result = segment;
                }
            }
            
            if (null == result)
            {
                throw new ArgumentErrorException(
                        "This segment id: " + segmentId + "is not supported.");
            }
            
            return result;
        }
    }
    
    /**
     * According to the segment id to delegate the command to corresponding
     * Segment handler.
     *
     * @param commandSet : The instance of Continua command set.
     *        Range: Valid object of ContinuaCommandSet.
     *        Unit: ContinuaCommandSet.
     *        Scaling: 1.
     *        
     * return void [out]: None
     */    
    @Override
    public void getData(ContinuaCommandSet commandSet)
    {
        try
        {
            Segment.getSegment(commandSet.getSegmentId()).getHandler()
                    .getData(commandSet);
        }
        catch (ArgumentErrorException e)
        {
            byte[] error = ParseUtils.parseInt16(ErrorCode.COMMAND_NOT_SUPPORTED);
            
            commandSet.getController().setSegmentDataToAgent(
                    ContinuaCommand.PM_SEGMENT_ENTRY, ParseUtils.appendCRC(error));
            e.printStackTrace();
        }
        finally
        {
            // Do nothing.
        }
    }

    /**
     * According to the segment id to delegate the command to corresponding
     * Segment handler.
     *
     * @param commandSet : The instance of Continua command set.
     *        Range: Valid object of ContinuaCommandSet.
     *        Unit: ContinuaCommandSet.
     *        Scaling: 1.
     *        
     * return void [out]: None
     */ 
    @Override
    public void getCount(ContinuaCommandSet commandSet)
    {
        try
        {
            Segment.getSegment(commandSet.getSegmentId()).getHandler()
                    .getCount(commandSet);
        }
        catch (ArgumentErrorException e)
        {
            byte[] error = ParseUtils.parseInt16(ErrorCode.COMMAND_NOT_SUPPORTED);
            
            commandSet.getController().setSegmentDataToAgent(
                    ContinuaCommand.PM_SEGMENT_ENTRY, ParseUtils.appendCRC(error));
            e.printStackTrace();
        }
        finally
        {
            // Do nothing.
        }
    }

    /**
     * Clear all segment data in Glucose extend without RPC configuration.
     *
     * @param commandSet : The instance of Continua command set.
     *        Range: Valid object of ContinuaCommandSet.
     *        Unit: ContinuaCommandSet.
     *        Scaling: 1.
     *        
     * return void [out]: None        
     */    
    @Override
    public void clearAllSegment(ContinuaCommandSet commandSet)
    {
        Context context = commandSet.getController().getContext();
        
        new DatabaseModel(UrlType.bgUri).deleteData(context, null);
        new DatabaseModel(UrlType.cgUri).deleteData(context, null);
        new DatabaseModel(UrlType.logBookUri).deleteData(context, null);
    }
}
