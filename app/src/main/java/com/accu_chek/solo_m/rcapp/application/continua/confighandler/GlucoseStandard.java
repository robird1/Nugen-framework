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
 * $Id: GlucoseStandard.java 20933 2015-10-05 08:34:57Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.confighandler;

import android.content.Context;

import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaConstants.ErrorCode;
import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaConstants.GlucoseSegmentId;
import com.accu_chek.solo_m.rcapp.application.continua.model.ParseUtils;
import com.accu_chek.solo_m.rcapp.application.continua.segment.glucose.BloodGlucose;
import com.accu_chek.solo_m.rcapp.application.continua.segment.glucose.ControlSolution;
import com.accu_chek.solo_m.rcapp.application.continua.segmenthandler.AllStandardHandler;
import com.accu_chek.solo_m.rcapp.application.continua.segmenthandler.IContinuaSegmentHandler;
import com.accu_chek.solo_m.rcapp.application.continua.segmenthandler.SingleSegmentHandler;
import com.accu_chek.solo_m.rcapp.application.continua.typehandler.IContinuaCommandHandler.ContinuaCommand;
import com.accu_chek.solo_m.rcapp.application.exception.ArgumentErrorException;
import com.accu_chek.solo_m.rcapp.data.nugendata.DatabaseModel;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData.UrlType;

/**
 * This class is used to handle the command of Glucose Standard.
 */
public class GlucoseStandard implements IContinuaConfigHandler
{    
    /**
     * The Segment of glucose standard configuration.
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
         * The instance of get all segment.
         */
        ALL(GlucoseSegmentId.ALL, new AllStandardHandler());
        
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
         *        Range: Valid object of IContinuaSegmnetHandler.
         *        Unit: IContinuaSegmentHandler.
         *        Scaling: 1.
         *        
         * see mSegmendId [out]
         * see mHandler [out]
         */
        private Segment(int segmentId, IContinuaSegmentHandler handler)
        {
            mSegmentId = segmentId;
            mHandler = handler;
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
         * Return the segment id of this segment.
         *
         * see mSegmentId [out]
         *
         * return int [out]: The segment id value of this segment.
         *         Range: Refer to this enumeration definition.
         *         Unit: Integer.
         *         Scaling: 1.
         */
        public int getSegmentHandlerId()
        {
            return mSegmentId;
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
        public static Segment getSegment(int segmentId) 
                throws ArgumentErrorException
        {
            Segment result = null;
            
            for (Segment handler : Segment.values())
            {
                int valueOfId = handler.getSegmentHandlerId();
                
                if (segmentId == valueOfId)
                {
                    result = handler;
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
     * Clear all segment data in Glucose standard.
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
    }
}
