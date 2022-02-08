/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.ContinuaCommandController.segmenthandler.BloodGlucoseHandler
 * Brief: 
 *
 * Create Date: 2015/3/23
 * $Revision: 24579 $
 * $Author: kevenwu $
 * $Id: AllExtendWithRPCHandler.java 24579 2015-11-23 03:00:24Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.segmenthandler;

import java.util.LinkedList;
import java.util.List;

import android.content.Context;

import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.model.ContinuaModel;
import com.accu_chek.solo_m.rcapp.application.continua.model.ParseUtils;
import com.accu_chek.solo_m.rcapp.application.continua.segment.AbstractContinuaSegment;
import com.accu_chek.solo_m.rcapp.application.continua.segment.glucose.BloodGlucose;
import com.accu_chek.solo_m.rcapp.application.continua.segment.glucose.ContextCarb;
import com.accu_chek.solo_m.rcapp.application.continua.segment.glucose.ContextMeal;
import com.accu_chek.solo_m.rcapp.application.continua.segment.glucose.ControlGlucoseRecord;
import com.accu_chek.solo_m.rcapp.application.continua.segment.glucose.ControlSolution;
import com.accu_chek.solo_m.rcapp.application.continua.segment.glucose.DeviceStatus;
import com.accu_chek.solo_m.rcapp.application.continua.segment.glucose.HealthEvent;
import com.accu_chek.solo_m.rcapp.application.continua.segment.glucose.NoteRecord;
import com.accu_chek.solo_m.rcapp.application.continua.segment.glucose.PatientRecord;
import com.accu_chek.solo_m.rcapp.application.continua.typehandler.IContinuaCommandHandler.ContinuaCommand;

/**
 * This class is used to handle the command to get all segment of extend with
 * RPC configuration.
 */
public class AllExtendWithRPCHandler implements IContinuaSegmentHandler
{
    /**
     * This is the override function of interface. It is an unused function.
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
        // This is the override function of interface. It is unused function.
    }

    /**
     * Get the count of all necessary data of extend with RPC configuration and 
     * transfers the data to Continua Agent via ContinuaCommandController.
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
        Context context = commandSet.getController().getContext();
        byte[] segmentSize = null;
        List<AbstractContinuaSegment> segments = new LinkedList<AbstractContinuaSegment>();
        
        segments.addAll(ContinuaModel.getDataByClass(context, BloodGlucose.class));
        segments.addAll(ContinuaModel.getDataByClass(context, ControlSolution.class));
        segments.addAll(ContinuaModel.getDataByClass(context, ContextMeal.class));
        segments.addAll(ContinuaModel.getDataByClass(context, ContextCarb.class));
        segments.addAll(ContinuaModel.getDataByClass(context, DeviceStatus.class));
        segments.addAll(ContinuaModel.getDataByClass(context, HealthEvent.class));
        segments.addAll(ContinuaModel.getDataByClass(context, PatientRecord.class));
        segments.addAll(ContinuaModel.getDataByClass(context, NoteRecord.class));
        segments.addAll(ContinuaModel.getDataByClass(context, ControlGlucoseRecord.class));
        
        segmentSize = ParseUtils.parseInt16(segments.size());
        
        commandSet.getController().setSegmentDataToAgent(ContinuaCommand.PM_TOTAL_ENTRY_COUNT, 
                ParseUtils.appendCRC(segmentSize));
    }
}