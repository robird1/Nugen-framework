/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.ContinuaCommandController.segmenthandler.SingleSegmentHandler
 * Brief: 
 *
 * Create Date: 2015/3/30
 * $Revision: 20933 $
 * $Author: kevenwu $
 * $Id: SingleSegmentHandler.java 20933 2015-10-05 08:34:57Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.segmenthandler;

import java.util.List;

import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaConstants.ErrorCode;
import com.accu_chek.solo_m.rcapp.application.continua.model.ContinuaModel;
import com.accu_chek.solo_m.rcapp.application.continua.model.ParseUtils;
import com.accu_chek.solo_m.rcapp.application.continua.segment.AbstractContinuaSegment;
import com.accu_chek.solo_m.rcapp.application.continua.segment.SegmentParseUtils;
import com.accu_chek.solo_m.rcapp.application.continua.typehandler.IContinuaCommandHandler.ContinuaCommand;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;

/**
 * This class is used to handle the command to get single type data
 * (e.g. blood glucose, control solution, meal, carbohydrate and health events).
 */
public class SingleSegmentHandler implements IContinuaSegmentHandler
{
    /**
     * The target segment class.
     */
    private final Class<? extends AbstractContinuaSegment> mClass;
        
    /**
     * Constructs a new instance of SingleSegmentHandler and assign a target
     * segment class.
     * 
     * @param target : The target segment class.
     *        Range: Valid object which extends from AbstractContinuaSegment.
     *        Unit: Class.
     *        Scaling: 1.
     */
    public SingleSegmentHandler(Class<? extends AbstractContinuaSegment> target)
    {
        mClass = target;
    }
    
    /**
     * Delegate the input parameter to DataTransferHandler to retrieve data.
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
        DataTransferHandler.getInstance().startTransfer(mClass, commandSet);
    }

    /**
     * Delegate the input parameter to DataTransferHandler to retrieve data count.
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
        List<? extends AbstractContinuaSegment> segments = ContinuaModel
                .getDataByClass(commandSet.getController().getContext(), mClass);
        SafetyByteArray result = null;
        
        if (!segments.isEmpty())
        {
            result = SegmentParseUtils.parseSegmentCountData(
                    commandSet.getSegmentId(),
                    segments.get(0).getTimeStamp().get(), 
                    segments.get(segments.size() - 1).getTimeStamp().get(), 
                    segments.size());
        }
        else
        {
            result = SegmentParseUtils.parseSegmentCountData(
                    commandSet.getSegmentId(), 0, 0, 0);
        }
        
        try
        {
            mClass.newInstance().transferCountToAgent(commandSet, result);
        }
        catch (InstantiationException e)
        {
            byte[] error = ParseUtils.parseInt16(ErrorCode.ERROR_APPLICATION);
            
            commandSet.getController().setSegmentDataToAgent(
                    ContinuaCommand.PM_SEGMENT_ENTRY_COUNT, ParseUtils.appendCRC(error));
            e.printStackTrace();
        }
        catch (IllegalAccessException e)
        {
            byte[] error = ParseUtils.parseInt16(ErrorCode.ERROR_APPLICATION);
            
            commandSet.getController().setSegmentDataToAgent(
                    ContinuaCommand.PM_SEGMENT_ENTRY_COUNT, ParseUtils.appendCRC(error));
            e.printStackTrace();
        }
        finally
        {
            // Do nothing.
        }
    }
}
