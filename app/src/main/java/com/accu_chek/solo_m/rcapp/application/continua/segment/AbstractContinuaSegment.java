/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.ContinuaCommandController.data.IContinuaData
 * Brief: 
 *
 * Create Date: 2015/3/19
 * $Revision: 23982 $
 * $Author: kevenwu $
 * $Id: AbstractContinuaSegment.java 23982 2015-11-12 09:22:48Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.segment;

import java.util.List;

import android.content.ContentValues;

import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.data.operationhandler.AbstractTable;

/**
 * This class is used to implement a Continua segment object, and provide a
 * function to convert the data to corresponding byte array structure.
 */
public abstract class AbstractContinuaSegment extends AbstractTable
{
    /**
     * Transfer the input data to Agent and return the transferred count.
     *
     * @param commandSet : The instance of Continua command set.
     *        Range: Valid object of ContinuaCommandSet.
     *        Unit: ContinuaCommandSet.
     *        Scaling: 1.
     * @param data : The required segment data.
     *        Range: Valid object of List.
     *        Unit: List.
     *        Scaling: 1.
     * 
     * return int [out]: The transferred count of segment data.
     *        Range: 0 to (2^31)-1.
     *        Unit: Integer.
     *        Scaling: 1.
     */
    public abstract int transferDataToAgent(ContinuaCommandSet commandSet,
            List<? extends AbstractContinuaSegment> data);
    
    /**
     * Transfer the data count to Agent.
     *
     * @param commandSet : The instance of Continua command set.
     *        Range: Valid object of ContinuaCommandSet.
     *        Unit: ContinuaCommandSet.
     *        Scaling: 1.
     * @param countData : The required segment count data.
     *        Range: Valid object of SafetyByteArray.
     *        Unit: SafetyByteArray.
     *        Scaling: 1.
     * 
     * return void [out]: None.
     */
    public abstract void transferCountToAgent(ContinuaCommandSet commandSet,
            SafetyByteArray countData);
    
    /**
     * Return the byte array depends on each segment structure.
     *
     * return SafetyByteArray [out]: The byte array data of target structure.
     *         Range: Refer to each segment structure.
     *         Unit: SafetyByteArray.
     *         Scaling: 1.
     */
    public abstract SafetyByteArray generateBytes();
    
    /**
     * Return the specified select type of SQL for target data.
     *
     * return IQuerySelectType [out]: The object contains the specified query command.
     *         Range: Valid object of IQuerySelectType.
     *         Unit: IQuerySelectType.
     *         Scaling: 1.
     */
    public abstract IQuerySelectType getSelectType();
    
    /**
     * Return the time stamp of this data.
     *
     * return SafetyNumber<Long> [out]: The time stamp of this data.
     *         Range: Valid object of SafetyNumber.
     *         Unit: Millisecond.
     *         Scaling: 1.
     */
    public abstract SafetyNumber<Long> getTimeStamp();

    /**
     * Abstract function of AbstractTable. Unused function.
     */    
    @Override
    protected void getDataFromContentValue(ContentValues values)
    {
        // Unused function because of this function is only used to insert or 
        // update database.
    }

    /**
     * Abstract function of AbstractTable. Unused function.
     */
    @Override
    public String getPrimaryKeyName()
    {
        // Unused function because of this function is only used to update database.
        return null;
    }
}
