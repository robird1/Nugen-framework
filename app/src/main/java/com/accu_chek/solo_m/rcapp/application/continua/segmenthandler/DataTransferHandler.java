/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 * 
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.segmenthandler.
 * DataTransferHandler
 * Brief:
 * 
 * Create Date: 2015/7/20
 * $Revision: 23982 $
 * $Author: kevenwu $
 * $Id: DataTransferHandler.java 23982 2015-11-12 09:22:48Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.segmenthandler;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import android.content.Context;

import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.model.ContinuaModel;
import com.accu_chek.solo_m.rcapp.application.continua.segment.AbstractContinuaSegment;

public class DataTransferHandler
{
    
    /**
     * The maximum size of transferring data to Continua Manager once.
     */
    public static final int APDU_TX_SIZE = 800;
    
    /**
     * The instance of this object.
     */
    private static final DataTransferHandler mInstance = new DataTransferHandler();

    /**
     * The segment data map which use to store the segment data not yet
     * transferred.
     */
    private Map<Class<?>, TransferredData> mSegmentDataMap = new HashMap<Class<?>, TransferredData>();

    private static class TransferredData
    {
        /**
         * The position of current transferred data.
         */
        private int mPosition = 0;

        /**
         * The list of target segment data.
         */
        private List<? extends AbstractContinuaSegment> mTransferredData = null;
    }

    /**
     * This construct uses private modifier to avoid other class accessing.
     */
    private DataTransferHandler()
    {
        // Use for the singleton pattern.
    }

    /**
     * Return the instance of this object.
     * 
     * return DataTransferHandler [out]: The singleton object instance.
     */
    public static DataTransferHandler getInstance()
    {
        return mInstance;
    }

    /**
     * Start to transfer the segment data according to the input target class.
     * 
     * @param target : The class of required segment data.
     *            Range: Valid object of Class.
     *            Unit: Class.
     *            Scaling: 1.
     * @param commandSet : The commandSet of request command.
     *            Range: Valid object of commandSet.
     *            Unit: ContinuaCommandSet.
     *            Scaling: 1.
     * 
     * return void [out]: None.
     */
    public void startTransfer(Class<? extends AbstractContinuaSegment> target,
            ContinuaCommandSet commandSet)
    {
        int endPosition = -1;
        int dataSize = -1;
        List<? extends AbstractContinuaSegment> dataToTransfer = null;
        TransferredData data = querySegmentData(target, commandSet
                .getController().getContext());
        
        dataSize = data.mTransferredData.size();

        dataToTransfer = data.mTransferredData.subList(data.mPosition,
                dataSize);

        try
        {
            int transferredCount = target.newInstance()
                    .transferDataToAgent(commandSet, dataToTransfer);
            
            endPosition = data.mPosition + transferredCount;
            
            if (endPosition > dataSize)
            {
                endPosition = dataSize;
                mSegmentDataMap.remove(target);
            }
            else
            {
                // Apply to the coding standard
            }
            
            data.mPosition = endPosition;
        }
        catch (InstantiationException e)
        {
            e.printStackTrace();
        }
        catch (IllegalAccessException e)
        {
            e.printStackTrace();
        }
        finally
        {
            // Apply to the coding standard
        }
    }

    /**
     * Get the required TransferredData from mSegmentDataMap. If no data found,
     * the data will be retrieved from database.
     * 
     * see mSegmentDataMap [in]
     * 
     * @param target : The class of required segment data.
     *            Range: Valid object of Class.
     *            Unit: Class.
     *            Scaling: 1.
     * @param context : The application context.
     *            Range: Valid object of Context.
     *            Unit: Context.
     *            Scaling: 1.
     * 
     * return TransferredData [out]: TransferredData contains the required information.
     *            Range: Valid object of TransferredData.
     *            Unit: TransferredData.
     *            Scaling: 1.
     */
    private TransferredData querySegmentData(
            Class<? extends AbstractContinuaSegment> target, Context context)
    {
        TransferredData result = mSegmentDataMap.get(target);

        if (null == result)
        {
            result = new TransferredData();
            result.mTransferredData = ContinuaModel.getDataByClass(context,
                    target);
            mSegmentDataMap.put(target, result);
        }

        return result;
    }
    
}
