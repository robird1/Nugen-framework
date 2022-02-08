/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.ContinuaCommandController.segment.BloodGlucose
 * Brief: 
 *
 * Create Date: 2015/3/23
 * $Revision: 23982 $
 * $Author: kevenwu $
 * $Id: BloodGlucose.java 23982 2015-11-12 09:22:48Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.segment.glucose;

import java.util.Iterator;
import java.util.List;

import android.database.Cursor;
import android.net.Uri;

import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaConstants.GlucoseSegmentId;
import com.accu_chek.solo_m.rcapp.application.continua.segment.AbstractContinuaSegment;
import com.accu_chek.solo_m.rcapp.application.continua.segment.SegmentParseUtils;
import com.accu_chek.solo_m.rcapp.application.continua.segmenthandler.DataTransferHandler;
import com.accu_chek.solo_m.rcapp.application.continua.typehandler.IContinuaCommandHandler.ContinuaCommand;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.data.nugendata.BGTable;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData;

/**
 * This class is used to indicate the structure of Continua blood glucose.
 */
public class BloodGlucose extends AbstractContinuaSegment
{   
    /**
     * The data structure from data model.
     */
    private BGTable mData = new BGTable();
    
    /**
     * The index of this data in the database.
     */
    private int mIndex = -1;
    
    /**
     * Return the time stamp of this data.
     *
     * see mData [in]
     *
     * return SafetyNumber<Long> [out]: The time stamp of this data.
     *         Range: Valid object of SafetyNumber.
     *         Unit: Milliseconds.
     *         Scaling: 1.
     */
    @Override
    public SafetyNumber<Long> getTimeStamp()
    {
        SafetyNumber<Long> result = new SafetyNumber<Long>();
        
        long time = CommonUtils.getOriginValue(mData.getTimestamp().getValueCH1(), 
                mData.getTimestamp().getValueCH2());
        
        result.set(time, -time);
        
        return result;
    }
    
    /**
     * Convert the structure to byte array.
     *
     * see mData [in]
     * see mIndex [in]
     *
     * return SafetyByteArray [out]: The byte array of this structure.
     *         Range: Valid object of SafetyByteArray.
     *         Unit: SafetyByteArray.
     *         Scaling: 1.
     */
    @Override
    public SafetyByteArray generateBytes()
    {   
        long time = CommonUtils.getOriginValue(mData.getTimestamp().getValueCH1(), 
                mData.getTimestamp().getValueCH2());
        int bg = CommonUtils.getOriginValue(mData.getBgValue().getValueCH1(), 
                mData.getBgValue().getValueCH2());
        
        return SegmentParseUtils.parseSegmentData(
                GlucoseSegmentId.BLOOD_GLUCOSE, 
                mIndex, 
                time, 
                bg);
    }

    /**
     * Transfer the input data to Agent via 10417 callback function.
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
    @Override
    public int transferDataToAgent(ContinuaCommandSet commandSet,
            List<? extends AbstractContinuaSegment> data)
    {
        final int EACH_SIZE = 10;
        Iterator<? extends AbstractContinuaSegment> iterator = data.iterator();
        int count = 0;
        int transferredSize = 0;
        boolean isFinished = !iterator.hasNext();
        
        while (!isFinished)
        {
            AbstractContinuaSegment segment = iterator.next();
            SafetyByteArray result = segment.generateBytes();
            
            isFinished = !iterator.hasNext();
            transferredSize += EACH_SIZE;
            
            if (DataTransferHandler.APDU_TX_SIZE >= transferredSize)
            {
                commandSet.getController().setSegmentDataOf10417(
                        ContinuaCommand.PM_SEGMENT_ENTRY, result);
                
                count++;
            }
            else
            {
                isFinished = true;
            }
        } 
        
        return count;
    }

    /**
     * Transfer the input segment count data to Agent via 10417 callback function.
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
     * return [out]: None.
     */    
    @Override
    public void transferCountToAgent(ContinuaCommandSet commandSet,
            SafetyByteArray countData)
    {
        commandSet.getController().setSegmentDataOf10417(
                ContinuaCommand.PM_SEGMENT_ENTRY_COUNT, countData);
    }

    /**
     * Put the data from database cursor to this object.
     *
     * @param cursor : The cursor contains the query data. Cursor is provided by Android.
     *        Range: Valid object of Cursor.
     *        Unit: Cursor.
     *        Scaling: 1.
     *        
     * see mIndex [in]
     * see mData [in]        
     *        
     * return IDBData [out]: This object with data from cursor.
     *         Range: Valid object of IDBData.
     *         Unit: IDBData.
     *         Scaling: 1.
     */    
    @Override
    public IDBData onQueryDataFromCursor(Cursor cursor)
    {   
        BloodGlucose data = new BloodGlucose();
        
        data.mIndex = cursor.getPosition();
        data.mData = (BGTable) mData.onQueryDataFromCursor(cursor);
        
        return data;
    }

    /**
     * Return the CRC of this data for checking the data integrity.
     * 
     * see mData [in]
     * 
     * return int [out]: The stored CRC of this data.
     *         Range: CRC 16 value.
     *         Unit: Integer.
     *         Scaling: 1.
     */    
    @Override
    public int getCRC()
    {
        return mData.getCRC();
    }

    /**
     * Return the CRC which is calculated by all necessary value of this data.
     * 
     * see mData [in]
     * 
     * return int [out]: The calculated CRC of this data
     *         Range: CRC 16 value.
     *         Unit: Integer.
     *         Scaling: 1.
     */    
    @Override
    public int generateCRC()
    {        
        return mData.generateCRC();
    }

    /**
     * Return the select type which queries all BG value and sort order by time stamp.
     *
     * return IQuerySelectType [out]: The select type which describes what data shall be queried.
     *         Range: Valid object of IQuerySelectType.
     *         Unit: IQuerySelectType.
     *         Scaling: 1.
     */    
    @Override
    public IQuerySelectType getSelectType()
    {
        return new IQuerySelectType()
        {
            @Override
            public String onSelection()
            {
                return null;
            }

            @Override
            public String[] onSelectionArgs()
            {
                return null;
            }

            @Override
            public String onOrderBy()
            {
                return BGTable.COLUMN_TIMESTAMP.concat(OrderByType.ASC);
            }            
        };
    }

    /**
     * Return the URI of BG table.
     *
     * return Uri [out]: The URI of BG table.
     *         Range: Refer to UrlType.
     *         Unit: Uri.
     *         Scaling: 1.
     */    
    @Override
    public Uri onUri()
    {
        return UrlType.bgUri;
    }
}
