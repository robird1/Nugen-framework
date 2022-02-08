/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.segment.glucose.ControlGlucoseRecord
 * Brief: 
 *
 * Create Date: 2015/10/27
 * $Revision: 24579 $
 * $Author: kevenwu $
 * $Id: ControlGlucoseRecord.java 24579 2015-11-23 03:00:24Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.segment.glucose;

import java.util.Iterator;
import java.util.List;

import org.apache.http.util.ByteArrayBuffer;

import android.database.Cursor;
import android.net.Uri;

import com.accu_chek.solo_m.rcapp.application.bgmcontrol.BgmUtils;
import com.accu_chek.solo_m.rcapp.application.config.ConfigParameter;
import com.accu_chek.solo_m.rcapp.application.config.ReadConfig;
import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaConstants.GlucoseSegmentId;
import com.accu_chek.solo_m.rcapp.application.continua.model.ParseUtils;
import com.accu_chek.solo_m.rcapp.application.continua.segment.AbstractContinuaSegment;
import com.accu_chek.solo_m.rcapp.application.continua.segmenthandler.DataTransferHandler;
import com.accu_chek.solo_m.rcapp.application.continua.typehandler.IContinuaCommandHandler.ContinuaCommand;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyChannel;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.data.nugendata.BGTable;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData;

public class ControlGlucoseRecord extends AbstractContinuaSegment
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
        ByteArrayBuffer buffer = new ByteArrayBuffer(0);
        ByteArrayBuffer dataBuffer = new ByteArrayBuffer(0);
        
        long time = CommonUtils.getOriginValue(mData.getTimestamp().getValueCH1(), 
                mData.getTimestamp().getValueCH2());
        int bg = CommonUtils.getOriginValue(mData.getBgValue().getValueCH1(), 
                mData.getBgValue().getValueCH2());
        
        byte[] segmentId = ParseUtils.parseInt16(GlucoseSegmentId.CONTROL_GLUCOSE);
        byte[] timeInBytes = ParseUtils.convertToRPCAbsTimeFormat(time);
        byte[] bgInBytes = ParseUtils.makeLittleEndian(ParseUtils.parseInt16(bg));
        byte[] index = ParseUtils.parseInt16(mIndex);        
        
        byte[] data = null;
        byte[] length = null;
        
        dataBuffer.append(timeInBytes, 0, timeInBytes.length);
        dataBuffer.append(fillTestFlag(bg, mData.getBgResult()));
        dataBuffer.append(bgInBytes, 0, bgInBytes.length);
        
        data = ParseUtils.appendCRC(dataBuffer.toByteArray()).getByteArray();
        length = ParseUtils.parseInt16(data.length);
        
        buffer.append(segmentId, 0, segmentId.length);
        buffer.append(index, 0, index.length);
        buffer.append(length, 0, length.length); 
        buffer.append(data, 0, data.length);
        
        return ParseUtils.appendCRC(buffer.toByteArray());
    }
    
    /**
     * To recognize the bG HI, LO and hypo, then return the flag of Continua defined. 
     * 
     * @param bg : The bG value of this record.
     *        Range: -2^31 to (2^31)-1
     *        Unit: int
     *        Scaling: 1 
     * @param flag : The result flag of this record.
     *        Range: Valid object of SafetyChannel<Integer>.
     *        Unit: SafetyChannel<Integer>.
     *        Scaling: 1.
     * 
     * return byte [out]: The test flag of this bG record.
     *        Range: -128 to 127
     *        Unit: byte
     *        Scaling: 1
     */
    private byte fillTestFlag(int bg, SafetyChannel<Integer> flag)
    {
        final String KEY_HYPO = ConfigParameter.KEY_HYPO_THRESHOLD_DEFAULT;
        final int INDEX_HI_OF_ME = 3;
        final int INDEX_LO_OF_ME = 2;
        final int INDEX_HI_CONTINUA = 1;
        final int INDEX_LO_CONTINUA = 2;
        final int INDEX_HYPO = 5;
        
        byte result = 0;
        
        SafetyNumber<Integer> hypo = ReadConfig.getIntegerDataByKey(
                new SafetyString(KEY_HYPO, CRCTool.generateCRC16(KEY_HYPO.getBytes())));
        
        SafetyBoolean isLO = BgmUtils.getFlagContentValue(flag, INDEX_LO_OF_ME);
        SafetyBoolean isHI = BgmUtils.getFlagContentValue(flag, INDEX_HI_OF_ME);
        
        if (SafetyBoolean.TRUE.getByte() == isLO.getByte())
        {
            result |= (1 << INDEX_LO_CONTINUA);
        }
        else
        {
            // Apply to coding standard.
        }
        
        if (SafetyBoolean.TRUE.getByte() == isHI.getByte())
        {
            result |= (1 << INDEX_HI_CONTINUA);
        }
        else
        {
            // Apply to coding standard.
        }
        
        if (hypo.get() > bg)
        {
            result |= (1 << INDEX_HYPO);
        }
        else
        {
            // Apply to coding standard.
        }
        
        return result;
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
        Iterator<? extends AbstractContinuaSegment> iterator = data.iterator();
        int count = 0;
        int transferredSize = 0;
        boolean isFinished = !iterator.hasNext();
        
        while (!isFinished)
        {
            AbstractContinuaSegment segment = iterator.next();
            SafetyByteArray result = segment.generateBytes();
            
            isFinished = !iterator.hasNext();
            transferredSize += result.getByteArray().length;
            
            if (DataTransferHandler.APDU_TX_SIZE > transferredSize)
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
        ControlGlucoseRecord data = new ControlGlucoseRecord();
        
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
// (R23982 2015-11-12 05:22:48 kevenwu)
// ----------------------------------------------------------------------------
// Refine BgmUtils function name.
