/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.segment.glucose.NoteRecord
 * Brief: 
 *
 * Create Date: 2015/10/27
 * $Revision: $
 * $Author: $
 * $Id: $
 */

package com.accu_chek.solo_m.rcapp.application.continua.segment.glucose;

import java.util.Iterator;
import java.util.List;

import org.apache.http.util.ByteArrayBuffer;

import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaConstants.GlucoseSegmentId;
import com.accu_chek.solo_m.rcapp.application.continua.model.ParseUtils;
import com.accu_chek.solo_m.rcapp.application.continua.segment.AbstractContinuaSegment;
import com.accu_chek.solo_m.rcapp.application.continua.segmenthandler.DataTransferHandler;
import com.accu_chek.solo_m.rcapp.application.continua.typehandler.IContinuaCommandHandler.ContinuaCommand;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyChannel;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.data.nugendata.LogBookTable;

public class NoteRecord extends AbstractLogBookData
{

    /**
     * Convert the note record structure to byte array as Continua defined.
     *
     * see mIndex [in]
     * see mTime [in]
     * see mCarb [in]
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
        
        SafetyChannel<Integer> recordId = mData.getRecordId();
                
        byte[] segmentId = ParseUtils.parseInt16(GlucoseSegmentId.NOTE_RECORD);
        byte[] indexInBytes = ParseUtils.parseInt16(mIndex);
        byte[] recordIdInBytes = CRCTool.getBytes(CommonUtils.getOriginValue(
                recordId.getValueCH1(), recordId.getValueCH2()));
        byte[] noteInBytes = mData.getNote().getString().getBytes();

        byte[] data = null;
        byte[] length = null;
        
        buffer.append(recordIdInBytes, 0, recordIdInBytes.length);
        buffer.append(noteInBytes, 0, noteInBytes.length);
        
        data = ParseUtils.appendCRC(buffer.toByteArray()).getByteArray();
        length = ParseUtils.parseInt16(data.length);
        
        buffer.clear();
        
        buffer.append(segmentId, 0, segmentId.length);
        buffer.append(indexInBytes, 0, indexInBytes.length);
        buffer.append(length, 0, length.length);
        buffer.append(data, 0, data.length);
        
        return ParseUtils.appendCRC(buffer.toByteArray());
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
     * Return the select type which queries all note record and sort order by time stamp.
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
                return LogBookTable.COLUMN_NOTE.concat(" IS NOT NULL");
            }

            @Override
            public String[] onSelectionArgs()
            {
                return null;
            }

            @Override
            public String onOrderBy()
            {
                return LogBookTable.COLUMN_TIMESTAMP_DB.concat(OrderByType.ASC);
            }
        };
    }
    
    /**
     * Return current instance of AbstractLogBookData.
     * 
     * return AbstractLogBookData [out]: The instance of this object.
     *         Range: Valid object of ContextCarb.
     *         Unit: ContextCarb.
     *         Scaling: 1.
     */
    @Override
    public AbstractLogBookData getTargetData()
    {
        return new NoteRecord();
    }

}
