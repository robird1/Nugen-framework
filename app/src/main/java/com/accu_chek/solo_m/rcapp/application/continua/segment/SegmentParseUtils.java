/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.segment.SegmentParseUtils
 * Brief: 
 *
 * Create Date: 2015/7/30
 * $Revision: 23982 $
 * $Author: kevenwu $
 * $Id: SegmentParseUtils.java 23982 2015-11-12 09:22:48Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.segment;

import org.apache.http.util.ByteArrayBuffer;

import com.accu_chek.solo_m.rcapp.application.continua.model.ParseUtils;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;

public class SegmentParseUtils
{    
    
    /**
     * Parse the input data to segment data structure. The structure is 
     * {segmentId(2 bytes), index(2 bytes), time(8 bytes), value(2 bytes)}
     *
     * @param segmentId : The target segment id.
     *        Range: Valid value of segment id.
     *        Unit: Integer.
     *        Scaling: 1.
     * @param index : The index value.
     *        Range: -2^31 to (2^31)-1.
     *        Unit: Integer.
     *        Scaling: 1.
     * @param time : The time value.
     *        Range: -2^63 to (2^63)-1.
     *        Unit: millisecond.
     *        Scaling: 1.
     * @param value : The target data value.
     *        Range: -2^31 to (2^31)-1.
     *        Unit: Integer.
     *        Scaling: 1.
     *        
     * return SafetyByteArray [out]: The data in the segment data structure.
     *         Range: Valid object of SafetyByteArray.
     *         Unit: SafetyByteArray.
     *         Scaling: 1.
     */
    public static SafetyByteArray parseSegmentData(int segmentId, int index, 
            long time, int value)
    {
        ByteArrayBuffer buffer = new ByteArrayBuffer(0);
        
        byte[] segmentIdInBytes = ParseUtils.parseInt16(segmentId);
        byte[] indexInBytes = ParseUtils.parseInt16(index);
        byte[] timeInBytes = ParseUtils.parseAbsoluteTime(time);
        byte[] valueInBytes = ParseUtils.parseInt16(value);
                
        buffer.append(segmentIdInBytes, 0, segmentIdInBytes.length);
        buffer.append(indexInBytes, 0, indexInBytes.length);
        buffer.append(timeInBytes, 0, timeInBytes.length);
        buffer.append(valueInBytes, 0, valueInBytes.length);
                
        return ParseUtils.appendCRC(buffer.toByteArray());
    }
    
    /**
     * Parse the input data to segment count data structure. The structure is
     * {start time(8 bytes), end time(8 bytes), total count(2 bytes)}.
     *
     * @param startTime : The input start time value.
     *        Range: -2^63 to (2^63)-1.
     *        Unit: Millisecond.
     *        Scaling: 1.
     * @param endTime : The input end time value.
     *        Range: -2^63 to (2^63)-1.
     *        Unit: Millisecond.
     *        Scaling: 1.
     * @param count : The total count value.
     *        Range: -2^31 to (2^31)-1.
     *        Unit: Integer.
     *        Scaling: 1.
     *        
     * return SafetyByteArray [out]: The data in segment count structure.
     *         Range: Valid object of SafetyByteArray.
     *         Unit: SafetyByteArray.
     *         Scaling: 1.
     */
    public static SafetyByteArray parseSegmentCountData(int segmentId, 
            long startTime, long endTime, int count)
    {       
        ByteArrayBuffer buffer = new ByteArrayBuffer(0);
        
        byte[] idInBytes = ParseUtils.parseInt16(segmentId);
        byte[] startTimeInBytes = ParseUtils.parseAbsoluteTime(startTime);
        byte[] endTimeInBytes = ParseUtils.parseAbsoluteTime(endTime);
        byte[] countInBytes = ParseUtils.parseInt16(count);
        
        buffer.append(idInBytes, 0, idInBytes.length);
        buffer.append(startTimeInBytes, 0, startTimeInBytes.length);
        buffer.append(endTimeInBytes, 0, endTimeInBytes.length);
        buffer.append(countInBytes, 0, countInBytes.length);
                        
        return ParseUtils.appendCRC(buffer.toByteArray());
    }
    
}
