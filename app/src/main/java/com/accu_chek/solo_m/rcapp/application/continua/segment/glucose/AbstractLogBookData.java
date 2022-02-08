/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.ContinuaCommandController.segment.LogBookData
 * Brief: 
 *
 * Create Date: 2015/4/2
 * $Revision: 24579 $
 * $Author: kevenwu $
 * $Id: AbstractLogBookData.java 24579 2015-11-23 03:00:24Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.segment.glucose;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import android.database.Cursor;
import android.net.Uri;

import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.segment.AbstractContinuaSegment;
import com.accu_chek.solo_m.rcapp.application.continua.segment.glucose.ContextMeal.MealType;
import com.accu_chek.solo_m.rcapp.application.continua.segment.glucose.HealthEvent.HealthEventType;
import com.accu_chek.solo_m.rcapp.application.continua.segmenthandler.DataTransferHandler;
import com.accu_chek.solo_m.rcapp.application.continua.typehandler.IContinuaCommandHandler.ContinuaCommand;
import com.accu_chek.solo_m.rcapp.application.exception.ArgumentErrorException;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyChannel;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.data.nugendata.LogBookTable;
import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData;

/**
 * This class is used to store the log book data.
 */
public abstract class AbstractLogBookData extends AbstractContinuaSegment
{
    /**
     * The data structure of data tier which contains the data of log book table.
     */
    protected LogBookTable mData = new LogBookTable();
    
    /**
     * The index of this data in the database.
     */
    protected int mIndex = -1;
    
    /**
     * The value of BG id.
     */
    protected int mBGId = -1;
    
    /**
     * The value of Bolus id.
     */
    protected int mBolusId = -1;
    
    /**
     * The value of segment id.
     */
    protected int mSegmentId = -1;
    
    /**
     * The value of basal insulin MDI.
     */
    protected int mBasalInsulinMDI = -1;
    
    /**
     * The value of carbohydrate.
     */
    protected int mCarb = -1;
    
    /**
     * The value of time stamp.
     */
    protected long mTime = -1;
    
    /**
     * The value of meal time.
     */
    protected int mMeal = -1;
    
    /**
     * The value of health events.
     */
    protected int mHealthEvent = -1;
    
    /**
     * The value of CRC.
     */
    protected int mCRC = -1;
    
    /**
     * The target data is used to distinguish between different sub class of 
     * this data type.
     *
     * return AbstractLogBookData [out]: The subclass of this data type.
     *         Range: Valid object of AbstractLogBookData.
     *         Unit: AbstractLogBookData.
     *         Scaling: 1.
     */
    public abstract AbstractLogBookData getTargetData();
    
    /**
     * Get the time of this data.
     *
     * see mTime [out]
     *
     * return SafeteyNumber<Long> [out]: The time stamp of this data.
     *        Range: -2^63 to (2^63)-1
     *        Unit: Millisecond.
     *        Scaling: 1.
     */
    @Override
    public SafetyNumber<Long> getTimeStamp()
    {
        return new SafetyNumber<Long>(mTime, -mTime);
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
     * see mIndex [out]
     * see mData [out]
     * see mCarb [out]
     * see mHealthEvent [out]
     * see mMeal [out]
     * see mTime [out]       
     *        
     * return IDBData [out]: This object with data from cursor.
     *         Range: Valid object of IDBData.
     *         Unit: IDBData.
     *         Scaling: 1.
     */    
    @Override
    public IDBData onQueryDataFromCursor(Cursor cursor)
    {
        AbstractLogBookData data = getTargetData();
        
        SafetyChannel<Integer> carb = null;
        SafetyChannel<Integer> meal = null;
        SafetyChannel<Long> time = null;
        SafetyString healthEvent = null;
        
        data.mIndex = cursor.getPosition();
        data.mData = (LogBookTable) mData.onQueryDataFromCursor(cursor);
        
        carb = data.mData.getCarbValue();
        meal = data.mData.getMealtime();
        time = data.mData.getTimestamp();
        healthEvent = data.mData.getHealthEventFlag();
        
                
        data.mHealthEvent = convertHealthEventFromDB(healthEvent);
        data.mCarb = CommonUtils.getOriginValue(carb.getValueCH1(), 
                carb.getValueCH2());
        data.mTime = CommonUtils.getOriginValue(time.getValueCH1(), 
                time.getValueCH2());
        
        try
        {
            data.mMeal = MealType.getMealTypeByValue(CommonUtils.getOriginValue(
                    meal.getValueCH1(), meal.getValueCH2())).getMealType();
        }
        catch (ArgumentErrorException e)
        {
            e.printStackTrace();
        }
        finally
        {
            // Apply to coding standard.
        }
        
        return data;
    }
    
    /**
     * Convert the health events from database to Continua definition.
     *
     * @param events : The value stored in database.
     *        Range: Valid object of SafetyString.
     *        Unit: SafetyString.
     *        Scaling: 1.
     *
     * return int [out]: The value of Continua health event.
     *        Range: Refer to the HealthEventType definition.
     *        Unit: Integer.
     *        Scaling: 1.
     */
    public static int convertHealthEventFromDB(SafetyString events)
    {
        List<HealthEventType> list = new ArrayList<HealthEventType>();
        
        String[] healthEvents = events.getString().split(",");
        
        for (String event : healthEvents)
        {
            try
            {
                list.add(HealthEventType.getEventType(Integer.parseInt(event)));
            }
            catch (NumberFormatException e)
            {
                e.printStackTrace();
            }
            catch (ArgumentErrorException e)
            {
                e.printStackTrace();
            }
            finally
            {
                // Apply to coding standard.
            }
        } 
        
        return HealthEventType.parseHealthEventsForContinua(list);
    }

    /**
     * Return the URI of log book table.
     *
     * return Uri [out]: The URI of log book table.
     *         Range: Refer to UrlType.
     *         Unit: Uri.
     *         Scaling: 1.
     */ 
    @Override
    public Uri onUri()
    {
        return UrlType.logBookUri;
    }

    /**
     * Return the stored CRC value of this data.
     *
     * see mData [in]
     *
     * return int [out]: The stored CRC value.
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
     * Calculate the CRC value for this data and return .
     *
     * see mData [in]
     *
     * return int [out]: The calculated CRC value.
     *         Range: CRC 16 value.
     *         Unit: Integer.
     *         Scaling: 1.
     */  
    @Override
    public int generateCRC()
    {
        return mData.generateCRC();
    }
}
