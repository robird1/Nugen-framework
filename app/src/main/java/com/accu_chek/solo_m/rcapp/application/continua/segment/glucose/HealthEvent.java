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
 * $Revision: 23531 $
 * $Author: kevenwu $
 * $Id: HealthEvent.java 23531 2015-11-06 09:01:33Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.segment.glucose;

import java.util.List;

import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants.CommonConstants;
import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaConstants.GlucoseSegmentId;
import com.accu_chek.solo_m.rcapp.application.continua.segment.SegmentParseUtils;
import com.accu_chek.solo_m.rcapp.application.exception.ArgumentErrorException;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.data.nugendata.LogBookTable;

/**
 * This class is used to indicate the structure of Continua health event.
 */
public class HealthEvent extends AbstractLogBookData
{   
    /**
     * The enumeration of Continua health events type.
     */
    enum HealthEventType
    {
        /**
         * The instance of exercise 1.
         */
        EXERCISE(1 << 15, CommonConstants.HEALTH_EVENT_EXERCISE1),
        
        /**
         * The instance of stress.
         */
        STRESS(1 << 14, CommonConstants.HEALTH_EVENT_STRESS),
        
        /**
         * The instance of illness.
         */
        ILLNESS(1 << 13, CommonConstants.HEALTH_EVENT_ILLNESS),
        
        /**
         * The instance of exercise 2.
         */
        EXERCISE2(1 << 12, CommonConstants.HEALTH_EVENT_EXECRISE2),
        
        /**
         * The instance of premenstrual.
         */
        PREMENSTRUAL(1 << 11, CommonConstants.HEALTH_EVENT_PMS),
        
        /**
         * The instance of custom 1.
         */
        CUSTOM1(1 << 10, CommonConstants.HEALTH_EVENT_CUSTOM1),
        
        /**
         * The instance of custom 2.
         */
        CUSTOM2(1 << 9, CommonConstants.HEALTH_EVENT_CUSTOM2),
        
        /**
         * The instance of custom 3.
         */
        CUSTOM3(1 << 8, CommonConstants.HEALTH_EVENT_CUSTOM3),
        
        /**
         * The instance of no input.
         */
        NO_ENTRY(0, CommonConstants.HEALTH_EVENT_NO_VALUE);
        
        /**
         * The bit position of the health event item.
         */
        private final int mValue;
        
        /**
         * The health event value which is stored in DB.
         */
        private final int mValueInDB;
                
        /**
         * Put the health event type value of Continua and corresponding value in database.
         * 
         * @param value : The type value defined by Continua.
         *        Range: Refer to the definition of HealthEventType.
         *        Unit: Integer.
         *        Scaling: 1.
         * @param valueInDB : The type value defined by device.
         *        Range: Refer to the definition of HealthEventType.
         *        Unit: Integer.
         *        Scaling: 1.
         */
        private HealthEventType(int value, int valueInDB)
        {
            mValue = value;
            mValueInDB = valueInDB;
        }
        
        /**
         * Return the bit position of this item.
         *
         * see mBitPosition [in]
         *
         * return int [out]: The bit position.
         *         Range: Refer to the definition of this enumeration.
         *         Unit: Integer.
         *         Scaling: 1.
         */
        public int getValue()
        {
            return mValue;
        }
        
        /**
         * Return the value in DB of this item.
         *
         * see mValueInDB [in]
         * 
         * return int [out]: The value of this item.
         *        Range: Refer to the definition of this enumeration.
         *        Unit: Integer.
         *        Scaling: 1.
         */
        public int getValueInDB()
        {
            return mValueInDB;
        }
        
        /**
         * Return the instance according to the input 
         *
         * @param valueInDB : The input value for get the corresponding item.
         *        Range: -2^31 to (2^31)-1.
         *        Unit: Integer.
         *        Scaling: 1.
         *        
         * return HealthEventType [out]: The item with input value in definition.
         *        Range: Valid object of HealthEventType.
         *        Unit: HealthEventType.
         *        Scaling: 1.
         * 
         * throws ArgumentErrorException if the value is not supported.
         */
        public static HealthEventType getEventType(int valueInDB) throws ArgumentErrorException
        {
            HealthEventType result = null;
            
            for (HealthEventType type : HealthEventType.values())
            {
                int valueOfType = type.getValueInDB();
                
                if (valueOfType == valueInDB)
                {
                    result = type;
                }
            }
            
            if (null == result)
            {
                throw new ArgumentErrorException();
            }
            
            return result;
        }
        
        /**
         * Convert the HealthEventType to the value which is defined by Continua.
         *
         * @param types : The input health event type.
         *        Range: Valid object of HealthEventType.
         *        Unit: HealthEventType.
         *        Scaling: 1.
         *        
         * return int [out]: The value which contains all input health event type.
         *        Range: -2^31 to (2^31)-1.
         *        Unit: Integer.
         *        Scaling: 1.
         */
        public static int parseHealthEventsForContinua(List<HealthEventType> types)
        {
            int nResult = 0;
            
            for (HealthEventType event : types)
            {
                nResult |= event.getValue();
            }
            
            return nResult;
        }
    }
    
    /**
     * Convert the structure to byte array.
     *
     * see mIndex [in]
     * see mTime [in]
     * see mHealthEvent [in]
     *
     * return SafetyByteArray [out]: The byte array of this structure.
     *         Range: Valid object of SafetyByteArray.
     *         Unit: SafetyByteArray.
     *         Scaling: 1.
     */
    @Override
    public SafetyByteArray generateBytes()
    {    
        return SegmentParseUtils.parseSegmentData(
                GlucoseSegmentId.HEALTH_EVENTS, 
                mIndex, 
                mTime, 
                mHealthEvent);
    }
    
    /**
     * Return the select type which queries all health event value and sort order by time stamp.
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
                return LogBookTable.COLUMN_HEALTH_EVENT_FALGS.concat(" IS NOT NULL");
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
     *         Range: Valid object of HealthEvent.
     *         Unit: HealthEvent.
     *         Scaling: 1.
     */
    @Override
    public AbstractLogBookData getTargetData()
    {
        return new HealthEvent();
    }
}
