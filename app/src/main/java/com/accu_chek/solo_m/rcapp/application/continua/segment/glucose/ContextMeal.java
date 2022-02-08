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
 * $Id: ContextMeal.java 23531 2015-11-06 09:01:33Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.segment.glucose;

import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants.CommonConstants;
import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaConstants.GlucoseSegmentId;
import com.accu_chek.solo_m.rcapp.application.continua.segment.SegmentParseUtils;
import com.accu_chek.solo_m.rcapp.application.exception.ArgumentErrorException;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.data.nugendata.LogBookTable;

/**
 * This class is used to indicate the structure of Continua meal.
 */
public class ContextMeal extends AbstractLogBookData
{
    /**
     * The enumeration of Continua meal time type.
     */
    enum MealType
    {
        /**
         * The instance of meal preprandial.
         */
        MEAL_PREPRANDIAL(29260, CommonConstants.MEAL_TIME_BEFORE_MEAL),
        
        /**
         * The instance of meal postprandial.
         */
        MEAL_POSTPRANDIAL(29264, CommonConstants.MEAL_TIME_AFTER_MEAL),
        
        /**
         * The instance of meal fasting.
         */
        MEAL_FASTING(29268, CommonConstants.MEAL_TIME_FASTING),
        
        /**
         * The instance of meal causal.
         */
        MEAL_CASUAL(29272, CommonConstants.MEAL_TIME_OTHER),
        
        /**
         * The instance of meal bad time.
         */
        MEAL_BEDTIME(29300, CommonConstants.MEAL_TIME_BEDTIME);
        
        /**
         * The value of Continua meal type.
         */
        private final int mMealType;
        
        /**
         * The value of meal type in database.
         */
        private final int mTypeInDB;
        
        /**
         * Put the meal type value of Continua and corresponding value of database.
         * 
         * @param mealType : The value which is defined by Continua.
         *        Range: Refer to the definition of MealType.
         *        Unit: Integer.
         *        Scaling: 1.
         * @param typeInDB : The value which is defined by device.
         *        Range: Refer to the definition of MealType.
         *        Unit: Integer.
         *        Scaling: 1.
         */
        private MealType(int mealType, int typeInDB)
        {
            mMealType = mealType;
            mTypeInDB = typeInDB;
        }
        
        /**
         * Return the value stored in database.
         *
         * return int [out]: The meal type value.
         *        Range: Refer to the definition of this enumeration.
         *        Unit: Integer.
         *        Scaling: 1.
         */
        public int getTypeInDB()
        {
            return mTypeInDB;
        }
        
        /**
         * Return the value defined in Continua.
         *
         * return int [out]: The meal type value.
         *         Range: Refer to the definition of this enumeration.
         *         Unit: Integer.
         *         Scaling: 1.
         */
        public int getMealType()
        {
            return mMealType;
        }
        
        /**
         * Return the MealType according to the input value.
         *
         * @param typeInDB : The value stored in database.
         *        Range: -2^31 to (2^31)-1.
         *        Unit: Integer.
         *        Scaling: 1.
         *        
         * return MealType [out]: The MealType with the input value.
         *        Range: Valid object of MealType.
         *        Unit: MealType.
         *        Scaling: 1.
         *        
         * throws ArgumentErrorException if the value is not supported.
         */
        public static MealType getMealTypeByValue(int typeInDB) throws ArgumentErrorException
        {
            MealType result = null;
            
            for (MealType meal : MealType.values())
            {
                int valueOfType = meal.getTypeInDB();
                
                if (valueOfType == typeInDB)
                {
                    result = meal;
                }
            }
            
            if (null == result)
            {
                throw new ArgumentErrorException();
            }
            
            return result;
        }
    }
    
    /**
     * Convert the structure to byte array.
     *
     * see mIndex [in]
     * see mTime [in]
     * see mMeal [in]
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
                GlucoseSegmentId.CONTEXT_MEAL, 
                mIndex, 
                mTime,
                mMeal);
    }
    
    /**
     * Return current instance of AbstractLogBookData.
     * 
     * return AbstractLogBookData [out]: The instance of this object.
     *         Range: Valid object of ContextMeal.
     *         Unit: ContextMeal.
     *         Scaling: 1.
     */
    @Override
    public AbstractLogBookData getTargetData()
    {
        return new ContextMeal();
    }
    
    /**
     * Return the select type which queries all meal value and sort order by time stamp.
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
                return LogBookTable.COLUMN_MEAL_TIME.concat(" IS NOT NULL");
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
}