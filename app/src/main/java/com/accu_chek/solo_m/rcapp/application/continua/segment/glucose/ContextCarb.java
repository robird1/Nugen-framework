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
 * $Id: ContextCarb.java 23531 2015-11-06 09:01:33Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.segment.glucose;

import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaConstants.GlucoseSegmentId;
import com.accu_chek.solo_m.rcapp.application.continua.segment.SegmentParseUtils;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.data.nugendata.LogBookTable;

/**
 * This class is used to indicate the structure of Continua carbohydrate.
 */
public class ContextCarb extends AbstractLogBookData
{    
    /**
     * Convert the structure to byte array.
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
        return SegmentParseUtils.parseSegmentData(
                GlucoseSegmentId.CONTEXT_CARBOHYDRATE, 
                mIndex, 
                mTime, 
                mCarb);
    }
    
    /**
     * Return the select type which queries all Carb value and sort order by time stamp.
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
                return LogBookTable.COLUMN_CARB_VALUE.concat(" IS NOT NULL");
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
        return new ContextCarb();
    }
}
