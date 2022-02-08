/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.ContinuaCommandController.model.ContinuaModel
 * Brief: 
 *
 * Create Date: 2015/3/19
 * $Revision: 20933 $
 * $Author: kevenwu $
 * $Id: ContinuaModel.java 20933 2015-10-05 08:34:57Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.model;

import java.util.LinkedList;
import java.util.List;

import android.content.Context;

import com.accu_chek.solo_m.rcapp.application.continua.segment.AbstractContinuaSegment;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.data.operationhandler.QueryHandler;

/**
 * This class is used to provide some kind of function for query database.
 */
public class ContinuaModel
{
    /**
     * Get target data (T) from database according to the selection type of 
     * target data.
     *
     * @param context : The application context. Context is provided by Android.
     *        Range: Valid object of Context.
     *        Unit: Context.
     *        Scaling: 1.
     * @param clazz : The target type data. It should be a class that extends AbstractContinuaSegment.
     *        Range: The data should extend AbstractContinuaSegment.
     *        Unit: Class.
     *        Scaling: 1.
     * 
     * return List<T> [out]: The list contains all target data. Empty list if no record is found.
     *         Range: Valid object of List.
     *         Unit: List.
     *         Scaling: 1.
     */
    public static <T extends AbstractContinuaSegment> List<T> getDataByClass( 
            Context context, Class<T> clazz)
    {   
        List<T> result = null;
        T target = null;
        
        CommonUtils.objectCheck(context, clazz);
        
        try
        {
            target = clazz.newInstance();
            
            target.setQuerySelectTypeInterface(target.getSelectType());
            
            result = new QueryHandler<T>(context, null, target).start();
        }
        catch (IllegalAccessException exception)
        {
            exception.printStackTrace();
        }
        catch (InstantiationException exception)
        {
            exception.printStackTrace();
        }
        finally
        {
            // do nothing
        }
        
        if (null == result)
        {
            result = new LinkedList<T>();
        }
        else
        {
            // Apply to coding rule.
        }
        
        return result;
    }
}
