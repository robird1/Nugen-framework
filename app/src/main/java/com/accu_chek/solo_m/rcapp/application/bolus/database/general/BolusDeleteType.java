/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.bolus.database.BolusDeleteType
 * Brief: 
 *
 * Create Date: 2015¦~7¤ë30¤é
 * $Revision: 20521 $
 * $Author: DWYang $
 * $Id: BolusDeleteType.java 20521 2015-10-01 11:09:05Z DWYang $
 */

package com.accu_chek.solo_m.rcapp.application.bolus.database.general;

import java.lang.ref.SoftReference;
import java.util.ArrayList;

import com.accu_chek.solo_m.rcapp.data.operationhandler.IDBData.IDeleteSelectType;

public class BolusDeleteType implements IDeleteSelectType
{

    private String mSelection = null;
    
    private ArrayList<String> mStrArgs = null;
    
    public BolusDeleteType()
    {
        SoftReference<ArrayList<String>> ref = null;
        ref = new SoftReference< ArrayList<String>>(new ArrayList<String>());
        mStrArgs = ref.get();
    }
    
    /**
     * 
     * Set select type for delete fucntion
     *
     * param str [in]: The valid select type string
     *       Range: The valid selct type string in SelectionType.
     *       Unit: String.
     *       Scale: 1.
     */
    public void setSelection(String str)
    {
        mSelection = str;
    }
    
    /**
     * 
     * Set select argument for delete function.
     *
     * param arg [in]: The valid select argument integer data.
     *       Range: 0 ... 5000.
     *       Unit: Integer.
     *       Scale: 1.
     */
    public void setSelectionArgs(long arg)
    {
        String argString = String.valueOf(arg);
        mStrArgs.add(argString);
    }
    
    /**
     * 
     * Return select type to do Update function.
     *
     * return String [out]: The valid selct type String.
     *        Range: The valid selct type String in SelectionType.
     *        Unit: String.
     *        Scale: 1.
     */  
    @Override
    public String onSelection()
    {
        
        return mSelection;
    }
    
    /**
     * 
     * Return select argument to do Update function.
     *
     * return String[] [out]: The valid select argument string.
     *        Range: Valid select argument string.
     *        Unit: String[].
     *        Scale: 1.
     */
    @Override
    public String[] onSelectionArgs()
    {
        String[] args = new String[mStrArgs.size()];
        args = mStrArgs.toArray(args);
        return args;
    }

}
// (R20521 2015-10-01 07:09:05 DWYang)
// ----------------------------------------------------------------------------
// [NSM-2889] Rearrange packages
