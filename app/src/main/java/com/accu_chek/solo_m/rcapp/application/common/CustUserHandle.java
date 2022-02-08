/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: CustUserHandle
 * Brief: Used for senbroadCastasUser function.
 *
 * Create Date: 9/17/2015
 * $Revision: 18253 $
 * $Author: AdamChen $
 * $Id: CustUserHandle.java 18253 2015-09-17 08:55:45Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.common;

import java.lang.reflect.Field;

import android.os.UserHandle;

public class CustUserHandle
{
    private static Class<?> mClass = null;
    
    static
    {
        try
        {
            mClass = Class.forName("android.os.UserHandle");
        }
        catch (ClassNotFoundException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }
    
    
    public static UserHandle getALL()
    {
        UserHandle all = null;
        
        try
        {
            Field field_all = mClass.getField("ALL");
            all = (UserHandle) field_all.get(null);
        }
        catch (NoSuchFieldException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        catch (IllegalArgumentException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        catch (IllegalAccessException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        
        return all;
    }
}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
 
