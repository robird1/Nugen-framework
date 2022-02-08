/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 * 
 * Class name: com.accu_chek.solo_m.rcapp.application.util.
 * CustSystemProperty
 * Brief:
 * 
 * Create Date: 2015/5/5
 * $Revision: 22984 $
 * $Author: AdamChen $
 * $Id: RCSystemProperty.java 22984 2015-11-02 03:15:07Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.util;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import com.accu_chek.solo_m.rcapp.application.util.IRCSystemPeoperty;

import android.content.Context;

public class RCSystemProperty extends IRCSystemPeoperty.Stub
{
    
    private Context mContext = null;
    
    public RCSystemProperty(Context context)
    {
        mContext = context;
    }
    
    
    /**
     * Set the value of a particular system property.
     * 
     * param propName [in] The name of the particular property
     * Range: Vaild String
     * Unit: String
     * Scaling: 1
     * param val [in] The value to be set for the particular property
     * Range: Vaild String
     * Unit: String
     * Scaling: 1
     */
    public void setProperty(String propName, String val)
    {
        // Reflection API: android.os.SystemProperties.set
        Class<?> clazz = null;
        String className = "android.os.SystemProperties";
        Method method = null;
        String methodName = "set";

        try
        {
            clazz = Class.forName(className);
            method = clazz.getDeclaredMethod(methodName, String.class,
                    String.class);
            method.setAccessible(true);
            method.invoke(null, propName, val);
        }
        catch (ClassNotFoundException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        catch (NoSuchMethodException e)
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
        catch (InvocationTargetException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        finally
        {
            // Apply to the coding standard
        }

    }

    /**
     * Get the value of a particular system property.
     * If the value of the particular property does not exist, then return the
     * default value.
     * Note: Be careful that it will not set the default value to the particular
     * property if the particular property does not exist.
     * 
     * param propName [in] The name of the particular property
     * Range: Vaild String
     * Unit: String
     * Scaling: 1
     * param defVal [in] The default value to be returned if the value of the
     * particular property does not existed
     * Range: Vaild String
     * Unit: String
     * Scaling: 1
     * return val [out] The value of the particular property to be returned
     * Range: Vaild String
     * Unit: String
     * Scaling: 1
     */
    public String getProperty(String propName, String defVal)
    {
        // Reflection API: android.os.SystemProperties.get
        Class<?> clazz = null;
        String className = "android.os.SystemProperties";
        Method method = null;
        String methodName = "get";
        String val = "";

        try
        {
            clazz = Class.forName(className);
            method = clazz.getDeclaredMethod(methodName, String.class,
                    String.class);
            method.setAccessible(true);
            val = (String) method.invoke(null, propName, defVal);
        }
        catch (ClassNotFoundException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        catch (NoSuchMethodException e)
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
        catch (InvocationTargetException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        finally
        {
            // Apply to the coding standard
        }

        return val;
    }
}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
// (R19842 2015-09-25 05:33:00 AdamChen)
// ----------------------------------------------------------------------------
// ¦s¨ú³Q©Ú¡C
