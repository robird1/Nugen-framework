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
 * $Revision: 20555 $
 * $Author: DWYang $
 * $Id: CustSystemProperty.java 20555 2015-10-01 13:50:22Z DWYang $
 */

package com.accu_chek.solo_m.rcapp.application.util;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

public abstract class CustSystemProperty
{
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
    public static void setProperty(String propName, String val)
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
    public static String getProperty(String propName, String defVal)
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
